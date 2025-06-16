#' Intuitive Iteration with Clean Syntax
#'
#' The \code{fly} function provides an intuitive way to iterate over data
#' objects with clean, readable syntax. It supports various input types
#' including lists, vectors, matrices, and data frames, with optional parallel
#' processing and result simplification.
#'
#' @param X The data object to iterate over. Can be a list, vector, matrix,
#'   array, or data frame.
#' @param expr An expression or function to execute for each element. Can be a
#'   code block in braces, a function name, or a function object.
#' @param ... Additional arguments passed to the function when \code{expr} is a
#'   function, or made available as variables when \code{expr} is an expression.
#' @param .var Name of the variable in which each input element is stored. Default is
#'   \code{".x"}.
#' @param .margin For matrix/array inputs, specifies whether to iterate over
#'   rows (1) or columns (2). Default is 1 (rows).
#' @param .parallel Optional parallel processing. Either a number specifying the
#'   number of cores to use, or a cluster object from the \code{parallel}
#'   package. Default is \code{NULL} (sequential processing).
#' @param .index_var Name of the variable in which the current iteration index is stored.
#'   Default is \code{".i"}. Set to \code{NULL} or empty string to disable.
#' @param simplify Logical or character string; should the result be simplified
#'   to a vector, matrix or higher dimensional array if possible? For
#'   \code{sapply} it must be named "array". Default is \code{FALSE} (always
#'   return a list).
#'
#' @return By default, returns a list. When \code{simplify = TRUE}, attempts to
#'   simplify to a vector or array like \code{sapply}. Names from the original
#'   data object are preserved when present.
#'
#' @export
fly <- function(X, expr, ..., .var = ".x", .margin = 1, .parallel = NULL, 
								.index_var = ".i", simplify = FALSE) {
	
	# Handle different input types and convert to list format.
	X_names = NULL
	if (is.matrix(X) || is.array(X)) {
		if (.margin == 1) {
			# Iterate over rows.
			X_names <- rownames(X)
			X <- as.list(as.data.frame(t(X)))
		} else if (.margin == 2) {
			# Iterate over columns.
			X_names <- colnames(X)
			X <- as.list(as.data.frame(X))
		} else {
			stop(".margin must be 1 (rows) or 2 (columns) for matrices.")
		}
	} else if (is.data.frame(X)) {
		# Handle data.frame - convert to list of columns (like lapply default).
		X_names <- names(X)
		X <- as.list(X)
	} else if (is.vector(X) && !is.list(X)) {
		# Handle atomic vectors - convert to list.
		X_names <- names(X)
		X <- as.list(X)
	} else if (is.list(X)) {
		# Already a list - use as is.
		X_names <- names(X)
	} else {
		# Try to convert to list for other types.
		X <- as.list(X)
		X_names <- names(X)
	}
	
	# Clean up potentially massive raw data ghosts.
	if(object.size(X) > 1e6) gc(verbose = FALSE)
	
	#-----------------------------------
	# Capture the calling environment for proper scoping
	calling_env <- parent.frame()
	
	# Classify the expression and capture it.
	expr_sub <- substitute(expr)
	is_fn <- tryCatch(exists(as.character(expr_sub), mode = "function"), 
										error = function(e) FALSE, warning = function(e) FALSE)
	
	if (!is_fn) {
		expr_deparsed <- deparse(substitute(expr))
		is_code <- tryCatch(grepl("\\{", expr_deparsed[1]), 
												error = function(e) FALSE, warning = function(e) FALSE)
	}
	
	# Capture additional arguments.
	arguments <- list(...)
	
	# Create the worker function with proper environment handling.
	if (is_fn) {
		# For function calls, create a wrapper that preserves the calling environment.
		worker_func <- function(worker_item) {
			item_data <- worker_item[[1]]
			item_index <- worker_item[[2]]
			
			# Get the function from the calling environment.
			fn <- get(as.character(expr_sub), envir = calling_env, mode = "function")
			
			# Call with additional arguments.
			do.call(fn, c(list(item_data), arguments))
		}
		# Set the environment of worker_func to include the calling environment.
		environment(worker_func) <- list2env(list(
			expr_sub = expr_sub,
			calling_env = calling_env,
			arguments = arguments
		), parent = calling_env)
		
	} else if (is_code) {
		# For code blocks, create a function that evaluates in the calling environment.
		worker_func <- function(worker_item) {
			item_data <- worker_item[[1]]
			item_index <- worker_item[[2]]
			
			# Create a new environment that inherits from the calling environment.
			eval_env <- new.env(parent = calling_env)
			
			# Set up the iteration variables.
			assign(.var, item_data, envir = eval_env)
			if (!is.null(.index_var) && .index_var != "") {
				assign(.index_var, item_index, envir = eval_env)
			}
			
			# Add any additional arguments to the environment.
			if (length(arguments) > 0) {
				for (name in names(arguments)) {
					assign(name, arguments[[name]], envir = eval_env)
				}
			}
			
			# Evaluate the expression in this environment.
			eval(expr_sub, envir = eval_env)
		}
		# Set the environment to preserve access to calling environment.
		environment(worker_func) <- list2env(list(
			expr_sub = expr_sub,
			calling_env = calling_env,
			arguments = arguments,
			.var = .var,
			.index_var = .index_var
		), parent = calling_env)
		
	} else {
		stop("The expression is not a valid function or code block.")
	}
	
	# Prepare worker data
	worker_data <- mapply(
		function(x, i) list(data = x, index = i), 
		X, 
		seq_along(X), 
		SIMPLIFY = FALSE
	)
	
	#-----------------------------------
	if (!is.null(.parallel)) {
		if(.parallel == TRUE || .parallel == -1) { # Alternative codes for 'all cores'.
			.parallel <- parallel::detectCores()
		}
		# Parallel execution.
		if (!requireNamespace("parallel", quietly = TRUE)) {
			stop("The `parallel` package is required for parallel execution.")
		}
		
		if (is.numeric(.parallel)) {
			# For parallel execution, we need to export the calling environment.
			# Use FORK cluster on Unix-like systems for better environment sharing.
			if (.Platform$OS.type == "unix") {
				cl <- parallel::makeForkCluster(.parallel)
			} else {
				# On Windows, create PSOCK cluster and export necessary objects.
				cl <- parallel::makeCluster(.parallel)
				
				# Export all objects from the calling environment.
				if (is_code) {
					# Extract variables used in the expression, excluding iteration variables
					exclude <- c(.var, .index_var, .margin, .parallel, names(arguments))
					used_vars <- extract_variables(expr_sub, exclude_vars = exclude)
					
					# Filter to only variables that actually exist in the calling environment
					existing_vars <- used_vars[sapply(used_vars, function(v) exists(v, envir = calling_env))]
					
					# Export only the variables that are actually used and exist
					if (length(existing_vars) > 0) {
						parallel::clusterExport(cl, existing_vars, envir = calling_env)
					}
				} else if (is_fn) {
					# For function calls, just export the function itself
					func_name <- as.character(expr_sub)
					if (exists(func_name, envir = calling_env)) {
						parallel::clusterExport(cl, func_name, envir = calling_env)
					}
				}
				
				# Also export any packages loaded in the calling environment
				search_paths <- search()
				pkg_paths <- search_paths[grepl("^package:", search_paths)]
				if (length(pkg_paths) > 0) {
					pkg_names <- sub("^package:", "", pkg_paths)
					for (pkg in pkg_names) {
						if (pkg != "base") {
							try(parallel::clusterEvalQ(cl, library(pkg, character.only = TRUE)), 
									silent = TRUE)
						}
					}
				}
			}
		} else if (inherits(.parallel, "cluster")) {
			# Use provided cluster.
			cl <- .parallel
		} else {
			stop(".parallel must be a number of cores or a cluster object.")
		}
		
		# Run in parallel
		result <- parallel::parLapply(cl, worker_data, worker_func)
		
		if (is.numeric(.parallel)) {
			parallel::stopCluster(cl)
		}
	} else {
		# Sequential execution
		result <- lapply(worker_data, worker_func)
	}
	
	# Always ensure result is a proper list (even if elements are NULL).
	if (!is.list(result)) {
		result <- as.list(result)
	}
	if(object.size(X) > 1e6) {
		rm(X)
		gc(verbose = FALSE)
	}
	
	# Preserve names from original data object.
	names(result) <- X_names
	
	# Apply simplification if requested (using sapply logic).
	if (simplify != FALSE) {
		result <- simplify2array(result, higher = (simplify == "array"))
	}
	
	return(result)
}
