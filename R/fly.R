#' Intuitive Iteration with Clean Syntax
#'
#' The \code{fly} function provides an intuitive way to iterate over data
#' objects with clean, readable syntax. It supports various input types
#' including lists, vectors, matrices, and data frames, with optional parallel
#' processing and result simplification.
#'
#' \code{fly} is essentially a wrapper for \code{lapply} but which conbines the
#' simplicity of a \code{for} loop while retaining the flexibilty and power of
#' \code{lapply}. The basic syntax is intuitive and requires much less typing
#' than \code{lapply}---especially for typical use cases.
#' 
#' Some handy features are included:
#' - Custom functions do not need the \code{function} keyword, just insert the body of the function as a code block.
#' - The variable holding the data for each iteration does not need to be declared. By default it is ".x".
#' - The index of the iteration is automatically made available as a variable. By default called ".i".
#' - Element names (from the input list or matrix) are used to name the output list.
#' - Output is always a list (including empty elements) so it will always align with the input.
#' - Parallelisation is built-in. No need to interface with the \code{parLapply} syntax directly. The same code will work sequentially or in parallel by just changing one parameter.
#' - Lists and matrices use the same function.
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
#' @details The \code{fly} function supports several input types:
#' \itemize{
#'   \item \strong{Lists}: Iterates over list elements
#'   \item \strong{Vectors}: Converts to list and iterates over elements
#'   \item \strong{Matrices/Arrays}: Iterates over rows or columns based on \code{.margin}
#'   \item \strong{Data frames}: Iterates over columns (like \code{lapply})
#' }
#'
#' The function can execute either expressions (code blocks) or functions:
#' \itemize{
#'   \item \strong{Expressions}: Use braces \code{\{...\}} to write code that has
#'     access to the iteration variable (default \code{.x}) and any additional arguments
#'   \item \strong{Functions}: Pass function names or objects that will be called
#'     with each element plus any additional arguments
#' }
#'
#' The \code{simplify} parameter works exactly like in \code{sapply}:
#' \itemize{
#'   \item \code{FALSE}: Always return a list (default)
#'   \item \code{TRUE}: Simplify to vector/matrix when possible
#'   \item \code{"array"}: Simplify to array when possible
#' }
#'
#' @examples
#' # Basic list iteration
#' data <- list(a = 1:4, b = 5:8)
#' result <- fly(data, sum)
#' print(result)
#'
#' # Using expressions with default .x variable
#' result <- fly(data, { length(.x) })
#' print(result)
#'
#' # With additional arguments
#' text_data <- list(a = c("hello", "world"), b = c("foo", "bar"))
#' result <- fly(text_data, { paste(.x, collapse = sep) }, sep = "-")
#' print(result)
#'
#' # Matrix iteration by rows (default)
#' mat <- matrix(1:12, nrow = 3)
#' rownames(mat) <- paste0("row", 1:3)
#' result <- fly(mat, sum)
#' print(result)
#'
#' # Matrix iteration by columns
#' result <- fly(mat, mean, .margin = 2)
#' print(result)
#'
#' # Custom variable name
#' result <- fly(data, { row_sum <- sum(row_data); row_sum^2 }, .var = "row_data")
#' print(result)
#'
#' # Accessing iteration index
#' result <- fly(data, { paste("Item", .i, "sum:", sum(.x)) })
#' print(result)
#'
#' # Simplify results
#' numbers <- list(a = 1:3, b = 4:6, c = 7:9)
#' result_list <- fly(numbers, sum)                    # Returns list
#' result_vector <- fly(numbers, sum, simplify = TRUE) # Returns named vector
#' print(result_list)
#' print(result_vector)
#'
#' # Parallel processing
#' \dontrun{
#' large_list <- as.list(1:100)
#' result <- fly(large_list, { Sys.sleep(0.01); .x^2 }, .parallel = 4)
#' }
#'
#' @seealso \code{\link{lapply}}, \code{\link{sapply}}, \code{\link{apply}}
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
			stop(".margin must be 1 (rows) or 2 (columns) for matrices")
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
	gc(verbose = FALSE)
	
	#-----------------------------------
	# Classify the expression and capture it.
	is.fn = tryCatch(is.function(expr), error = function(e) FALSE, warning = function(e) FALSE)
	is.name = tryCatch(exists(expr), error = function(e) FALSE, warning = function(e) FALSE)
	if(!is.name) expr <- deparse(substitute(expr))
	is.code = tryCatch(grepl("\\{", expr[1]), error = function(e) FALSE, warning = function(e) FALSE)
	
	# Capture additional arguments.
	arguments <- list(...)
	if(length(arguments) > 0){
		for(i in seq_along(arguments)){
			if(is.character(arguments[[i]])) arguments[[i]] = paste0('\"', arguments[[i]], '\"')
		}
		arguments = paste(names(arguments), arguments, sep = " = ")
		arguments = paste(arguments, collapse = ", ")
		arguments = paste(",", arguments)
	}else{
		arguments = ""
	}

	# Prepare the function.
	if(is.fn | is.name){
		func_string = paste0(
			"function(",
			.var,
			"){",
			expr,
			"(",
			paste0(.var, "[[1]]"),
			arguments,
			")}",
			collapse = ""
		)
	}else if(is.code){
		func_string = paste0(
			"function(",
			.var,
			arguments,
			"){\n",
			paste0(.index_var, " = ", .var, "[[2]]\n"),
			paste0(.var, " = ", .var, "[[1]]\n"),
			paste(expr[-1], collapse = "\n"),
			collapse = ""
		)
	}else{
		stop("The expression is not a valid function.")
	}
	
	worker_func <- eval(parse(text = func_string))
	worker_data <- mapply(
		function(x, i) list(data = x, index = i), 
		X, 
		seq_along(X), 
		SIMPLIFY = FALSE
	)
	
	#-----------------------------------
	if (!is.null(.parallel)) {
		# Parallel execution.
		if (!requireNamespace("parallel", quietly = TRUE)) {
			stop("The `parallel` package is required for parallel execution.")
		}
		
		if (is.numeric(.parallel)) {
			# Create cluster with specified number of cores.
			cl <- parallel::makeCluster(.parallel)
		} else if (inherits(.parallel, "cluster")) {
			# Use provided cluster.
			cl <- .parallel
		} else {
			stop(".parallel must be a number of cores or a cluster object.")
		}
		
		# Run in parallel - each worker gets only its specific item + index.
		result <- parallel::parLapply(cl, worker_data, worker_func)

		if (is.numeric(.parallel)) {
			parallel::stopCluster(cl)
		}
	} else {
		# Sequential execution
		result <- mapply(worker_func, worker_data, SIMPLIFY = FALSE, USE.NAMES = FALSE)
	}
	
	# Always ensure result is a proper list (even if elements are NULL).
	if (!is.list(result)) {
		result <- as.list(result)
	}
	rm(X)
	gc(verbose = FALSE)
	
	# Preserve names from original data object.
	names(result) <- X_names

	# Apply simplification if requested (using sapply logic).
	if (simplify != FALSE) {
		result <- simplify2array(result, higher = (simplify == "array"))
	}
	
	return(result)
}
