# Internal utility functions

#' Extract variable names from expressions.
#' @keywords internal
#' @noRd
# Function to extract variable names from an expression.
extract_variables <- function(expr, exclude_vars = c()) {
	if (is.null(expr)) return(character(0))
	
	# Convert to character if it's a name/symbol.
	if (is.name(expr)) {
		var_name <- as.character(expr)
		if (!var_name %in% exclude_vars) {
			return(var_name)
		} else {
			return(character(0))
		}
	}
	
	# If it's a call (function call), recursively extract from arguments.
	if (is.call(expr)) {
		# Get function name (but don't include built-in functions)
		func_name <- as.character(expr[[1]])
		vars <- character(0)
		
		# Add function name if it's not a built-in operator/function
		if (!is_builtin(func_name)) {
			if (!func_name %in% exclude_vars) {
				vars <- c(vars, func_name)
			}
		}
		
		# Recursively extract from all arguments.
		if (length(expr) > 1) {
			for (i in 2:length(expr)) {
				vars <- c(vars, extract_variables(expr[[i]], exclude_vars))
			}
		}
		
		return(unique(vars))
	}
	
	# If it's a pairlist (like function arguments), extract from each element.
	if (is.pairlist(expr)) {
		vars <- character(0)
		for (item in expr) {
			vars <- c(vars, extract_variables(item, exclude_vars))
		}
		return(unique(vars))
	}
	
	# For other types (constants, etc.), return empty.
	return(character(0))
}

#' Test if object is from base R.
#' @keywords internal
#' @noRd
# Function to test if an object is base R (and does not need to be exported).
is_builtin <- function(name) {
	exists(name, envir = baseenv()) || 
		exists(name, envir = as.environment("package:base"))
}



