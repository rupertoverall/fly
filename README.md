# fly

<!-- badges: start -->
[![R-CMD-check](https://github.com/rupertoverall/fly/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rupertoverall/fly/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/fly)](https://CRAN.R-project.org/package=fly)
<!-- badges: end -->

The `fly` package provides an intuitive way to iterate over data objects using an uncluttered syntax while retaining the flexibility and power of `lapply()`. 

## Motivation

When writing a lot of *apply loops, some of the syntax becomes rather tedious. This package aims to strip away redundant code and make the most-used features a bit more accessible.
 -  Custom functions do not need the `function` keyword, just insert the body of the function as a code block.
 -  The variable holding the data for each iteration does not need to be declared. By default it is ".x".
 -  The index of the iteration is automatically made available as a variable. By default called ".i".
 -  Element names (from the input list or matrix) are used to name the output list.
 -  Output is always a list (including empty elements) so it will always align with the input.
 -  Parallelisation is built-in. No need to interface with the `parLapply` syntax directly. The same code will work sequentially or in parallel by just changing one parameter.
 - Lists and matrices use the same function.

## Installation

You can install this version of fly with:

```r
devtools::install_github("rupertoverall/fly")
```

## Examples

```r
library(fly)

mat = matrix(c(1:11, NA), nrow = 3)
fly(mat, median) # By rows.

fly(mat, median, na.rm = TRUE) # Pass additional arguments.

fly(mat, median, na.rm = TRUE, .margin = 2) # By columns.

fly(mat, { # No 'function' keyword required.
	is.even = .x %% 2 == 0 # '.x' is the in-built variable from the input (in this example, a row of the matrix)
	sum.evens = sum(.x[is.even], na.rm = TRUE)
	return(sum.evens)
})

fly(letters[1:6], {which(.x != "d")}) # Empty value retained.

input = list(a = 1:4, b = 5:8, c = 9:11)
fly(input, max) # Names are retained.

fly(input, {
	c(paste0("Value_", .i), max(.x)) # Internal index included.
})

fly(mat, median, .parallel = 4) # Runs on 4 cores. 
# Be aware that the memory and object handling overhead makes parallelisation only worthwhile for very compute-intensive functions.

```
