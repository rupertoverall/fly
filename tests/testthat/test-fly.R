test_that("fly works with basic lists and functions", {
	data <- list(a = 1:3, b = 4:6)
	result <- fly(data, sum)
	
	expect_type(result, "list")
	expect_equal(length(result), 2)
	expect_equal(names(result), c("a", "b"))
	expect_equal(result$a, 6)
	expect_equal(result$b, 15)
})

test_that("fly works with expressions using .x", {
	data <- list(a = 1:3, b = 4:6)
	result <- fly(data, { sum(.x) * 2 })
	
	expect_equal(result$a, 12)
	expect_equal(result$b, 30)
})

test_that("fly works with custom variable names", {
	data <- list(a = 1:3, b = 4:6)
	result <- fly(data, { sum(item) }, .var = "item")
	
	expect_equal(result$a, 6)
	expect_equal(result$b, 15)
})

test_that("fly works with additional arguments", {
	data <- list(a = c("hello", "world"), b = c("foo", "bar"))
	result <- fly(data, { paste(.x, collapse = sep) }, sep = "-")
	
	expect_equal(result$a, "hello-world")
	expect_equal(result$b, "foo-bar")
})

test_that("fly works with matrices - rows", {
	mat <- matrix(1:6, nrow = 2)
	rownames(mat) <- c("r1", "r2")
	result <- fly(mat, sum)
	
	expect_type(result, "list")
	expect_equal(names(result), c("r1", "r2"))
	expect_equal(result$r1, 9)  # 1 + 3 + 5
	expect_equal(result$r2, 12) # 2 + 4 + 6
})

test_that("fly works with matrices - columns", {
	mat <- matrix(1:6, nrow = 2)
	colnames(mat) <- c("c1", "c2", "c3")
	result <- fly(mat, sum, .margin = 2)
	
	expect_equal(names(result), c("c1", "c2", "c3"))
	expect_equal(result$c1, 3)  # 1 + 2
	expect_equal(result$c2, 7)  # 3 + 4
	expect_equal(result$c3, 11) # 5 + 6
})

test_that("fly works with vectors", {
	vec <- c(a = 10, b = 20, c = 30)
	result <- fly(vec, { .x * 2 })
	
	expect_type(result, "list")
	expect_equal(names(result), c("a", "b", "c"))
	expect_equal(result$a, 20)
	expect_equal(result$b, 40)
	expect_equal(result$c, 60)
})

test_that("fly works with data frames", {
	df <- data.frame(x = 1:3, y = 4:6)
	result <- fly(df, sum)
	
	expect_equal(names(result), c("x", "y"))
	expect_equal(result$x, 6)
	expect_equal(result$y, 15)
})

test_that("fly handles index variable", {
	data <- list(a = 10, b = 20)
	result <- fly(data, { .i })
	
	expect_equal(result$a, 1)
	expect_equal(result$b, 2)
})

test_that("fly handles custom index variable", {
	data <- list(a = 10, b = 20)
	result <- fly(data, { pos }, .index_var = "pos")
	
	expect_equal(result$a, 1)
	expect_equal(result$b, 2)
})

test_that("simplify = FALSE returns list", {
	data <- list(a = 1:3, b = 4:6)
	result <- fly(data, sum, simplify = FALSE)
	
	expect_type(result, "list")
	expect_equal(length(result), 2)
})

test_that("simplify = TRUE returns vector when possible", {
	data <- list(a = 1:3, b = 4:6)
	result <- fly(data, sum, simplify = TRUE)
	
	expect_type(result, "integer")
	expect_equal(names(result), c("a", "b"))
	expect_equal(result[["a"]], 6)
	expect_equal(result[["b"]], 15)
})

test_that("simplify works with different return types", {
	# Returns consistent single values -> vector
	data <- list(a = 1:3, b = 4:6)
	result <- fly(data, length, simplify = TRUE)
	expect_type(result, "integer")
	
	# Returns different types -> list
	mixed_data <- list(a = 1:3, b = "hello")
	result2 <- fly(mixed_data, { .x }, simplify = TRUE)
	expect_type(result2, "list")  # Can't simplify mixed types
})

test_that("fly always returns a list when simplify = FALSE", {
	# Test with NULL results
	data <- list(a = 1, b = 2)
	result <- fly(data, { NULL })
	
	expect_type(result, "list")
	expect_equal(length(result), 2)
	expect_null(result$a)
	expect_null(result$b)
})

test_that("fly handles function names", {
	data <- list(a = 1:3, b = 4:6)
	result <- fly(data, sum)
	
	expect_equal(result$a, 6)
	expect_equal(result$b, 15)
})

test_that("fly error handling", {
	expect_error(fly(matrix(1:4, 2, 2), sum, .margin = 3), ".margin must be 1")
})

test_that("fly preserves names from various input types", {
	# Named vector
	vec <- c(x = 1, y = 2, z = 3)
	result <- fly(vec, { .x })
	expect_equal(names(result), c("x", "y", "z"))
	
	# Named list
	lst <- list(first = 1:2, second = 3:4)
	result2 <- fly(lst, length)
	expect_equal(names(result2), c("first", "second"))
	
	# Data frame columns
	df <- data.frame(col1 = 1:3, col2 = 4:6)
	result3 <- fly(df, sum)
	expect_equal(names(result3), c("col1", "col2"))
})

test_that("fly can pass through variables from the parent environment", {
	# Named vector
	vec <- c(x = 1, y = 2, z = 3)
	
	# Secondary vector
	sec <- 4
	
	# Sequential execution
	result <- fly(vec, {
		.x + sec
	})
	expect_equal(result, list(x = 5, y = 6, z = 7))
	
	# Parallel execution
	result <- fly(vec, {
		.x + sec
	}, .parallel = 2)
	expect_equal(result, list(x = 5, y = 6, z = 7))
	
})

