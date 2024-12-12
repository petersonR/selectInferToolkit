#' Hello, world!
#'
#' @description
#'
#' This is an example function named 'hello'
#' which prints 'Hello, world!'.
#'
#' @details You can learn more about package authoring with RStudio at:
#'
#'   https://r-pkgs.org
#'
#' @export
hello <- function() {
  print("Hello, world!")
}

#' Add two numbers, create new class
#'
#' @description
#'
#' This is an example of how to create a new class.
#
#'
#' @param x the first number
#' @param y the second number
#' @param ... Additional arguments to pass through
#'
#' @return A list of class `my_new_class`
#'
#' @export
add_2_numbers <- function(x, y, ...) {
  val_to_return <- list(x=x, y=y, val = x + y, other_args = list(...))
  class(val_to_return) <- "my_new_class"
  val_to_return
}

#' @method tidy my_new_class
#' @importFrom tibble tibble
#' @importFrom broom tidy
#' @export
tidy.my_new_class <- function(x, ...) {
  tibble(x= x$x, y = x$y, sum = x$val)
}

#' @method print my_new_class
#' @export
print.my_new_class <- function(x, ...) {
 print(x$val)
}
