#' Day 01: Sonar Sweep
#'
#' [Sonar Sweep](https://adventofcode.com/2021/day/1)
#'
#' @name day01
#' @rdname day01
#' @param x some data
#' @export
#' @examples
#' solve01a(example_data_01())
#' solve01b(example_data_01())

solve01a <- \(x) solve01(x, n = 1)

#' @rdname day01
#' @export

solve01b <- \(x) solve01(x, n = 3)

# Solve -------------------------------------------------------------------

#' @importFrom utils head tail

solve01 <- function(x, n) {
  x <- as.integer(x)
  ins  <- tail(x, -n)
  outs <- head(x, -n)
  sum(ins > outs)
}

# Example -----------------------------------------------------------------

#' @param example Which example data to use (by position or name).
#' @rdname day01
#' @export

example_data_01 <- function(example = 1) {
  l <- list(
    c(
      "199",
      "200",
      "208",
      "210",
      "200",
      "207",
      "240",
      "269",
      "260",
      "263"
    )
  )
  l[[example]]
}
