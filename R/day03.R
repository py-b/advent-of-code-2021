#' Day 03: Binary Diagnostic
#'
#' [Binary Diagnostic](https://adventofcode.com/2021/day/3)
#'
#' @name day03
#' @rdname day03
#' @param x some data
#' @export
#' @examples
#' solve03a(example_data_03())
#' solve03b(example_data_03())

solve03a <- function(x) {

  prop1 <- x |> parse03() |> colMeans()

  gamma   <- round(prop1)
  epsilon <- round(1 - prop1)

  bin_to_dec(gamma) * bin_to_dec(epsilon)

}

#' @rdname day03
#' @export

solve03b <- function(x) {

  bits <- parse03(x)

  oxygen <- filt(bits, most_common)
  co2    <- filt(bits, least_common)

  bin_to_dec(oxygen) * bin_to_dec(co2)

}


# Utils -------------------------------------------------------------------

parse03 <- function(x) {
  x |>
    strsplit("") |>
    unlist() |>
    as.integer() |>
    matrix(ncol = nchar(x)[1], byrow = TRUE)
}

bin_to_dec <- function(x) {
  n <- length(x)
  sum(x * 2^(n:1 - 1))
}

most_common  <- function(bits) as.integer(mean(bits) >= .5)
least_common <- function(bits) as.integer(mean(bits) <  .5)

filt <- function(bits, criteria) {
  n <- 1
  repeat {
    keep_bit <- criteria(bits[, n])
    bits <- bits[bits[, n] == keep_bit, ]
    n <- n + 1
    if (is.null(dim(bits))) break
  }
  bits
}


# Examples ----------------------------------------------------------------

#' @param example Which example data to use (by position or name).
#' @rdname day03
#' @export

example_data_03 <- function(example = 1) {
  l <- list(
    c(
      "00100",
      "11110",
      "10110",
      "10111",
      "10101",
      "01111",
      "00111",
      "11100",
      "10000",
      "11001",
      "00010",
      "01010"
    )
  )
  l[[example]]
}
