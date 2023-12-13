#' Day 07: The Treachery of Whales
#'
#' [The Treachery of Whales](https://adventofcode.com/2021/day/7)
#'
#' @name day07
#' @rdname day07
#' @param x raw crab position
#' @export
#' @examples
#' solve07a(example_data_07())
#' solve07b(example_data_07())

solve07a <- function(x) solve07(x, fuel_cost_a)

#' @rdname day07
#' @export

solve07b <- function(x) solve07(x, fuel_cost_b)

# Helpers ----------------------------------------------------------------------

parse07 <- \(x) x |> strsplit(",") |> unlist() |> as.integer()

solve07 <- function(x, fuel_cost) {
  crabs <- parse07(x)
  pos <- seq(min(crabs), max(crabs))
  outer(pos, crabs, fuel_cost) |> rowSums() |> min()
}

fuel_cost_a <- function(crab, pos) abs(crab - pos)

fuel_cost_b <- function(crab, pos) {
  n <- abs(crab - pos)
  (n * (n + 1)) / 2
}


# Test data ---------------------------------------------------------------

#' @param example Which example data to use (by position or name).
#' @rdname day07
#' @export

example_data_07 <- function(example = 1) {
  l <- list(
    c(
     "16,1,2,0,4,2,7,1,2,14"
    )
  )
  l[[example]]
}
