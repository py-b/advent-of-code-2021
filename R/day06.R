#' Day 06: Lanternfish
#'
#' [Lanternfish](https://adventofcode.com/2021/day/6)
#'
#' @name day06
#' @rdname day06
#' @param x age of several lanternfishes
#' @export
#' @examples
#' solve06a(example_data_06())
#' solve06b(example_data_06())

solve06a <- function(x) solve06(x, days = 80)

#' @rdname day06
#' @export

solve06b <- function(x) solve06(x, days = 256)


# Helpers ----------------------------------------------------------------------

parse06 <- \(x) as.integer(strsplit(x, ",")[[1]])

solve06 <- function(x, days) {

  fish <- parse06(x)

  fish_count <- table(factor(fish, levels = 0:8))

  for (i in seq(days)) {
    new_gen <- fish_count[1]
    fish_count[1:8]     <- fish_count[2:9]
    fish_count[9]       <- 0
    fish_count[c(7, 9)] <- fish_count[c(7, 9)] + new_gen
  }

  sum(fish_count)

}


# Test data --------------------------------------------------------------------

#' @param example Which example data to use (by position or name).
#' @rdname day06
#' @export

example_data_06 <- function(example = 1) {
  l <- list(
    c("3,4,3,1,2")
  )
  l[[example]]
}
