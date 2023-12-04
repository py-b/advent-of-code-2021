#' Day 05: Hydrothermal Venture
#'
#' [Hydrothermal Venture](https://adventofcode.com/2021/day/5)
#'
#' @name day05
#' @rdname day05
#' @param x some data
#' @importFrom stats setNames
#' @export
#' @examples
#' solve05a(example_data_05())
#' solve05b(example_data_05())

solve05a <- \(x) solve05(x, diag = FALSE)

#' @rdname day05
#' @export

solve05b <- \(x) solve05(x)


# Helpers ----------------------------------------------------------------------

solve05 <- function(x, diag = TRUE) {

  pts_count <-
    x |>
    parse05() |>
    line_filter(diag = diag) |>
    lapply(points) |>
    unlist(recursive = FALSE) |>
    vapply(paste, collapse = ";", character(1)) |>
    table()

  sum(pts_count > 1)

}

parse05 <- function(x) regmatches(x, gregexpr("\\d+", x)) |> lapply(as_coord)

as_coord <- function(chr_coord)
  chr_coord |>
    as.integer() |>
    as.list() |>
    setNames(c("x1", "y1", "x2", "y2"))

line_filter <- function(coords, diag = TRUE)
  if (diag) coords else Filter(\(.) .$x1 == .$x2 || .$y1 == .$y2, coords)

points <- function(line) {

  x <- seq(line$x1, line$x2)
  y <- seq(line$y1, line$y2)

  if (line$x1 == line$x2) x <- rep(line$x1, length(line$y1))
  if (line$y1 == line$y2) y <- rep(line$y1, length(line$x1))

  Map(c, x, y)

}


# Test data --------------------------------------------------------------------

#' @param example Which example data to use (by position or name).
#' @rdname day05
#' @export

example_data_05 <- function(example = 1) {
  l <- list(
    c(
      "0,9 -> 5,9",
      "8,0 -> 0,8",
      "9,4 -> 3,4",
      "2,2 -> 2,1",
      "7,0 -> 7,4",
      "6,4 -> 2,0",
      "0,9 -> 2,9",
      "3,4 -> 1,4",
      "0,0 -> 8,8",
      "5,5 -> 8,2"
    )
  )
  l[[example]]
}
