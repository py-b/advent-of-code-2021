#' Day 04: Giant Squid
#'
#' [Giant Squid](https://adventofcode.com/2021/day/4)
#'
#' @name day04
#' @rdname day04
#' @param x some data
#' @export
#' @examples
#' solve04a(example_data_04())
#' solve04b(example_data_04())

solve04a <- function(x) {

  game_data <- parse04(x)

  for (n in game_data$played)
    for (grid in game_data$grids) {
      grid$play(n)
      if (grid$win()) return(grid$score())
    }

  stop("no winning grid")

}

#' @rdname day04
#' @export

solve04b <- function(x) {

  game_data <- parse04(x)
  unfinished <- length(game_data$grids)

  for (n in game_data$played)
    for (grid in game_data$grids) {
      if (!grid$win()) {
        grid$play(n)
        if (grid$win()) {
          unfinished <- unfinished - 1
          if (unfinished == 0) return(grid$score())
        }
      }
    }

  stop("no winning grid")

}


# Helpers ----------------------------------------------------------------------

complete_row <- function(x) apply(x, 1, \(.) all(is.na(.)))
complete_col <- function(x) apply(x, 2, \(.) all(is.na(.)))

Bingo <- R6::R6Class("Bingo",

  public = list(

    grid = NULL,
    last_played = NULL,

    initialize = function(grid) self$grid <- grid,

    play = function(number) {
      self$grid[self$grid == number] <- NA
      self$last_played = number
      invisible(self)
    },

    win = function() {
      any(complete_row(self$grid)) || any(complete_col(self$grid))
    },

    score = function() sum(self$grid, na.rm = TRUE) * self$last_played

  )

)

parse04 <- function(x) {

  # numbers

  played <- x[[1]] |> strsplit(",") |> unlist() |> as.integer()

  # grids

  split_by_5 <- \(x) split(x, (seq_along(x) - 1) %/% 5) |> unname()
  parse_grid <- \(x) do.call(rbind, strsplit(x, " +")) |>
                     apply(1:2, as.integer) |>
                     Bingo$new()

  x <- x |> tail(-2) |> trimws()
  grids <- x[x != ""] |> split_by_5() |> lapply(parse_grid)

  # return as a list

  list(played = played, grids = grids)

}


# Test data --------------------------------------------------------------------

#' @param example Which example data to use (by position or name).
#' @rdname day04
#' @export

example_data_04 <- function(example = 1) {
  l <- list(
    c(
      "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1",
      "",
      "22 13 17 11  0",
      " 8  2 23  4 24",
      "21  9 14 16  7",
      " 6 10  3 18  5",
      " 1 12 20 15 19",
      "",
      " 3 15  0  2 22",
      " 9 18 13 17  5",
      "19  8  7 25 23",
      "20 11 10 24  4",
      "14 21 16 12  6",
      "",
      "14 21 17 24  4",
      "10 16 15  9 19",
      "18  8 23 26 20",
      "22 11 13  6  5",
      " 2  0 12  3  7"
    )
  )
  l[[example]]
}
