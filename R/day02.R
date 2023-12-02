#' Day 02: Dive!
#'
#' [Dive!](https://adventofcode.com/2021/day/2)
#'
#' @name day02
#' @rdname day02
#' @param x some data
#' @export
#' @examples
#' solve02a(example_data_02())
#' solve02b(example_data_02())

solve02a <- function(x) {
  s <- Submarine$new()
  apply(read_cmds(x), 1, \(x) s$move_a(x[1], as.integer(x[2])))
  s$horiz * s$depth
}

#' @rdname day02
#' @export

solve02b <- function(x) {
  s <- Submarine$new()
  apply(read_cmds(x), 1, \(x) s$move_b(x[1], as.integer(x[2])))
  s$horiz * s$depth
}

# utils -------------------------------------------------------------------

read_cmds <- \(x) stringr::str_match(x, "(forward|up|down) (\\d+)")[,-1]

Submarine <- R6::R6Class("Submarine",

  public = list(

    depth = 0L,
    horiz = 0L,
    aim = 0L,

    move_a = function(direction, X) {
      if (direction == "forward")
        self$horiz <- self$horiz + X
      else if (direction == "down")
        self$depth <- self$depth + X
      else if (direction == "up")
        self$depth <- self$depth - X
    },

    move_b = function(direction, X) {
      if (direction == "forward") {
        self$horiz <- self$horiz + X
        self$depth <- self$depth + self$aim * X
      } else if (direction == "down")
        self$aim <- self$aim + X
      else if (direction == "up")
        self$aim <- self$aim - X
    }

  )

)

# Example -----------------------------------------------------------------

#' @param example Which example data to use (by position or name).
#' @rdname day02
#' @export

example_data_02 <- function(example = 1) {
  l <- list(
    a = c(
      "forward 5",
      "down 5",
      "forward 8",
      "up 3",
      "down 8",
      "forward 2"
    )
  )
  l[[example]]
}
