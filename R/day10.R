#' Day 10: Syntax Scoring
#'
#' [Syntax Scoring](https://adventofcode.com/2021/day/10)
#'
#' @name day10
#' @rdname day10
#' @param x raw symbols
#' @importFrom stats median
#' @export
#' @examples
#' solve10a(example_data_10())
#' solve10b(example_data_10())

solve10a <- function(x)
  x |> strsplit("") |> sapply(score_corrupted) |> sum()


#' @rdname day10
#' @export

solve10b <- function(x)
  x |> strsplit("") |> sapply(score_incomplete) |> median(na.rm = TRUE)


# Helpers ----------------------------------------------------------------------

score_corrupted <- function(symbols) {

  open_stack <- NULL

  for (s in symbols) {
    if (s %in% c("(", "[", "{", "<")) {
      # push
      open_stack <- c(s, open_stack)
    } else {
      # pop and compare
      if (open_stack[1] != "(" && s == ")") return(3)
      if (open_stack[1] != "[" && s == "]") return(57)
      if (open_stack[1] != "{" && s == "}") return(1197)
      if (open_stack[1] != "<" && s == ">") return(25137)
      open_stack <- open_stack[-1]
    }
  }

  return(0)

}

score_incomplete <- function(symbols) {

  if (score_corrupted(symbols)) return(NA_integer_)

  open_symbols <- c("(", "[", "{", "<")

  open_stack <- NULL

  for (s in symbols)
    if (s %in% open_symbols)
      open_stack <- c(s, open_stack) # push
    else
      open_stack <- open_stack[-1]   # pop

  res <- 0
  symbol_to_num <- setNames(1:4, open_symbols)
  for (s in open_stack) res <- res * 5 + symbol_to_num[s]

  res

}


# Test data --------------------------------------------------------------------

#' @param example Which example data to use (by position or name).
#' @rdname day10
#' @export

example_data_10 <- function(example = 1) {
  l <- list(
    c(
      "[({(<(())[]>[[{[]{<()<>>",
      "[(()[<>])]({[<{<<[]>>(",
      "{([(<{}[<>[]}>{[]{[(<()>",
      "(((({<>}<{<{<>}{[]{[]{}",
      "[[<[([]))<([[{}[[()]]]",
      "[{[{({}]{}}([{[{{{}}([]",
      "{<[[]]>}<{[{[{[]{()[[[]",
      "[<(<(<(<{}))><([]([]()",
      "<{([([[(<>()){}]>(<<{{",
      "<{([{{}}[<[[[<>{}]]]>[]]"
    )
  )
  l[[example]]
}
