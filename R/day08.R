#' Day 08: Seven Segment Search
#'
#' [Seven Segment Search](https://adventofcode.com/2021/day/8)
#'
#' @name day08
#' @rdname day08
#' @param x some data
#' @export
#' @examples
#' solve08a(example_data_08())
#' solve08b(example_data_08())

solve08a <- function(x)
  x |>
    parse08() |>
    lapply(\(.) .$output) |>
    unlist() |>
    nchar() |>
    `%in%`(c(2, 3, 4, 7)) |>
    sum()

#' @rdname day08
#' @export

solve08b <- function(x) {

  entries  <- parse08(x)
  patterns <- lapply(entries, \(.) .$patterns)
  output   <- lapply(entries, \(.) .$output)

  wires <- lapply(patterns, decode_input)

  output_int <- mapply(decode_output, output, wires)
  sum(output_int)

}


# Helpers ----------------------------------------------------------------------

parse08 <- function(x)
  x |>
    strsplit("[^a-g]+") |>
    lapply(\(.) list(patterns = .[1:10], output = .[11:14]))

#  --A--
#  F   B
#  --G--
#  E   C
#  --D--

segments_to_int <- function(segments)
  switch(
    segments,
    "ABCDEF"  = 0,
    "BC"      = 1,
    "ABDEG"   = 2,
    "ABCDG"   = 3,
    "BCFG"    = 4,
    "ACDFG"   = 5,
    "ACDEFG"  = 6,
    "ABC"     = 7,
    "ABCDEFG" = 8,
    "ABCDFG"  = 9
  )

decode_output <- function(output, wires) {

  output |>
    strsplit("") |>
    sapply(
      \(.) wires[.] |> sort() |> paste(collapse = "") |> segments_to_int()
    ) |>
    paste(collapse = "") |>
    as.integer()

}

decode_input <- function(patterns) {

  patterns <- strsplit(patterns, "")

  one   <- patterns[lengths(patterns) == 2] |> unlist()
  four  <- patterns[lengths(patterns) == 4] |> unlist()
  seven <- patterns[lengths(patterns) == 3] |> unlist()
  eight <- patterns[lengths(patterns) == 7] |> unlist()

  `A` <- setdiff(seven, one)

  three_detect <- \(p) length(p) == 5 && all(seven %in% p)
  three <- Filter(three_detect, patterns) |> unlist()

  `G` <- four |> setdiff(one) |> intersect(three)
  `F` <- four |> setdiff(three)
  `D` <- three |> setdiff(seven) |> setdiff(four)

  five_detect <- \(p) length(p) == 5 && all(c(`A`, `D`, `F`, `G`) %in% p)
  five <- Filter(five_detect, patterns) |> unlist()

  `C` <- intersect(one, five)
  `B` <- setdiff(one, five)
  `E` <- setdiff(eight, c(`A`, `B`, `C`, `D`, `F`, `G`))

  res <- LETTERS[1:7]
  names(res) <- c(`A`, `B`, `C`, `D`, `E`, `F`, `G`)
  res[sort(names(res))]

}


# Test data --------------------------------------------------------------------

#' @param example Which example data to use (by position or name).
#' @rdname day08
#' @export

example_data_08 <- function(example = 1) {
  l <- list(
    c(
      "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe",
      "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc",
      "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg",
      "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb",
      "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea",
      "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb",
      "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe",
      "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef",
      "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb",
      "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
    )
  )
  l[[example]]
}
