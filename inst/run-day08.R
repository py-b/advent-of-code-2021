x <- readLines("./inst/input08.txt")

(p1 <- solve08a(x))
(p2 <- solve08b(x))

stopifnot(p1 == aoc_solutions$day08a)
stopifnot(p2 == aoc_solutions$day08b)
