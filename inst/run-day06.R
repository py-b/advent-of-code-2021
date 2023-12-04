x <- readLines("./inst/input06.txt")

(p1 <- solve06a(x))
options(digits = 22)
(p2 <- solve06b(x))

stopifnot(p1 == aoc_solutions$day06a)
stopifnot(p2 == aoc_solutions$day06b)
