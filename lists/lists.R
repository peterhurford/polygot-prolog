### Install
if (!require("devtools")) { install.packages("devtools"); require("devtools") }
if (!require("assertthat")) { install.packages("assertthat"); require("assertthat") }
if (!require("batbelt")) { install_github("peterhurford/batbelt"); require("batbelt") }


### 1.01 Find the last element of a list.
last <- function(ll) unlist(tail(ll, 1))
assert_that(last(list("a", "b", "c", "d")) == "d")

### 1.02 Find the last but one element of a list.
almost_last <- function(ll) ll[length(ll) - 1]
assert_that(almost_last(list("a", "b", "c", "d")) == "c")

### 1.03 Find the K'th element of a list.
### The first element in the list is number 1.
kth <- function(ll, k) ll[[k]]
assert_that(kth(list("a", "b", "c", "d"), 2) == "b")

### 1.04 Find the number of elements of a list.
assert_that(length(list("a", "b", "c", "d")) == 4)

### 1.05 Reverse a list.
assert_that(identical(rev(list("a", "b", "c", "d")), list("d", "c", "b", "a")))

### 1.06 Find out whether a list is a palindrome.
is.palindrome <- function(ll) identical(rev(ll), ll)
assert_that(isFALSE(is.palindrome(list("n", "o"))))
assert_that(isTRUE(is.palindrome(list("a", "b", "b", "a"))))

### 1.07 Flatten a nested list structure.
flatten <- function(ll) as.list(unlist(ll))
assert_that(identical(
  flatten(list("a", list("b", list("c", "d")))),
  list("a", "b", "c", "d")
))

### 1.08 Eliminate consecutive duplicates of list elements.
oompress <- function(ll) { as.list(rle(unlist(ll))$values) }
assert_that(identical(
  compress(list("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")),
  list("a", "b", "c", "a", "d", "e")
))

### 1.09 Pack consecutive duplicates of list elements into sublists.
pack <- function(ll) {
  rll <- rle(unlist(ll))$lengths
  ll <- unlist(compress(ll))
  ol <- list()
  length(ol) <- length(ll)
  for (i in seq_along(ll)) {
    ol[[i]] <- as.list(rep(ll[[i]], rll[[i]]))
  }
  ol
}
assert_that(identical(
  pack(list("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")),
  list(list("a", "a", "a", "a"), list("b"), list("c", "c"), list("a", "a"), list("d"), list("e", "e", "e", "e"))
))

### 1.10 Run-length encoding of a list (using the solution from 1.09).
encode <- function(ll) {
  lapply(pack(ll), function(x) {
    as.list(list(length(x), x[[1]]))
  })
}
assert_that(identical(
  encode(list("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")),
  list(list(4, "a"), list(1, "b"), list(2, "c"), list(2, "a"), lis(1, "d"), list(4, "e"))
))

### 1.11 (*) Modified run-length encoding (so 1 item elements are just that element).
encode_modified <- function(ll) {
  lapply(encode(ll), function(x) {
    if (x[[1]] == 1) { x <- x[2] } else { x }
  })
}
assert_that(identical(
  encode_modified(list("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")),
  list(list(4, "a"), list("b"), list(2, "c"), list("d"), list(4, "e"))
))

### 1.12 (**) Decode a run-length encoded list. (decode(encode(x)) == x)
decode <- function(ll) {
  as.list(unlist(lapply(ll, function(x) {
    rep(x[[2]], x[[1]])
  })))
}
assert_that(identical(
  decode(encode(list("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"))),
  list("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")
))

### 1.13 (**) Run-length encoding of a list (direct solution).
encode_direct <- function(ll) {
  unname(as.list(as.data.frame(mapply(
    list,
    rle(unlist(ll))$lengths,
    rle(unlist(ll))$values
  ))))
}
assert_that(identical(
  encode_direct(list("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")),
  list(list(4, "a"), list(1, "b"), list(2, "c"), list(1, "d"), list(4, "e"))
))

### 1.14 (*) Duplicate the elements of a list.
dupli <- function(ll) {
  as.list(unlist(lapply(ll, function(x) {
    rep(x, 2)
  })))
}
assert_that(identical(
  dupli(list("a", "b", "c", "c", "d")),
  list("a", "a", "b", "b", "c", "c", "c", "c", "d", "d")
))
