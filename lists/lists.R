### 1.01 Find the last element of a list.
last <- function(ll) unlist(tail(ll, 1))
last(list("a", "b", "c", "d"))
# [1] "d"

### 1.02 Find the last but one element of a list.
almost_last <- function(ll) ll[length(ll) - 1]
almost_last(list("a", "b", "c", "d"))
# [1] "c"

### 1.03 Find the K'th element of a list.
### The first element in the list is number 1.
kth <- function(ll, k) ll[[k]]
kth(list("a", "b", "c", "d"), 2)
# [1] "b"

### 1.04 Find the number of elements of a list.
length(list("a", "b", "c", "d"))
# [1] 4

### 1.05 Reverse a list.
rev(list("a", "b", "c", "d"))

### 1.06 Find out whether a list is a palindrome.
is.palindrome <- function(ll) identical(rev(ll), ll)
is.palindrome(list("n", "o"))
# [1] FALSE
is.palindrome(list("a", "b", "b", "a"))
# [1] TRUE

### 1.07 Flatten a nested list structure.
flatten <- function(ll) list(unlist(ll))
flatten(list("a", list("b", list("c", "d"))))
# [[1]]
# [1] "a" "b" "c" "d"

### 1.08 Eliminate consecutive duplicates of list elements.
compress <- function(ll) { list(rle(unlist(ll))$values) }
compress(list("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"))
# list("a", "b", "c", "a", "d", "e")

### 1.09 Pack consecutive duplicates of list elements into sublists.
pack <- function(ll) {
  rll <- rle(unlist(ll))$lengths
  ll <- compress(ll)[[1]]
  ol <- list()
  length(ol) <- length(ll)
  for (i in seq_along(ll)) {
    ol[[i]] <- list(rep(ll[[i]], rll[[i]]))
  }
  ol
}
pack(list("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"))

### 1.10 Run-length encoding of a list (using the solution from 1.09).
encode <- function(ll) {
  lapply(pack(ll), function(x) {
    list(x[[1]][[1]], length(x[[1]])) 
  })
}
encode(list("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"))

### 1.11 (*) Modified run-length encoding (so 1 item elements are just that element).
encode_modified <- function(ll) {
  lapply(encode(ll), function(x) {
    if (x[[1]] == 1) { x <- x[2] } else { x }
  })
}
encode_modified(list("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"))

### 1.12 (**) Decode a run-length encoded list. (decode(encode(x)) == x)
decode <- function(ll) {
  as.list(unlist(lapply(ll, function(x) {
    rep(x[[2]], x[[1]])
  })))
}
decode(encode(list("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")))

### 1.13 (**) Run-length encoding of a list (direct solution).
encode_direct <- function(ll) {
  unname(as.list(as.data.frame(mapply(
    list,
    rle(unlist(ll))$lengths,
    rle(unlist(ll))$values
  ))))
}
encode_direct(list("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"))
