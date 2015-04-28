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
