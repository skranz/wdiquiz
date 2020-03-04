is.empty = function(x) {
  if (length(x)==0) return(TRUE)
  if (all(is.na(x))) return(TRUE)
  if (is.character(x)) {
    if (identical(x,"")) return(TRUE)
  }
  FALSE
}
