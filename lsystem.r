letters_in_nu <- function(nu) {
  letters <- c()
  while (nchar(nu) > 0) {
    a <- substring(nu,1,1)
    b <- substring(nu,2,2)
    c <- substring(nu,3,3)
    if (b == "_") {
      letters <- c(paste(a,b,c,sep=""), letters)
      nu <- substring(nu,4)
    } else {
      letters <- c(a, letters)
      nu <- substring(nu,2)
    }    
  }
  rev(letters[letters != ""])
}

lsystem <- function(alphabet, axiom, productions) {
  function(n) {
    new_word <- ""
    while (n > 0) {
      for (symbol in letters_in_nu(axiom)) {
        new_word  <- paste(new_word,
                           productions[symbol],
                           sep="")
      }
      n <- n - 1
      axiom <- new_word
      new_word <- ""
    }
    axiom
  }
}
