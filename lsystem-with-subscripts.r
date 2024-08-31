lsystem  <- function(alphabet, axiom, productions) {

  derivation  <- function(axiom, new_word) {
    a <- substring(axiom,1,1)
    b <- substring(axiom,2,2)
    c <- substring(axiom,3,3)
    if (a  == "") {
      new_word
    } else if (b == "_") {
      derivation(substring(axiom,4),
                 paste(new_word,
                       productions[paste(a,b,c,sep="")],
                       sep=""))
    }
    else derivation(substring(axiom,2),
                    paste(new_word,
                          productions[a],
                          sep=""))
  }

  derive_n  <- function(axiom, n) {
    if (n == 0) {
      axiom
    } else derive_n(derivation(axiom, ""),
                    n - 1)
  }

  function(n) {
    derive_n(axiom, n)
  }
}

symbols_in_nu <- function(nu, symlist) {
    a <- substring(nu,1,1)
    b <- substring(nu,2,2)
    c <- substring(nu,3,3)

    if (a == "") {
      symlist
    } else if (b == "_") {
      symbols_in_nu(substring(nu,4),
                    c(paste(a,b,c,sep=""), symlist))
    } else {
      symbols_in_nu(substring(nu,2),
                    c(a, symlist))
    }
}


symbols_in_nu_iter <- function(nu) {
  symlist <- c()
  for (i in 1:nchar(nu)) {
    a <- substring(nu,i,i)
    b <- substring(nu,i + 1,i + 1)
    c <- substring(nu,i + 2,i + 2)
    if (b == "_") {
      symlist <- c(paste(a,b,c,sep=""), symlist)
      nu <- paste(substring(nu,0,i), substring(nu,i + 3), sep="")
    } else {
      symlist <- c(a, symlist)
    }
  }
  rev(symlist[symlist != ""])
}



lsystem_iter <- function(alphabet, axiom, productions) {
  function(n) {
    new_word <- ""
    while (n > 0) {
      for (symbol in symbols_in_nu_iter(axiom)) {
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
