lsystem  <- function(alphabet, axiom, productions) {

    derivation  <- function(axiom, new_word) {
        a <- substring(axiom,1,1)
        if (a  == "") {
            new_word
        } else derivation(substring(axiom,2),
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

lsystem_iter <- function(alphabet, axiom, productions) {
    function(n) {
        new_word <- ""
        while (n > 0) {
            for (char in as.list(strsplit(axiom,""))[[1]]) {
                new_word  <- paste(new_word,
                                   productions[char],
                                   sep="")
            }
            n <- n - 1
            axiom <- new_word
            new_word <- ""
        }
        axiom
    }
}
