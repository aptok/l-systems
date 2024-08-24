lsystem  <- function(alphabet, axiom, prods) {

    derivation  <- function(axiom, new_word) {
        a <- substring(axiom,1,1)
        if (a  == "") {
            new_word
        } else derivation(substring(axiom,2),
                          paste(new_word,
                                prods[a],
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

lsystem_iter <- function(alphabet, axiom, prods) {
    function(n) {
        new_word <- ""
        while (n > 0) {
            for (char in as.list(strsplit(axiom,""))[[1]]) {
                new_word  <- paste(new_word,
                                   prods[char],
                                   sep="")
            }
            n <- n - 1
            axiom <- new_word
            new_word <- ""
        }
        axiom
    }
}

lsystem_derivation  <- function(axiom, prods, new_word) {
    a <- substring(axiom,1,1)
    if (a  == "") {
        new_word
    } else lsystem_derivation(substring(axiom,2),
                              prods,
                              paste(new_word,
                                    find_production(a,prods),
                                    sep=""))
}

lsystem_derivation_iter <- function(axiom, prods, new_word) {
    for (x in as.list(strsplit(axiom,""))[[1]]) {
        new_word  <- paste(new_word,
                           find_production(x, prods),
                           sep="")
    }
    new_word
}

lsystem_with_dict <- function(axiom, dict, n) {
    new_word <- ""
    while (n > 0) {

        for (char in as.list(strsplit(axiom,""))[[1]]) {
            new_word  <- paste(new_word,
                               prodict[char],
                               sep="")
        }
        n <- n - 1
        axiom <- new_word
        new_word <- ""
    }
    axiom
}




find_production <- function(char,prods) {
    if (length(prods) == 0) {
        char
    } else  if (char == prods[[1]][1]) {
        prods[[1]][2]
    } else find_production(char,prods[-1])
}

lsystem_derive_n_iter  <- function(axiom, prods, new_word, n) {
    while (n > 0) {
        axiom <- lsystem_derivation_iter(axiom, prods, "")
        n  <- n - 1
    }
    axiom
}


lsystem_derive_n  <- function(axiom, prods, new_word, n) {
    if (n == 0) {
        axiom
    } else lsystem_derive_n(lsystem_derivation(axiom, prods, ""), prods, "", n - 1)
}

lsystem_derive_n_with_iter  <- function(axiom, prods, new_word, n) {
    if (n == 0) {
        axiom
    } else lsystem_derive_n_with_iter(lsystem_derivation_iter(axiom, prods, ""), prods, "", n - 1)
}
                                        #trace(lsystem_derivation, tracer = quote(cxoat(sprintf("tracing pt(*, ncp = %.15g)\n", ncp))),print = FALSE)

find_with_prods <- function(prods) {
    function(char) {
        find_production(char,prods)
    }
}
