## Copyright (c) 2016, James P. Howard, II <jh@jameshoward.us>
##
## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions are
## met:
##
##     Redistributions of source code must retain the above copyright
##     notice, this list of conditions and the following disclaimer.
##
##     Redistributions in binary form must reproduce the above copyright
##     notice, this list of conditions and the following disclaimer in
##     the documentation and/or other materials provided with the
##     distribution.
##
## THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
## "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
## LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
## A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
## HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
## SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
## LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
## DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
## THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
## (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
## OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#' @title Phonex Name Coding
#'
#' @description
#' The Phonex name coding procedure.
#'
#' @param word string or vector of strings to encode
#' @param maxCodeLen maximum length of the resulting encodings, in characters
#'
#' @details
#'
#' The variable \code{word} is the name to be encoded.  The variable
#' \code{maxCodeLen} is the limit on how long the returned name code
#' should be.  The default is 4.
#'
#' @return the Phonex encoded character vector
#'
#' @references
#'
#' A.J. Lait and Brian Randell. "An assessment of name matching
#' algorithms." Technical Report Series-University of Newcastle Upon
#' Tyne Computing Science (1996).
#'
#' @family phonics
#'
#' @examples
#' phonex("William")
#' phonex(c("Peter", "Peady"))
#' phonex("Stevenson", maxCodeLen = 8)
#'
#' @export
phonex <- function(word, maxCodeLen = 4) {

    ## First, remove any nonalphabetical characters and uppercase it
    word <- gsub("[^[:alpha:]]*", "", word)
    word <- toupper(word)

    ## Remove umlauts and eszett
    word <- gsub("\u00C4", "A", word)
    word <- gsub("\u00DC", "U", word)
    word <- gsub("\u00D6", "O", word)
    word <- gsub("\u00DF", "S", word)

    ## Preprocess the name
    word <- gsub("S+$", "", word)
    word <- gsub("^KN", "N", word)
    word <- gsub("^WR", "R", word)
    word <- gsub("^PH", "F", word)
    word <- gsub("^H", "", word)
    word <- gsub("^(E|I|O|U|Y)", "A", word)
    word <- gsub("^P", "B", word)
    word <- gsub("^V", "F", word)
    word <- gsub("^(K|Q)", "C", word)
    word <- gsub("^J", "G", word)
    word <- gsub("^Z", "S", word)

    ## First character of key = first character of name
    first <- substr(word, 1, 1)
    word <- substr(word, 2, nchar(word))

    ## R -> 6, if not followed by vowel or end of name
    word <- gsub("R[AEHIOUWY]|R$", "6", word)
    word <- gsub("R", "", word)

    ## L -> 4, if not followed by vowel or end of name
    word <- gsub("L[AEHIOUWY]|L$", "4", word)
    word <- gsub("L", "", word)

    ## Delete vowels and Y, W, and H
    word <- gsub("A|E|H|I|O|U|W|Y", "", word)

    ## M, N -> 5, ignore next letter if either D or G.
    word <- gsub("[MN][DG]*", "5", word)

    ## B, F, P, V -> 1
    word <- gsub("B|F|P|V", "1", word)

    ## D, T -> 3, if not followed by C
    word <- gsub("[DT]C", "C", word)
    word <- gsub("[DT]", "3", word)

    ## C, G, J, K, Q, S, X, Z -> 2
    word <- gsub("C|G|J|K|Q|S|X|Z", "2", word)

    ## Remove duplicate consecutive characters
    word <- gsub("([0-6])\\1+", "\\1", word)

    ## Append word except for first character to first
    word <- paste(first, word, sep = "")

    ## Zero-pad and truncate to requested length
    word <- gsub("$", paste(rep(0, maxCodeLen), collapse = ""), word)
    word <- substr(word, 1, maxCodeLen)

    return(word)
}
