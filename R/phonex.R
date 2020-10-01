## Copyright (c) 2015-2019, James P. Howard, II <jh@jameshoward.us>
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
#' @param clean if \code{TRUE}, return \code{NA} for unknown alphabetical characters
#'
#' @details
#'
#' The variable \code{word} is the name to be encoded.  The variable
#' \code{maxCodeLen} is the limit on how long the returned name code
#' should be.  The default is 4.
#'
#' The \code{phonex} algorithm is only defined for inputs over the
#' standard English alphabet, \emph{i.e.}, "A-Z," "Ä," "Ö," "Ü," and
#' "ß." Non-alphabetical characters are removed from the string in a
#' locale-dependent fashion.  This strips spaces, hyphens, and numbers.
#' Other letters, such as "ç," may be permissible in the current locale
#' but are unknown to \code{phonex}.  For inputs outside of its known
#' range, the output is undefined and \code{NA} is returned and a
#' \code{warning} this thrown.  If \code{clean} is \code{FALSE},
#' \code{phonex} attempts to process the strings.  The default is
#' \code{TRUE}.
#' 
#' @return the Phonex encoded character vector
#'
#' @references
#'
#' James P. Howard, II, "Phonetic Spelling Algorithm Implementations
#' for R," \emph{Journal of Statistical Software}, vol. 25, no. 8,
#' (2020), p. 1--21, <10.18637/jss.v095.i08>.
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
phonex <- function(word, maxCodeLen = 4, clean = TRUE) {

    ## Remove umlauts and eszett
    word <- toupper(word)
    word <- gsub("\u00C4", "A", word, perl = TRUE)
    word <- gsub("\u00DC", "U", word, perl = TRUE)
    word <- gsub("\u00D6", "O", word, perl = TRUE)
    word <- gsub("\u00DF", "S", word, perl = TRUE)

    ## First, uppercase it and test for unprocessable characters
    word[is.null(word)] <- NA
    listNAs <- is.na(word)
    if(any(nonalpha <- grepl("[^A-Z]", word, perl = TRUE)) && clean)
        warning("unknown characters found, results may not be consistent")
    word <- gsub("[^A-Z]*", "", word, perl = TRUE)
    
    ## Preprocess the name
    word <- gsub("S+$", "", word, perl = TRUE)
    word <- gsub("^KN", "N", word, perl = TRUE)
    word <- gsub("^WR", "R", word, perl = TRUE)
    word <- gsub("^PH", "F", word, perl = TRUE)
    word <- gsub("^H", "", word, perl = TRUE)
    word <- gsub("^(E|I|O|U|Y)", "A", word, perl = TRUE)
    word <- gsub("^P", "B", word, perl = TRUE)
    word <- gsub("^V", "F", word, perl = TRUE)
    word <- gsub("^(K|Q)", "C", word, perl = TRUE)
    word <- gsub("^J", "G", word, perl = TRUE)
    word <- gsub("^Z", "S", word, perl = TRUE)

    ## First character of key = first character of name
    first <- substr(word, 1, 1)
    word <- substr(word, 2, nchar(word))

    ## R -> 6, if not followed by vowel or end of name
    word <- gsub("R[AEHIOUWY]|R$", "6", word, perl = TRUE)
    word <- gsub("R", "", word, perl = TRUE)

    ## L -> 4, if not followed by vowel or end of name
    word <- gsub("L[AEHIOUWY]|L$", "4", word, perl = TRUE)
    word <- gsub("L", "", word, perl = TRUE)

    ## Delete vowels and Y, W, and H
    word <- gsub("A|E|H|I|O|U|W|Y", "", word, perl = TRUE)

    ## M, N -> 5, ignore next letter if either D or G.
    word <- gsub("[MN][DG]*", "5", word, perl = TRUE)

    ## B, F, P, V -> 1
    word <- gsub("B|F|P|V", "1", word, perl = TRUE)

    ## D, T -> 3, if not followed by C
    word <- gsub("[DT]C", "C", word, perl = TRUE)
    word <- gsub("[DT]", "3", word, perl = TRUE)

    ## C, G, J, K, Q, S, X, Z -> 2
    word <- gsub("C|G|J|K|Q|S|X|Z", "2", word, perl = TRUE)

    ## Remove duplicate consecutive characters
    word <- gsub("([0-6])\\1+", "\\1", word, perl = TRUE)

    ## Append word except for first character to first
    word <- paste(first, word, sep = "")

    ## Zero-pad and truncate to requested length
    zeros <- paste(rep(0, maxCodeLen), sep = "", collapse = "")
    word <- gsub("$", zeros, word, perl = TRUE)
    word <- substr(word, 1, maxCodeLen)
    word <- gsub(zeros, "", word, perl = TRUE)

    ## Yeah, we already processed them, but now get rid of them
    word[listNAs] <- NA
    if(clean)
        word[nonalpha] <- NA

    return(word)
}
