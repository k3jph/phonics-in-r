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

#' @title Lein Name Coding
#'
#' @description
#' The Lein name coding procedure.
#'
#' @param word string or vector of strings to encode
#' @param maxCodeLen   maximum length of the resulting encodings, in characters
#' @param clean if \code{TRUE}, return \code{NA} for unknown alphabetical characters
#' 
#' @details
#'
#' The variable \code{word} is the name to be encoded.  The variable
#' \code{maxCodeLen} is the limit on how long the returned name code
#' should be.  The default is 4.
#'
#' The \code{lein} algorithm is only defined for inputs over the
#' standard English alphabet, \emph{i.e.}, "A-Z.". Non-alphabetical
#' characters are removed from the string in a locale-dependent fashion.
#' This strips spaces, hyphens, and numbers.  Other letters, such as
#' "Ü," may be permissible in the current locale but are unknown to
#' \code{lein}.  For inputs outside of its known range, the output is
#' undefined and \code{NA} is returned and a \code{warning} this thrown.
#' If \code{clean} is \code{FALSE}, \code{lein} attempts to process the
#' strings.  The default is \code{TRUE}.
#'
#' @return the Lein encoded character vector
#'
#' @references
#'
#' James P. Howard, II, "Phonetic Spelling Algorithm Implementations
#' for R," \emph{Journal of Statistical Software}, vol. 25, no. 8,
#' (2020), p. 1--21, <10.18637/jss.v095.i08>.
#'
#' Billy T. Lynch and William L. Arends. "Selection of surname coding
#' procedure for the SRS record linkage system." United States
#' Department of Agriculture, Sample Survey Research Branch, Research
#' Division, Washington, 1977.
#'
#' @family phonics
#'
#' @examples
#' lein("William")
#' lein(c("Peter", "Peady"))
#' lein("Stevenson", maxCodeLen = 8)
#'
#' @export
lein <- function(word, maxCodeLen = 4, clean = TRUE) {

    ## First, uppercase it and test for unprocessable characters
    word <- toupper(word)
    listNulls <- is.null(word)
    listNAs <- is.na(word)
    if(any(nonalpha <- grepl("[^A-Z]", word, perl = TRUE)) && clean)
        warning("unknown characters found, results may not be consistent")
    word <- gsub("[^A-Z]*", "", word, perl = TRUE)

    ## First character of key = first character of name
    first <- substr(word, 1, 1)
    word <- substr(word, 2, nchar(word))

    ## Delete vowels and Y, W, and H
    word <- gsub("A|E|I|O|U|Y|W|H", "", word, perl = TRUE)

    ## Remove duplicate consecutive characters
    word <- gsub("([A-Z])\\1+", "\\1", word, perl = TRUE)

    ## D, T -> 1
    word <- gsub("D|T", "1", word, perl = TRUE)

    ## M, N -> 2
    word <- gsub("M|N", "2", word, perl = TRUE)

    ## L, R -> 3
    word <- gsub("L|R", "3", word, perl = TRUE)

    ## B, F, P, V -> 4
    word <- gsub("B|F|P|V", "4", word, perl = TRUE)

    ## C, J, K, G, Q, S, X, Z -> 5
    word <- gsub("C|J|K|G|Q|S|X|Z", "5", word, perl = TRUE)

    ## Append word except for first character to first
    word <- paste(first, word, sep = "")

    ## Zero-pad and truncate to requested length
    word <- gsub("$", paste(rep(0, maxCodeLen), collapse = ""), word, perl = TRUE)
    word <- substr(word, 1, maxCodeLen)

    ## Manage some edge cases
    word <- sub("0000", "", word, perl = TRUE)

    ## Yeah, we already processed them, but now get rid of them
    word[listNulls] <- NA
    word[listNAs] <- NA
    if(clean)
        word[nonalpha] <- NA

    return(word)
}

