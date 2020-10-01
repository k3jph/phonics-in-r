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

#' @rdname metaphone
#' @name metaphone
#' @title Generate phonetic versions of strings with Metaphone
#'
#' @description
#' The function \code{metaphone} phonentically encodes the
#' given string using the metaphone algorithm.
#'
#' @param word string or vector of strings to encode
#' @param maxCodeLen  maximum length of the resulting encodings, in characters
#' @param clean if \code{TRUE}, return \code{NA} for unknown alphabetical characters
#'
#' @details
#' There is some discrepency with respect to how the metaphone algorithm
#' actually works. For instance, there is a version in the Java Apache
#' Commons library.  There is a version provided within PHP. These do
#' not provide the same results.  On the questionable theory that the
#' implementation in PHP is probably more well known, this code should
#' match it in output.
#'
#' This implementation is based on a Javascript implementation which is
#' itself based on the PHP internal implementation.
#'
#' The variable \code{maxCodeLen} is the limit on how long the returned
#' metaphone should be.
#'
#' The \code{metaphone} algorithm is only defined for inputs over the
#' standard English alphabet, \emph{i.e.}, "A-Z.". Non-alphabetical
#' characters are removed from the string in a locale-dependent fashion.
#' This strips spaces, hyphens, and numbers.  Other letters, such as
#' "Ü," may be permissible in the current locale but are unknown to
#' \code{metaphone}.  For inputs outside of its known range, the output
#' is undefined and \code{NA} is returned and a \code{warning} this
#' thrown.  If \code{clean} is \code{FALSE}, \code{metaphone} attempts
#' to process the strings.  The default is \code{TRUE}.
#'
#' @return a character vector containing the metaphones of \code{word},
#' or an NA if the \code{word} value is NA
#'
#' @references
#'
#' James P. Howard, II, "Phonetic Spelling Algorithm Implementations
#' for R," \emph{Journal of Statistical Software}, vol. 25, no. 8,
#' (2020), p. 1--21, <10.18637/jss.v095.i08>.
#'
#' @family phonics
#'
#' @examples
#' metaphone("wheel")
#' metaphone(c("school", "benji"))
#'
#' @export
metaphone <- function(word, maxCodeLen = 10L, clean = TRUE) {

    ## First, uppercase it and test for unprocessable characters
    word <- toupper(word)
    word[is.null(word)] <- NA
    if(any(nonalpha <- grepl("[^A-Z]", word, perl = TRUE)) && clean)
        warning("unknown characters found, results may not be consistent")
    word <- gsub("[^A-Z]*", "", word, perl = TRUE)
    
    word <- metaphone_internal(word, maxCodeLen)

    ## Yeah, we already processed them, but now get rid of them
    if(clean)
        word[nonalpha] <- NA

    return(word)
}

