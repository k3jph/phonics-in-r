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

#' @title Oxford Name Compression Algorithm
#'
#' @description
#' The Oxford Name Compression Algorithm name coding procedure
#'
#' @param word string or vector of strings to encode
#' @param maxCodeLen   maximum length of the resulting encodings, in characters
#' @param clean if \code{TRUE}, return \code{NA} for unknown alphabetical characters
#' @param modified if \code{TRUE}, use the modified \code{nysiis} function
#' @param refined if \code{TRUE}, use the \code{refinedSoundex} function
#'
#' @details
#'
#' The variable \code{word} is the name to be encoded.  The variable
#' \code{maxCodeLen} is the limit on how long the returned name code
#' should be.  The default is 4.
#'
#' The \code{onca} algorithm is only defined for inputs over the
#' standard English alphabet, \emph{i.e.}, "A-Z.". Non-alphabetical
#' characters are removed from the string in a locale-dependent fashion.
#' This strips spaces, hyphens, and numbers.  Other letters, such as
#' "Ü," may be permissible in the current locale but are unknown to
#' \code{onca}.  For inputs outside of its known range, the output is
#' undefined and \code{NA} is returned and a \code{warning} this thrown.
#' If \code{clean} is \code{FALSE}, \code{onca} attempts to process the
#' strings.  The default is \code{TRUE}.
#'
#' @return the ONCA encoded character vector
#'
#' @references
#'
#' Gill, Leicester. "OX-LINK: the Oxford medical record linkage system." (1997).
#'
#' James P. Howard, II, "Phonetic Spelling Algorithm Implementations
#' for R," \emph{Journal of Statistical Software}, vol. 25, no. 8,
#' (2020), p. 1--21, <10.18637/jss.v095.i08>.
#'
#' @family phonics
#'
#' @examples
#' onca("William")
#' onca(c("Peter", "Peady"))
#' onca("Stevenson", maxCodeLen = 8)
#'
#' @export
onca <- function(word, maxCodeLen = 4, clean = TRUE, modified = FALSE, refined = FALSE) {

    ## Yes, it really is this simple, but maxCodeLen * 2 is kind of eyeballing it
    word <- nysiis(word, maxCodeLen = maxCodeLen * 2, clean = clean)
    if(refined)
        word <- refinedSoundex(word, maxCodeLen, clean = clean)
    else
        word <- soundex(word, maxCodeLen, clean = clean)
    
    return(word)
}
