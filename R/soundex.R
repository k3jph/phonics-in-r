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

#' @rdname soundex
#' @name soundex
#' @title Soundex
#'
#' @description
#' The Soundex phonetic algorithms
#'
#' @param word string or vector of strings to encode
#' @param maxCodeLen  maximum length of the resulting encodings, in characters
#' @param clean if \code{TRUE}, return \code{NA} for unknown alphabetical characters
#'
#' @details The function \code{soundex} phonentically encodes the given
#' string using the soundex algorithm.  The function \code{refinedSoundex}
#' uses Apache's refined soundex algorithm.  Both implementations are loosely
#' based on the Apache Commons Java editons.
#'
#' The variable \code{maxCodeLen} is the limit on how long the returned
#' soundex should be.
#'
#' The \code{soundex} and \code{revisedSoundex} algorithms are only
#' defined for inputs over the standard English alphabet, \emph{i.e.},
#' "A-Z." Non-alphabetical characters are removed from the string in a
#' locale-dependent fashion.  This strips spaces, hyphens, and numbers.
#' Other letters, such as "Ü," may be permissible in the current locale
#' but are unknown to \code{soundex} and \code{revisedSoundex}.  For
#' inputs outside of its known range, the output is undefined and
#' \code{NA} is returned and a \code{warning} this thrown.  If
#' \code{clean} is \code{FALSE}, \code{soundex} and
#' \code{revisedSoundex} attempts to process the strings.  The default
#' is \code{TRUE}.
#'
#' @return soundex encoded character vector
#'
#' @section Caveats:
#' The \code{soundex} and \code{refinedSoundex} algorithms are only
#' defined for inputs over the standard English alphabet, \emph{i.e.},
#' "A-Z." For inputs outside this range, the output is undefined.
#'
#' @references
#'
#' Charles P. Bourne and Donald F. Ford, "A study of methods for
#' systematically abbreviating English words and names," \emph{Journal
#' of the ACM}, vol. 8, no. 4 (1961), p. 538-552.
#'
#' James P. Howard, II, "Phonetic Spelling Algorithm Implementations
#' for R," \emph{Journal of Statistical Software}, vol. 25, no. 8,
#' (2020), p. 1--21, <10.18637/jss.v095.i08>.
#'
#' Howard B. Newcombe, James M. Kennedy, "Record linkage: making
#' maximum use of the discriminating power of identifying information,"
#' \emph{Communications of the ACM}, vol. 5, no. 11 (1962), p. 563-566.
#'
#' @family phonics
#'
#' @examples
#' soundex("wheel")
#' soundex(c("school", "benji"))
#'
#' @export
soundex <- function(word, maxCodeLen = 4L, clean = TRUE) {

    ## First, uppercase it and test for unprocessable characters
    word <- toupper(word)
    word[is.null(word)] <- NA
    if(any(nonalpha <- grepl("[^A-Z]", word, perl = TRUE)) && clean)
        warning("unknown characters found, results may not be consistent")
    word <- gsub("[^A-Z]*", "", word, perl = TRUE)
    
    word <- soundex_internal(word, maxCodeLen)

    ## Yeah, we already processed them, but now get rid of them
    if(clean)
        word[nonalpha] <- NA

    return(word)
}

#' @rdname soundex
#' @export
refinedSoundex <- function(word, maxCodeLen = 10L, clean = TRUE) {
    
    ## First, uppercase it and test for unprocessable characters
    word <- toupper(word)
    word[is.null(word)] <- NA
    if(any(nonalpha <- grepl("[^A-Z]", word, perl = TRUE)) && clean)
        warning("unknown characters found, results may not be consistent")
    word <- gsub("[^A-Z]*", "", word, perl = TRUE)
    
    word <- refinedSoundex_internal(word, maxCodeLen)

    ## Yeah, we already processed them, but now get rid of them
    if(clean)
        word[nonalpha] <- NA

    return(word)
}
