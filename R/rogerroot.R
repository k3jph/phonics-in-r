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

#' @title Roger Root Name Coding Procedure
#'
#' @description
#' Provides the Roger Root name coding system
#'
#' @param word string or vector of strings to encode
#' @param maxCodeLen  maximum length of the resulting encodings, in characters
#' @param clean if \code{TRUE}, return \code{NA} for unknown alphabetical characters
#'
#' @details
#'
#' The \code{rogerroot} function phonetically encodes the given string
#' using the Roger Root algorithm.  The variable \code{word} is a string
#' or vector of strings to encode.
#'
#' The variable \code{maxCodeLen} is the limit on how long the returned
#' code should be.  The default is 5.
#'
#' The \code{rogerroot} algorithm is only defined for inputs over the
#' standard English alphabet, \emph{i.e.}, "A-Z.". Non-alphabetical
#' characters are removed from the string in a locale-dependent fashion.
#' This strips spaces, hyphens, and numbers.  Other letters, such as
#' "Ü," may be permissible in the current locale but are unknown to
#' \code{rogerroot}.  For inputs outside of its known range, the output
#' is undefined and \code{NA} is returned and a \code{warning} this
#' thrown.  If \code{clean} is \code{FALSE}, \code{rogerroot} attempts
#' to process the strings.  The default is \code{TRUE}.
#'
#' @return the Roger Root encoded character vector
#'
#' @references
#'
#' James P. Howard, II, "Phonetic Spelling Algorithm Implementations
#' for R," \emph{Journal of Statistical Software}, vol. 95, no. 8,
#' (2020), p. 1--21, <10.18637/jss.v095.i08>.
#'
#' Billy T. Lynch and William L. Arends, \emph{Selection of a Surname
#' Coding Procedure for the SRS Record Linkage System}, United States
#' Department of Agriculture, 1977, Appendix B.
#'
#' @family phonics
#'
#' @examples
#' rogerroot("William")
#' rogerroot(c("Peter", "Peady"))
#' rogerroot("Stevenson")
#'
#' @export
rogerroot <- function(word, maxCodeLen = 5, clean = TRUE) {

    maxCodeLen <- .validate_max_code_len(maxCodeLen)

    ## First, uppercase it and test for unprocessable characters
    word <- toupper(word)
    word[is.null(word)] <- NA
    listNAs <- is.na(word)
    if(any(nonalpha <- grepl("[^A-Z]", word, perl = TRUE)) && clean)
        warning("unknown characters found, results may not be consistent")
    word <- gsub("[^A-Z]*", "", word, perl = TRUE)
    
    word <- vapply(word, rogerroot_encode_one, character(1), USE.NAMES = FALSE)

    ## Truncate to requested length
    empty <- !is.na(word) & !nzchar(word)
    zeros <- paste(rep(0, maxCodeLen), sep = "", collapse = "")
    word <- paste0(word, zeros)
    word <- substr(word, 1, maxCodeLen)
    word[empty] <- ""
    
    ## Yeah, we already processed them, but now get rid of them
    word[listNAs] <- NA
    if(clean)
        word[nonalpha] <- NA

    return(word)
}

rogerroot_encode_one <- function(word) {
    if(is.na(word))
        return(NA_character_)
    if(!nzchar(word))
        return("")

    firstTable <- c(
        TSCH = "06", SCH = "06", CE = "00", CH = "06", CI = "00",
        CY = "00", DC = "07", GF = "08", GM = "03", GN = "02",
        KN = "02", PF = "08", PH = "08", PN = "02", SH = "06",
        TSH = "06", TS = "00", WR = "04", A = "1", B = "09",
        C = "07", D = "01", E = "1", F = "08", G = "07", H = "2",
        I = "1", J = "3", K = "07", L = "05", M = "03", N = "02",
        O = "1", P = "09", Q = "07", R = "04", S = "00", T = "01",
        U = "1", V = "08", W = "4", X = "07", Y = "5", Z = "00"
    )
    basicTable <- c(
        TSCH = "6", SCH = "6", CE = "0", CH = "6", CI = "0",
        CY = "0", DC = "7", PH = "8", SH = "6", TSH = "6",
        TS = "0", B = "9", C = "7", D = "1", F = "8", G = "7",
        J = "6", K = "7", L = "5", M = "3", N = "2", P = "8",
        Q = "7", R = "4", S = "0", T = "1", V = "8", X = "7",
        Z = "0"
    )

    firstPattern <- names(firstTable)[startsWith(word, names(firstTable))][1]
    code <- unname(firstTable[firstPattern])
    lastValue <- substr(code, nchar(code), nchar(code))
    position <- nchar(firstPattern) + 1L

    while(position <= nchar(word)) {
        remaining <- substr(word, position, nchar(word))
        pattern <- names(basicTable)[startsWith(remaining, names(basicTable))][1]
        if(is.na(pattern)) {
            lastValue <- ""
            position <- position + 1L
        } else {
            value <- unname(basicTable[pattern])
            if(value != lastValue)
                code <- paste0(code, value)
            lastValue <- value
            position <- position + nchar(pattern)
        }
    }

    code
}
