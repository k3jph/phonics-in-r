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

#' @title Cologne Phonetic Name Coding
#'
#' @description
#' The Cologne phonetic name coding procedure.
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
#' The \code{cologne} algorithm is only defined for inputs over the
#' standard English alphabet, \emph{i.e.}, "A-Z," "Ä," "Ö," "Ü," and
#' "ß." Non-alphabetical characters are removed from the string in a
#' locale-dependent fashion.  This strips spaces, hyphens, and numbers.
#' Other letters, such as "ç," may be permissible in the current locale
#' but are unknown to \code{cologne}.  For inputs outside of its known
#' range, the output is undefined and \code{NA} is returned and a
#' \code{warning} this thrown.  If \code{clean} is \code{FALSE},
#' \code{cologne} attempts to process the strings.  The default is
#' \code{TRUE}.
#'
#' @return the Cologne encoded character vector
#'
#' @references
#'
#' James P. Howard, II, "Phonetic Spelling Algorithm Implementations
#' for R," \emph{Journal of Statistical Software}, vol. 25, no. 8,
#' (2020), p. 1--21, <10.18637/jss.v095.i08>.
#'
#' Hans Joachim Postel. "Die Koelner Phonetik. Ein Verfahren zur
#' Identifizierung von Personennamen auf der Grundlage der
#' Gestaltanalyse."  \emph{IBM-Nachrichten} 19. Jahrgang, 1969,
#' p. 925-931.
#'
#' @family phonics
#'
#' @examples
#' cologne("William")
#' cologne(c("Peter", "Peady"))
#' cologne("Stevenson", maxCodeLen = 8)
#'
#' @export
cologne <- function(word, maxCodeLen = NULL, clean = TRUE) {

    ## First, uppercase it and test for unprocessable characters
    word <- toupper(word)
    listNulls <- is.null(word)
    listNAs <- is.na(word)
    
    ## Remove umlauts and eszett
    word <- gsub("\u00C4", "A", word, perl = TRUE)
    word <- gsub("\u00DC", "U", word, perl = TRUE)
    word <- gsub("\u00D6", "O", word, perl = TRUE)
    word <- gsub("\u00DF", "S", word, perl = TRUE)

    if(any(nonalpha <- grepl("[^A-Z]", word, perl = TRUE)) && clean)
        warning("unknown characters found, results may not be consistent")
    word <- gsub("[^A-Z]*", "", word, perl = TRUE)
    
    ## Work through the rules...but backwards, mostly, here's 8s
	word <- gsub("([CKQ])X", "\\18", word, perl = TRUE)
    word <- gsub("[DT]([CSZ])", "8\\1", word, perl = TRUE)
    word <- gsub("([SZ])C", "\\18", word, perl = TRUE)
    word <- gsub("^C([^AHKLOQRUX])", "8\\1", word, perl = TRUE)
    word <- gsub("C([^AHKOQUX])", "8\\1", word, perl = TRUE)
    word <- gsub("[SZ]", "8", word, perl = TRUE)

    ## Rule #7
    word <- gsub("R", "7", word, perl = TRUE)

    ## Rule #6
    word <- gsub("[MN]", "6", word, perl = TRUE)

    ## Rule #5
    word <- gsub("L", "5", word, perl = TRUE)

    ## Rule #48
    word <- gsub("X", "48", word, perl = TRUE)

    ## Rule #4
    word <- gsub("[CGKQ]", "4", word, perl = TRUE)

    ## Rule #3
    ## And we can strip the H since it will not be coded
    word <- gsub("PH|[FVW]", "3", word, perl = TRUE)

    ## Rule #2
    word <- gsub("[DT]", "2", word, perl = TRUE)

    ## Rule #1
    word <- gsub("[BP]", "1", word, perl = TRUE)

    ## Rule #H
    word <- gsub("H", "", word, perl = TRUE)

    ## Rule #0
    word <- gsub("[AEIJOUY]", "0", word, perl = TRUE)

    ## Remove duplicate consecutive characters
    word <- gsub("([0-9])\\1+", "\\1\\2", word, perl = TRUE)

    ## Remove all 0s, except first
    first <- substr(word, 1, 1)
    word <- substr(word, 2, nchar(word))
	word <- gsub("0", "", word, perl = TRUE)
    word <- paste(first, word, sep = "")

    ## Yeah, we already processed them, but now get rid of them
    word[listNulls] <- NA
    word[listNAs] <- NA
    if(clean)
        word[nonalpha] <- NA

    return(word)
}
