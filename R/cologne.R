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
#' \code{maxCodeLen = NULL} preserves the algorithm's historical unbounded
#' output. An explicit nonnegative whole-number value truncates each code to
#' at most that many characters.
#'
#' The \code{cologne} algorithm is only defined for inputs over the
#' standard English alphabet, \emph{i.e.}, "A-Z," "Ä," "Ö," "Ü," and
#' "ß." Non-alphabetical characters are removed from the string in a
#' locale-dependent fashion.  This strips spaces, hyphens, and numbers.
#' Other letters, such as "ç," may be permissible in the current locale
#' but are unknown to \code{cologne}.  For inputs outside of its known
#' range, the output is undefined and \code{NA} is returned and a
#' \code{warning} is issued.  If \code{clean} is \code{FALSE},
#' \code{cologne} attempts to process the strings.  The default is
#' \code{TRUE}.
#'
#' @return the Cologne encoded character vector
#'
#' @references
#'
#' James P. Howard, II, "Phonetic Spelling Algorithm Implementations
#' for R," \emph{Journal of Statistical Software}, vol. 95, no. 8,
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

    if(!is.null(maxCodeLen))
        maxCodeLen <- .validate_max_code_len(maxCodeLen)

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
    
    word <- vapply(word, cologne_encode_one, character(1), USE.NAMES = FALSE)

    if(!is.null(maxCodeLen))
        word <- substr(word, 1, maxCodeLen)

    ## Yeah, we already processed them, but now get rid of them
    word[listNulls] <- NA
    word[listNAs] <- NA
    if(clean)
        word[nonalpha] <- NA

    return(word)
}

cologne_encode_one <- function(word) {
    if(is.na(word))
        return(NA_character_)
    if(!nzchar(word))
        return("")

    letters <- strsplit(word, "", fixed = TRUE)[[1]]
    code <- character()
    for(i in seq_along(letters)) {
        current <- letters[i]
        previous <- if(i > 1L) letters[i - 1L] else ""
        following <- if(i < length(letters)) letters[i + 1L] else ""

        value <- if(current %in% c("A", "E", "I", "J", "O", "U", "Y")) {
            "0"
        } else if(current == "H") {
            ""
        } else if(current == "B") {
            "1"
        } else if(current == "P") {
            if(following == "H") "3" else "1"
        } else if(current %in% c("D", "T")) {
            if(following %in% c("C", "S", "Z")) "8" else "2"
        } else if(current %in% c("F", "V", "W")) {
            "3"
        } else if(current %in% c("G", "K", "Q")) {
            "4"
        } else if(current == "C") {
            if(i == 1L)
                if(following %in% c("A", "H", "K", "L", "O", "Q", "R", "U", "X")) "4" else "8"
            else if(previous %in% c("S", "Z") ||
                    !(following %in% c("A", "H", "K", "O", "Q", "U", "X")))
                "8"
            else
                "4"
        } else if(current == "X") {
            if(previous %in% c("C", "K", "Q")) "8" else "48"
        } else if(current == "L") {
            "5"
        } else if(current %in% c("M", "N")) {
            "6"
        } else if(current == "R") {
            "7"
        } else if(current %in% c("S", "Z")) {
            "8"
        } else {
            ""
        }
        code <- c(code, value)
    }

    code <- paste0(code, collapse = "")
    code <- gsub("([0-9])\\1+", "\\1", code, perl = TRUE)
    paste0(substr(code, 1, 1), gsub("0", "", substr(code, 2, nchar(code))))
}
