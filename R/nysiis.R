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

#' @title New York State Identification and Intelligence System
#'
#' @description
#' The NYSIIS phonetic algorithm
#'
#' @param word string or vector of strings to encode
#' @param maxCodeLen   maximum length of the resulting encodings, in characters
#' @param modified     if \code{TRUE}, use the modified NYSIIS algorithm
#' @param clean if \code{TRUE}, return \code{NA} for unknown alphabetical characters
#'
#' @details The \code{nysiis} function implements the original NYSIIS
#' character scan described by Taft and the USDA modified scan documented by
#' Lynch and Arends.
#'
#' The variable \code{maxCodeLen} is the limit on how long the returned
#' NYSIIS code should be.  The default is 6.
#'
#' Set \code{modified = TRUE} for the USDA variant. That publication classifies
#' names ending in \code{JR} or \code{SR} as errors; this function warns and
#' returns \code{NA} for those inputs.
#'
#' The \code{nysiis} algorithm is only defined for inputs over the
#' standard English alphabet, \emph{i.e.}, "A-Z.". Non-alphabetical
#' characters are removed from the string in a locale-dependent fashion.
#' This strips spaces, hyphens, and numbers.  Other letters, such as
#' "Ü," may be permissible in the current locale but are unknown to
#' \code{nysiis}.  For inputs outside of its known range, the output is
#' undefined and \code{NA} is returned and a \code{warning} is issued.
#' If \code{clean} is \code{FALSE}, \code{nysiis} attempts to process the
#' strings.  The default is \code{TRUE}.
#'
#' @return the NYSIIS encoded character vector
#'
#' @references
#'
#' James P. Howard, II, "Phonetic Spelling Algorithm Implementations
#' for R," \emph{Journal of Statistical Software}, vol. 95, no. 8,
#' (2020), p. 1--21, <10.18637/jss.v095.i08>.
#'
#' Robert L. Taft, \emph{Name search techniques}, Bureau of Systems
#' Development, Albany, New York, 1970.
#'
#' Billy T. Lynch and William L. Arends, \emph{Selection of a Surname
#' Coding Procedure for the SRS Record Linkage System}, United States
#' Department of Agriculture, 1977, Appendix B.
#'
#' @family phonics
#'
#' @examples
#' nysiis("Robert")
#' nysiis("rupert")
#' nysiis(c("Alabama", "Alaska"), modified = TRUE)
#' nysiis("mississippi", 4)
#'
#' @export
nysiis <- function(word, maxCodeLen = 6, modified = FALSE, clean = TRUE) {
    maxCodeLen <- .validate_max_code_len(maxCodeLen)

    ## First, uppercase it and test for unprocessable characters
    word <- toupper(word)
    word[is.null(word)] <- NA
    listNAs <- is.na(word)
    if(any(nonalpha <- grepl("[^A-Z]", word, perl = TRUE)) && clean)
        warning("unknown characters found, results may not be consistent")
    word <- gsub("[^A-Z]*", "", word, perl = TRUE)
    
    invalidModified <- if(modified)
        !is.na(word) & grepl("(JR|SR)$", word, perl = TRUE)
    else
        rep(FALSE, length(word))
    if(any(invalidModified, na.rm = TRUE))
        warning("modified NYSIIS cannot encode names ending in JR or SR")

    word <- vapply(
        word,
        nysiis_encode_one,
        character(1),
        maxCodeLen = maxCodeLen,
        modified = modified,
        USE.NAMES = FALSE
    )
    word[invalidModified] <- NA_character_

    ## Yeah, we already processed them, but now get rid of them
    word[listNAs] <- NA
    if(clean)
        word[nonalpha] <- NA

    return(word)
}

nysiis_original <- function(word, maxCodeLen = 6) {
    vapply(
        word,
        nysiis_encode_one,
        character(1),
        maxCodeLen = maxCodeLen,
        modified = FALSE,
        USE.NAMES = FALSE
    )
}

nysiis_modified <- function(word, maxCodeLen = 6) {
    vapply(
        word,
        nysiis_encode_one,
        character(1),
        maxCodeLen = maxCodeLen,
        modified = TRUE,
        USE.NAMES = FALSE
    )
}

nysiis_encode_one <- function(word, maxCodeLen, modified) {
    if(is.na(word))
        return(NA_character_)
    if(!nzchar(word))
        return("")

    vowels <- c("A", "E", "I", "O", "U")
    originalFirst <- substr(word, 1, 1)

    if(startsWith(word, "MAC"))
        word <- sub("^MAC", "MCC", word)
    else if(startsWith(word, "KN"))
        word <- sub("^KN", "NN", word)
    else if(startsWith(word, "K"))
        word <- sub("^K", "C", word)
    else if(startsWith(word, "PH") || startsWith(word, "PF"))
        word <- sub("^(PH|PF)", "FF", word)
    else if(startsWith(word, "SCH"))
        word <- sub("^SCH", "SSS", word)
    else if(modified && startsWith(word, "WR"))
        word <- sub("^WR", "RR", word)
    else if(modified && startsWith(word, "RH"))
        word <- sub("^RH", "RR", word)
    else if(modified && startsWith(word, "DG"))
        word <- sub("^DG", "GG", word)
    else if(modified && originalFirst %in% vowels)
        substr(word, 1, 1) <- "A"

    if(modified)
        word <- sub("[SZ]$", "", word)

    if(grepl("(EE|IE)$", word))
        word <- sub("(EE|IE)$", "Y", word)
    else if(modified && grepl("YE$", word))
        word <- sub("YE$", "Y", word)
    else if(grepl("(DT|RT|RD)$", word))
        word <- sub("(DT|RT|RD)$", "D", word)
    else if(grepl("(NT|ND)$", word))
        word <- sub("(NT|ND)$", if(modified) "N" else "D", word)
    else if(modified && grepl("IX$", word))
        word <- sub("IX$", "ICK", word)
    else if(modified && grepl("EX$", word))
        word <- sub("EX$", "ECK", word)

    if(!nzchar(word))
        return("")

    letters <- strsplit(word, "", fixed = TRUE)[[1]]
    key <- letters[1]

    if(length(letters) > 1L) {
        for(i in 2:length(letters)) {
            current <- letters[i]
            previous <- letters[i - 1L]
            following <- if(i < length(letters)) letters[i + 1L] else ""
            remaining <- paste(letters[i:length(letters)], collapse = "")

            if(current %in% vowels) {
                if(startsWith(remaining, "EV")) {
                    letters[i] <- "A"
                    letters[i + 1L] <- "F"
                } else {
                    letters[i] <- "A"
                }
            } else if(modified && current == "Y" && i < length(letters)) {
                letters[i] <- "A"
            } else if(current == "Q") {
                letters[i] <- "G"
            } else if(current == "Z") {
                letters[i] <- "S"
            } else if(current == "M") {
                letters[i] <- "N"
            } else if(current == "K") {
                letters[i] <- if(following == "N") "N" else "C"
            } else if(startsWith(remaining, "SCH")) {
                letters[i:(i + 2L)] <- if(modified && i + 2L == length(letters))
                    c("S", "S", "A") else c("S", "S", "S")
            } else if(modified && startsWith(remaining, "SH")) {
                letters[i:(i + 1L)] <- if(i + 1L == length(letters))
                    c("S", "A") else c("S", "S")
            } else if(startsWith(remaining, "PH")) {
                letters[i:(i + 1L)] <- c("F", "F")
            } else if(modified && startsWith(remaining, "GHT")) {
                letters[i:(i + 2L)] <- c("T", "T", "T")
            } else if(modified && startsWith(remaining, "DG")) {
                letters[i:(i + 1L)] <- c("G", "G")
            } else if(modified && startsWith(remaining, "WR")) {
                letters[i:(i + 1L)] <- c("R", "R")
            } else if(current == "H" &&
                      (!(previous %in% vowels) || !(following %in% vowels))) {
                letters[i] <- previous
            } else if(current == "W" && previous %in% vowels) {
                letters[i] <- previous
            }

            if(letters[i] != substr(key, nchar(key), nchar(key)))
                key <- paste0(key, letters[i])
        }
    }

    key <- sub("S$", "", key)
    key <- sub("AY$", "Y", key)
    key <- sub("A$", "", key)
    if(modified && startsWith(key, "A"))
        substr(key, 1, 1) <- originalFirst

    substr(key, 1, maxCodeLen)
}
