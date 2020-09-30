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
#' @details The \code{nysiis} function phentically encodes the given
#' string using the New York State Identification and Intelligence
#' System (NYSIIS) algorithm. The algorithm is based on the
#' implementation provided by Wikipedia and is implemented in pure R
#' using regular expressions.
#'
#' The variable \code{maxCodeLen} is the limit on how long the returned
#' NYSIIS code should be.  The default is 6.
#'
#' The variable \code{modified} directs \code{nysiis} to use the
#' modified method instead of the original.
#'
#' The \code{nysiis} algorithm is only defined for inputs over the
#' standard English alphabet, \emph{i.e.}, "A-Z.". Non-alphabetical
#' characters are removed from the string in a locale-dependent fashion.
#' This strips spaces, hyphens, and numbers.  Other letters, such as
#' "Ü," may be permissible in the current locale but are unknown to
#' \code{nysiis}.  For inputs outside of its known range, the output is
#' undefined and \code{NA} is returned and a \code{warning} this thrown.
#' If \code{clean} is \code{FALSE}, \code{nysiis} attempts to process the
#' strings.  The default is \code{TRUE}.
#'
#' @return the NYSIIS encoded character vector
#'
#' @references
#'
#' James P. Howard, II, "Phonetic Spelling Algorithm Implementations
#' for R," \emph{Journal of Statistical Software}, vol. 25, no. 8,
#' (2020), p. 1--21, <10.18637/jss.v095.i08>.
#'
#' Robert L. Taft, \emph{Name search techniques}, Bureau of Systems
#' Development, Albany, New York, 1970.
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
    ## Both NYSIIS and the modified NYSIIS are based on the
    ## implementation described at
    ## http://www.dropby.com/NYSIISTextStrings.html

    ## First, uppercase it and test for unprocessable characters
    word <- toupper(word)
    word[is.null(word)] <- NA
    listNAs <- is.na(word)
    if(any(nonalpha <- grepl("[^A-Z]", word, perl = TRUE)) && clean)
        warning("unknown characters found, results may not be consistent")
    word <- gsub("[^A-Z]*", "", word, perl = TRUE)
    
    if(modified == TRUE)
        word <- nysiis_modified(word, maxCodeLen)
    else
        word <- nysiis_original(word, maxCodeLen)

    ## Yeah, we already processed them, but now get rid of them
    word[listNAs] <- NA
    if(clean)
        word[nonalpha] <- NA

    return(word)
}

nysiis_original <- function(word, maxCodeLen = 6) {

    ## Translate first characters of name: MAC to MCC, KN to N, K to C, PH,
    ## PF to FF, SCH to SSS
    word <- gsub("^MAC", "MCC", word, perl = TRUE)
    word <- gsub("KN", "NN", word, perl = TRUE)
    word <- gsub("K", "C", word, perl = TRUE)
    word <- gsub("^PF", "FF", word, perl = TRUE)
    word <- gsub("PH", "FF", word, perl = TRUE)
    word <- gsub("SCH", "SSS", word, perl = TRUE)

    ## Translate last characters of name: EE to Y, IE to Y, DT, RT, RD,
    ## NT, ND to D
    word <- gsub("EE$", "Y", word, perl = TRUE)
    word <- gsub("IE$", "Y", word, perl = TRUE)
    word <- gsub("DT$", "D", word, perl = TRUE)
    word <- gsub("RT$", "D", word, perl = TRUE)
    word <- gsub("RD$", "D", word, perl = TRUE)
    word <- gsub("NT$", "D", word, perl = TRUE)
    word <- gsub("ND$", "D", word, perl = TRUE)

    ## First character of key = first character of name.
    first <- substr(word, 1, 1)
    word <- substr(word, 2, nchar(word))

    ## EV to AF else A, E, I, O, U to A
    word <- gsub("EV", "AF", word, perl = TRUE)
    word <- gsub("E|I|O|U", "A", word, perl = TRUE)

    ## Q to G, Z to S, M to N
    word <- gsub("Q", "G", word, perl = TRUE)
    word <- gsub("Z", "S", word, perl = TRUE)
    word <- gsub("M", "N", word, perl = TRUE)

    ## KN to N else K to C
    ## SCH to SSS, PH to FF
    ## Rules are implemented as part of opening block

    ## Put back first letter before applying remaining rules
    word <- paste(first, word, sep = "")

    ## H to If previous or next is non-vowel, previous.
    word <- gsub("([^AEIOU])H", "\\1\\1", word, perl = TRUE)
    word <- gsub("(.)H(?=([^AEIOU]|$))", "\\1\\1", word, perl= TRUE)

    ## W to If previous is vowel, A
    word <- gsub("([AEIOU])W", "A", word, perl = TRUE)

    ## Remove duplicate consecutive characters
    word <- gsub("([A-Z])\\1+", "\\1", word, perl = TRUE)

    ## If last character is S, remove it
    word <- gsub("S$", "", word, perl = TRUE)

    ## If last characters are AY, replace with Y
    word <- gsub("AY$", "Y", word, perl = TRUE)

    ## If last character is A, remove it
    word <- gsub("A$", "", word, perl = TRUE)

    ## Truncate to requested length
    word <- substr(word, 1, maxCodeLen)

    return(word)
}

nysiis_modified <- function(word, maxCodeLen = 6) {

    ## Translate first characters of name: MAC to MC, PF to FF
    word <- gsub("^MAC", "MC", word, perl = TRUE)
    word <- gsub("^PF", "FF", word, perl = TRUE)

    ## First character of key = first character of name.
    first <- substr(word, 1, 1)

    ## Remove a trailing S
    word <- gsub("S$|Z$", "", word, perl = TRUE)

    ## Translate last characters of name
    word <- gsub("IX$", "IC", word, perl = TRUE)
    word <- gsub("EX$", "EC", word, perl = TRUE)
    word <- gsub("YE$|EE$|IE$", "Y", word, perl = TRUE)
    word <- gsub("DT$|RT$|RD$|NT$|ND$", "D", word, perl = TRUE)

    ## transcode 'EV' to 'EF' if not at start of name
    word <- gsub("(.+)EV", "\1EF", word, perl = TRUE)

    ## EV to AF else A, E, I, O, U to A
    word <- gsub("E|I|O|U", "A", word, perl = TRUE)

    ## W to If previous is vowel, A
    word <- gsub("([AEIOU])W", "A", word, perl = TRUE)

    ## transcode 'GHT' to 'GT'
    word <- gsub("GHT", "GT", word, perl = TRUE)

    ## transcode 'DG' to 'G'
    word <- gsub("DG", "G", word, perl = TRUE)

    ## Q to G,  M to N, SH to S, SCH to S, YW to Y, Y to A,
    word <- gsub("M", "N", word, perl = TRUE)
    word <- gsub("Q", "G", word, perl = TRUE)
    word <- gsub("(.+)SH", "\\1S", word, perl = TRUE)
    word <- gsub("(.+)SCH", "\\1S", word, perl = TRUE)
    word <- gsub("YW", "Y", word, perl = TRUE)

    ## if not first or last character, change 'Y' to 'A'
    last <- substring(word, nchar(word), nchar(word))
    word <- substring(word, 1, nchar(word) - 1)
    word <- gsub("Y", "A", word, perl = TRUE)
    word <- paste(word, last, sep = "")

    ## WR to R, Z to S
    word <- gsub("WR", "R", word, perl = TRUE)
    word <- gsub("Z", "S", word, perl = TRUE)

    ## Remove duplicate consecutive characters
	word <- gsub("([A-Z])\\1+", "\\1", word, perl = TRUE)

    ## If last character is A, remove it
    word <- gsub("A$", "", word, perl = TRUE)

    ## Append word except for first character to first
    word <- substr(word, 2, nchar(word))
    word <- paste(first, word, sep = "")

    ## transcode 'PH' to 'F'
    word <- gsub("PH", "F", word, perl = TRUE)

    ## change 'KN' to 'N', else 'K' to 'C'
    word <- gsub("KN", "N", word, perl = TRUE)
    word <- gsub("K", "C", word, perl = TRUE)

    ## H to If previous or next is non-vowel, previous.
    word <- gsub("([^AEIOU])H", "\\1", word, perl = TRUE)
    word <- gsub("(.)H[^AEIOU]", "\\1", word, perl = TRUE)

    ## transcode terminal 'AY' to 'Y'
    word <- gsub("AY$", "Y", word, perl = TRUE)

    ## Remove duplicate consecutive characters
    word <- gsub("([A-Z])\\1+", "\\1", word, perl = TRUE)

    ## Truncate to requested length
    word <- substr(word, 1, maxCodeLen)

    return(word)
}
