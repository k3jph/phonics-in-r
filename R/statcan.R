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

#' @title Statistics Canada Name Coding
#'
#' @description
#' The modified Statistics Canada name coding procedure
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
#' The \code{statcan} algorithm is only defined for inputs over the
#' standard French alphabet. Non-alphabetical characters are removed
#' from the string in a locale-dependent fashion.  This strips spaces,
#' hyphens, and numbers.  Other letters, such as "Ü," may be permissible
#' in the current locale but are unknown to \code{statcan}.  For inputs
#' outside of its known range, the output is undefined and \code{NA} is
#' returned and a \code{warning} this thrown.  If \code{clean} is
#' \code{FALSE}, \code{statcan} attempts to process the strings.  The
#' default is \code{TRUE}.
#'
#' @return the Statistics Canada encoded character vector
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
#' statcan("William")
#' statcan(c("Peter", "Peady"))
#' statcan("Stevenson", maxCodeLen = 8)
#'
#' @export
statcan <- function(word, maxCodeLen = 4, clean = TRUE) {

    ## Remove umlauts and eszett
    word <- gsub("\u00C0|\u00C2", "A", word, perl = TRUE)
    word <- gsub("\u00C8|\u00C9|\u00CA|\u00CB", "E", word, perl = TRUE)
    word <- gsub("\u00CE|\u00CF", "I", word, perl = TRUE)
    word <- gsub("\u00D4", "O", word, perl = TRUE)
    word <- gsub("\u00D9|\u00DB|\u00DC", "U", word, perl = TRUE)
    word <- gsub("\u0178", "Y", word, perl = TRUE)
    word <- gsub("\u00C7", "C", word, perl = TRUE)

    ## First, uppercase it and test for unprocessable characters
    word <- toupper(word)
    word[is.null(word)] <- NA
    listNAs <- is.na(word)
    if(any(nonalpha <- grepl("[^A-Z]", word, perl = TRUE)) && clean)
        warning("unknown characters found, results may not be consistent")
    word <- gsub("[^A-Z]*", "", word, perl = TRUE)
    
    ## First character of key = first character of name
    first <- substr(word, 1, 1)
    word <- substr(word, 2, nchar(word))

    ## Delete vowels and Y
    word <- gsub("A|E|I|O|U|Y", "", word, perl = TRUE)

    ## Append word except for first character to first
    word <- paste(first, word, sep = "")

    ## Remove duplicate consecutive characters
    word <- gsub("([A-Z])\\1+", "\\1", word, perl = TRUE)

    ## Truncate to requested length
    word <- substr(word, 1, maxCodeLen)

    ## Yeah, we already processed them, but now get rid of them
    word[listNAs] <- NA
    if(clean)
        word[nonalpha] <- NA

    return(word)
}
