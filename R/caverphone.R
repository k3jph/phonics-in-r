## Copyright (c) 2015, James P. Howard, II <jh@jameshoward.us>
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

#' @title Caverphone
#'
#' @description
#' The Caverphone family of phonetic algorithms
#'
#' @param word string or vector of strings to encode
#' @param maxCodeLen   maximum length of the resulting encodings, in characters
#' @param modified     if \code{TRUE}, use the Caverphone 2 algorithm
#'
#' @details
#'
#' The variable \code{maxCodeLen} is the limit on how long the returned
#' Caverphone code should be.  The default is 6, unless \code{modified}
#' is set to \code{TRUE}, then the default is 10.
#'
#' The variable \code{modified} directs \code{caverphone} to use the
#' Caverphone2 method, instead of the original.
#'
#' @return the Caverphone encoded character vector
#'
#' @section Caveats:
#' The \code{caverphone} algorithm is only
#' defined for inputs over the standard English alphabet, \emph{i.e.},
#' "A-Z." For inputs outside this range, the output is undefined.
#'
#' @references
#'
#' David Hood, "Caverphone: Phonetic matching algorithm," Technical
#' Paper CTP060902, University of Otago, New Zealand, 2002.
#'
#' David Hood, "Caverphone Revisited," Technical Paper CTP150804
#' University of Otago, New Zealand, 2004.
#'
#' @family phonics
#'
#' @examples
#' caverphone("William")
#' caverphone(c("Peter", "Peady"), modified = TRUE)
#' caverphone("Stevenson", maxCodeLen = 4)
#'
#' @export
caverphone <- function(word, maxCodeLen = NULL, modified = FALSE) {
    ## From here on, this is a line-for-line translation of the Apache
    ## Commons Caverphone and Caverphone2 implementations, which both
    ## used regular expressions for substantially all of the work.

    ## Set the maxCodeLen if not set
    if(is.null(maxCodeLen))
        if(modified == TRUE)
            maxCodeLen <- 10
        else
            maxCodeLen <- 6

    ## First, remove any nonalphabetical characters and lowercase it
    word <- gsub("[^[:alpha:]]*", "", word, perl = TRUE)
    word <- tolower(word)

    if(modified == TRUE)
        word <- caverphone_modified(word)
    else
        word <- caverphone_original(word)

    ## Pad the wording with maxCodeLen 1s and truncate
	word <- gsub("$", paste(rep(1, maxCodeLen), collapse = ""), word, perl = TRUE)
    word <- substr(word, 1, maxCodeLen)
    return(word)
}

caverphone_original <- function(word) {

    ## Starting and ending special cases
    word <- gsub("^cough", "cou2f", word, perl = TRUE)
    word <- gsub("^rough", "rou2f", word, perl = TRUE)
    word <- gsub("^tough", "tou2f", word, perl = TRUE)
    word <- gsub("^enough", "enou2f", word, perl = TRUE)
    word <- gsub("^gn", "2n", word, perl = TRUE)
    word <- gsub("mb$", "m2", word, perl = TRUE)

    ## Core encodings
    word <- gsub("cq", "2q", word, perl = TRUE)
    word <- gsub("ci", "si", word, perl = TRUE)
    word <- gsub("ce", "se", word, perl = TRUE)
    word <- gsub("cy", "sy", word, perl = TRUE)
    word <- gsub("tch", "2ch", word, perl = TRUE)
    word <- gsub("c", "k", word, perl = TRUE)
    word <- gsub("q", "k", word, perl = TRUE)
    word <- gsub("x", "k", word, perl = TRUE)
    word <- gsub("v", "f", word, perl = TRUE)
    word <- gsub("dg", "2g", word, perl = TRUE)
    word <- gsub("tio", "sio", word, perl = TRUE)
    word <- gsub("tia", "sia", word, perl = TRUE)
    word <- gsub("d", "t", word, perl = TRUE)
    word <- gsub("ph", "fh", word, perl = TRUE)
    word <- gsub("b", "p", word, perl = TRUE)
    word <- gsub("sh", "s2", word, perl = TRUE)
    word <- gsub("z", "s", word, perl = TRUE)
    word <- gsub("^[aeiou]", "A", word, perl = TRUE)
    word <- gsub("[aeiou]", "3", word, perl = TRUE)
    word <- gsub("3gh3", "3kh3", word, perl = TRUE)
    word <- gsub("gh", "22", word, perl = TRUE)
    word <- gsub("g", "k", word, perl = TRUE)
    word <- gsub("ss*", "S", word, perl = TRUE)
    word <- gsub("tt*", "T", word, perl = TRUE)
    word <- gsub("pp*", "P", word, perl = TRUE)
    word <- gsub("kk*", "K", word, perl = TRUE)
    word <- gsub("ff*", "F", word, perl = TRUE)
    word <- gsub("mm*", "M", word, perl = TRUE)
    word <- gsub("nn*", "N", word, perl = TRUE)
    word <- gsub("w3", "W3", word, perl = TRUE)
    word <- gsub("wy", "Wy", word, perl = TRUE)
    word <- gsub("wh3", "Wh3", word, perl = TRUE)
    word <- gsub("why", "Why", word, perl = TRUE)
    word <- gsub("w", "2", word, perl = TRUE)
    word <- gsub("^h", "A", word, perl = TRUE)
    word <- gsub("h", "2", word, perl = TRUE)
    word <- gsub("r3", "R3", word, perl = TRUE)
    word <- gsub("ry", "Ry", word, perl = TRUE)
    word <- gsub("r", "2", word, perl = TRUE)
    word <- gsub("l3", "L3", word, perl = TRUE)
    word <- gsub("ly", "Ly", word, perl = TRUE)
    word <- gsub("l", "2", word, perl = TRUE)
    word <- gsub("j", "y", word, perl = TRUE)
    word <- gsub("y3", "Y3", word, perl = TRUE)
    word <- gsub("y", "2", word, perl = TRUE)
    word <- gsub("[23]", "", word, perl = TRUE)

    return(word)
}

caverphone_modified <- function(word) {

    ## Starting and ending special cases
    word <- gsub("e$", "", word, perl = TRUE)
    word <- gsub("^cough", "cou2f", word, perl = TRUE)
    word <- gsub("^rough", "rou2f", word, perl = TRUE)
    word <- gsub("^tough", "tou2f", word, perl = TRUE)
    word <- gsub("^enough", "enou2f", word, perl = TRUE)
    word <- gsub("^trough", "trou2f", word, perl = TRUE)
    word <- gsub("^gn", "2n", word, perl = TRUE)
    word <- gsub("mb$", "m2", word, perl = TRUE)

    ## Core encodings
    word <- gsub("cq", "2q", word, perl = TRUE)
    word <- gsub("ci", "si", word, perl = TRUE)
    word <- gsub("ce", "se", word, perl = TRUE)
    word <- gsub("cy", "sy", word, perl = TRUE)
    word <- gsub("tch", "2ch", word, perl = TRUE)
    word <- gsub("c", "k", word, perl = TRUE)
    word <- gsub("q", "k", word, perl = TRUE)
    word <- gsub("x", "k", word, perl = TRUE)
    word <- gsub("v", "f", word, perl = TRUE)
    word <- gsub("dg", "2g", word, perl = TRUE)
    word <- gsub("tio", "sio", word, perl = TRUE)
    word <- gsub("tia", "sia", word, perl = TRUE)
    word <- gsub("d", "t", word, perl = TRUE)
    word <- gsub("ph", "fh", word, perl = TRUE)
    word <- gsub("b", "p", word, perl = TRUE)
    word <- gsub("sh", "s2", word, perl = TRUE)
    word <- gsub("z", "s", word, perl = TRUE)
    word <- gsub("^[aeiou]", "A", word, perl = TRUE)
    word <- gsub("[aeiou]", "3", word, perl = TRUE)
    word <- gsub("j", "y", word, perl = TRUE)
    word <- gsub("^y3", "Y3", word, perl = TRUE)
    word <- gsub("^y", "a", word, perl = TRUE)
    word <- gsub("y", "3", word, perl = TRUE)
    word <- gsub("3gh3", "3kh3", word, perl = TRUE)
    word <- gsub("gh", "22", word, perl = TRUE)
    word <- gsub("g", "k", word, perl = TRUE)
    word <- gsub("ss*", "S", word, perl = TRUE)
    word <- gsub("tt*", "T", word, perl = TRUE)
    word <- gsub("pp*", "P", word, perl = TRUE)
    word <- gsub("kk*", "K", word, perl = TRUE)
    word <- gsub("ff*", "F", word, perl = TRUE)
    word <- gsub("mm*", "M", word, perl = TRUE)
    word <- gsub("nn*", "N", word, perl = TRUE)
    word <- gsub("w3", "W3", word, perl = TRUE)
    word <- gsub("wh3", "Wh3", word, perl = TRUE)
    word <- gsub("w$", "3", word, perl = TRUE)
    word <- gsub("w", "2", word, perl = TRUE)
    word <- gsub("^h", "A", word, perl = TRUE)
    word <- gsub("h", "2", word, perl = TRUE)
    word <- gsub("r3", "R3", word, perl = TRUE)
    word <- gsub("r$", "3", word, perl = TRUE)
    word <- gsub("r", "2", word, perl = TRUE)
    word <- gsub("l3", "L3", word, perl = TRUE)
    word <- gsub("l$", "3", word, perl = TRUE)
    word <- gsub("l", "2", word, perl = TRUE)
    word <- gsub("2", "", word, perl = TRUE)
    word <- gsub("3$", "A", word, perl = TRUE)
	word <- gsub("3", "", word, perl = TRUE)

    return(word)
}
