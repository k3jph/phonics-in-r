## Copyright (c) 2016, James P. Howard, II <jh@jameshoward.us>
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
#'
#' @details
#'
#' The variable \code{word} is the name to be encoded.  The variable
#' \code{maxCodeLen} is the limit on how long the returned name code
#' should be.  The default is 4.
#' 
#' @return the ONCA encoded character vector
#'
#' @references
#'
#' Gill, Leicester. "OX-LINK: the Oxford medical record linkage system." (1997). 
#'
#' @family phonics
#'
#' @examples
#' onca("William")
#' onca(c("Peter", "Peady"))
#' onca("Stevenson", maxCodeLen = 8)
#'
#' @export
onca <- function(word, maxCodeLen = 4) {

    ## Yes, it really is this simple.
    
    word <- soundex(nysiis(word, maxCodeLen = 10), maxCodeLen)
    return(word)
}
