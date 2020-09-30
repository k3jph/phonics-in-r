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

#' @title Phonetic Spelling Algorithms
#'
#' @description 
#'
#' The \code{phonics} package for \code{R} is designed to provide a
#' variety of phonetic indexing algorithms in common and not-so-common
#' use today.  The algorithms generally reduce a string to a symbolic
#' representation approximating the sound made by pronouncing the
#' string.  They can be used to match names, strings, and as a proxy for
#' assorted string distance algorithms.  The algorithm reduces a string
#' to a symbolic representation approximating the sound.  It can be used
#' to match names, strings, and as a proxy for assorted string distance
#' algorithms.
#'
#' @param word string or vector of strings to encode
#' @param method vector of method names to use
#' @param clean if \code{TRUE}, return \code{NA} for unknown alphabetical characters
#'
#' @details
#'
#' The \code{phonics} package for \code{R} is designed to provide a
#' variety of phonetic indexing algorithms in common and not-so-common
#' use today.  The algorithms generally reduce a string to a symbolic
#' representation approximating the sound made by pronouncing the
#' string.  They can be used to match names, strings, and as a proxy for
#' assorted string distance algorithms.  The algorithm reduces a string
#' to a symbolic representation approximating the sound.  It can be used
#' to match names, strings, and as a proxy for assorted string distance
#' algorithms.
#' 
#' The variable \code{word} is a character string or a vector of
#' character strings to be encoded.
#'
#' Different phonetic algorithm are only defined for inputs over the
#' limited alphabets, Non-alphabetical characters are removed from the
#' string in a locale-dependent fashion. This strips spaces, hyphens,
#' and numbers. For inputs outside of its known range, the output is
#' undefined and \code{NA} is returned and a warning this thrown. If
#' \code{clean} is \code{FALSE}, \code{phonics} attempts to process the
#' strings. The default is \code{TRUE}.
#'
#' The \code{method} parameter should be a character vector containing one or
#' more methods that should be used. The available list of methods is
#' "caverphone", "caverphone.modified", "cologne", "lein", "metaphone",
#' "nysiis", "nysiis.modified", "onca", "onca.modified", "onca.refined",
#' "onca.modified.refined", "phonex", "rogerroot", "soundex",
#' "soundex.refined", and "statcan".
#'
#' @return
#'
#' Returns a data frame containing the phonetic spellings of the input
#' for each method applied.
#'
#' @references
#'
#' James P. Howard, II, "Phonetic Spelling Algorithm Implementations
#' for R," \emph{Journal of Statistical Software}, vol. 25, no. 8,
#' (2020), p. 1--21, <10.18637/jss.v095.i08>.
#'
#' @family phonics
#'
#' @examples
#' phonics(c("Peter", "Peady"), c("soundex", "soundex.refined"))
#'
#' @importFrom data.table :=
#' @importFrom data.table data.table
#'
#' @export
phonics <- function(word, method, clean = TRUE) {
    ret <- data.table(word = word)

    for(i in method) {
        if("caverphone" %in% i)
            ret[, "caverphone" := caverphone(word, clean = clean)]
        else if("caverphone.modified" %in% i)
            ret[, "caverphone.modified" := caverphone(word, modified = TRUE, clean = clean)]
        else if("cologne" %in% i)
            ret[, "cologne" := cologne(word, clean = clean)]
        else if("lein" %in% i)
            ret[, "lein" := lein(word, clean = clean)]
        else if("metaphone" %in% i)
            ret[, "metaphone" := metaphone(word, clean = clean)]
        else if("nysiis" %in% i)
            ret[, "nysiis" := nysiis(word, clean = clean)]
        else if("nysiis.modified" %in% i)
            ret[, "nysiis.modified" := nysiis(word, modified = TRUE, clean = clean)]
        else if("onca" %in% i)
            ret[, "onca" := onca(word, clean = clean)]
        else if("onca.modified" %in% i)
            ret[, "onca.modified" := onca(word, modified = TRUE, clean = clean)]
        else if("onca.refined" %in% i)
            ret[, "onca.refined" := onca(word, refined = TRUE, clean = clean)]
        else if("onca.modified.refined" %in% i)
            ret[, "onca.modified.refined" := onca(word, modified = TRUE, refined = TRUE, clean = clean)]
        else if("phonex" %in% i)
            ret[, "phonex" := phonex(word, clean = clean)]
        else if("rogerroot" %in% i)
            ret[, "rogerroot" := rogerroot(word, clean = clean)]
        else if("soundex" %in% i)
            ret[, "soundex" := soundex(word, clean = clean)]
        else if("soundex.refined" %in% i)
            ret[, "soundex.refined" := refinedSoundex(word, clean = clean)]
        else if("statcan" %in% i)
            ret[, "statcan" := statcan(word, clean = clean)]
        else
            warning(paste("unknown phonetic spelling algorithm:", i))
    }

    ## Clean it up and make it pretty
    ret <- as.data.frame(ret)
    return(ret)
}
