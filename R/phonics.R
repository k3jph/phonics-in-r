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
#' Apply one or more phonetic encoders to a character vector and return their
#' codes together in a data frame.
#'
#' @param word A character vector to encode.
#' @param method A character vector naming the methods to apply.
#' @param clean If \code{TRUE}, warn and return \code{NA} for inputs containing
#'   characters outside an encoder's supported alphabet. If \code{FALSE},
#'   unsupported characters are discarded before encoding.
#'
#' @details
#'
#' Available methods are \code{"caverphone"},
#' \code{"caverphone.modified"}, \code{"cologne"}, \code{"lein"},
#' \code{"metaphone"}, \code{"nysiis"}, \code{"nysiis.modified"},
#' \code{"onca"}, \code{"onca.modified"}, \code{"onca.refined"},
#' \code{"onca.modified.refined"}, \code{"phonex"},
#' \code{"rogerroot"}, \code{"soundex"}, \code{"soundex.refined"}, and
#' \code{"statcan"}. Unknown method names produce a warning and no result
#' column.
#'
#' @return
#'
#' A data frame whose first column is \code{word}, followed by one character
#' column for each recognized method.
#'
#' @references
#'
#' Howard, J. P., II (2020). "Phonetic Spelling Algorithm Implementations
#' for R." \emph{Journal of Statistical Software}, 95(8), 1--21.
#' \doi{10.18637/jss.v095.i08}.
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
