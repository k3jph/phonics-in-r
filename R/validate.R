.validate_max_code_len <- function(maxCodeLen) {
    valid <- is.numeric(maxCodeLen) &&
        length(maxCodeLen) == 1L &&
        !is.na(maxCodeLen) &&
        is.finite(maxCodeLen) &&
        maxCodeLen >= 0 &&
        maxCodeLen == floor(maxCodeLen) &&
        maxCodeLen <= .Machine$integer.max

    if (!valid) {
        stop(
            "`maxCodeLen` must be a single non-negative whole number",
            call. = FALSE
        )
    }

    as.integer(maxCodeLen)
}
