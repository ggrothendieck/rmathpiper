print.mpr <- function(x, ...) {
    msg <- attr(x, "msg")
	if (!is.null(msg) && nchar(msg) > 0) warning(msg)
    cat(x, ..., "\n", sep = "")
}
