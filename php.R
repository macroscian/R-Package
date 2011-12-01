##' Translate scalars and vectors into php assignments
##' @param The scalar or vector to be transformed into a PHP assigment

##' @return A string representing the LHS of an assigment
vec2php <- function(x) {
    key <- sprintf("'%s'", names(x))
    if (is.character(x)) {
        x <- sprintf("'%s'", x)
    }
    if (length(x)==1) {
        res <- x
    } else {
        if (length(key)==0) {
            arrayContent <- paste(x, collapse=",")
        } else {
            arrayContent <- paste(key, x, sep=" => ", collapse=",")
        }
        res <- sprintf("array(%s);", arrayContent)
    }
    res
}

##' Translate R scalars and vectors into PHP assigments of variables and arrays.
##' @param ... Arguments to be converted.
##' @param fname An filename that the PHP code will be written to.
##' @return Purely a side-effect: the file is written to
list2php <- function(...,file="") {
    lst=list(...)
    cat("<?php\n", file=file)
    vars <- lapply(lst, vec2php)
    nams <- sapply(match.call()[-1], deparse)
    nams <- nams[names(nams)!="file"]
    if (!is.null(names(lst))) {
        ind <- names(lst)!=""
        nams[ind] <- names(lst)[ind]
    }
    cat(paste("$",nams, "=", vars, ";", sep="", collapse="\n"),
        file=file, append=TRUE)
    cat("\n?>\n", file=file, append=TRUE)
}
