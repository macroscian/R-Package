##' Parse a character vector into an xml string
##' @param The element names
##' @param The text values
##' @param close Whether to close the element with a separate group
##' @param args A dataframe of arguments that are to be included in the elements

##' @return ...
catXML <- function(tag, txt, close=TRUE, args=NULL) {
    if (is.null(args)) {
        if (close) {
            xml <- sprintf("<%1$s>%2$s</%1$s>", tag, txt)
        } else {
            xml <- sprintf("<%1$s />%2$s", tag, txt)
        }
    } else {
        tmpfn <- function(...) sprintf(paste(names(args), "='%s'", collapse=" ", sep=""),...)
        attrs <- apply(args, 1, function(x) do.call(tmpfn, as.list(x)))        
        if (close) {
            xml <- sprintf("<%1$s %2$s>%3$s</%1$s>", tag, attrs, txt)
        } else {
           xml <- sprintf("<%1$s %2$s />%3$s", tag, attrs, txt)
        }
    }
    cat(xml, sep="\n")
}
