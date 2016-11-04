#' Threshold-based summary counts
#'
#' Counts the number of up and down values, also those above and below given threshold
#'
#' @param x Vector of numbers
#' @param lambda threshold
#' @return vector of counts
updownthresh <- function(x, lambda=0) {
    if (lambda<1) {
        res <- c(below=sum(x<lambda))
        names(res) <- sprintf("#<%g",lambda)
    } else {
        res <- c(up=sum(x>0),
                    down=sum(x<0),
                    above=sum(x>lambda),
                    below=sum(x< -lambda))
        names(res)[3] <- sprintf("# > %g", lambda)
        names(res)[4] <- sprintf("# < %g", -lambda)
    }
    res
}


#' Generate dataframe of results for microarray
#'
#' Generate dataframe of results for microarray
#'
##' @param con con
##' 
##' @param dFrame 
##' @param tableName 
##' @param field 
##' @param join 
##' @param vals 
##' @param aggfun 
##' @param html 
getFields <- function(con, dFrame, tableName, field=list(), join=list(), vals=list(), aggfun=list(), html=list()) {
    if (length(field)==0) {
        field <- as.list(names(dFrame))
        names(field) <- names(dFrame)
    }
    names(field) <- make.db.names(con,names(field))
    df <- data.frame(tabName=rep(tableName, ncol(dFrame)),
                     field=make.db.names(con,names(dFrame)),
                     text=names(dFrame),
                     num=sapply(dFrame, is.numeric),
                     nrow=sapply(dFrame, function(x) sum(!is.na(x))),
                     stringsAsFactors=FALSE,
                     joyn=NA,
                     vals=NA,
                     aggfun=NA,
                     html=NA
                     )
    isna <- sapply(field, is.na)
    if (any(isna)) {
        df <- df[-match(names(field)[isna], df$field),]
        field <- field[!isna]
    }
    df$text[match(names(field), df$field)] <- unlist(field)
    df$joyn[match(names(join), df$field)] <- unlist(join)
    df$vals[match(names(vals), df$field)] <- unlist(vals)
    df$aggfun[match(names(aggfun), df$field)] <- unlist(aggfun)
    df$html[match(names(html), df$field)] <- unlist(html)
    df
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param con 
##' @param dFrame 
##' @param tableName 
##' @param field 
##' @param join 
##' @param vals 
##' @param aggfun 
##' @param html 
##' @return 
##' @author 
getFields2 <- function(con, dFrame, tableName, field=list(), join=list(), vals=list(), aggfun=list(), html=list()) {
    df <- data.frame(tabName=rep(tableName, ncol(dFrame)),
                     field=make.db.names(con,names(dFrame)),
                     text=names(dFrame),
                     num=sapply(dFrame, is.numeric),
                     nrow=sapply(dFrame, function(x) sum(!is.na(x))),
                     stringsAsFactors=FALSE,
                     joyn=NA,
                     vals=NA,
                     aggfun=NA,
                     html=NA
                     )
    isna <- sapply(field, is.na)
    if (any(isna)) {
        df <- df[-match(names(field)[isna], df$field),]
        field <- field[!isna]
    }
    if (length(names(field)))
        df$text[match(make.db.names(con,names(field)), df$field)] <- unlist(field)
    if (length(names(join)))
        df$joyn[match(make.db.names(con,names(join)), df$field)] <- unlist(join)
    if (length(names(vals)))
        df$vals[match(make.db.names(con,names(vals)), df$field)] <- unlist(vals)
    if (length(names(aggfun)))
        df$aggfun[match(make.db.names(con,names(aggfun)), df$field)] <- unlist(aggfun)
    if (length(names(html)))
        df$html[match(make.db.names(con,names(html)), df$field)] <- unlist(html)
    df
}


##' Converts decideTests results into lists of d.e. genes
##'
##' .. content for \details{} ..
##' @title 
##' @param tests 
##' @return 
##' @author 
test2list <- function(tests,       # From decideTests
                      updown=FALSE # Separate lists for up and down?
                      ) {
    if (updown) {
        up = apply(tests,2, function(x) names(x)[x==1])
        if (!is.list(up)) {up=lapply(as.data.frame(up), as.character)}
        names(up) = paste(names(up),"+",sep="")
        down = apply(tests,2, function(x) names(x)[x==-1])
        if (!is.list(down)) {down=lapply(as.data.frame(down), as.character)}
        names(down) = paste(names(down),"-", sep="")
        testGenes=c(up,down)
        testGenes = testGenes[order(rep(1:ncol(tests),2))]
    } else {
        testGenes=apply(tests, 2, function(x) names(x)[x!=0])
        if (!is.list(testGenes)) {testGenes=lapply(as.data.frame(testGenes), as.character)}
        names(testGenes)=colnames(tests)
	}
    testGenes = testGenes[sapply(testGenes, function(x) length(x)!=0)]
}
