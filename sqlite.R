getFields <- function(con, dFrame, tableName) {
    data.frame(tabName=rep(tableName, ncol(dFrame)),
               field=make.db.names(con,names(dFrame)),
               text=names(dFrame),
               num=sapply(dFrame, is.numeric),
               nrow=sapply(dFrame, function(x) sum(!is.na(x))),
               stringsAsFactors=FALSE
               )
}

rankDF <- function(dFrame) {
    isNum <- sapply(dFrame, is.numeric)
    newFrame <- lapply(dFrame[isNum], rank)
    names(newFrame) <- sprintf("r_%s",names(dFrame)[isNum])
    return(data.frame(newFrame, check.names=FALSE))
}

want <- data.frame(id=colnames(fit),
                   text="",
                   
eb2db <- function(eb, want) {
    resFrame <- data.frame(
    
