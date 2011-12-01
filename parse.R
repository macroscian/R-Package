
##' Break a wide dataframe into a long one by chunks

##' @param df The dataframe to be broken apart
##' @param startRE A regular expression determining the beginning of each block
##' @param endRE A regular expression determining the end of each block
##' @param colRE A RE determining which columns should be kept within a block

##' @return A dataframe with the relevant columns stacked.
columnBreaker <- function(df, startRE, endRE, colRE=NULL) {
    starts <- grep(startRE, df[1,])
    ends <- grep(endRE, df[1,])
    dfInd <- (starts[1]):(ends[1])
    if (!is.null(colRE))
        dfInd <- dfInd[grep(colRE, df[1, dfInd])]
    dfOut <- df[dfInd]
    for (i in seq(along=starts)[-1]) {
        dfInd <- (starts[i]):(ends[i])
        if (!is.null(colRE))
            dfInd <- dfInd[grep(colRE, df[1, dfInd])]
        dfThis <- df[,dfInd]
        names(dfThis) <- names(dfOut)
        dfOut <- rbind(dfOut,dfThis)
    }
    dfOut
}

##' Delete rows from a dataframe based on regular expressions.
##' @param df The dataframe from which rows are to be removed.
##' @param killRE If a list, the names are the columns that are to be matched, and the values are the respective regular expressions; otherwise, the whole dataframe will be matched against the pattern.

##' @return A dataframe with the rows removed
rowKiller <- function(df, killRE) {
    if (is.list(killRE)) {
        killRows <- unique(unlist(lapply(names(killRE), function(x) grep(killRE[[x]], df[[x]]))))
    } else {
        killRows <- unique(unlist(lapply(df, function(x) grep(killRE,x))))
    }
    df[-killRows, ]
}

##' Generate a new column that is based on a match of another column
##' @param col The column to be matched against
##' @param colRE The regular expression that will detect rows to be propogated
##' @param replacement A replacement expression that be used to generate the new row values
##' @return The column containing propogated values
colAdder <- function(col, colRE, replacement="\\1") {
    inds <- grep(colRE, col)
    vals <- sub(colRE, replacement, col)[inds]
    inds <- c(inds, length(col)+1)
    newCol <- rep(NA, length(col))
    class(newCol) <- class(vals)
    for (i in seq(along=vals)) {
        newCol[(inds[i]):(inds[i+1]-1)] <- vals[i]
    }
    newCol
}

