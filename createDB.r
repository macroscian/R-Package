library(RSQLite)
dbname <- file.path(resDir, "results.db")
unlink(dbname)
m <- dbDriver("SQLite")
con <- dbConnect(m, dbname)


dbWriteTable(con, "fielddef", fieldDef, row.names=FALSE, over=TRUE)
dbWriteTable(con, "tabledef", tableDef, row.names=FALSE, over=TRUE)
lapply(names(resFrame), function(tbl)
       dbWriteTable(con, tbl, resFrame[[tbl]], row.names=FALSE, over=TRUE)
       )


sql <- as.list(sprintf("CREATE INDEX %1$s_idx ON %1$s(id);", fieldDef$tabName[fieldDef$field=="id"]))
ind <- !is.na(fieldDef$joyn)
sql <- c(sql, as.list(sprintf("CREATE INDEX %s_%s_jidx ON %s(%s);", fieldDef$tabName[ind], fieldDef$field[ind], fieldDef$tabName[ind], fieldDef$field[ind] )))


lapply(sql, function(s) {print(s)
                         res <- dbSendQuery(con, s)
                        dbClearResult(res)
                     })

dbDisconnect(con)

expandJoin <- function(tbl, dframe=NULL, matchto=NULL) {
    ind <- fieldDef$tabName==tbl
    newtab <- resFrame[[tbl]]
    if (!is.null(matchto)) {
        reordcol <- match(matchto, newtab$id)
        newtab <- newtab[reordcol,]
        newtab$id <- NULL
        rm(reordcol)
    }
    reind <- match(names(newtab), fieldDef$field[ind])
    newtab <- newtab[!is.na(reind)]
    onames <- names(newtab)
    names(newtab) <- fieldDef$text[ind][na.omit(reind)]
    if (is.null(dframe)) {
        dframe <- newtab
    } else {
        dframe <- cbind(dframe, newtab)
    }
    for (itab in which(!is.na(fieldDef$joyn[ind]))) {
        tab <- fieldDef$joyn[ind][itab]
        fie <- as.character(fieldDef$field[ind][itab])
        dframe <- expandJoin(tab, dframe, newtab[[match(fie, onames)]])
    }
    return(dframe)
}

for (tbl in as.character(tableDef$id)) {
    toWrite <- expandJoin(tbl)
    baseFile <- sprintf("%s.txt", file.path(resDir, tbl))
    write.table(toWrite, file=baseFile,
                sep="\t", row.names=FALSE, quote=FALSE, na="")
    system(sprintf("zip -9 -jm %1$s.zip %1$s", baseFile))
}
