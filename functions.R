counter <- local({
    ii <- list()
    function(reset=FALSE, prefix="x") {
        if (reset) ii[[prefix]] <<- 1
        if (is.null(ii[[prefix]])) ii[[prefix]] <<- 1
        ii[[prefix]] <<- ii[[prefix]]+1
        sprintf("%s%X",prefix, ii[[prefix]]-1)
    }
})


################################################################
#### dataframe to sqlite
################################################################
sevennum <- function(x, pval=FALSE, thresh=ifelse(pval, 0.001, 2)) {
    qq <- summary(x)[-c(2,5)]
    if (length(x)<1000) {
        ind <- c(10, length(na.omit(x))-9)
    } else {
        ind <- c(100,length(na.omit(x))-100)
    }
    if (pval) {
        qq <- qq[c("Min.","Max.")]
        ind <- ind[1]
    }
    sorted <- sort(na.omit(x), partial=ind)[ind]
    names(sorted) <- paste(ind[1], "th ", c("lowest","highest")[1:length(ind)], sep="")
    qq <- c(qq, sorted, unlist(updownthresh(x, thresh)))
    resStr <- paste("<tr><th>", names(qq), "</th><td>", sprintf("%.4g",qq), "</td></tr>",
                    sep="", collapse="")
    resStr <- paste("<table>", resStr, "</table>")
    attr(resStr, "vals") <- qq
    resStr
}

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

################################################################
#### R to PHP
################################################################
vec2php <- function(x) {
    if (is.numeric(x) & length(x)==1) {
        res <- x
    } else {
            res <- paste("'", paste(x, collapse=","), "'", sep="")
    }
    res
}
list2php <- function(...,fname="") {
    lst=list(...)
    cat("<?php\n", file=fname)
    vars <- lapply(lst, vec2php)
    cat(paste("$",names(lst), "=", vars, ";", sep="", collapse="\n"),
        file=fname, append=TRUE)
    cat("\n?>\n", file=fname, append=TRUE)
}


df2js <- function(dframe, name, caption="", file="") {
    gridList <- df2jqgrid(dframe)
    cat(sprintf("var %s = %s;\n", name, gridList$data), file=file, append=TRUE)
    cat(sprintf("jQuery(\"#table%s\").jqGrid({\n", name), file=file, append=TRUE)
    cat(sprintf("data: %s,\n", name), file=file, append=TRUE)
    cat("datatype: \"local\",\n", file=file, append=TRUE)
    cat(sprintf("colNames:%s,\n", gridList$colNames), file=file, append=TRUE)
    cat(sprintf("colModel:%s,\n", gridList$colModel), file=file, append=TRUE)
    cat(sprintf("pager: false,\n", name), file=file, append=TRUE)
    cat(sprintf("height: \"auto\",\n", name), file=file, append=TRUE)
    cat(sprintf("caption: \"%s\"\n", caption), file=file, append=TRUE)
    cat("});\n", file=file, append=TRUE)
}



df2jqgrid <- function(dframe, headings=names(dframe)) {
    r2formatter <- list("integer"="integer",
                        "numeric"="number",
                        "character"="text")
    out <- list(colNames=sprintf("[%s]", paste('"', headings,'"', sep="", collapse=",")))
    names(dframe) <- sprintf("r%i", 1:ncol(dframe))
#    dframe <- cbind(id=1:nrow(dframe), dframe)
    out$data <- df2json(dframe)
    colmod <- data.frame(name=names(dframe),
                         index=names(dframe),
                         sorttype=sapply(dframe, function(x) r2formatter[[class(x)]]),
                         width=max(100, 10*nchar(names(res)))
                         )
    out$colModel <- df2json(colmod)
    out
}

df2json <- function(dframe) {
    rows <- apply(dframe, 1, function(x) paste( sprintf('%s:"%s"',names(dframe), x), collapse=","))
    sprintf("[%s]", paste("{", rows, "}",sep="", collapse=","))
}




################################################################
#### Parallel coordinate plot
################################################################
parcoord <- function (y,            # Values
                      x=1:ncol(y),  # X coordinates
                      col = 1,      # Colour of lines
                      ax.col=rep("grey70",ncol(y)), # colour of vertical lines
                      lty = 1,
                      add=FALSE,
                      ...) {
    if (!add) {
    	matplot(x, t(y), type = "l", col = col, lty = lty,
                xlab = "", ylab = "", axes = FALSE,  ...)
    	axis(1, at = x, labels = colnames(y),...)
    	if (is.factor(ax.col))
            ax.col=rainbow(length(levels(ax.col)))[as.integer(ax.col)]
    	for (i in x) lines(c(i, i), range(y,na.rm=TRUE), col = ax.col[i])
    } else {
    	matlines(x, t(y), col = col, lty = lty,
                 xlab = "", ylab = "", axes = FALSE)
    }
    invisible()
}

################################################################
#### Converts decideTests results into lists of d.e. genes
################################################################
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


################################################################
#### Convert a log-fold change into a signed fold change
################################################################
log2sign <- function(x, base=2) {
  if (!is.logical(x))  x <- sign(x)*(base^abs(x))
  return(x)
}
sign2log <- function(x, base=2) {
    y <- sign(x) * log(abs(x), base)
  return(y)
}

################################################################
#### List all unordered pairs from a vector (copied from GOstats)
################################################################
enumPairs <- function (iVec) {
    leni <- length(iVec)
    if (leni < 2)
        return(vector(mode(iVec), length = 0))
    eP <- vector("list", length = choose(leni, 2)/2)
    k <- 1
    for (i in 1:(leni - 1)) {
        for (j in (i + 1):leni) {
            eP[[k]] <- c(iVec[i], iVec[j])
            k <- k + 1
        }
    }
    return(eP)
}

################################################################
#### Create a contrast representing all pairs
################################################################
allPair <- function(mdl, between=1:ncol(mdl)) {
    allP <- enumPairs(between)
    cntr <- matrix(0, nrow=ncol(mdl), ncol=length(allP))
    rownames(cntr) <- colnames(mdl)
    colnames(cntr) <- 1:ncol(cntr)
    for (thisPair in seq(along=allP)) {
        cntr[allP[[thisPair]][1],thisPair] <- -1
        cntr[allP[[thisPair]][2],thisPair] <- 1
        colnames(cntr)[thisPair] <- paste(colnames(mdl)[allP[[thisPair]][2]],
                                          "V",
                                          colnames(mdl)[allP[[thisPair]][1]]
                                          )
    }
    return(cntr)
}

################################################################
#### Add genes and their folds, sigs
################################################################
##' @param fit
##' @param want
##' @param logfn

##' @return ...
addGenes <- function(fit, want, logfn=log2sign) {
    for (i in names(want)) {
        fiti <- want[[i]]
        if (!is.null(tested[[fiti]])) {
            genes$test[[i]] <<- tested[[fiti]]
            folds$test[[i]] <<- logfn(fit$coef[genes$test[[i]], fiti])
            if (!is.null(fit$p.value))
                sigs$test[[i]]  <<- fit$p.value[genes$test[[i]],fiti]
        }
        if (!is.null(fit$p.value)) {
            genes$top[[i]]  <<- topTable(fit,
                                        coef=fiti,
                                        n=thresh$topN,
                                        adjust=thresh$adjust)$ID
            sigs$top[[i]]   <<- ebFit$p.value[genes$top[[i]],fiti]
            if (thresh$all) sigs$all[[i]]   <<- ebFit$p.value[,fiti]
        } else {
            ind <- order(abs(fit$coef[,fiti]),
                         decreasing=TRUE)[1:thresh$topN]
            genes$top[[i]] <<- fit$genes$ID[ind]
        }
        folds$top[[i]]  <<- logfn(fit$coef[genes$top[[i]], fiti])
        if (thresh$all) folds$all[[i]]  <<- logfn(fit$coef)[,fiti]
    }
}

################################################################
#### Find GO terms that are over-represented in genelists
################################################################
# requires gsea, ngsea and gseaTotal in parent environment
overGO <- function(sheet, glist) {
    geneIds(params) <- lookUp(genes[[sheet]][[glist]], chip, "ENTREZID")
    if (length(unique(geneIds(params)))>1) {
        res <- summary(hyperGTest(params))
        gsea[[sheet]][[glist]] <- res$GOBPID
        ngsea[[sheet]][[glist]] <- res$Count
        gseaTotal[res$GOBPID] <- res$Size
    }
    if (glist %in% names(folds[[sheet]])) {
        entrez <- geneIds(params)
        ## Positives
        geneIds(params) <- entrez[folds[[sheet]][[glist]]>0]
        if (length(geneIds(params))>0) {
            glistsub <- paste(glist, "+", sep="")
            res <- summary(hyperGTest(params))
            gsea[[sheet]][[glistsub]] <- res$GOBPID
            ngsea[[sheet]][[glistsub]] <- res$Count
            gseaTotal[res$GOBPID] <- res$Size
        }
        ## Negatives
        geneIds(params) <- entrez[folds[[sheet]][[glist]]<0]
        if (length(geneIds(params))>0) {
            glistsub <- paste(glist, "-", sep="")
            res <- summary(hyperGTest(params))
            gsea[[sheet]][[glistsub]] <- res$GOBPID
            ngsea[[sheet]][[glistsub]] <- res$Count
            gseaTotal[res$GOBPID] <- res$Size
        }
    }
}


################################################################
#### Get annotation for genes
################################################################
getAnno <- function(genes, chip) {
data.frame(
           symbol      = getSYMBOL(genes, chip),
           accnum      = unlist(lookUp(genes, chip, "ACCNUM")),
           entrez      = getLL(genes, chip),
           description = unlist(lookUp(genes, chip, "GENENAME")),
           chrom       = sapply(lookUp(genes, chip, "CHR"),"[[",1),
           chromLoc    = sapply(lookUp(genes, chip, "CHRLOC"), "[[",1),
           cyto        = sapply(lookUp(genes, chip, "MAP"), "[[",1)
           )
}

################################################################
#### Link data.frames representing sqlite tables
################################################################
expandJoin <- function(tbl, dframe=NULL, matchto=NULL) {
    ind <- fieldDef$tabName==tbl
    newtab <- tosql[[tbl]]$dframe
    if (!is.null(matchto)) {
        reordcol <- match(matchto, newtab$id)
        newtab <- newtab[reordcol,]
        newtab$id <- NULL
        rm(reordcol)
    }
    reind <- match(names(newtab), fieldDef$field[ind])
    newtab <- newtab[!is.na(reind)]
    names(newtab) <- fieldDef$text[ind][na.omit(reind)]
    if (is.null(dframe)) {
        dframe <- newtab
    } else {
        dframe <- cbind(dframe, newtab)
    }
    for (itab in which(!is.na(fieldDef$joyn[ind]))) {
        tab <- fieldDef$joyn[ind][itab]
        fie <- fieldDef$field[ind][itab]
        dframe <- expandJoin(tab, dframe, tosql[[tbl]]$dframe[[fie]])
    }
    return(dframe)
}

topLabel <- function(x, y, labels, n=10, exclude=NA,log="",...) {
    require(maptools)
    ind <-  !is.na(x+y) & !is.na(labels);
    if (length(grep("x",log)))
        fx <- log10
    else
        fx <- I
    if (length(grep("y",log)))
        fy <- log10
    else
        fy <- I
    distMat <- as.matrix(dist(cbind(fx(x[ind]),fy(y[ind]))))
    diag(distMat) <- Inf
    if (is.na(exclude)) {
        aveDist <- apply(distMat,1,min)
    } else {
        aveDist <- apply(distMat, 1, function(x) sort(x,partial=exclude+1)[exclude+1])
    }
    ord <- order(aveDist, decreasing=TRUE)[1:n]
    pointLabel(x[ind][ord], y[ind][ord], labels[ord])
}


toFlot <- function(json) {
    return(apply(rbind(json$x, json$y), 2, as.list))
}
