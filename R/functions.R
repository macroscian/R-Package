##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param fname 
##' @param txt 
##' @return 
##' @author 
pngInfo <- function(fname, txt) {
    #Needs to have a 'Cairo(type='raster') prefix, and a dev.off() after image
    require(png)
    writePNG(dev.capture(native=TRUE), fname,text=txt)
}



##' dataframe to sqlite
##'
##' .. content for \details{} ..
##' @title 
##' @param x 
##' @param pval 
##' @param thresh 
##' @return 
##' @author 
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





##' Convert a log-fold change into a signed fold change
##'
##' .. content for \details{} ..
##' @title 
##' @param x 
##' @param base 
##' @return 
##' @author 
log2sign <- function(x, base=2) {
  if (!is.logical(x))  x <- sign(x)*(base^abs(x))
  return(x)
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param x 
##' @param base 
##' @return 
##' @author 
sign2log <- function(x, base=2) {
    y <- sign(x) * log(abs(x), base)
  return(y)
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param x 
##' @return 
##' @author 
log2signedFold <- function(x) {
  if (is.null(attr(x,"log")) || attr(x,"log")) {
    if (is.list(x))
      x <- lapply(x, log2sign)
    else
      x <- log2sign(x)
    attr(x,"log") <- FALSE
  }
  return(x)
}



##' List all unordered pairs from a vector (copied from GOstats)
##'
##' .. content for \details{} ..
##' 
##' @title 
##' @param iVec 
##' @return 
##' @author 
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

##' Create a contrast representing all pairs
##'
##' .. content for \details{} ..
##' @title 
##' @param mdl 
##' @param between 
##' @return 
##' @author 
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

##' Add genes and their folds, sigs
##'
##' .. content for \details{} ..
##' @title 
##' @param fit 
##' @param want 
##' @param logfn 
##' @return 
##' @author 
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

