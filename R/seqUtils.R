#' Calculate summary set sizes on a DESeq2 result object
#'
#' Derives sizes of up and down regulated genes, along with the number of filtered and outlier genes
#' 
#' @param object The DESeq2 result object on which we are to calculate summaries
#' @param alpha the false discovery rate used to choose significant genes
#' @return list containing the gene counts
updown.DESeqResults <- function(object, alpha=0.05) {
  notallzero <- sum(object$baseMean > 0)
  allZero <- sum(object$baseMean == 0)
    up <- sum(object$padj < alpha & object$log2FoldChange > 0, 
              na.rm = TRUE)
    down <- sum(object$padj < alpha & object$log2FoldChange < 
                    0, na.rm = TRUE)
    filt <- sum(!is.na(object$pvalue) & is.na(object$padj))
    outlier <- sum(object$baseMean > 0 & is.na(object$pvalue))
  #    list(nonzero=notallzero, allzero=allZero,up=up, down=down, outliers=outlier, lowCount=filt)
      list(`Non zero`=notallzero, `All zero`=allZero, Up=up, Down=down, Outliers=outlier, `Low count`=filt)
}

#' Read fastqc files
#'
#' Read fastqc files
#'
#' @param file filename of the fastqc file
#' @return list of fastqc columns
read.fastqc.txt <- function(file){
    stopifnot(file.exists(file),
              grepl(pattern="##FastQC", x=readLines(file, n=1)))
    temp <- readLines(file)
    temp <- gsub("#", "", temp)
    temp <- temp[!grepl(">>END_MODULE", temp)]
    temp <- split(temp, cumsum(grepl("^>>", temp)))[-1]
    names(temp) <- sapply(temp, function(x) {
                   gsub("^>>", "", gsub("\t.*", "", gsub(" ", "_", x[1])))
               })
    temp <- lapply(temp, function(x) {
        if(length(x)==1)
            return(data.frame())
        x <- strsplit(x[-1], split="\t")
        tab <- as.data.frame(do.call(rbind, x[-1]), stringsAsFactors=FALSE)
        for(i in 1:ncol(tab))
            if(!any(is.na(suppressWarnings(as.numeric(tab[,i])))))
                tab[,i] <- as.numeric(tab[,i])
        colnames(tab) <- x[[1]]
        tab
    })
    return(temp)
}

##' One-way tests nested within model factors.
##'
##' Generates a list of DESeq2 objects, ach restricted to a different data slice
##' @title 
##' @param dds The original DESeq data objects
##' @param nests The factors that are to slice the DESeq object, can be given as a list of factors, or a formula on terms of colData(dds) 
##' @param design Which factor are we to use for the new design
##' @return A list of DESeq objects
##' @author 
nestedDESeq <- function(dds, nests, design=BiocGenerics:::design(dds), recalc=FALSE, ...) {
    if (class(nests)=="formula")
        nests <- eval(attr(terms(nests), "variables"), colData(dds))
    tapply(1:nrow(colData(dds)), nests, 
           function(x) {
        newDDS <- dds[,x]
         design(newDDS) <- design
        colData(newDDS) <- droplevels(colData(newDDS))
        if (recalc) {
            return(DESeq(newDDS,...))
        } else {
            return(newDDS)
        }
    }
    )
}


##' Converts beween expanded and standard contrast forms
##'
##' Currently only works for single factor designs, 
##' @title 
##' @param contr the contrasts, either a list or numeric one at the moment
##' @param dds the deseq object, so we can find out what levels there are
##' @param to are we converting to standard, or expanded (latter not thoroughly tested)
##' @return 
##' @author 
convertContrasts <- function(contr, dds, to="standard") {
    if (is.numeric(contr) && to=="standard") {
        newContr <- contr
        newContr[-1] <- newContr[-1]+newContr[1]/length(newContr[-1]) # spread out intercept across other terms
        newContr <- newContr[-1]
        newContr[1] <- sum(newContr)
    }
    if (is.numeric(contr) && to=="expanded") {
        newContr <- contr
        newContr[1] <- newContr[1]-sum(newContr[-1])
        newContr <- c(0, newContr)
    }
    if (is.list(contr) && to=="standard") {
        contrFactor <- attr(terms(design(dds)), "term.labels")
        if (length(contrFactor)!=1) stop("Tested for only one design factor")
        newContr <- rep(0, nlevels(dds[[contrFactor]]))
        newContr[sprintf("%s%s", contrFactor, levels(dat$sample))==contr[[1]]] <- 1
        newContr[sprintf("%s%s", contrFactor, levels(dat$sample))==contr[[2]]] <- -1
        newContr[1] <- 0
    }
    return(newContr)
}

##' Takes all pairwise comparisons within a single design factor
##'
##' Still need to run DESeq on the returned components.  The data and coldata are simply subsetted
##' @title 
##' @param dds the DESeq object to disect
##' @return A list of un-recalculated DESeq object.
##' @author 
all2 <- function(dds, base=NULL) {
  contrFactor <- attr(terms(design(dds)), "term.labels")
  if (length(contrFactor)!=1) stop("Tested for only one design factor")
  f1 <- dds[[contrFactor[1]]]
  ddsList <- list()
  if (is.null(base)) {
      combs <- combn(levels(f1), 2)
    } else {
    combs <- rbind(levels(f1)[levels(f1)!=base], base)
    }
  for (i in 1:ncol(combs)) {
    tmp <- dds[,f1 %in% combs[,i]]
    colData(tmp) <- droplevels(colData(tmp))
    ddsList[[sprintf("%s vs %s", combs[1,i], combs[2,i])]] <- tmp
  }
  return(ddsList)
}
                    

##' Generate all possible 2x2 designs from a two-factor DESeq design
##'
##' Still need to run DESeq on the returned components.  The data and coldata are simply subsetted
##' @title 
##' @param dds The original DESeq object
##' @return A list of un-recalculated DESeq objects.
##' @author 
all2x2 <- function(dds) {
    contrFactor <- row.names(attr(terms(design(dds)), "factors"))
    if (length(contrFactor)!=2) stop("Tested for only two design factors")
    f1 <- dds[[contrFactor[1]]]
    f2 <- dds[[contrFactor[2]]]
    ddsList <- list()
    combs1 <- combn(levels(f1), 2)
    combs2 <- combn(levels(f2), 2)
    for (i1 in 1:ncol(combs1)) {
        for (i2 in 1:ncol(combs2)) {
            tmp <- dds[, (f1 %in% combs1[,i1]) & (f2 %in% combs2[,i2])]
            colData(tmp) <- droplevels(colData(tmp))
            ddsList[[sprintf("%s_%s vs %s_%s", combs1[1,i1], , combs1[2,i1], combs2[1,i2], combs2[2,i2])]] <- tmp
        }
    }
    return(ddsList)
}
                    
        
    
    
    
