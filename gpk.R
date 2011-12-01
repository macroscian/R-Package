ID = paste(sub(".* (.*) .*","\\1","$Revision: 1.1 $"), Sys.Date(), sep="_")
drop.levels <- function(dat){
      dat[] <- lapply(dat, function(x) x[,drop=TRUE])
        return(dat)
  }

parcoord <-function (y, x=1:ncol(y), col = 1, ax.col=rep("grey70",ncol(y)), lty = 1, add=FALSE, ...)
{
	if (!add) {
    	matplot(x, t(y), type = "l", col = col, lty = lty,
      	  xlab = "", ylab = "", axes = FALSE,  ...)
    	axis(1, at = x, labels = colnames(y),...)
    	if (is.factor(ax.col))
    		ax.col=rainbow(length(levels(ax.col)))[as.integer(ax.col)]
    	for (i in x) lines(c(i, i), c(min(y), max(y)), col = ax.col[i])
	} else {
    	matlines(x, t(y), col = col, lty = lty,
      	  xlab = "", ylab = "", axes = FALSE)
  	}
    invisible()
}

test2list = function(tests, updown=FALSE) {
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

log2sign <- function(x) {
  if (!is.logical(x))  x <- sign(x)*(2^abs(x))
  return(x)
}

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


ppath <- function(dir, ...) {
  paste(dir, paste(..., sep="."), sep="")
}
