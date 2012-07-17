calcKonce <- function(pos1, pos2, len, random=FALSE) {
  if (is.null(pos2)) {
    if (random) {
      pos1 <- ceiling(runif(length(pos1), min=0, max=len))
      pos1 <- sample(len,length(pos1))
    }
    dMat <- outer(pos1, pos1, "-")
    dists <- abs(dMat[upper.tri(dMat)])
  } else {
    if (random) {
      pos2 <- ((pos2+sample(len,1)) %% len)+1
    }
    dMat <- outer(pos1, pos2, "-")
    dists <- abs(as.vector(dMat))
  }
  y <- rep(0, len)
  distT <- table(dists)
  y[as.integer(names(distT))+1] <- as.vector(distT)
  y <- cumsum(y)
  return(y)
}


calcKgrouped <- function(pos1, pos2, len, boot=0, centile=0.05, grp1=NULL, grp2=NULL) {
  if (is.null(grp1)) {
    groups <- "all"
    grp1 <- rep(groups, length(pos1))
    grp2 <- rep(groups, length(pos2))
  } else {
      if (is.null(pos2)) {
          groups <- grp1
      } else {
          groups <- intersect(grp1, grp2)
      }
  }
  y <- yint <- x <- rnk <- list()
  hiCent <- loCent <- mid <- list()
  hi <- lo <- list()
  for (g in groups) {
    g1 <- grp1==g
    g2 <- grp2==g
    if (is.null(pos2)) {
      normFactor <- len[[g]]/(sum(g1)*sum(g1))
    } else {
      normFactor <- len[[g]]/(sum(g1)*sum(g2))
    }
    yint[[g]] <-   calcKonce(pos1[g1], pos2[g2], len[[g]])
    y[[g]] <- yint[[g]]*normFactor
    x[[g]] <- 1:(len[[g]])
    if (boot>0) {
      nCent <- max(2,ceiling(boot*centile/2))
      hiCent[[g]] <- matrix(-Inf, len[[g]], nCent)
      loCent[[g]] <- matrix( Inf, len[[g]], nCent)
      mid[[g]] <-  rnk[[g]] <- rep(0, len[[g]])
      for (i in 1:boot) {
        y0int <-calcKonce(pos1[g1], pos2[g2], len[[g]], random=TRUE)
        rnk[[g]] <- rnk[[g]]+(yint[[g]]>y0int)
        mid[[g]] <- mid[[g]]+y0int*normFactor/boot
        indRep <- cbind(1:len[[g]], max.col(-hiCent[[g]], ties.method="first"))
        hiCent[[g]][indRep] <- pmax(y0int, hiCent[[g]][indRep])
        indRep <- cbind(1:len[[g]], max.col(loCent[[g]], ties.method="first"))
        loCent[[g]][indRep] <- pmin(y0int, loCent[[g]][indRep])
      }
      hi[[g]] <- apply(hiCent[[g]], 1, max)*normFactor
      lo[[g]] <- apply(loCent[[g]], 1, min)*normFactor
      hiCent[[g]][is.infinite(hiCent[[g]])] <- Inf
      loCent[[g]][is.infinite(loCent[[g]])] <- -Inf
      hiCent[[g]] <- apply(hiCent[[g]], 1, min)*normFactor
      loCent[[g]] <- apply(loCent[[g]], 1, max)*normFactor
    }
  }
  if (boot>0) {
    return(list(x=x, y=y, hi=hi, lo=lo, mid=mid, hiCent=hiCent, loCent=loCent, rank=rnk))
  } else {
    return(y)
  }
}

calcKagg <- function(pos1, pos2, len, boot=0, centile=0.05, grp1=NULL, grp2=NULL) {
  if (is.null(grp1)) {
    groups <- "all"
    grp1 <- rep(groups, length(pos1))
    grp2 <- rep(groups, length(pos2))
  } else {
      if (is.null(pos2)) {
          groups <- grp1
      } else {
          groups <- intersect(grp1, grp2)
      }
  }
  y <- yint <- x <- rnk <- list()
  hiCent <- loCent <- mid <- list()
  hi <- lo <- list()
  for (g in groups) {
    g1 <- grp1==g
    g2 <- grp2==g
    if (is.null(pos2)) {
      normFactor <- len[[g]]/(sum(g1)*sum(g1))
    } else {
      normFactor <- len[[g]]/(sum(g1)*sum(g2))
    }
    yint[[g]] <-   calcKonce(pos1[g1], pos2[g2], len[[g]])
    y[[g]] <- yint[[g]]*normFactor
    x[[g]] <- 1:(len[[g]])
    if (boot>0) {
      nCent <- max(2,ceiling(boot*centile/2))
      hiCent[[g]] <- matrix(-Inf, len[[g]], nCent)
      loCent[[g]] <- matrix( Inf, len[[g]], nCent)
      mid[[g]] <-  rnk[[g]] <- rep(0, len[[g]])
      for (i in 1:boot) {
        y0int <-calcKonce(pos1[g1], pos2[g2], len[[g]], random=TRUE)
        rnk[[g]] <- rnk[[g]]+(yint[[g]]>y0int)
        mid[[g]] <- mid[[g]]+y0int*normFactor/boot
        indRep <- cbind(1:len[[g]], max.col(-hiCent[[g]], ties.method="first"))
        hiCent[[g]][indRep] <- pmax(y0int, hiCent[[g]][indRep])
        indRep <- cbind(1:len[[g]], max.col(loCent[[g]], ties.method="first"))
        loCent[[g]][indRep] <- pmin(y0int, loCent[[g]][indRep])
      }
      hi[[g]] <- apply(hiCent[[g]], 1, max)*normFactor
      lo[[g]] <- apply(loCent[[g]], 1, min)*normFactor
      hiCent[[g]][is.infinite(hiCent[[g]])] <- Inf
      loCent[[g]][is.infinite(loCent[[g]])] <- -Inf
      hiCent[[g]] <- apply(hiCent[[g]], 1, min)*normFactor
      loCent[[g]] <- apply(loCent[[g]], 1, max)*normFactor
    }
  }
  if (boot>0) {
    return(list(x=x, y=y, hi=hi, lo=lo, mid=mid, hiCent=hiCent, loCent=loCent, rank=rnk))
  } else {
    return(y)
  }
}

