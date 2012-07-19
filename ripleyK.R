alcKonce <- function(pos1, pos2, len, random=FALSE, spacer=NULL) {
  if (is.null(pos2)) {
    if (random) {
        n <- length(pos1)
        if (is.null(spacer)) {
            pos1 <- sample(len,n)
        } else {
            slack <- len+1-n*spacer
            r <- runif(n+1)
            r <- r[-1]*slack/sum(r)
            pos1 <- ceiling(cumsum(r) +seq(0, by=spacer, length=n))
        }
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


calcKgrouped <- function(pos1, pos2, len, boot=0, centile=0.05, grp1=NULL, grp2=NULL, spacer=NULL) {
  if (is.null(grp1)) {
    groups <- "all"
    grp1 <- rep(groups, length(pos1))
    grp2 <- rep(groups, length(pos2))
  } else {
      if (is.null(pos2)) {
          groups <- unique(grp1)
      } else {
          groups <- intersect(grp1, grp2)
      }
  }
  y0 <- y0int <- x <- rnk <- list()
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
    y0int[[g]] <-   calcKonce(pos1[g1], pos2[g2], len[[g]], spacer=spacer)
    y0[[g]] <- y0int[[g]]*normFactor
    x[[g]] <- 1:(len[[g]])
    if (boot>0) {
      nCent <- max(2,ceiling(boot*centile/2))
      hiCent[[g]] <- matrix(-Inf, len[[g]], nCent)
      loCent[[g]] <- matrix( Inf, len[[g]], nCent)
      mid[[g]] <-  rnk[[g]] <- rep(0, len[[g]])
      for (i in 1:boot) {
        bootVal <-calcKonce(pos1[g1], pos2[g2], len[[g]], random=TRUE, spacer=spacer)
        rnk[[g]] <- rnk[[g]]+(y0int[[g]]>bootVal)
        mid[[g]] <- mid[[g]]+bootVal*normFactor/boot
        indRep <- cbind(1:len[[g]], max.col(-hiCent[[g]], ties.method="first"))
        hiCent[[g]][indRep] <- pmax(bootVal, hiCent[[g]][indRep])
        indRep <- cbind(1:len[[g]], max.col(loCent[[g]], ties.method="first"))
        loCent[[g]][indRep] <- pmin(bootVal, loCent[[g]][indRep])
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
    return(list(x=x, y=y0, hi=hi, lo=lo, mid=mid, hiCent=hiCent, loCent=loCent, rank=rnk))
  } else {
    return(y)
  }
}

calcKagg <- function(pos1, pos2, len, boot=0, centile=0.05, grp1=NULL, grp2=NULL, spacer=NULL) {
  if (is.null(grp1)) {
    groups <- "all"
    grp1 <- rep(groups, length(pos1))
    grp2 <- rep(groups, length(pos2))
  } else {
      if (is.null(pos2)) {
          groups <- unique(grp1)
      } else {
          groups <- intersect(grp1, grp2)
      }
  }
  y0 <- y0int <- x <- rnk <- list()
  hiCent <- loCent <- mid <- list()
  hi <- lo <- list()
  normFactor <- g1 <- g2 <- list()
  ### init
  for (g in groups) {
    g1[[g]] <- grp1==g
    g2[[g]] <- grp2==g
    if (is.null(pos2)) {
      normFactor[[g]] <- len[[g]]/(sum(g1[[g]])*sum(g1[[g]]))
    } else {
      normFactor[[g]] <- len[[g]]/(sum(g1[[g]])*sum(g2[[g]]))
    }
    y0int[[g]] <-   calcKonce(pos1[g1[[g]]], pos2[g2[[g]]], len[[g]], spacer=spacer)
    y0[[g]] <- y0int[[g]]*normFactor[[g]]
    x[[g]] <- 1:(len[[g]])
    if (boot>0) {
      nCent <- max(2,ceiling(boot*centile/2))
      hiCent[[g]] <- matrix(-Inf, len[[g]], nCent)
      loCent[[g]] <- matrix( Inf, len[[g]], nCent)
      mid[[g]] <-  rnk[[g]] <- rep(0, len[[g]])
    }
  }
  maxLen <- max(sapply(x, length))
  tmp <-  sapply(y0, function(x) {length(x) <- maxLen; x})
  y0$all <- apply(tmp, 1, mean, na.rm=TRUE)
  x$all <- 1:maxLen
  if (boot>0) {
    hiCent$all <- matrix(-Inf, maxLen, nCent)
    loCent$all <- matrix( Inf, maxLen, nCent)
    mid$all <-  rnk$all <- rep(0, maxLen)
    for (i in 1:boot) {
      bootValExtend <- list()
      for (g in groups) {
        bootVal <-calcKonce(pos1[g1[[g]]], pos2[g2[[g]]], len[[g]], random=TRUE, spacer=spacer)
        rnk[[g]] <- rnk[[g]]+(y0int[[g]]>bootVal)
        mid[[g]] <- mid[[g]]+bootVal*normFactor[[g]]/boot
        indRep <- cbind(1:len[[g]], max.col(-hiCent[[g]], ties.method="first"))
        hiCent[[g]][indRep] <- pmax(bootVal, hiCent[[g]][indRep])
        indRep <- cbind(1:len[[g]], max.col(loCent[[g]], ties.method="first"))
        loCent[[g]][indRep] <- pmin(bootVal, loCent[[g]][indRep])
        length(bootVal) <- maxLen
        bootValExtend[[g]] <- bootVal*normFactor[[g]]
      }
      tmp <- do.call(cbind, bootValExtend)
      bootValAll <- rowMeans(tmp, na.rm=TRUE)
      rnk$all <- rnk$all+(y0$all>bootValAll)
      mid$all <- mid$all+bootValAll/boot
      indRep <- cbind(1:maxLen, max.col(-hiCent$all, ties.method="first"))
      hiCent$all[indRep] <- pmax(bootValAll, hiCent$all[indRep])
      indRep <- cbind(1:maxLen, max.col(loCent$all, ties.method="first"))
      loCent$all[indRep] <- pmin(bootValAll, loCent$all[indRep])
    }
    for (g in groups) {
      hi[[g]] <- apply(hiCent[[g]], 1, max)*normFactor[[g]]
      lo[[g]] <- apply(loCent[[g]], 1, min)*normFactor[[g]]
      hiCent[[g]][is.infinite(hiCent[[g]])] <- Inf
      loCent[[g]][is.infinite(loCent[[g]])] <- -Inf
      hiCent[[g]] <- apply(hiCent[[g]], 1, min)*normFactor[[g]]
      loCent[[g]] <- apply(loCent[[g]], 1, max)*normFactor[[g]]
    }
    hi$all <- apply(hiCent$all, 1, max)
    lo$all <- apply(loCent$all, 1, min)
    hiCent$all[is.infinite(hiCent$all)] <- Inf
    loCent$all[is.infinite(loCent$all)] <- -Inf
    hiCent$all <- apply(hiCent$all, 1, min)
    loCent$all <- apply(loCent$all, 1, max)
}


  if (boot>0) {
    return(list(x=x, y=y0, hi=hi, lo=lo, mid=mid, hiCent=hiCent, loCent=loCent, rank=rnk))
  } else {
    return(y)
  }
}

