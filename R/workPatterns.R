##' As if re-starting R in a new diretory
##'
##' Clears variables, detaches packages and changes directory
##' @title Switch Projects
##' @param path The new directory to transfer to
##' @return invisibly, the new path
##' @author gavin.kelly@crick.ac.uk
switchProject <- function(path) {
  setwd(path)
  rm(list = ls(all = TRUE))
  if (length(toRemove <- names(sessionInfo()$otherPkgs)))
    lapply(paste('package:',toRemove,sep=""), detach, character.only=TRUE, unload=TRUE)
  invisible()
  }
