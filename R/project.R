#' Only execute code if file dependencies haven't changed
#'
#' If a snapshot exists, and none of the files in the directories pointed to have changed, then skip execution, otherwise run the code and create a snapshot
#' @title Conditional code execution
#' @param tag the name of the snapshot to examine/create
#' @param file modification, deletion or creation of files on these paths will trigger execution
#' @param x the code to execute
#' @return 
#' @author 
cacher <- function(tag, file, x) {
    snapFile <- file.path("snapshot",tag)
    bodyFile <- sprintf("%s_script", snapFile)
    if (!missing(file)) {
        file <- as.character(file)
        toEval <- rerun(snapFile, file)
    }
    if (toEval) {
        eval.parent(substitute(x))
        tmp <- fileSnapshot(file)
        saveRDS(tmp, file=snapFile)
        tmp <- digest::digest(substitute(x))
        saveRDS(tmp, file=bodyFile)
    } else {
        if (!file.exists(bodyFile)) {
            warning("No cache of script exists")
        } else {
            oldScript <- readRDS(bodyFile)
            newScript <- digest::digest(substitute(x))
            if (oldScript!=newScript) {
                warning("Script has changed - consider 'resetCache'ing")
            }
        }
        message(sprintf("%s not rerun, as %s unchanged", tag, file))
    }
    }

##' Reset cached view of the files
##'
##' If execution had already been completed, but no snapshot had been made, use this to create it.  Alternatively, to remove snapshot
##' @title Reset Cache
##' @param tag the name of the snapshot to examine/create
##' @param file modification, deletion or creation of files on these paths will trigger execution
##' @param delete if true, remove the snapshot
##' @return 
##' @author 
cacheReset <- function(tag, file, delete=FALSE) {
    snapFile <- file.path("snapshot",tag)
    if (delete) {
        unlink(snapFile)
        return()
    }
    tmp <- fileSnapshot(file)
    saveRDS(tmp, file=snapFile)
}




rerun <- function(fname, newFile) {
    if (!file.exists(fname))
        return(TRUE)
    snap <- readRDS(fname)
    cF <- changedFiles(snap, fileSnapshot(newFile))
    delta <- unlist(cF[c("added","deleted","changed")])
    return(length(delta)!=0)
}
    

#' Work out related directory locations
#'
#' Based on unix system variables set in ~/.bashrc, work out associated scrach and html directories
#'
#' @param target The name of the environment variable that describes the associated directory
#' @param create if true, then create the directory
#' @return return the path to the associated directory
derivedDir <- function(target, create=FALSE) { #relies on environment variables set up in ~/.bashrc
    dr <- sub(Sys.getenv("MY_WORKING"),Sys.getenv(target),getwd())
    if (create)
        dir.create(dr, recursive=TRUE)
    return(dr)
}

#' Initialise a closure that will contain variables that define the analysis
#'
#' Create a closure that contains at least the version number and the working directory of the current file.  Additional name-value pairs can be added at init time, or alternatively the closure can be called with 'run-time' name-value pairs.  Calling the resulting closure with a character argument will return the parameter value; if there is no argument, the closure will return a list of all current name-values.
#'
#' @param ... the initial set of name-value pairs to be stored in the closure
#' @return A function that can be called to read/write other variables 
params_init <- function(...) {
    argu <- list(...)
    function(...) {
        if (length(names(list(...)))>0)
            argu <<- modifyList(argu, list(...))
        vers <- system2("git", "log -1 --pretty=format:%h", stdout=TRUE, stderr=FALSE)
        if (length(vers)==0)
            vers <- "Not under Version Control!"
        if (any(grepl("^ M",system2("git", "status --porcelain", stdout=TRUE, stderr=FALSE))))
            vers <- sprintf("%s-M", vers)
        prm <- c(argu,
                 wd=getwd(),
                 fVersion = vers
                 )
        if (length(names(list(...))) ==1) {
            return(unlist(list(...)))
        }
        if (length(names(list(...))) > 0 | length(list(...))==0)
            return(prm)
        else
            return(prm[[as.character(...)]])
    }
}




#' Make command-line arguments available as variables
#'
#' Allows Rscript invocation to set variables directly in R session
#'
#' @param defaultParams the default variables that can be over-written by the call
#' @return the new and unchanged variables
getParams <- function(defaultParams) {
    cArgs <- strsplit(commandArgs(TRUE), "=")
    newParams <- lapply(cArgs, "[", 2)
    names(newParams) <- lapply(cArgs, "[", 1)
    badNames <- setdiff(names(newParams), names(defaultParams))
    if (length(badNames)!=0) stop(sprintf("Trying to set invalid parameter: %s", paste(badNames)))
    isNumeric <- sapply(defaultParams, is.numeric)
    defaultParams[names(newParams)] <- unlist(newParams)
    defaultParams[isNumeric] <- as.numeric(defaultParams[isNumeric])
    for (i in names(defaultParams)) {
        assign(i, defaultParams[[i]], envir = .GlobalEnv)
    }
    paramStr <- paste(names(defaultParams), unlist(defaultParams), sep="", collapse="_")
    return(defaultParams)
}

#' Allows a variety of counters to be initiated and incremented
#'
#' Closure that stores a list of counter values
#'
#' @param prefix contains the name of the counter
#' @return the current value of the specified counter, prefixed by the counter name
counter <- local({
    ii <- list()
    function(reset=FALSE, prefix="x") {
        if (reset) ii[[prefix]] <<- 1
        if (is.null(ii[[prefix]])) ii[[prefix]] <<- 1
        ii[[prefix]] <<- ii[[prefix]]+1
        sprintf("%s%X",prefix, ii[[prefix]]-1)
    }
})
