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
  

projectFromDir <- function() {
  parts <- strsplit(getwd(), "/")[[1]]
  if (length(parts)!=10 || parts[7]!="projects") {
    res <- list(type=NA)
  } else {
    res <- list(
      bioinf=parts[6],
      lab=parts[8],
      scientist=parts[9],
      project=parts[10]
    )
    res$type <- names(Filter(function(x) res$lab %in% dir(x), list("stp"="/camp/stp", "lab"="/camp/lab")))
  }
  res
}
  

#' Work out related directory locations
#'
#' Based on unix system variables set in ~/.bashrc, work out associated scrach and html directories
#'
#' @param target The name of the environment variable that describes the associated directory
#' @param create if true, then create the directory
#' @return return the path to the associated directory
derivedDir <- function(target, create=FALSE) { #relies on environment variables set up in ~/.bashrc
  if (grepl(Sys.getenv("my_working"),Sys.getenv(target))) {
    dr <- sub(Sys.getenv("my_working"),Sys.getenv(target),getwd())
    if (create)
      dir.create(dr, recursive=TRUE)
    return(dr)
  } else {
    paste0(Sys.getenv(target), sub(Sys.getenv("my_working"), "", getwd()))
  }
}

derivedDirs <- function(publish=NA, subResults=NA) { #relies on environment variables set up in ~/.bashrc
  parts <- projectFromDir()
  prefix <- sub("/$", "", Sys.getenv("my_lab", unset="."))
  if (length(parts)==1) {
    res <- list(
      wd=getwd(),
      results="results",
      objects="objects",
      data="data",
      output="results",
      html="results",
      web="results",
      input="data"
    )
  } else {
    res <- with(parts,
                list(
                  wd=getwd(),
                  results="results",
                  objects="objects",
                  data="data",
                  output=file.path(prefix, "outputs", lab, scientist, "gavin.kelly", project, publish),
                  html=file.path(prefix, "www", bioinf, "public_html/LIVE/projects", lab, scientist, project),
                  web=file.path(paste0("https://shiny-bioinformatics.crick.ac.uk/~",bioinf),
                                "projects", lab, scientist, project),
                  input=file.path("//data.thecrick.org",
                                  sprintf("%s%s", ifelse(type=="lab", "lab-", ""), lab),
                                  "input", "babs", scientist, Sys.getenv("my_emailname"), project, publish)
                )
                )
  }
  if (!is.na(subResults)) {
    res$results <- file.path(res$results, subResults)
  }
  if (is.na(publish)) {
    res$output=res$results
    res$input <- sub("/NA$", "", res$input)
  } else {
    res$results=res$output
  }
  res
}

#' Supply pretty-printed git stats
#'
#'
#'

git_stats <- function() {
  vers <- try(suppressWarnings(system2("git", "log -1 --pretty=format:%h", stdout=TRUE, stderr=FALSE)), silent=TRUE)
  gitTag <- try(suppressWarnings(system2("git", "describe --tags --exact", stdout=TRUE, stderr=FALSE)), silent=TRUE)
  if (class(gitTag)=="try-error" || (!is.null(attr(gitTag, "status")) && attr(gitTag, "status")!=0)) {
    gitTag <- NA
  }
  if (length(vers)==0 || class(vers)=="try-error") {
    vers <- "uncontrolled"
    gitTag <- NA
  } else {
    if (any(grepl("^ M",system2("git", "status --porcelain", stdout=TRUE, stderr=FALSE)))) {
      vers <- sprintf("%s-M", vers)
      gitTag <- NA
    }
  }
  list(version=vers, tag=gitTag)
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
        vers <- git_stats()
        prm <- c(argu,
                 wd=getwd(),
                 fVersion = vers$version,
                 release=vers$tag
                 )
        if (!is.na(grepl(Sys.getenv("my_projects", unset=NA),  getwd()))) {
          pth <- strsplit(sub(Sys.getenv("my_projects"), "",  getwd()), "/")[[1]]
          prm$lab <- pth[1]
          prm$scientist <- pth[2]
          prm$project <- pth[3]
        }
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


#' Open a device with a trackable name
#'
#' Can be used with 'pdf' 'write.table' etc
#'
#' @param dev contains the function which will be called (which itself will get its 'file' param filled in automatically)
#' @param file string will be used as a starting point for the filename.  If it doesn't contain an extension, will predict from 'dev'
#' @return the filename actually used
vDevice <- function(dev, file="plot", ..., dir="results") {
  if (!grepl("\\.", file)) file <- paste0(file, ".", as.character(substitute(dev)))
  gv <- git_stats()$version
  dName <- file.path(dir, params("fVersion"))
  if (!dir.exists(dName)) {
    dir.create(dName)
  }
  file=file.path(dName,sub("([^.]*)(.*)",paste0("\\1_", gv, "\\2"), file))
  dev(..., file= file)
  file
  }
