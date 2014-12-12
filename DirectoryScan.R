DirectoryScan <- function(path.to.directory, file.name.regex = NULL) {
    # return function Next() --> next full path file name or NULL (if no more file names)
    files <- list.files( path = path.to.directory
                        ,pattern = file.name.regex
                        ,all.files = FALSE     # don't return hidden and visible file names
                        ,full.names = FALSE    # don't prepand directory path to file name
                        ,recursive = FALSE     # don't recurse into subdirectories
                        ,ignore.case = TRUE    # ignore file name case (as for OS X)
                        ,include.dirs = FALSE  # don't include directory names
                        ,no.. = TRUE           # exclude files . and ..
                        )
    file.index <- 0
    Next <- function() {
        # return next file name or NULL if no more file names
        file.index <<- file.index + 1
        if (file.index > length(files))
            NULL
        else
            files[[file.index]]
    }
    Next
}

DirectoryScanTest <- function() {
    # unit test
    source('Directory.R')
    path.to.directory <- paste0(Directory('working'), 'e-cv-cells')
    file.name.regex <- 'global_rf_2003on_avm_logprice_*_level_*_100_*_*' # 1% samples
    file.name.regex <- '^global_rf_2003on_avm_logprice_*_100_*' # 1% samples
    file.name.regex <- '^global_rf_2003on_avm_logprice_.*_level_[0-9]*_100_.*RData$' # 1% samples
    file.name.regex <- '^global_rf_2003on_avm_logprice_(alwaysNoAssessment|best??)_level_[0-9]*_100_.*RData$' # 1% samples
    file.name.regex <- '^global_rf_2003on_avm_logprice_(alwaysNoAssessment|best..)_level_[0-9]*_100_.*RData$' # 1% samples
    #file.name.regex <- '*'
    Next <- DirectoryScan( path.to.directory = path.to.directory
                          ,file.name.regex = file.name.regex
                          )
    browser()
    while (!is.null(next.file.name <- Next())) {
        cat(next.file.name, '\n')
    }
    cat('no more files matching pattern\n')
    browser()
}

#DirectoryScanTest()
    
