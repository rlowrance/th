ReadDeedsAlSample <- function(path) {
    #cat('start ReadDeedsAl', path, '\n'); browser()
    deeds.al <- NULL
    loaded <- load(path)  # may load more than parcels.sfr
    stopifnot(!is.null(deeds.al))
    deeds.al
}
