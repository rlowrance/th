ReadDeedsAl <- function(path) {
    #cat('start ReadDeedsAl', path, '\n'); browser()
    arms.length <- NULL
    loaded <- load(path)  # may load more than parcels.sfr
    stopifnot(!is.null(arms.length))
    deeds.al <- arms.length
    deeds.al
}
