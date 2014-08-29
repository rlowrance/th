ReadParcelsSample <- function(path) {
    # Return data frame for parcels sample
    #cat('start ReadParcelsSample', path, '\n'); browser()
    parcels.sample <- NULL
    loaded <- load(path)  # may load more than parcels.sfr
    stopifnot(!is.null(parcels.sample))
    parcels.sample
}
