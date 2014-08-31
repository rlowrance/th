ReadParcelsSfrSample <- function(path) {
    # Return data frame for parcels sample
    #cat('start ReadParcelsSample', path, '\n'); browser()
    parcels.sfr <- NULL
    loaded <- load(path)  # may load more than parcels.sfr
    stopifnot(!is.null(parcels.sfr))
    parcels.sfr
}
