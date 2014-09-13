ReadTransactionsAlSfr <- function(path) {
    # return data.frame with all the transaction in the path
    #cat('start ReadParcelsSfr', path, '\n'); browser()
    transactions.al.sfr <- NULL
    loaded <- load(path)  # may load more than parcels.sfr
    stopifnot(!is.null(transactions.al.sfr))
    transactions.al.sfr
}
