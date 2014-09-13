ReadTransactions <- function(path) {
    # return data.frame with all the transaction in the path
    #cat('start ReadParcelsSfr', path, '\n'); browser()
    transactions <- NULL
    loaded <- load(path)  # may load more than transactions
    str(loaded)
    stopifnot(!is.null(transactions))
    transactions
}
