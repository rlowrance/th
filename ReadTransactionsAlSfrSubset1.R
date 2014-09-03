ReadTransactionsAlSfrSubset1 <- function(path) {
    # return transactions.al.sfr.subset1 object
    transactions.al.sfr.subset1 <- NULL
    loaded <- load(path)
    stopifnot(!is.null(transactions.al.sfr.subset1))
    transactions.al.sfr.subset1
}
