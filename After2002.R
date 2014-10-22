After2002 <- function(data.all.years) {
    # return data frame containing transactions with sale dates in 2003 or later
    # data.all.years: a data frame with column saleDate

    jan.1.2003 <- as.Date('2003-01-01')
    is.after.2002 <- data.all.years$saleDate >= jan.1.2003
    data.after.2002 <- data.all.years[is.after.2002,]
    data.after.2002
}
