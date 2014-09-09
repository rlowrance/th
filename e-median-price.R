# main program to determine median prices in the subset1 file.
# command line options:
# --by   : 'year' or 'month'
# --from : int, starting year
# --to   : int, ending year

# input files OUTPUT/transactions-subset1-price.rsave
#                                        -sale-year.rsave
#                                        -sale-month.rsave
#                         
# write files OUTPUT/an-01.rsave
#                    an-01-log.txt


source('DirectoryLog.R')
source('DirectorySplits.R')
source('DirectoryWorking.R')

source('Libraries.R')

Control <- function(parsed.command.args) {
    from <- as.integer(parsed.command.args$from)
    to <- as.integer(parsed.command.args$to)

    # output file name depends on parsed.command.args
    me <- 'e-median-price'
    log.file.base <- sprintf( '%s_by_%s_from_%d_to_%d'
                             ,me
                             ,parsed.command.args$by
                             ,from
                             ,to
                             )
    
    log <- DirectoryLog()
    splits <- DirectorySplits()
    working <- DirectoryWorking()

    control <- list( from = from
                    ,to = to
                    ,by = parsed.command.args$by
                    ,path.in.splits = splits
                    ,path.out.log = paste0(log, log.file.base, '.log')
                    ,path.out.rdata = paste0(working, log.file.base, '.RData')
                    ,splits.to.read = c('price', 'sale.year', 'sale.month')
                    ,testing = FALSE
                    )
    control
}

MedianPricesByMonth <- function(from.year, to.year, data) {
    # return data frame containing median prices for each month
    MedianPrice <- function(year, month) {
        # return median price in data fro year and month
        #cat('starting MedianPrice\n', year, month, '\n'); browser()
        selected <- data$sale.year == year & data$sale.month == month
        price <- data[selected, 'price']
        median.price <- median(price, na.rm = TRUE)
        median.price
    }
    analysis <- NULL
    for (sale.year in from.year:to.year) {
        last.sale.month <- if (sale.year == 2009) 11 else 12
        for (sale.month in 1:last.sale.month) {
            median.price <- MedianPrice(sale.year, sale.month)
            next.row <- data.frame( stringsAsFactors = FALSE
                                   ,sale.year = sale.year
                                   ,sale.month = sale.month
                                   ,median.price = median.price
                                   )
            analysis <- rbind(analysis, next.row)
        }
    }
    analysis
}
MedianPricesByYear <- function(from.year, to.year, data) {
    # return data frame containing median prices for each month
    MedianPrice <- function(year) {
        # return median price in data fro year and month
        #cat('starting MedianPrice\n', year, month, '\n'); browser()
        selected <- data$sale.year == year
        price <- data[selected, 'price']
        median.price <- median(price, na.rm = TRUE)
        median.price
    }
    analysis <- NULL
    for (sale.year in from.year:to.year) {
        median.price <- MedianPrice(sale.year)
        next.row <- data.frame( stringsAsFactors = FALSE
                               ,sale.year = sale.year
                               ,median.price = median.price
                               )
        analysis <- rbind(analysis, next.row)
    }
    analysis
}
Main <- function(control, data) {
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)

    median.prices <- 
        if (control$by == 'month') {
            MedianPricesByMonth(control$from, control$to, data)
        } else {
            MedianPricesByYear(control$from, control$to, data)
        }
    save(median.prices, file = control$path.out.rdata)

    str(control)
}

#debug(ParseCommandLine)
#debug(Control)
#debug(Main)
#debug(ReadTransactionSplits)
#debug(ReadSplit)

default.args <- NULL
#default.args <- list('--by', 'year', '--from', '2000', '--to', '2001')
#default.args <- list('--by', 'month', '--from', '2000', '--to', '2001')

command.args <- if (is.null(default.args)) CommandArgs() else default.args
parsed.command.args <- ParseCommandLine( cl = command.args
                                        ,keywords = c('by', 'from', 'to')
                                        ,ignoreUnexpected = TRUE
                                        )
control <- Control(parsed.command.args)

if (!exists('transaction.data')) {
    transaction.data <- ReadTransactionSplits( path.in.base = control$path.in.splits
                                              ,split.names = control$splits.to.read
                                              )
}

Main(control, transaction.data)

cat('done\n')
