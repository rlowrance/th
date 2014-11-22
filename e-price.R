# e-price.R
# main program to summarizes prices by time period in the subset1 file.
# compute  price in every month for which we have data
# store results in output file e-price.RData containing these object
# control
# price.by.month data frame with these columns: $year $month $day $num.transactions $median.price $mean.price
# price.by.month data frame with these columns: $year $month      $num.transactions $median.price $mean.price
# price.by.year data frame with these columns:  $year             $num.transactions $median.price $mean.price

source('Directory.R')

source('Libraries.R')

source('ReadTransactionSplits.R')


Control <- function(default.arg) {
    # no command line arguments
    stopifnot(is.null(default.args))

    me <- 'e-price'

    log <- Directory('log')
    splits <- Directory('splits')
    working <- Directory('working')

    control <- list( path.in = paste0(working, 'transactions-subset1-train-splits/')
                    ,path.out.log = paste0(log, me, '.log')
                    ,path.out.rdata = paste0(working, me, '.RData')
                    ,splits.to.read = c( 'census.tract'
                                        ,'property.city'
                                        ,'price'
                                        ,'sale.year'
                                        ,'sale.month'
                                        ,'sale.day'
                                        ,'zip5'
                                        )
                    ,testing = FALSE
                    )
    control
}
PricesByDay <- function(data) {
    # return data frame

    # build up data frame day by day
    all.rows <- NULL
    years <- sort(unique(data$year))
    for (year in years) {
        in.year <- data$year == year
        months <- sort(unique(data[in.year,]$month))
        for (month in months) {
            for (day in 1:31) {
                in.day <- 
                    data$year == year & 
                    data$month == month & 
                    data$day == day 
                period <- data[in.day,]
                next.row <- data.frame( year = year
                                       ,month = month
                                       ,day = day
                                       ,num.transactions = nrow(period)
                                       ,median.price = median(period$price)
                                       ,mean.price = mean(period$price)
                                       )
                print(next.row)
                all.rows <- rbind(all.rows, next.row)
            }
        }
    }
    Printf('by day nrow %d\n', nrow(all.rows))
    all.rows
}
PricesByMonth <- function(data) {
    # return data frame

    # build up data frame month by month
    all.rows <- NULL
    years <- sort(unique(data$year))
    for (year in years) {
        for (month in 1:12) {
            in.period <- 
                data$year == year & 
                data$month == month
            period <- data[in.period,]
            next.row <- data.frame( year = year
                                   ,month = month
                                   ,num.transactions = nrow(period)
                                   ,median.price = median(period$price)
                                   ,mean.price = mean(period$price)
                                   )
            print(next.row)
            all.rows <- rbind(all.rows, next.row)
        }
    }
    Printf('by month nrow %d\n', nrow(all.rows))
    all.rows
}
PricesByYear <- function(data) {
    # return data frame   

    # build up data frame year by year
    all.rows <- NULL
    years <- sort(unique(data$year))
    for (year in years) {
        in.period <- data$year == year
        period <- data[in.period,]
        next.row <- data.frame( year = year
                               ,num.transactions = nrow(period)
                               ,median.price = median(period$price)
                               ,mean.price = mean(period$price)
                               )
        print(next.row)
        all.rows <- rbind(all.rows, next.row)
    }
    Printf('by year nrow %d\n', nrow(all.rows))
    all.rows
}
PriceBySubmarket2003On <- function(submarket.field, data) {
    # return list
    # $price: named list of all prices, names are the submarkets
    # $medians: names list of all median prices
    all.prices <- NULL
    all.medians <- NULL
    unique.submarkets <- unique(data[[submarket.field]])
    for (submarket in unique.submarkets) {
        in.submarket <- data[[submarket.field]] == submarket
        in.time.period <- data$year >= 2003
        prices <- data$price[in.submarket & in.time.period]
        all.prices[[submarket]] <- prices
        all.medians[[submarket]] <- median(prices)
    }
    result <- list( prices = all.prices
                   ,medians = all.medians
                   )
    result
}
PrintMedians <- function(median.values, submarket.name) {
    cat('sorted median values for submarket', submarket.name, '\n')
    sorted <- sort(median.values)
    for (name in names(sorted)) {
        Printf('%30s %7.0f\n', name, sorted[[name]])
    }
}
Main <- function(control, data) {
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)


    price.by.census.tract.2003on <- PriceBySubmarket2003On('census.tract', data)
    price.by.property.city.2003on <- PriceBySubmarket2003On('property.city', data)
    price.by.zip5.2003on <- PriceBySubmarket2003On('zip5', data)
    
    PrintMedians(price.by.property.city.2003on$medians, 'property.city')

    by.year <- PricesByYear(data)
    by.month <- PricesByMonth(data)
    by.day <- if (control$testing) NULL else PricesByDay(data)

    save( control
         ,by.year
         ,by.month
         ,by.day
         ,price.by.census.tract.2003on
         ,price.by.property.city.2003on
         ,price.by.zip5.2003on
         ,file = control$path.out.rdata
         )
}

clock <- Clock()
default.args <- NULL
control <- Control(default.args)

if (!exists('e.price.relevant.transaction.data')) {
    transaction.data <- ReadTransactionSplits( path.in.base = control$path.in
                                              ,split.names = control$splits.to.read
                                              )
    # add year and month of transaction
    e.price.relevant.transaction.data <- 
        data.frame( stringsAsFactors = FALSE
                   ,year = transaction.data$sale.year
                   ,month = transaction.data$sale.month
                   ,day = transaction.data$sale.day

                   ,price = transaction.data$price
                   
                   ,census.tract = as.character(transaction.data$census.tract)
                   ,property.city = as.character(transaction.data$property.city)
                   ,zip5 = as.character(transaction.data$zip5)
                   )
}

Main(control, e.price.relevant.transaction.data)
Printf('took %f CPU minutes\n', clock$Cpu() / 60)
Printf('took %f wallclock minutes\n', clock$Wallclock() / 60)

if (control$testing)
    cat('DISCARD RESULTS: TESTING\n')

cat('done\n')
