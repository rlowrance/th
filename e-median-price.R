# main program to determine median prices in the subset1 file.
# command line options:
# --by   : 'year' or 'month'
# --from : int, starting year
# --to   : int, ending year
#
# objects in the output RData file
# data frame containing median prices
# chart      containing Cleveland Dot Plot of the median prices over time
#
# may also write as output a pdf file containing the chart in pdf form

source('DirectoryLog.R')
source('DirectorySplits.R')
source('DirectoryWorking.R')

source('Libraries.R')

library(ggplot2)

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
                    ,path.out.pdf = paste0(working, log.file.base, '.pdf')
                    ,splits.to.read = c('price', 'sale.year', 'sale.month')
                    ,show.chart = TRUE
                    ,chart.width = 14  # inches
                    ,chart.heigh = 10  # inches
                    ,write.pdf = TRUE
                    ,testing = FALSE
                    )
    control
}
MakeChart <- function(df, labels) {
     # Return Cleveland Dot Plot as a ggplot2 object
     # x is df$median.price
     # y is labels
    period.factor <- factor( labels
                            ,levels = sort(labels, decreasing = TRUE)
                            )
    new.df <- data.frame( median.price = df$median.price
                         ,period = period.factor
                         )
    gg <- ggplot( new.df
                 ,aes(x = median.price, y = period)
                 )
    max.median.price <- max(new.df$median.price)
    g <-
        gg +
        geom_point(size = 3) +
        xlim(0, max.median.price) +
        theme_bw() +
        theme( panel.grid.major.x = element_blank()
              ,panel.grid.minor.y = element_blank()
              ,panel.grid.major.x = element_blank()
              )
    
    g
}
MedianPricesByMonth <- function(from.year, to.year, data) {
     # return list
     # $ analysis: data frame with median prices and time periods
     # $ chart   : Cleveland Dot Plot

    MakeDataFrame <- function() {
        MedianPrice <- function(year, month) {
            # return median price in data fro year and month
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

    Labels <- function(df) {
        # return vector of labels for the time period
        label <- sprintf('%04d-%02d', df$sale.year, df$sale.month)
    }

    analysis <- MakeDataFrame()
    chart <- MakeChart(analysis, Labels(analysis))
    list( analysis = analysis
         ,chart = chart
         )

}
MedianPricesByYear <- function(from.year, to.year, data) {
     # return list
     # $ analysis: data frame with median prices and time periods
     # $ chart   : Cleveland Dot Plot

    MakeDataFrame <- function() {
        MedianPrice <- function(year) {
            # return median price in data fro year and month
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

    Labels <- function(df) {
        # return vector of labels for the time period
        label <- sprintf('%04d', df$sale.year)
    }

    #debug(MakeDataFrame)
    #debug(Labels)

    analysis <- MakeDataFrame()
    chart <- MakeChart(analysis, Labels(analysis))
    list( analysis = analysis
         ,chart = chart
         )
}
Main <- function(control, data) {
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)

    medianPrices.chart <- 
        if (control$by == 'month') {
            MedianPricesByMonth(control$from, control$to, data)
        } else {
            MedianPricesByYear(control$from, control$to, data)
        }
    median.prices <- medianPrices.chart$analysis
    chart <- medianPrices.chart$chart

    # maybe show the chart
    if (control$show.chart) {
        X11( width = control$chart.width
            ,height = control$chart.height
            )
        print(chart)
    }

    # maybe write chart to a pdf file
    if (control$write.pdf) {
        pdf( file = control$path.out.pdf
            ,width = control$chart.width
            ,height = control$chart.height
            )
        print(chart)
        dev.off()
    }

    save(median.prices, chart, file = control$path.out.rdata)

    str(control)
}

#debug(ParseCommandLine)
#debug(Control)
#debug(Main)
#debug(ReadTransactionSplits)
#debug(ReadSplit)
#debug(MakeChart)
#debug(MedianPricesByMonth)
#debug(MedianPricesByYear)

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
