# e-verify-assessment-chart.R
# main program to produce charts containing emperical price analysis from program e-price.R

source('Directory.R')
source('Libraries.R')

source('ChartCleveland01.R')

library(ggplot2)

Control <- function(default.args) {
    me <- 'e-verify-assessment-chart'
    
    log <- Directory('log')
    working <- Directory('working')

    control <- list( path.in = paste0(working, 'e-verify-assessment.RData')
                    ,path.out.log = paste0(log, me, '.log')
                    ,path.out.chart1 = paste0(working, me, '_chart1.pdf')
                    ,path.out.chart2 = paste0(working, me, '_chart2.pdf')
                    ,show.chart = FALSE
                    ,write.chart = TRUE
                    ,chart.width  = 14  # inches
                    ,chart.height = 10  # inches
                    ,testing = FALSE
                    )
    control
}
ReadData <- function(my.control) {
    loaded <- load(file = my.control$path.in)
    print(loaded)
    stopifnot(!is.null(accuracy.by.month))

    result <- list( accuracy = accuracy.by.month
                   ,control  = control
                   )
    result
}
Chart1 <- function(control, data) {
    # cleveland dot plot
    Round <- function(x) {
        # round to 2 decimal places
        # 0.001 ==> 0.00
        round(x * 100) / 100
    }
    accuracy <- data$accuracy
    labels <- list()
    values <- list()
    index <- 0
    for (year in min(accuracy$year):max(accuracy$year)) {
        for (month in 1:12) {
            fraction.error.zero <- accuracy[accuracy$year == year &
                                            accuracy$month == month,
                                            ]$fraction.error.zero
            if (length(fraction.error.zero) > 0) {
                index <- index + 1
                labels[[index]] <- sprintf('%4d-%02d', year, month)
                values[[index]] <- Round(fraction.error.zero)
            }
        }
    }
    chart <- ChartCleveland01( labels = sapply(labels, function(x) x)
                              ,values = sapply(values, function(x) x)
                              ,label.axis = 'year-month'
                              ,value.axis = 'fraction of transactions with (price - assessment) = 0'
                              )
                       
    chart
}
Charts <- function(control, data) {
    CreateShowWrite <- function(F, path.out) {
        Show <- function(chart) {
            # show on console
            X11( width = control$chart.width
                ,height = control$chart.height
                )
            print(chart)
        }

        Write <- function(chart) {
            # write to file
            pdf( file = path.out
                ,width = control$chart.width
                ,height = control$chart.height
                )
            print(chart)
            dev.off()
        }

        chart <- F(control, data)
        if (control$show.chart)
            Show(chart)
        if (control$write.chart)
            Write(chart)
    }

    CreateShowWrite(Chart1, control$path.out.chart1)
}
Main <- function(control) {
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)

    data <- ReadData(control)

    Charts(control, data)
}


default.args <- NULL
control <- Control(default.args)


Main(control)
if (control$testing)
    cat('DISCARD RESULTS: TESTING\n')
cat('done\n')
