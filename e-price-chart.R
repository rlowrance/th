# e-price-chart.R
# main program to produce charts containing emperical price analysis from program e-price.R

source('Directory.R')
source('Libraries.R')

library(ggplot2)

Control <- function(default.args) {
    me <- 'e-price-chart'
    
    log <- Directory('log')
    working <- Directory('working')

    control <- list( path.in = paste0(working, 'e-price.RData')
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
    stopifnot(!is.null(by.year))
    stopifnot(!is.null(by.month))
    stopifnot(!is.null(by.day))

    result <- list( by.year  = by.year
                   ,by.month = by.month
                   ,by.day   = by.day
                   ,control  = control
                   )
    result
}
Cleveland <- function(labels, values, label.axis.name, value.axis.name) {
    # return ggplot2 object, a Cleveland Dot plot
    # df$value: num, the plot point
    # df$label: chr name for the plot point
    # ref: R Graphics Cookbook p.42 and following
    y.factor <- factor( labels
                       ,levels = sort(labels, decreasing = TRUE)
                       )
    df <- data.frame( x = values
                     ,y = y.factor
                     )
    gg <- ggplot( df
                 ,aes( x = x
                      ,y = y
                      )
                 )
    g <-
        gg +
        geom_point(size = 3) +
        xlim(0, max(df$x)) +
        xlab(value.axis.name) +
        ylab(label.axis.name) +
        theme_bw() +
        theme( panel.grid.major.x = element_blank()
              ,panel.grid.minor.y = element_blank()
              ,panel.grid.major.x = element_blank()
              )
    g
}
Chart1 <- function(control, data) {
    # Cleveland plot by year 1984 - 2009
    #cat('Chart1\n'); browser()

    # select the portion we want in the chart
    in.chart <- 
        data$by.year$year >= 1984 &
        data$by.year$year <= 2009
    chart.data <- data$by.year[in.chart,]

    # produce the chart
    chart <- Cleveland( labels = sprintf('%4d', chart.data$year)
                       ,values = chart.data$median.price
                       ,label.axis = 'year'
                       ,value.axis = 'median price'
                       )
                       
    chart
}
Chart2 <- function(control, data) {
    # Cleveland plot by year month 2007 - 2009
    #cat('Chart2\n'); browser()

    # produce labels and values
    labels <- list()
    values <- list()
    index <- 0
    for (year in 2007:2009) {
        for (month in 1:12) {
            median.price <- data$by.month[data$by.month$year == year &
                                          data$by.month$month == month,
                                          ]$median.price
            if (!is.na(median.price)) {
                index <- index + 1
                labels[[index]] <- sprintf('%4d-%2d', year, month)
                values[[index]] <- median.price
            }
        }
    }

    # produce the chart
    chart <- Cleveland( labels = sapply(labels, function(x) x)
                       ,values = sapply(values, function(x) x)
                       ,label.axis = 'year-month'
                       ,value.axis = 'median price'
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
    CreateShowWrite(Chart2, control$path.out.chart2)
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
