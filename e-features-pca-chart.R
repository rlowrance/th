# e-features-pca-chart.R
# main program
# Produce charts using as input file e-features-pcs.RData
# Write file
#  e-features-pcs-chart.1.txt
#
# Command line arguments NONE

source('Directory.R')
source('Libraries.R')

library(optparse)

Control <- function() {
    me <- 'e-features-pca-chart' 

    log <- Directory('log')
    working <- Directory('working')

    testing <- FALSE
    #testing <- TRUE
    out.base <-
        sprintf('%s'
                ,me
                )

    path.out.chart1 <-
        sprintf( '%s_1.txt'
                ,me
                )
    
    path.out.chart2 <- sapply(1:10,  # create paths for 10 possible principal components
                             function(component.num) 
                                 paste0( working
                                        ,out.base
                                        ,sprintf('_2_%02d.txt', component.num)
                                        )
                             )
    in.file <-
        sprintf( 'e-features-pca.RData'
                )


    control <- list( path.in = paste0(working, in.file)
                    ,path.out.log = paste0(log, out.base, '.log')
                    ,path.out.rdata = paste0(working, out.base, '.RData')
                    ,path.out.chart1 = paste0(working, path.out.chart1)
                    ,path.out.chart2 = path.out.chart2  # a vector of paths
                    ,testing = testing
                    ,debug = FALSE
                    )
    control
}
ParseCommandArgs <- function(command.args) {
    # return name list of values from the command args
    opt.predictors <- make_option( opt_str = c('--predictors')
                                  ,action = 'store'
                                  ,type = 'character'
                                  ,help = 'name of feature set to use'
                                  )
    option.list <- list( opt.predictors
                        )
    opt <- parse_args( object = OptionParser(option_list = option.list)
                      ,args = command.args
                      ,positional_arguments = FALSE
                      )
}
PCACharts <- function(my.control) {
    # create all charts for PCA
    Chart1 <- function(control, sdev) {
        num.components <- length(sdev)
        variance <- sdev * sdev
        total.variance <- sum(variance)
        
        Body <- function() {
            lapply( 1:num.components
                   ,function(component.num) {
                       sum.variance <- sum(variance[1:component.num])
                       sprintf('Principal Component %2d  Variance %15.0f  Cum Fraction Total Variance %8.6f'
                               ,component.num
                               ,variance[[component.num]]
                               ,sum.variance / total.variance
                               )
                   }
                   )
        }

        lines <- c(
                   'PCA Principal Component Variances'
                   ,' '
                   ,Body()
                   )
        lines
    }
    Chart2 <- function(control, rotation, component.number) {
        Body <- function() {
            feature.name <- dimnames(rotation)[[1]]
            weight <- rotation[,component.number]
            sapply(1:length(feature.name)
                   ,function(feature.index) {
                       sprintf('Feature %30s  Weight %+10.6f'
                               ,feature.name[[feature.index]]
                               ,weight[[feature.index]]
                               )
                   }
                   )
        }
        lines <- c(
                   sprintf('PCA Rotation for Principal Component %2d', component.number)
                   ,' '
                   ,Body()
                   )
        lines
    }


    prcomp.result <- NULL
    loaded <- load(file = my.control$path.in)
    str(loaded)  # NOTE: control has been replaced
    stopifnot(!is.null(prcomp.result))

    # produce charts

    chart1 <- sapply(Chart1(control, prcomp.result$sdev), function(x) x)
    writeLines(text = chart1, con = my.control$path.out.chart1)
    print(chart1)


    # create all the component chart analyses
    ComponentChart <- function(component.num) {
        # create one component chart analysis
        chart2 <- Chart2(control, prcomp.result$rotation, component.num)
        writeLines(text = chart2, con = my.control$path.out.chart2[[component.num]])
        print(chart2)
        chart2
    }
    chart2s <- lapply(1:length(my.control$path.out.chart2), ComponentChart)


    print(prcomp.result)
    print(chart2s[[1]])  # check first two principal components
    print(chart2s[[2]])

}
Main <- function(control) {
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)

    PCACharts(control)

    str(control)
    if (control$testing)
        cat('TESTING: DISCARD RESULTS\n')
    
}

control <- Control()

#debug(Main)
Main(control)
cat('done\n')
