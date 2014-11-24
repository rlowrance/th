# e-cv-chart.R  
# main program
# Produce charts using input files e-cv_SCOPE_MODEL_TIMEPERIOD_SCENARIO_RESPONSE_PREDICTORSFORM_PREDICTORS_NAME_NDAYS_QUERY_C_NTREE_MTRY.RData
# output files have these names
# WORKING/e-cv-chart_SOMETHING.SUFFIX
# e-cv-chart-generated.makefile
#   describes all file dependencies for each chart
#
# Command line arguments:
# --chart NN: number of chart family to create, NN in {05, 06, ..., 14}
# --makefile: FLAG, if present only create file e-cv-chart.makefile

source('Directory.R')
source('Libraries.R')

# actual charts, some of which are deprecated and not copied in
#source('Chart_1_2.R')
#source('Chart_3.R')
#source('Chart_4.R')
source('Chart5.R')
source('Chart6.R')
source('Chart7.R')
source('Chart8.R')
source('Chart9.R')
source('Chart10.R')
source('Chart11.R')
source('Chart12.R')
source('Chart13.R')
source('Chart14.R')

# functions called by at least one of Chart5 .. Chart14
source('Chart9And10.R')
source('Chart12LambdaValues.R')
source('CIChart.R')
source('CIMedian.R')
source('CITable.R')
source('CvCell.R')
source('HeadersFixed.R')
source('Lines.R')
source('MeanWithin10.R')
source('MedianRMSE.R')
source('Predictors2.R')
source('RootMedianSquaredErrors.R')
source('Table5And6.R')
source('Table7A.R')
source('Table7B.R')
source('Table8.R')



library(boot)
library(ggplot2)
library(optparse)
library(memoise)

Control <- function(default.args) {
    # parse command line arguments in command.args
    opt <- ParseCommandArgs( command.args = commandArgs(trailingOnly = TRUE)
                            ,default.args = default.args
                            )

    me <- 'e-cv-chart' 

    log <- Directory('log')
    working <- Directory('working')
    cells <- paste0(working, 'e-cv-cells/')

    out.base <-
        sprintf('%s'
                ,me
                )
    in.base <- 
        sprintf('%s'
                ,'e-cv'
                )

    control <- list( path.in.base = paste0(working, in.base)
                    ,path.in.chart9.features = paste0(working, 'e-features-lcv2.txt')
                    ,path.in.submarkets = paste0(working, 'submarkets.RData')
                    ,path.out.log = paste0(log, out.base, '.log')
                    ,path.out.makefile = paste0(me, '-generated.makefile')
                    #,path.out.chart.1 = paste0(working, out.base, '_chart1.txt')
                    #,path.out.chart.2 = paste0(working, out.base, '_chart2.txt')
                    #,path.out.chart.3 = paste0(working, out.base, '_chart3.txt')
                    #,path.out.chart.4 = paste0(working, out.base, '_chart4.txt')
                    ,path.out.chart.5 = paste0(working, out.base, '_chart5.txt')
                    ,path.out.chart.6 = paste0(working, out.base, '_chart6.txt')
                    ,path.out.chart.7 = paste0(working, out.base, '_chart7.txt')
                    ,path.out.chart.8 = paste0(working, out.base, '_chart8.txt')
                    ,path.out.chart.9.txt = paste0(working, out.base, '_chart9.txt')
                    ,path.out.chart.9.gg1 = paste0(working, out.base, '_chart9_1.pdf')
                    ,path.out.chart.9.gg2 = paste0(working, out.base, '_chart9_2.pdf')
                    ,path.out.chart.10.txt = paste0(working, out.base, '_chart10.txt')
                    ,path.out.chart.10.gg1 = paste0(working, out.base, '_chart10_1.pdf')
                    ,path.out.chart.10.gg2 = paste0(working, out.base, '_chart10_2.pdf')
                    ,path.out.chart.11.gg1 = paste0(working, out.base, '_chart11_1.pdf')
                    ,path.out.chart.11.gg2 = paste0(working, out.base, '_chart11_2.pdf')
                    ,path.out.chart.12.gg1 = paste0(working, out.base, '_chart12_1.pdf')
                    ,path.out.chart.12.gg2 = paste0(working, out.base, '_chart12_2.pdf')
                    ,path.out.chart.12.txt = paste0(working, out.base, '_chart12_3.txt')
                    ,path.out.chart.13.indicators.txt = paste0(working, out.base, '_chart13_indicators.txt')
                    ,path.out.chart.13.submarkets.summary.txt = 
                        paste0(working, out.base, '_chart13_submarkets_summary.txt')
                    ,path.out.chart.13.submarkets.census.txt = 
                        paste0(working, out.base, '_chart13_submarkets_census.txt')
                    ,path.out.chart.13.submarkets.property.city.txt = 
                        paste0(working, out.base, '_chart13_submarkets_property_city.stxt')
                    ,path.out.chart.13.submarkets.zip5.txt = 
                        paste0(working, out.base, '_chart13_submarkets_zip5.txt')
                    ,path.out.chart.13.submarkets.examples.census.txt=
                        paste0(working, out.base, '_chart13_submarkets_examples_census.txt')
                    ,path.out.chart.13.submarkets.examples.property.city.txt =
                        paste0(working, out.base, '_chart13_submarkets_examples_property_city.txt')
                    ,path.out.chart.13.submarkets.examples.zip5.txt =
                        paste0(working, out.base, '_chart13_submarkets_examples_zip5.txt')

                    ,path.out.chart.14.txt   = paste0(working, out.base, '_chart14.txt')
                    ,path.out.chart.14.b.txt = paste0(working, out.base, '_chart14b.txt')
                    
                    ,path.cells = cells
                    ,chart.width = 14  # inches
                    ,chart.height = 10 # inches
                    ,working = working
                    ,testing = FALSE
                    ,debug = FALSE
                    ,me = me
                    ,opt = opt
                    )
    control
}
ParseCommandArgs <- function(command.args, default.args) {
    # return name list of values from the command args
    OptionChr <- function(name, help) {
        make_option( opt_str = c(sprintf('--%s', name))
                    ,action = 'store'
                    ,type = 'character'
                    ,default = default.args[[name]]
                    ,help = help
                    )
    }
    OptionInt <- function(name, help) {
        make_option( opt_str = c(sprintf('--%s', name))
                    ,action = 'store'
                    ,type = 'integer'
                    ,default = default.args[[name]]
                    ,help = help
                    )
    }

    option.list <-
        list( OptionChr('chart',          'one of {05, 06, ..., 14}')
             )

    opt <- parse_args( object = OptionParser(option_list = option.list)
                      ,args = command.args
                      ,positional_arguments = FALSE
                      )
    opt
}
MakeCharts <- function(control) {
    # write chart files:

    switch( control$opt$chart
           ,'05' = { 
               chart.5.txt <- Chart5(control)
               writeLines( text = chart.5.txt
                          ,con = control$path.out.chart.5
                          )
           }
           ,'06' = {
               chart.6.txt <- Chart6(control)
               writeLines( text = chart.6.txt
                          ,con = control$path.out.chart.6
                          )
           }
           ,'07' = {
               chart.7.txt <- Chart7(control)
               writeLines( text = chart.7.txt
                          ,con = control$path.out.chart.7
                          )
           }
           ,'08' = {
               chart.8.txt <- Chart8(control)
               writeLines( text = chart.8.txt
                          ,con = control$path.out.chart.8
                          )
           }
           ,'09' = {
               chart.9 <- Chart9(control)
               writeLines( text = chart.9$txt
                          ,con = control$path.out.chart.9.txt
                          )

               pdf( file = control$path.out.chart.9.gg1
                   ,width = control$chart.width
                   ,height = control$chart.height
                   )
               print(chart.9$gg1)
               dev.off()

               pdf( file = control$path.out.chart.9.gg2
                   ,width = control$chart.width
                   ,height = control$chart.height
                   )
               print(chart.9$gg2)
               dev.off()
           }
           ,'10' = {
               chart.10 <- Chart10(control)
               writeLines( text = chart.10$txt
                          ,con = control$path.out.chart.10.txt
                          )

               pdf( file = control$path.out.chart.10.gg1
                   ,width = control$chart.width
                   ,height = control$chart.height
                   )
               print(chart.10$gg1)
               dev.off()

               pdf( file = control$path.out.chart.10.gg2
                   ,width = control$chart.width
                   ,height = control$chart.height
                   )
               print(chart.10$gg2)
               dev.off()
           }
           ,'11' = {
               chart.11 <- Chart11(control)
               #    writeLines( text = chart.11$txt
               #               ,con = control$path.out.chart.11.txt
               #               )

               pdf( file = control$path.out.chart.11.gg1
                   ,width = control$chart.width
                   ,height = control$chart.height
                   )
               print(chart.11$gg1)
               dev.off()

               pdf( file = control$path.out.chart.11.gg2
                   ,width = control$chart.width
                   ,height = control$chart.height
                   )
               print(chart.11$gg2)
               dev.off()
           }
           ,'12' = {
               chart.12 <- Chart12(control)
               writeLines( text = chart.12$txt
                          ,con = control$path.out.chart.12.txt
                          )

               pdf( file = control$path.out.chart.12.gg1
                   ,width = control$chart.width
                   ,height = control$chart.height
                   )
               print(chart.12$gg1)
               dev.off()

               pdf( file = control$path.out.chart.12.gg2
                   ,width = control$chart.width
                   ,height = control$chart.height
                   )
               print(chart.12$gg2)
               dev.off()
           }
           ,'13' = {
               chart.13 <- Chart13(control)
               str(chart.13)
               writeLines( text = chart.13$indicators
                          ,con = control$path.out.chart.13.indicators.txt
                          )
               writeLines( text = chart.13$submarkets.summary
                          ,con = control$path.out.chart.13.submarkets.summary.txt
                          )
               writeLines( text = chart.13$submarkets.census
                          ,con = control$path.out.chart.13.submarkets.census.txt
                          )
               writeLines( text = chart.13$submarkets.property.city
                          ,con = control$path.out.chart.13.submarkets.property.city.txt
                          )
               writeLines( text = chart.13$submarkets.zip5
                          ,con = control$path.out.chart.13.submarkets.zip5.txt
                          )
               writeLines( text = chart.13$submarkets.examples.census
                          ,con = control$path.out.chart.13.submarkets.examples.census.txt
                          )
               writeLines( text = chart.13$submarkets.examples.property.city
                          ,con = control$path.out.chart.13.submarkets.examples.property.city.txt
                          )
               writeLines( text = chart.13$submarkets.examples.zip5
                          ,con = control$path.out.chart.13.submarkets.examples.zip5.txt
                          )
           }
           ,'14' = {
               chart.14 <- Chart14(control)
               writeLines( text = chart.14$a
                          ,con = control$path.out.chart.14.txt
                          )
               writeLines( text = chart.14$b
                          ,con = control$path.out.chart.14.b.txt
                          )
           }
           ,stop(paste0('bad control$opt$chart value: ', as.character(control$opt$chart)))
           )
}
Main <- function(control) {
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)

    MakeCharts(control)

    str(control)
}


### Execution starts here

default.args <- list(chart='14')

control <- Control(default.args)

Main(control)
if (control$testing)
    cat('DISCARD RESULTS: TESTING\n')
cat('done\n')
