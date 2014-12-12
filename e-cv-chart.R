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
source('AbsoluteRelativeErrors.R')
source('Chart9And10.R')
source('Chart12LambdaValues.R')
source('CIChart.R')
source('CIMedian.R')
source('CITable.R')
source('CvCell.R')
source('DirectoryScan.R')
source('HeadersFixed.R')
source('Lines.R')
source('MeanMARE.R')
source('MeanWithin10.R')
source('MeanRMSE.R')
source('MedianMARE.R')
source('MedianRMSE.R')
source('Predictors2.R')
source('RootMeanSquaredErrors.R')
source('RootMedianSquaredErrors.R')
source('Table5And6Horizontal.R')
source('Table5And6Vertical.R')
#source('Table7AHorizontal.R')
#source('Table7BHorizontal.R')
source('Table7Metrics3.R')
source('Table7Metrics5.R')
#source('Table8Horizontal.R')
source('Table8Vertical.R')



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

    control <- 
        list( path.in.base = paste0(working, in.base)
             ,path.in.chart9.features = paste0(working, 'e-features-lcv2.txt')
             ,path.in.submarkets = paste0(working, 'submarkets.RData')
             ,path.out.log = paste0(log, out.base, '.log')
             ,path.out.makefile = paste0(me, '-generated.makefile')
             #,path.out.chart.1 = paste0(working, out.base, '_chart1.txt')
             #,path.out.chart.2 = paste0(working, out.base, '_chart2.txt')
             #,path.out.chart.3 = paste0(working, out.base, '_chart3.txt')
             #,path.out.chart.4 = paste0(working, out.base, '_chart4.txt')
             ,path.out.chart.5.horizontal.txt = paste0(working, out.base, '_chart5.txt')
             ,path.out.chart.5.vertical.txt = paste0(working, out.base, '_chart5_vertical.txt')

             ,path.out.chart.6.horizontal.txt = paste0(working, out.base, '_chart6.txt')
             ,path.out.chart.6.vertical.1.txt = paste0(working, out.base, '_chart6_vertical.txt')
             ,path.out.chart.6.vertical.100.txt = paste0(working, out.base, '_chart6_vertical100.txt')

             ,path.out.chart.7.txt3 = paste0(working, out.base, '_chart7_3.txt')
             ,path.out.chart.7.txt5 = paste0(working, out.base, '_chart7_5.txt')

             ,path.out.chart.8.txt = paste0(working, out.base, '_chart8.txt')
             
             ,path.out.chart.9.txt = paste0(working, out.base, '_chart9.txt')
             ,path.out.chart.9.gg1 = paste0(working, out.base, '_chart9_1.pdf')
             ,path.out.chart.9.gg2 = paste0(working, out.base, '_chart9_2.pdf')

             ,path.out.chart.10.txt = paste0(working, out.base, '_chart10.txt')
             ,path.out.chart.10.gg1.pdf = paste0(working, out.base, '_chart10_1.pdf')
             ,path.out.chart.10.gg2.pdf = paste0(working, out.base, '_chart10_2.pdf')

             ,path.out.chart.11.gg1 = paste0(working, out.base, '_chart11_1.pdf')
             ,path.out.chart.11.gg2 = paste0(working, out.base, '_chart11_2.pdf')
             
             ,path.out.chart.12.gg1 = paste0(working, out.base, '_chart12_1.pdf')
             ,path.out.chart.12.gg2 = paste0(working, out.base, '_chart12_2.pdf')
             ,path.out.chart.12.txt = paste0(working, out.base, '_chart12.txt')
            
             ,path.out.chart.13.indicators.medRMSE.txt = 
                paste0(working, out.base, '_chart13_indicators_medRMSE.txt')
             ,path.out.chart.13.indicators.medMARE.txt = 
                paste0(working, out.base, '_chart13_indicators_medMARE.txt')
             
             ,path.out.chart.13.submarkets.summary.medRMSE.txt = 
                paste0(working, out.base, '_chart13_submarkets_summary_medRMSE.txt')
             ,path.out.chart.13.submarkets.summary.medMARE.txt = 
                paste0(working, out.base, '_chart13_submarkets_summary_medMARE.txt')

             ,path.out.chart.13.submarkets.census.medRMSE.txt = 
                paste0(working, out.base, '_chart13_submarkets_census_medRMSE.txt')
             ,path.out.chart.13.submarkets.census.medMARE.txt = 
                paste0(working, out.base, '_chart13_submarkets_census_medMARE.txt')

             ,path.out.chart.13.submarkets.property.city.medRMSE.txt = 
                paste0(working, out.base, '_chart13_submarkets_property_city_medRMSE.txt')
             ,path.out.chart.13.submarkets.property.city.medMARE.txt = 
                paste0(working, out.base, '_chart13_submarkets_property_city_medMARE.txt')

             ,path.out.chart.13.submarkets.zip5.medRMSE.txt = 
                paste0(working, out.base, '_chart13_submarkets_zip5_medRMSE.txt')
             ,path.out.chart.13.submarkets.zip5.medMARE.txt = 
                paste0(working, out.base, '_chart13_submarkets_zip5_medMARE.txt')

             ,path.out.chart.13.submarkets.examples.census.medRMSE.txt=
                paste0(working, out.base, '_chart13_submarkets_examples_census_medRMSE.txt')
             ,path.out.chart.13.submarkets.examples.census.medMARE.txt=
                paste0(working, out.base, '_chart13_submarkets_examples_census_medMARE.txt')

             ,path.out.chart.13.submarkets.examples.property.city.medRMSE.txt =
                paste0(working, out.base, '_chart13_submarkets_examples_property_city_medRMSE.txt')
             ,path.out.chart.13.submarkets.examples.property.city.medMARE.txt =
                paste0(working, out.base, '_chart13_submarkets_examples_property_city_medMARE.txt')

             ,path.out.chart.13.submarkets.examples.zip5.medRMSE.txt =
                paste0(working, out.base, '_chart13_submarkets_examples_zip5_medRMSE.txt')
             ,path.out.chart.13.submarkets.examples.zip5.medMARE.txt =
                paste0(working, out.base, '_chart13_submarkets_examples_zip5_medMARE.txt')

             ,path.out.chart.14.1.30.txt = paste0(working, out.base, '_chart14_1_30.txt')
             ,path.out.chart.14.1.60.txt = paste0(working, out.base, '_chart14_1_60.txt')
             ,path.out.chart.14.5.30.txt = paste0(working, out.base, '_chart14_5_30.txt')
             ,path.out.chart.14.5.60.txt = paste0(working, out.base, '_chart14_5_60.txt')
             ,path.out.chart.14.survey.1.txt = paste0(working, out.base, '_chart14_survey_1.txt')
             ,path.out.chart.14.survey.5.txt = paste0(working, out.base, '_chart14_survey_5.txt')

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
               chart <- Chart5()$Chart(control)
               writeLines( text = chart$horizontal
                          ,con = control$path.out.chart.5.horizontal.txt
                          )
               writeLines( text = chart$vertical
                          ,con = control$path.out.chart.5.vertical.txt
                          )
           }
           ,'06' = {
               chart <- Chart6()$Chart(control)
               writeLines( text = chart$horizontal
                          ,con = control$path.out.chart.6.horizontal.txt
                          )
               writeLines( text = chart$vertical.1
                          ,con = control$path.out.chart.6.vertical.1.txt
                          )
               writeLines( text = chart$vertical.100
                          ,con = control$path.out.chart.6.vertical.100.txt
                          )
           }
           ,'07' = {
               chart <- Chart7()$Chart(control)
               writeLines( text = chart$txt3
                          ,con = control$path.out.chart.7.txt3
                          )
               writeLines( text = chart$txt5
                          ,con = control$path.out.chart.7.txt5
                          )
           }
           ,'08' = {
               chart <- Chart8()$Chart(control)
               writeLines( text = chart$txt
                          ,con = control$path.out.chart.8.txt
                          )
           }
           ,'09' = {
               # only produce charts for the 100% sample
               chart <- Chart9()$Chart(control)
               writeLines( text = chart$txt
                          ,con = control$path.out.chart.9.txt
                          )

               pdf( file = control$path.out.chart.9.gg1
                   ,width = control$chart.width
                   ,height = control$chart.height
                   )
               print(chart$gg1)
               dev.off()

               pdf( file = control$path.out.chart.9.gg2
                   ,width = control$chart.width
                   ,height = control$chart.height
                   )
               print(chart$gg2)
               dev.off()
           }
           ,'10' = {
               hart <- Chart10()$Chart(control)
               writeLines( text = chart$txt
                          ,con = control$path.out.chart.10.txt
                          )

               pdf( file = control$path.out.chart.10.gg1
                   ,width = control$chart.width
                   ,height = control$chart.height
                   )
               print(chart$gg1)
               dev.off()

               pdf( file = control$path.out.chart.10.gg2
                   ,width = control$chart.width
                   ,height = control$chart.height
                   )
               print(chart$gg2)
               dev.off()
           }
           ,'11' = {
               chart <- Chart11()$Chart(control)
               #    writeLines( text = chart.11$txt
               #               ,con = control$path.out.chart.11.txt
               #               )

               pdf( file = control$path.out.chart.11.gg1
                   ,width = control$chart.width
                   ,height = control$chart.height
                   )
               print(chart$gg1)
               dev.off()

               pdf( file = control$path.out.chart.11.gg2
                   ,width = control$chart.width
                   ,height = control$chart.height
                   )
               print(chart$gg2)
               dev.off()
           }
           ,'12' = {
               chart <- Chart12()$Chart(control)
               writeLines( text = chart$txt
                          ,con = control$path.out.chart.12.txt
                          )

               pdf( file = control$path.out.chart.12.gg1
                   ,width = control$chart.width
                   ,height = control$chart.height
                   )
               print(chart$gg1)
               dev.off()

               pdf( file = control$path.out.chart.12.gg2
                   ,width = control$chart.width
                   ,height = control$chart.height
                   )
               print(chart$gg2)
               dev.off()
           }
           ,'13' = {
               chart <- Chart13()$Chart(control)

               writeLines( text = chart$indicators.medRMSE
                          ,con = control$path.out.chart.13.indicators.medRMSE.txt
                          )
               writeLines( text = chart$indicators.medMARE
                          ,con = control$path.out.chart.13.indicators.medMARE.txt
                          )

               writeLines( text = chart$submarkets.summary.medRMSE
                          ,con = control$path.out.chart.13.submarkets.summary.medRMSE.txt
                          )
               writeLines( text = chart$submarkets.summary.medMARE
                          ,con = control$path.out.chart.13.submarkets.summary.medMARE.txt
                          )

               writeLines( text = chart$submarkets.census.medRMSE
                          ,con = control$path.out.chart.13.submarkets.census.medRMSE.txt
                          )
               writeLines( text = chart$submarkets.census.medMARE
                          ,con = control$path.out.chart.13.submarkets.census.medMARE.txt
                          )

               writeLines( text = chart$submarkets.property.city.medRMSE
                          ,con = control$path.out.chart.13.submarkets.property.city.medRMSE.txt
                          )
               writeLines( text = chart$submarkets.property.city.medMARE
                          ,con = control$path.out.chart.13.submarkets.property.city.medMARE.txt
                          )

               writeLines( text = chart$submarkets.zip5.medRMSE
                          ,con = control$path.out.chart.13.submarkets.zip5.medRMSE.txt
                          )
               writeLines( text = chart$submarkets.zip5.medMARE
                          ,con = control$path.out.chart.13.submarkets.zip5.medMARE.txt
                          )

               writeLines( text = chart$submarkets.examples.census.medRMSE
                          ,con = control$path.out.chart.13.submarkets.examples.census.medRMSE.txt
                          )
               writeLines( text = chart$submarkets.examples.census.medMARE
                          ,con = control$path.out.chart.13.submarkets.examples.census.medMARE.txt
                          )

               writeLines( text = chart$submarkets.examples.property.city.medRMSE
                          ,con = control$path.out.chart.13.submarkets.examples.property.city.medRMSE.txt
                          )
               writeLines( text = chart$submarkets.examples.property.city.medMARE
                          ,con = control$path.out.chart.13.submarkets.examples.property.city.medMARE.txt
                          )

               writeLines( text = chart$submarkets.examples.zip5.medRMSE
                          ,con = control$path.out.chart.13.submarkets.examples.zip5.medRMSE.txt
                          )
               writeLines( text = chart$submarkets.examples.zip5.medMARE
                          ,con = control$path.out.chart.13.submarkets.examples.zip5.medMARE.txt
                          )
           }
           ,'14' = {
               chart <- Chart14()$Chart(control)
               writeLines( text = chart$txt.1.30
                          ,con = control$path.out.chart.14.1.30.txt
                          )
               writeLines( text = chart$txt.1.60
                          ,con = control$path.out.chart.14.1.60.txt
                          )
               writeLines( text = chart$txt.5.30
                          ,con = control$path.out.chart.14.5.30.txt
                          )
               writeLines( text = chart$txt.5.60
                          ,con = control$path.out.chart.14.5.60.txt
                          )
               writeLines( text = chart$txt.1.survey
                          ,con = control$path.out.chart.14.survey.1.txt
                          )
               writeLines( text = chart$txt.5.survey
                          ,con = control$path.out.chart.14.survey.5.txt
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
