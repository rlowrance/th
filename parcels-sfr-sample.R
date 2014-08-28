# parcels-sfr-sample.Rmd
# main program to create file OUTPUT/parcels-sfr-sample.RData, holding 
# a 1% random sample of OUTPUT/parcels-sfr.RData

source('Directory.R')
source('ReadParcelsSfr.R')

library(devtools)
load_all(Directory('utilities'))
load_all(Directory('realestate'))

# set the control variables
Control <- function() {
    # return list of control variables
    me <-'parcels-sfr-sample'
    working <- Directory('working')
    
    control <- list(
         path.in.parcels = paste0(working, 'parcels-sfr.RData')
        ,path.out.parcels = paste0(working, 'parcels-sfr-sample.RData')
        ,random.seed = 123
        ,fraction.retained = .10
        ,path.out.log = paste0(log, me, '.log')
        ,testing = FALSE
        )
    control
}
Main <- function(control) {
    #cat('start Main\n'); browser()
    
    # write control variables
    InitializeR( random.seed = control$random.seed
                ,duplex.output.to = control$path.out.log
                )
    str(control)

    # read all the parcels
    all <- ReadParcelsSfr(control$path.in.parcels)
    
    # select the ones to retain
    is.retained <- ifelse( runif(nrow(all), min = 0, max = 1) <= control$fraction.retained
                          ,TRUE
                          ,FALSE
    )
    parcels.sfr <- all[is.retained,]
    
    Printf('Read %d sfr parcels; will retain %d of them\n', nrow(all), nrow(parcels.sfr))
    
    # write the subset
    # Note: use the same variable name as for the inputfile, so that
    # the function ReadParcelsSfr can find the variable
    save(control, parcels.sfr, file = control$path.out.parcels)

    # write control variables
    str(control)
    if (control$testing)
        cat('DISCARD OUTPUT: TESTING\n')
    
}

Main(Control())
cat('done\n')
