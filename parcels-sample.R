# parcels-sample.Rmd
# main program to create file OUTPUT/parcels-sample.RData, holding 
# a 1% random sample of the raw parcels

source('DirectoryLog.R')
source('DirectoryRaw.R')
source('DirectoryWorking.R')
source('Libraries.R')
source('ReadRawParcels.R')

# set the control variables
Control <- function() {
    # return list of control variables
    me <-'parcels-sample'
    
    log <- DirectoryLog()
    raw <- DirectoryRaw()
    working <- DirectoryWorking()
    
    control <- list(
         path.to.raw.directory = raw
        ,path.out.parcels = paste0(working, 'parcels-sample.RData')
        ,random.seed = 123
        ,fraction.retained = .01
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
    all <- ReadRawParcels( nrow = if (control$testing) 1000 else 0
                          ,path.to.raw.directory = control$path.to.raw.directory
                          ,verbose = TRUE
                          )
    
    # select the ones to retain
    is.retained <- ifelse( runif(nrow(all), min = 0, max = 1) <= control$fraction.retained
                          ,TRUE
                          ,FALSE
                          )
    parcels.sample <- all[is.retained,]
    
    Printf('Read %d sfr parcels; will retain %d of them\n', nrow(all), nrow(parcels.sample))
    
    # write the subset
    # Note: use the same variable name as for the inputfile, so that
    # the function ReadParcelsSfr can find the variable
    save(control, parcels.sample, file = control$path.out.parcels)

    # write control variables
    str(control)
    if (control$testing)
        cat('DISCARD OUTPUT: TESTING\n')
    
}

Main(Control())
cat('done\n')
