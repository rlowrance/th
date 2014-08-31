# deeds-al-sample.Rmd
# main program to create file OUTPUT/deeds-al-sample.RData, holding 
# a 1% random sample of OUTPUT/deeds-al.RData

source('DirectoryLog.R')
source('DirectoryWorking.R')
source('Libraries.R')
source('ReadDeedsAl.R')

# set the control variables
Control <- function() {
    # return list of control variables
    me <-'deeds-al-sample'
    
    log <- DirectoryLog()
    working <- DirectoryWorking()
    
    control <- list(
         path.in.parcels = paste0(working, 'deeds-al.RData')
        ,path.out = paste0(working, 'deeds-al-sample.RData')
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
    all <- ReadDeedsAl(control$path.in.parcels)
    
    # select the ones to retain
    is.retained <- ifelse( runif(nrow(all), min = 0, max = 1) <= control$fraction.retained
                          ,TRUE
                          ,FALSE
    )
    deeds.al <- all[is.retained,]
    
    Printf('Read %d sfr parcels; will retain %d of them\n', nrow(all), nrow(deeds.al))
    
    # write the subset
    # Note: use the same variable name as for the inputfile, so that
    # the function ReadParcelsSfr can find the variable
    save(control, deeds.al, file = control$path.out)

    # write control variables
    str(control)
    if (control$testing)
        cat('DISCARD OUTPUT: TESTING\n')
    
}

Main(Control())
cat('done\n')
