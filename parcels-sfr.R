# parcels-sfr.Rmd
# main program to create file OUTPUT/parcels-sfr.RData, holding 
# all features of single-family residential parcles
# File layout is in 2580...
# NOTE: the features of parcels are in the taxroll files

source('Directory.R')
source('LUSEI.R')
source('ReadRawParcels.R')

library(devtools)
load_all(Directory('utilities'))
load_all(Directory('realestate'))

# set the control variables
Control <- function() {
    # return list of control variables
    me <-'parcels-sfr'
    
    log <- Directory('log')
    raw <- Directory('raw')
    working <- Directory('working')
    
    control <- list(
         path.out.log = paste0(log, me, '.log')
        ,path.out.deeds = paste0(working, 'parcels-sfr.RData')
        ,path.to.raw.directory = raw
        ,testing = FALSE
        )
    control
}
Main <- function(control) {
    #cat('start Main\n'); browser()
    
    # write control variables
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)

    # read all the parcels
    all <- ReadRawParcels( nrows = if (control$testing) 1000 else -1
                          ,path.to.raw.directory = control$path.to.raw.directory
                          ,verbose = TRUE
                          )

    # Retain only observations coded as single-family residential
    is.sfr <- LUSEI(all$UNIVERSAL.LAND.USE.CODE, 'sfr')
    parcels.sfr <- all[is.sfr,]

    # count records
    nrow.all <- nrow(all)
    nrow.sfr <- nrow(parcels.sfr)
    
    Printf('Read %d deeds\n', nrow.all)
    Printf('Retained %d as single-family residential\n', nrow.sfr)
    str(parcels.sfr)
    
    # Write RData
    cat('writing output\n')
    save(parcels.sfr, nrow.all, nrow.sfr, file = control$path.out.deeds)


    # write control variables
    str(control)
    if (control$testing)
        cat('DISCARD OUTPUT: TESTING\n')
    
}

Main(Control())
cat('done\n')
