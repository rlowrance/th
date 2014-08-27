# parcels-sfr.Rmd
# main program to create file OUTPUT/parcels-sfr.RData, holding 
# all features of single-family residential parcles
# File layout is in 2580...
# NOTE: the features of parcels are in the taxroll files

source('Directory.R')
source('LUSEI.R')

library(devtools)
load_all(Directory('utilities'))
load_all(Directory('realestate'))

# set the control variables
Control <- function() {
    # return list of control variables
    me <-'parcels-sfr'
    
    raw <- Directory('raw')
    working <- Directory('working')
    
    volume1 <- 'corelogic-deeds-090402_07/'
    volume2 <- 'corelogic-deeds-090402_09/'
    
    Parcels <- function(n) {
        # return path to parcels
        paste0(
               raw
               ,'corelogic-taxrolls-090402_05/'
               ,sprintf('CAC06037F%d.zip', n)
               )
    }
    
    control <- list(
         path.in.parcels = list( Parcels(1)
                                ,Parcels(2)
                                ,Parcels(3)
                                ,Parcels(4)
                                ,Parcels(5)
                                ,Parcels(6)
                                ,Parcels(7)
                                ,Parcels(8)
                                )
        ,path.out.log = paste0(working, me, '.log')
        ,path.out.deeds = paste0(working, 'parcels-sfr.RData')
        ,testing = FALSE
        )
    control
}
ReadParcelsFile <- function(control, num) {
    # return dataframe containing all observations in a taxroll file
    #cat('start ReadParcelsFile', num, '\n'); browser()

    path <- control$path.in.parcels[[num]]
    len <- nchar(path)
    filename <- paste0(substr(path, len-13, len-4), '.txt')

    df <- read.table( file = unz(path, filename)
                     ,header=TRUE
                     ,sep="\t"
                     ,quote=""
                     ,comment=""
                     ,stringsAsFactors=FALSE
                     ,na.strings=""
                     ,nrows=if (control$testing) 1000 else -1
                     )
    
    Printf('Read %d observations\n from file %s\n in zip %s\n', nrow(df), filename, path)
    
    # track original source
    df$parcel.file.number <- rep(num,nrow(df))
    df$parcel.record.number <- 1:nrow(df)

    df
}
ReadAll <- function(control) {
    # Read all the parcels into one hug data.frame
    # ARGS: none
    # RETURNS: list
    # $df : data.frame with lots of rows
    # $num.dropped : number of non-single family residences found
    df <- NULL
    for (file.number in 1:8) {
        parcels <- ReadParcelsFile(control, file.number)
        df <- rbind(df, parcels)
    }
    df
}
Main <- function(control) {
    #cat('start Main\n'); browser()
    
    # write control variables
    InitializeR(duplex.output.to = control$path.out.log)
    print(str(control))

    # read all the parcels
    all <- ReadAll(control)
    cat('number of single-family residential parcels', nrow(all$df), '\n')
    cat('number of non SFR parcels', all$num.dropped, '\n')

    # Retain only observations coded as single-family residential
    is.sfr <- LUSEI(all$UNIVERSAL.LAND.USE.CODE, 'sfr')
    sfr <- all[is.sfr,]

    # count records
    nrow.all <- nrow(all)
    nrow.sfr <- nrow(sfr)
    
    Printf('Read %d deeds\n', nrow.all)
    Printf('Retained %d as single-family residential\n', nrow.sfr)
    print(str(sfr))
    
    # Write RData
    save(sfr, nrow.all, nrow.sfr, file = control$path.out.deeds)


    # write control variables
    print(str(control))
    if (control$testing)
        cat('DISCARD OUTPUT: TESTING\n')
    
}

Main(Control())
cat('done\n')
