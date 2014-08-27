# deeds-al.Rmd
# main program to create file OUTPUT/deeds-al.RData, hold all features of arms-length deeds.
# Record layout for the input is in 1080_Record_layout.csv

source('Directory.R')
source('PRICATCODE.R')

library(devtools)
load_all(Directory('utilities'))
load_all(Directory('realestate'))

Control <- function() {
    # return list of values that control the script
    #cat('start Control\n'); browser()
    me <-'deeds-al'
    
    raw <- Directory('raw')
    working <- Directory('working')
    
    volume1 <- 'corelogic-deeds-090402_07/'
    volume2 <- 'corelogic-deeds-090402_09/'
    
    Deeds <- function(n) {
        # return path to deeds
        paste0(
             raw
            ,if (n <= 4) volume1 else volume2
            ,sprintf('CAC06037F%d.zip', n)
            )
    }
    
    control <- list(
         path.in.deeds = list(Deeds(1), Deeds(2), Deeds(3), Deeds(4), Deeds(5), Deeds(6), Deeds(7), Deeds(8))
        ,path.out.log = paste0(working, me, '.log')
        ,path.out.deeds = paste0(working, 'deeds-al.RData')
        ,testing = FALSE
        )
    control
}
ReadDeedsFile <- function(control, num) {
    # Return dataframe containing all deeds from input file num
    # ARGS:
    # num: number of input file; in {1, 2, ..., 8}

    # read a deeds file
    # Note: In file 5, data record 945 has an NA value for APN.FORMATTED
    #cat('start ReedDeedsFile', num, '\n'); browser()
    path <- control$path.in.deeds[[num]]
    len <- nchar(path)
    filename <- paste0(substr(path, len-13, len-4), '.txt')
    
    # NOTE: Don't convert strings to factors, because the other input
    # files may have different values than this file
    # read.table cannot read a zip file directly
    df <- read.table(
         file = unz(path, filename)
        ,header=TRUE
        ,sep="\t"
        ,quote=""
        ,comment.char=""
        ,stringsAsFactors=FALSE
        ,na.strings=""
        ,nrows=if(control$testing) 1000 else -1
        )
    
    Printf('Read %d observations\n from file %s\n in zip %s\n', nrow(df), filename, path)

    # track original source
    df$deed.file.number=rep(num, nrow(df))
    df$deed.record.number=1:nrow(df)
    
    df
}
ReadAll <- function(control) {
    # Return all the arms-length deeds files into one big data.frame
    # ARGS:
    # control : list of control values
    df <- NULL
    for (file.number in 1:8) {
        deeds<- ReadDeedsFile(control, file.number)
        df <- rbind(df, deeds)
        #cat('after file.number', file.number, '\n'); browser()
    }
    df
}
Main <- function(control) {
    #cat('start Main\n'); browser()
    
    # write control variables
    InitializeR(duplex.output.to = control$path.out.log)
    print(str(control))
    
    # Read all the deeds
    all <- ReadAll(control)
    
    # Retain only observations coded as arms-length
    is.arms.length <- PRICATCODE(all$PRI.CAT.CODE, 'arms.length.transaction')
    arms.length <- all[is.arms.length,]
    
    # count records
    nrow.all <- nrow(all)
    nrow.arms.length <- nrow(arms.length)
    
    Printf('Read %d deeds\n', nrow.all)
    Printf('Retained %d as arms-length\n', nrow.arms.length)
    print(str(arms.length))
    
    # Write RData
    save(arms.length, nrow.all, nrow.arms.length, file = control$path.out.deeds)


    # write control variables
    print(str(control))
    if (control$testing)
        cat('DISCARD OUTPUT: TESTING\n')
}

Main(Control())
cat('done\n')
