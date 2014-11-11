# submarkets.R
# main program to create file WORKING/submarkets.RData containing three vector of chr, each with
# the unique occurrences of the values
#  codes.census.tract
#  codes.city
#  codes.zip5

source('Directory.R')
source('Libraries.R')

source('Predictors2.R')
source('Lines.R')
source('ReadTransactionSplits.R')

Control <- function() {
    me <- 'submarkets'

    log <- Directory('log')
    splits <- Directory('splits')
    working <- Directory('working')

    identification <- Predictors2('identification')

    control <- list( path.in.splits = splits
                    ,path.out.log = paste0(log, me, '.log')
                    ,path.out.rdata = paste0(working, me, '.RData')
                    ,split.names = unique(identification)
                    ,testing = FALSE
                    ,debug = FALSE
                    ,me = me
                    )
    control
}
Codes.Number <- function(data, name) {
    values <- data[[name]]
    unique.values <- unique(values)

    lines <- Lines()
    lapply( unique.values
           ,function(u) {
               lines$Append(sprintf('%d', u))
               Printf('%s code %d occurs %d\n', name, u, sum(values == u))
           }
           )
    result <- lines$Get()
    result
}
Codes.String <- function(data, name) {
    values <- gsub( pattern = ' '
                   ,replacement = ''
                   ,data[[name]]
                   )
    unique.values <- unique(values)

    lines <- Lines()
    lapply( unique.values
           ,function(u) {
               lines$Append(sprintf('%s', u))
               Printf('%s code %25s occurs %d\n', name, u, sum(values == u))
           }
           )
    result <- lines$Get()
    result
}
Main <- function(control, data) {
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)

    codes.census.tract <- Codes.Number(data, 'census.tract')
    codes.property.city <- Codes.String(data, 'property.city')
    codes.zip5 <- Codes.Number(data, 'zip5')


    # verify that all the codes are different
    # since the codes are different, they can represent themselves in the file system
    i1 <- intersect(codes.census.tract, codes.property.city)
    i2 <- intersect(codes.census.tract, codes.zip5)
    i3 <- intersect(codes.property.city, codes.zip5)

    stopifnot(length(i1) == 0)
    stopifnot(length(i2) == 0)
    stopifnot(length(i3) == 0)

    save( control
         ,codes.census.tract
         ,codes.property.city
         ,codes.zip5
         ,file = control$path.out.rdata
         )
}

################## EXECUTION STARTS HERE
clock <- Clock()

control <- Control()

# cache transaction.data
if (!exists('submarkets.transaction.data')) {
    submarkets.transaction.data <- ReadTransactionSplits( path.in.base = control$path.in.splits
                                                         ,split.names = control$split.names
                                                         )
}

Main( control = control
     ,data = submarkets.transaction.data
     )

if (control$testing)
    cat('TESTING: DISCARD RESULTS\n')
Printf('took %f CPU minutes\n', clock$Cpu() / 60)
Printf('took %f wallclock minutes\n', clock$Wallclock() / 60)
Printf('finished at %s\n', as.character(Sys.time()))
cat('done\n')
