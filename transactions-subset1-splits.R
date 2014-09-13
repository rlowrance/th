# main program to create directory WORKING/transactions-subset1-splits

source('DirectoryLog.R')
source('DirectorySplits.R')
source('DirectoryWorking.R')
source('Libraries.R')

source('ReadTransactionsSubset1.R')

Control <- function() {
    me <- 'transactions-subset1-splits'

    log <- DirectoryLog()
    splits <- DirectorySplits()
    working <- DirectoryWorking()

    control <- 
        list( path.out.log = paste0(log, me, '.log')
             ,path.out.splits.dir = DirectorySplits()
             ,path.in.transactions.subset1 = paste0(working, 'transactions-subset1.RData')
             ,testing = FALSE
             )
}

# transformers

CenterLog <- function(current.value) {
    #cat('starting CenterLog', length(current.value), '\n'); browser()
    result <- Center(log(current.value))
    result
}

CenterLog1p <- function(current.value) {
    #cat('starting CenterLog1p', length(current.value), '\n'); browser()
    result <- Center(log1p(current.value))
    result
}

HasPool <- function(POOL.FLAG) {
    #cat('starting HasPool\n'); browser()
    result <- as.factor(ifelse(is.na(POOL.FLAG), FALSE, POOL.FLAG == 'Y'))
    result
}

Identity <- function(current.value) {
    current.value
}

Int2Date <- function(current.value) {
    # convert integer YYYYYMMDD to a Date
    #cat('starting Int2Date', length(current.value), '\n'); browser()
    result <- as.Date(as.character(current.value), format = '%Y%m%d')
    result
}

IsNewConstruction <- function(current.value) {
    #cat('starting IsNewConstruction', length(current.value), '\n'); browser()
    result <- factor(current.value == 'N')
    result
}

Log <- function(current.value) {
    #cat('starting Log', length(current.value), '\n'); browser()
    result <- log(current.value)
    result
}

Log1p <- function(current.value) {
    #cat('starting Log1p', length(current.value), '\n'); browser()
    result <- log1p(current.value)
    result
}

SplitDateDay <- function(current.value) {
    #cat('starting SplitDateDay', length(current.value), '\n'); browser()
    result <- SplitDate(current.value)$day
    result
}

SplitDateMonth <- function(current.value) {
    #cat('starting SplitDateMonth', length(current.value), '\n'); browser()
    result <- SplitDate(current.value)$month
    result
}

SplitDateYear <- function(current.value) {
    #cat('starting SplitDateYear', length(current.value), '\n'); browser()
    result <- SplitDate(current.value)$year
    result
}


# Main program

Main <- function(control, raw) {
    #cat('start Main\n'); browser()

    # delete and create the splits directory
    rm.command <- paste('rm -rf', control$path.out.splits.dir)
    system.result <- system(rm.command)
    if (system.result != 0) {
        stop(sprintf('system command to remove directory resulted in %d', system.result))
    }
    directory.created <- dir.create(control$path.out.splits.dir)
    if (!directory.created) {
        stop(sprintf('directory %s not created\n', control$path.out.splits.dir))
    }

    # add derived features

    raw$fraction.improvement.value <- 
        (raw$IMPROVEMENT.VALUE.CALCULATED / 
         (raw$IMPROVEMENT.VALUE.CALCULATED + raw$LAND.VALUE.CALCULATED))

    Split <- function(new.name, Transform, current.name) {
        # create file new.name.RData with data.frame with one column new.name
        cat('starting Split', new.name, current.name, '\n')
        #browser()

        # build vector new.value with the transformed current value
        current.value <- raw[[current.name]]
        stopifnot(!is.null(current.value))
        new.value <- Transform(current.value)
        stopifnot(length(current.value) == length(new.value))
        
        # create a data frame
        data <- data.frame(new.value)
        colnames(data) <- new.name

        # write the data frame to the split file
        file <- sprintf('%s%s.RData', control$path.out.splits.dir, new.name)
        save( data  # other code expects the name to be "data"
             ,file = file
             )
    }

    Split('apn', Identity, 'apn.recoded')

    Split('saleDate', as.Date, 'transaction.date')
    Split('sale.day', SplitDateDay, 'transaction.date')
    Split('sale.month', SplitDateMonth, 'transaction.date')
    Split('sale.year', SplitDateYear, 'transaction.date')

    Split('recordingDate', Int2Date, 'RECORDING.DATE')

    Split('price', Identity, 'SALE.AMOUNT')
    Split('log.price', Log, 'SALE.AMOUNT')

    Split('land.square.footage', Identity, 'LAND.SQUARE.FOOTAGE')
    Split('log.land.square.footage', Log, 'LAND.SQUARE.FOOTAGE')
    Split('centered.log.land.square.footage', CenterLog, 'LAND.SQUARE.FOOTAGE')
    Split('centered.land.square.footage', Center, 'LAND.SQUARE.FOOTAGE')

    Split('living.area', Identity, 'LIVING.SQUARE.FEET')
    Split('log.living.area', Log, 'LIVING.SQUARE.FEET')
    Split('centered.log.living.area', CenterLog, 'LIVING.SQUARE.FEET')
    Split('centered.living.area', Center, 'LIVING.SQUARE.FEET')

    Split('bedrooms', Identity, 'BEDROOMS')
    Split('log1p.bedrooms', Log1p, 'BEDROOMS')
    Split('centered.log1p.bedrooms', CenterLog1p, 'BEDROOMS')
    Split('centered.bedrooms', Center, 'BEDROOMS')

    Split('bathrooms', Identity, 'TOTAL.BATHS.CALCULATED')
    Split('log1p.bathrooms', Log1p, 'TOTAL.BATHS.CALCULATED')
    Split('centered.log1p.bathrooms', CenterLog1p, 'TOTAL.BATHS.CALCULATED')
    Split('centered.bathrooms', Center, 'TOTAL.BATHS.CALCULATED')

    Split('parking.spaces', Identity, 'PARKING.SPACES')
    Split('log1p.parking.spaces', Log1p, 'PARKING.SPACES')
    Split('centered.log1p.parking.spaces', CenterLog1p, 'PARKING.SPACES')
    Split('centered.parking.spaces', Center, 'PARKING.SPACES')

    Split('land.value', Identity, 'LAND.VALUE.CALCULATED')
    Split('log.land.value', Log, 'LAND.VALUE.CALCULATED')
    Split('centered.log.land.value', CenterLog, 'LAND.VALUE.CALCULATED')
    Split('centered.land.value', Center, 'LAND.VALUE.CALCULATED')

    Split('improvement.value', Identity, 'IMPROVEMENT.VALUE.CALCULATED')
    Split('log.improvement.value', Log, 'IMPROVEMENT.VALUE.CALCULATED')
    Split('centered.log.improvement.value', CenterLog, 'IMPROVEMENT.VALUE.CALCULATED')
    Split('centered.improvement.value', Center, 'IMPROVEMENT.VALUE.CALCULATED')

    Split('factor.parking.type', Identity, 'PARKING.TYPE.CODE')
    Split('factor.has.pool', HasPool, 'POOL.FLAG')
    Split('factor.foundation.type', Identity, 'FOUNDATION.CODE')
    Split('factor.roof.type', Identity, 'ROOF.TYPE.CODE')
    Split('factor.heating.code', Identity, 'HEATING.CODE')
    Split('factor.is.new.construction', IsNewConstruction, 'RESALE.NEW.CONSTRUCTION.CODE')

    Split('avg.commute.time', Identity, 'avg.commute')
    Split('centered.avg.commute.time', Center, 'avg.commute')

    Split('fraction.owner.occupied', Identity, 'fraction.owner.occupied')
    Split('centered.log.fraction.owner.occupied', CenterLog, 'fraction.owner.occupied')
    Split('centered.fraction.owner.occupied', Center, 'fraction.owner.occupied')
    
    Split('median.household.income', Identity, 'median.household.income')
    Split('centered.log.median.household.income', CenterLog, 'median.household.income')
    Split('centered.median.household.income', Center, 'median.household.income')
    
    Split('year.built', Identity, 'YEAR.BUILT')
    Split('centered.year.built', Center, 'YEAR.BUILT')

    Split('latitude', Identity, 'G.LATITUDE')
    Split('centered.latitude', Center, 'G.LATITUDE')

    Split('longitude', Identity, 'G.LONGITUDE')
    Split('centered.longitude', Center, 'G.LONGITUDE')

    Split('fraction.improvement.value', Identity, 'fraction.improvement.value')
    Split('centered.fraction.improvement.value', Center, 'fraction.improvement.value')
    
    Split('census.tract.has.industry', Identity, 'census.tract.has.industry')
    Split('census.tract.has.park', Identity, 'census.tract.has.park')
    Split('census.tract.has.retail', Identity, 'census.tract.has.retail')
    Split('census.tract.has.school', Identity, 'census.tract.has.school')

    Split('zip5.has.industry', Identity, 'zip5.has.industry')
    Split('zip5.has.park', Identity, 'zip5.has.park')
    Split('zip5.has.retail', Identity, 'zip5.has.retail')
    Split('zip5.has.school', Identity, 'zip5.has.school')
}

# EXECUTION STARTS HERE
control <- Control()
InitializeR(duplex.output.to = control$path.out.log)
str(control)
transactions.subset1 <-
    if (exists('transactions.subset1')) {
        transactions.subset1
    } else {
        cat('reading transactions.subset1\n')
        #debug(ReadTransactionsAlSfrSubset1)
        ReadTransactionsSubset1(path = control$path.in.transactions.subset1)
    }


Main(control, transactions.subset1)

str(control)
cat('done\n')
