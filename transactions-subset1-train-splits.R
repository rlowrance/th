# main program to create directory WORKING/transactions-subset1-splits

source('Directory.R')
source('Libraries.R')

source('ReadTransactionsSubset1Train.R')

Control <- function() {
    me <- 'transactions-subset1-train-splits'

    log <- Directory('log')
    splits <- Directory('splits')
    working <- Directory('working')

    control <- 
        list( path.out.log = paste0(log, me, '.log')
             ,path.out.splits.dir = splits
             ,path.in = paste0(working, 'transactions-subset1-train.RData')
             ,testing = FALSE
             ,summarize = TRUE
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

HasFireplace <- function(FIREPLACE.INDICATOR.FLAG) {
    result <- as.factor(ifelse(is.na(FIREPLACE.INDICATOR.FLAG), FALSE, TRUE))
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
    # after converting integer YYYYMM00 to  YYYYMM15
    #cat('starting Int2Date', length(current.value), '\n'); browser()
    revised <- ifelse(current.value %% 100 == 0, current.value + 15, current.value)
    result <- as.Date(as.character(revised), format = '%Y%m%d')
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

    raw$total.assessment <- raw$IMPROVEMENT.VALUE.CALCULATED + raw$LAND.VALUE.CALCULATED

    AllNA <- function(new.name, Transform, current.name) {
        cat('starting AllNA', current.name, '\n')
        values <- raw[[current.name]]
        stopifnot(all(is.na(values)))
    }
    AllZero <- function(new.name, Transform, current.name) {
        cat('starting AllNA', current.name, '\n')
        values <- raw[[current.name]]
        stopifnot(all(values == 0))
    }
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

        if (control$summarize) 
            print(summary(data))

        data
    }
    SplitSummarize <- function(new.name, Transform, current.name) {
        data <- Split(new.name, Transform, current.name)
        print(summary(data))
        data
    }
    SplitSizeLog <- function(new.base.name, current.name) {
        Split(new.base.name, Identity, current.name)
        Split(paste0(new.base.name, '.centered'), Center, current.name)
        Split(paste0(new.base.name, '.centered.log'), CenterLog, current.name)
        Split(paste0(new.base.name, '.log'), Log, current.name)
    }
    SplitSizeLog1p <- function(new.base.name, current.name) {
        Split(new.base.name, Identity, current.name)
        Split(paste0(new.base.name, '.centered'), Center, current.name)
        Split(paste0(new.base.name, '.centered.log1p'), CenterLog1p, current.name)
        Split(paste0(new.base.name, '.log1p'), Log1p, current.name)
    }

    Split('apn', Identity, 'apn.recoded')

    Split('saleDate', as.Date, 'transaction.date')
    Split('sale.day', SplitDateDay, 'transaction.date')
    Split('sale.month', SplitDateMonth, 'transaction.date')
    Split('sale.year', SplitDateYear, 'transaction.date')

    Split('recordingDate', Int2Date, 'RECORDING.DATE')

    Split('price', Identity, 'SALE.AMOUNT')
    Split('price.log', Log, 'SALE.AMOUNT')

    SplitSizeLog('total.assessment', 'total.assessment')
    SplitSizeLog('land.square.footage', 'LAND.SQUARE.FOOTAGE')
    SplitSizeLog('living.area', 'LIVING.SQUARE.FEET')
    SplitSizeLog1p('bedrooms', 'BEDROOMS')
    SplitSizeLog1p('bathrooms', 'TOTAL.BATHS.CALCULATED')
    SplitSizeLog1p('parking.spaces', 'PARKING.SPACES')
    SplitSizeLog('land.value', 'LAND.VALUE.CALCULATED')
    SplitSizeLog('improvement.value', 'IMPROVEMENT.VALUE.CALCULATED')

    Split('factor.parking.type', Identity, 'PARKING.TYPE.CODE')
    Split('factor.has.pool', HasPool, 'POOL.FLAG')
    Split('factor.foundation.type', Identity, 'FOUNDATION.CODE')
    Split('factor.roof.type', Identity, 'ROOF.TYPE.CODE')
    Split('factor.heating.code', Identity, 'HEATING.CODE')
    Split('factor.is.new.construction', IsNewConstruction, 'RESALE.NEW.CONSTRUCTION.CODE')

    Split('avg.commute.time', Identity, 'avg.commute')
    Split('avg.commute.time.centered', Center, 'avg.commute')

    Split('fraction.owner.occupied', Identity, 'fraction.owner.occupied')
    Split('fraction.owner.occupied.centered.log', CenterLog, 'fraction.owner.occupied')
    Split('fraction.owner.occupied.centered', Center, 'fraction.owner.occupied')
    
    Split('median.household.income', Identity, 'median.household.income')
    Split('median.household.income.centered.log', CenterLog, 'median.household.income')
    Split('median.household.income.centered', Center, 'median.household.income')
    
    Split('year.built', Identity, 'YEAR.BUILT')
    Split('year.built.centered', Center, 'YEAR.BUILT')

    Split('latitude', Identity, 'G.LATITUDE')
    Split('latitude.centered', Center, 'G.LATITUDE')

    Split('longitude', Identity, 'G.LONGITUDE')
    Split('longitude.centered', Center, 'G.LONGITUDE')

    Split('fraction.improvement.value', Identity, 'fraction.improvement.value')
    Split('fraction.improvement.value.centered', Center, 'fraction.improvement.value')
    
    Split('census.tract.has.industry', Identity, 'census.tract.has.industry')
    Split('census.tract.has.park', Identity, 'census.tract.has.park')
    Split('census.tract.has.retail', Identity, 'census.tract.has.retail')
    Split('census.tract.has.school', Identity, 'census.tract.has.school')

    Split('zip5.has.industry', Identity, 'zip5.has.industry')
    Split('zip5.has.park', Identity, 'zip5.has.park')
    Split('zip5.has.retail', Identity, 'zip5.has.retail')
    Split('zip5.has.school', Identity, 'zip5.has.school')

    
    AllZero('ground.floor.square.feet', Identity, 'GROUND.FLOOR.SQUARE.FEET')
    Split('basement.square.feet', Identity, 'BASEMENT.SQUARE.FEET')
    Split('garage.parking.square.feet', Identity, 'GARAGE.PARKING.SQUARE.FEET')
    Split('effective.year.built', Identity, 'EFFECTIVE.YEAR.BUILT')
    Split('total.rooms', Identity, 'TOTAL.ROOMS')
    AllZero('bath.fixtures', Identity, 'BATH.FIXTURES')

    # construction factors
    Split('air.conditioning.code', Identity, 'AIR.CONDITIONING.CODE')
    AllNA('basement.finish.code', Identity, 'BASEMENT.FINISH.CODE')
    AllNA('bldg.code', Identity, 'BLDG.CODE')
    AllNA('bldg.improvement.code', Identity, 'BLDG.IMPROVEMENT.CODE')
    Split('condition.code', Identity, 'CONDITION.CODE')
    Split('construction.type.code', Identity, 'CONSTRUCTION.TYPE.CODE')
    Split('exterior.walls.code', Identity, 'EXTERIOR.WALLS.CODE')
    Split('fireplace.indicator.flag', HasFireplace, 'FIREPLACE.INDICATOR.FLAG')
    Split('fireplace.number', Identity, 'FIREPLACE.NUMBER')
    #Split('fireplace.type.code', Identity, 'FIREPLACE.TYPE.CODE')  # redundant with fireplace.indicator.code
    Split('foundation.code', Identity, 'FOUNDATION.CODE')
    #Split('floor.code', Identity, 'FLOOR.CODE')  # all A00 or NA
    AllNA('frame.code', Identity, 'FRAME.CODE')
    Split('garage.code', Identity, 'GARAGE.CODE')
    Split('heating.code', Identity, 'HEATING.CODE')
    AllNA('mobile.home.indicator.flag', Identity, 'MOBILE.HOME.INDICATOR.FLAG')
    Split('parking.type.code', Identity, 'PARKING.TYPE.CODE')
    Split('pool.code', Identity, 'POOL.CODE')
    Split('quality.code', Identity, 'QUALITY.CODE')
    Split('roof.cover.code', Identity, 'ROOF.COVER.CODE')
    AllNA('stories.code', Identity, 'STORIES.CODE')
    Split('stories.number', Identity, 'STORIES.NUMBER')
    AllNA('electric.energy.code', Identity, 'ELECTRIC.ENERGY.CODE')
    AllNA('fuel.code', Identity, 'FUEL.CODE')  # omitted: all NAs
    Split('sewer.code', Identity, 'SEWER.CODE')
    Split('water.code', Identity, 'WATER.CODE')
}

# EXECUTION STARTS HERE
control <- Control()
InitializeR(duplex.output.to = control$path.out.log)
str(control)
transactions.subset1.train <-
    if (exists('transactions.subset1.train')) {
        transactions.subset1.train
    } else {
        cat('reading transactions.subset1.train\n')
        #debug(ReadTransactionsAlSfrSubset1)
        ReadTransactionsSubset1Train(path = control$path.in)
    }



Main(control, transactions.subset1.train)

str(control)
cat('done\n')
