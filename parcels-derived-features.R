# parcels-derived-features.R
# Create files OUTPUT/parcels-derived-features.RData

source('DirectoryLog.R')
source('DirectoryWorking.R')
source('EvaluateWithoutWarnings.R')
source('Libraries.R')
source('LUSEI.R')
source('PROPN.R')
source('ReadParcelsCoded.R')

Control <- function() {
    me <-'parcels-derived-features'
    
    log <- DirectoryLog()
    working <- DirectoryWorking()
    
    control <- list(
         path.in.parcels.coded = paste0(working, 'parcels-coded.RData')
        ,path.out.features = paste0(working, me, '.RData')
        ,path.out.log = paste0(log, me, '.log')
        ,testing = FALSE
        ,do.analysis = TRUE
        )
    control
}
Has <- function(location, indicator.column.name, parcels.coded) {
    # return TRUE iff for one of the locations in parcels.coded, the
    # value in parcels.coded$indicator.column.name is TRUE
    right.location <- location == parcels.coded$location
    sum(parcels.coded[right.location, indicator.column.name]) > 0
}
CreateLocationFeatures <- function(control, location.field.name, parcels.coded) {
    # return data.frame with these columns:
    # $location    : num, a zip5, zip9, or census tract
    # $has.industry: logi, true if any observation in $location[[i]] has $is.industry == TRUE
    #                TRUE iff exists at least one observation i in parcels.coded with
    #                parcels.coded$location[[i]] == result.location and
    #                parcels.coded$is.industry[[i]] =- TRUE
    #cat('starting CreateLocationFeatures\n'); browser()

    unique.locations <- unique(parcels.coded$location)
    features <- 
        data.frame( location = unique.locations
                   ,has.industry = vapply(unique.locations, Has, FALSE, 'is.industry', parcels.coded)
                   ,has.park = vapply(unique.locations, Has, FALSE, 'is.park', parcels.coded)
                   ,has.retail = vapply(unique.locations, Has, FALSE, 'is.retail', parcels.coded)
                   ,has.school = vapply(unique.locations, Has, FALSE, 'is.school', parcels.coded)
                   )
    features[[location.field.name]] <- features$location
    features$location <- NULL
    features
}
LocationDataframe <- function(IsValid, location.field.name, parcels.coded) {
    # return data frame containing valid locations and with a location field
    result <- data.frame( stringsAsFactors = FALSE
                         ,is.industry = parcels.coded$is.industry
                         ,is.park = parcels.coded$is.park
                         ,is.retail = parcels.coded$is.retail
                         ,is.school = parcels.coded$is.school
                         ,location = parcels.coded[[location.field.name]]
    )[IsValid(parcels.coded), ]
    result
}
FeaturesCensusTract <- function(control, parcels.coded) {
    cat('start FeaturesCensusTract\n')
    #browser()
    IsValid <- function(parcels.coded) {
        #cat('start FeaturesCensusTract::IsValid\n'); browser()
        # its not clear which census tracts are in Los Angeles Country
        # for now, accept all as valid
        rep(TRUE, nrow(parcels.coded))
    }
    locations <- LocationDataframe(IsValid, 'census.tract', parcels.coded)
    features <- CreateLocationFeatures(control, 'census.tract', locations)
}
FeaturesZip5 <- function(control, parcels.coded) {
    cat('start CreateZip5Features\n')
    #browser()
    IsValid <- function(parcels.coded) {
        #cat('start FeatureZip5::IsValid\n'); browser()
        zip <- parcels.coded$zip5
        result <- zip >= 90000 & zip <= 99999
        result
    }
    locations <- LocationDataframe(IsValid, 'zip5', parcels.coded)
    features <- CreateLocationFeatures(control, 'zip5', locations)
}
FeaturesZip9 <- function(control, parcels.coded) {
    cat('start FeaturesZip9\n');
    IsValid <- function(parcels.coded) {
        #cat('start FeaturesZip9::IsValid\n'); browser()
        zip <- parcels.coded$zip9
        result <- zip >= 900000000 & zip <= 999999999
        result
    }
    locations <- LocationDataframe(IsValid, 'zip9', parcels.coded)
    features <- CreateLocationFeatures(control, 'zip9', locations)
}
AnalyzeZip9OLD <- function(control, parcels.coded) {
    #cat('start AnalyzeZip9\n'); browser()
    # We don't create features for the 9-digit zipcode, as there are only about 5
    # parcels per 9-digit zip code
    parcels.coded <- na.omit(parcels.coded)
    n.unique.zip9 <- length(unique(parcels.coded$zip9))
    parcels.coded.per.zip9 <- nrow(parcels.coded) / n.unique.zip9
    cat('number of unique zip9 values', n.unique.zip9, '\n')
    cat('average parcels per zip9', parcels.coded.per.zip9, '\n')
    if (!control$testing) {
        # a testing subset may not satisfy this condition
        # but a production subset must satisfy this condition
        stopifnot(parcels.coded.per.zip9 < 5)
    }  
}
AnalyzeParcelCoded <- function(control, parcels.coded) {
    # print report to stdout showing frequncy of possible indicators
    #cat('starting AnalyzeParcelIndicators\n'); str(parcels.coded); browser()
    Printf('There are %d parcels, of which:\n', nrow(parcels.coded))
    lapply( c('is.industry', 'is.park', 'is.retail', 'is.school')
           ,function(is) Printf(' %7d %s\n', sum(parcels.coded[[is]]), is)
    )
    lapply( c('census.tract', 'zip5', 'zip9')
           ,function(location) Printf(' %7d have a %s\n', sum(!is.na(parcels.coded[[location]])), location)
    )
    
    NumPer <- function(field.name) {
        num.unique <- length(unique(parcels.coded[[field.name]]))
        avg.per.unique <- nrow(parcels.coded) / num.unique
        Printf('\n')
        Printf('There are %d unique %s values\n', num.unique, field.name)
        Printf('There are %f parcels on average per unique %s\n', avg.per.unique, field.name)
    }
    
    lapply(c('census.tract', 'zip5', 'zip9'), NumPer)
    
    NULL
}
IsValidZip5OLD <- function(x) {
    # return indicator vector TRUE iff x[[i]] is a valid zip5 zipcode
    is.valid <- !is.na(x) & x >= 90000 & x <= 999999
}
IsValidCensusTractOLD <- function(x) {
    # return indicator vector TRUE iff xx[[i]] is a valid census tract number
    is.valid <- !is.na(x)
}
ParcelsCodedSubset <- function (parcels.coded) {
    #cat('start ParcelsCodedSubset\n'); browser()
    
    # ZIPCODE is a character field that is sometimes missing (NA) and sometimes not a number
    parcels.coded.zip9 <- EvaluateWithoutWarnings(as.numeric(parcels.coded$ZIPCODE))
    parcels.coded.zip5 <- round(parcels.coded.zip9 / 10000)
    has.zipcode <- ifelse( is.na(parcels.coded.zip9) | is.na(parcels.coded.zip5)
                           ,FALSE
                           ,parcels.coded.zip5 >= 90000 & parcels.coded.zip5 <= 99999
    )
    has.census.tract <- !is.na(parcels.coded$CENSUS.TRACT)
    

    
    # construct subset where we know zip5, zip9, and census.tract
    parcels.coded.subset <- data.frame( stringsAsFactors = FALSE
                                        ,is.industry = parcels.coded$is.industry
                                        ,is.park = parcels.coded$is.park
                                        ,is.retail = parcels.coded$is.retail
                                        ,is.school = parcels.coded$is.school
                                        ,census.tract = parcels.coded$CENSUS.TRACT
                                        ,zip5 = parcels.coded.zip5
                                        ,zip9 = parcels.coded.zip9
    )[has.zipcode & has.census.tract,]
    
    str(parcels.coded.subset)
    print(summary(parcels.coded.subset))
    
    parcels.coded.subset
}
Main <- function(control, parcels.coded.subset) {
    #cat('starting Main\n'); browser()

    # write control variables
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)

    if (control$do.analysis) {
        AnalyzeParcelCoded(control, parcels.coded.subset)
    }

    # NOTE: if(TRUE/FALSE) used while debugging, as FeatureX is slow
    features.census.tract <- if (FALSE) FeaturesCensusTract(control, parcels.coded.subset)
    features.zip5 <- if (TRUE) FeaturesZip5(control, parcels.coded.subset)
    features.zip9 <- if (TRUE) FeaturesZip9(control, parcels.coded.subset)
    
    print(summary(features.zip5))
    print(summary(features.census.tract))

    save(control, features.census.tract, features.zip5, features.zip9, file = control$path.out.features)
    
    str(control)
    if (control$testing) cat('TESTING: DISCARD OUTPUT\n')
}

# process the data
control <- Control()
parcels.coded <- 
    if (exists('parcels.coded')) parcels.coded else ReadParcelsCoded(control$path.in.parcels.coded)
parcels.coded.subset <-
    if (exists('parcels.coded.subset')) parcels.coded.subset else ParcelsCodedSubset(parcels.coded)
Main(control, parcels.coded.subset)
cat('done\n')

