# e-features.R
# main program
# determine best features to use using PCA.
#
# Command line arguments: NONE

source('Directory.R')
source('Libraries.R')

source('Predictors.R')
source('ReadTransactionSplits.R')

library(optparse)

Control <- function() {
    me <- 'e-features-pca' 

    log <- Directory('log')
    splits <- Directory('splits')
    working <- Directory('working')

    # defines the splits that we use
    predictors <- Predictors('always.level')
    identification <- Predictors('identification')
    prices <- Predictors('prices')
    response <- 'price.log'

    formula <- Formula( predictors = predictors
                       ,response = response
                       )

    testing <- FALSE
    #testing <- TRUE

    out.base <-
        sprintf('%s'
                ,me
                )

    control <- list( path.in.splits = splits
                    ,path.out.log = paste0(log, out.base, '.log')
                    ,path.out.rdata = paste0(working, out.base, '.RData')
                    ,predictors = predictors
                    ,response = response
                    ,formula = formula
                    ,split.names = unique(c(predictors, identification, prices))
                    ,nfolds = 10
                    ,testing = testing
                    ,debug = FALSE
                    )
    control
}
PCAAnalysis <- function(control, data) {
    # use principle components analysis to determine most important features
    # ref: R Cookbook p. 338
    formula <- Formula( response = NULL
                       ,predictors = control$predictors
                       )
    # recode variables that are TRUE/FALSE to numeric
    if ('census.tract.has.industry' %in% control$predictors)
        data$census.tract.has.industry <- as.numeric(data$census.tract.has.industry)
    if ('census.tract.has.park' %in% control$predictors)
        data$census.tract.has.park <- as.numeric(data$census.tract.has.park)
    if ('census.tract.has.retail' %in% control$predictors)
        data$census.tract.has.retail <- as.numeric(data$census.tract.has.retail)
    if ('census.tract.has.school' %in% control$predictors)
        data$census.tract.has.school <- as.numeric(data$census.tract.has.school)

    if ('factor.is.new.construction' %in% control$predictors)
        data$factor.is.new.construction <- as.numeric(data$factor.is.new.construction)
    if ('factor.has.pool' %in% control$predictors)
        data$factor.has.pool <- as.numeric(data$factor.has.pool)

    if ('zip5.has.industry' %in% control$predictors)
        data$zip5.has.industry <- as.numeric(data$zip5.has.industry)
    if ('zip5.has.park' %in% control$predictors)
        data$zip5.has.park <- as.numeric(data$zip5.has.park)
    if ('zip5.has.retail' %in% control$predictors)
        data$zip5.has.retail <- as.numeric(data$zip5.has.retail)
    if ('zip5.has.school' %in% control$predictors)
        data$zip5.has.school <- as.numeric(data$zip5.has.school)
    
    prcomp.result <- prcomp( formula = formula
                            ,data = data
                            )
    print(prcomp.result)
    print(summary(prcomp.result))

    save( control
         ,prcomp.result
         ,file = control$path.out.rdata
         )
}
Main <- function(control, transaction.data.all.years) {
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)

    # use only transaction from 2003 on
    first.date <- as.Date('2003-01-01')
    is.starting.2003 <- transaction.data.all.years$saleDate >= first.date
    transaction.data <- transaction.data.all.years[is.starting.2003, ]

    PCAAnalysis(control, transaction.data)

    str(control)
    if (control$testing)
        cat('TESTING: DISCARD RESULTS\n')
    
}

############## EXECUTION START HERE

clock <- Clock()

control <- Control()


# cache transaction.data
if (!exists('e.features.pca.transaction.data')) {
    e.features.pca.transaction.data <- 
        ReadTransactionSplits( path.in.base = control$path.in.splits
                              ,split.names = control$split.names
                              )
}

Main(control, e.features.pca.transaction.data)
if (control$testing)
    cat('DISCARD RESULTS: TESTING\n')

Printf('took %f CPU minutes\n', clock$Cpu() / 60)
Printf('took %f wallclock minutes\n', clock$Wallclock() / 60)
Printf('finished at %s\n', as.character(Sys.time()))

cat('done\n')
