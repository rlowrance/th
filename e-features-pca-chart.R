# e-features-pca-chart.R
# main program
# Produce charts using as input file e-features-pcs--predictors-PREDICTORS.RData
# Write file
#  e-features-pcs-chart.1.txt
#
# Command line arguments
# -- predictors       : one of {chopra, all, always}
#                       chopra == 11 predictors based on Chopa's work
#                       all    == every predictors for which we have splits
#                       always == 25 predictors that are always present in every observation

source('Directory.R')
source('Libraries.R')

#source('ModelLinearLocal.R')
source('Predictors.R')
source('ReadTransactionSplits.R')

library(memoise)
library(optparse)

Control <- function(command.args) {
    # parse command line arguments in command.args
    opt <- ParseCommandArgs(command.args)

    me <- 'e-features-pca-chart' 

    log <- Directory('log')
    splits <- Directory('splits')
    working <- Directory('working')

    testing <- FALSE
    #testing <- TRUE
    out.base <-
        sprintf('%s--predictors-%s'
                ,me
                ,opt$predictors
                )
    path.out.chart <- sapply(1:10,  # create paths for 10 possible principal components
                             function(component.num) 
                                 paste0( working
                                        ,out.base
                                        ,sprintf('.%2d.txt', component.num)
                                        )
                             )
    in.file <-
        sprintf('e-features-pca--predictors-%s.RData',
                ,opt$predictors
                )


    control <- list( path.in = 
                    ,path.out.log = paste0(log, out.base, '.log')
                    ,path.out.rdata = paste0(working, out.base, '.RData')
                    ,path.out.chart = path.out.chart  # a vector of paths
                    ,testing = testing
                    ,debug = TRUE
                    )
    control
}
ParseCommandArgs <- function(command.args) {
    # return name list of values from the command args
    opt.predictors <- make_option( opt_str = c('--predictors')
                                  ,action = 'store'
                                  ,type = 'character'
                                  ,help = 'name of feature set to use'
                                  )
    option.list <- list( opt.predictors
                        )
    opt <- parse_args( object = OptionParser(option_list = option.list)
                      ,args = command.args
                      ,positional_arguments = FALSE
                      )
}
LCVChart1 <- function(control, cv.result, ordered.features) {
    Body <- function() {
        # return a vector lines, the body of chart 1
        #cat('Chart1Body\n'); browser()
        FeatureNames <- function() {
            c( 'Feature names ordered by importance'
              ,'1 == most important'
              ,sapply( 1:length(ordered.features)
                      ,function(index) 
                          sprintf(' %2d: %s'
                                  ,index
                                  ,ordered.features[[index]]
                                  )
                      )
              )
        }
        FeaturePerformance <- function() {
            RMedianSE <- function(model.index) {
                model.result <- cv.result[[model.index]]
                rMedianSE.values <- sapply( 1:length(model.result)
                                           ,function(fold.index) {
                                               evaluate <- model.result[[fold.index]]
                                               evaluate$rootMedianSquaredError
                                           }
                                           )
                result <- median(rMedianSE.values)
                result
            }
            sapply( 1:length(cv.result)
                   ,function(model.index) {
                       sprintf( ' RMedianSE for features 1 through %2d = %f'
                               ,model.index
                               ,RMedianSE(model.index)
                               )
                   }
                   )
        }
        result <- c( FeatureNames()
                    ,' '
                    ,FeaturePerformance()
                    )

    }
    Heading <- function() {
        description <- c(
                         sprintf( 'cv.result of %f Percent Random Sample of Training Transactions'
                                 ,control$query.sample * 100
                                 )
                         ,'AVM scenario'
                         ,'Log-linear form'
                         ,sprintf( 'Testing period: %s through %s'
                                  ,control$testing.period$first.date
                                  ,control$testing.period$last.date
                                  )
                         ,sprintf( 'Number of training days: %d'
                                  ,control$num.training.days
                                  )
                         )
    }
    chart1 <- c( Heading()
                ,' '
                ,Chart1Body(control, cv.result, ordered.features)
                )
}
PCAChart1 <- function(control, prcomp.result) {
    Body <- function() {
        browser()
        stop('write me')
    }
    Heading <- function() {
        c(
          sprintf('PCA results for %d features', length(control$predictors))
          )
    }
    chart <- c( Heading()
               ,' '
               ,Body()
               )
}
Evaluate <- function(prediction, actual) {
    # return list of evaluations
    # ARGS
    # prediction: vector of predictions or NA
    # actual    : vector of actual values
    #cat('start Evaluate\n'); browser()

    # most evaluations compare only where predictions are available
    is.prediction <- !is.na(prediction)
    num.predictions <- sum(is.prediction)
    price <- actual[is.prediction]
    coverage <- length(price) / length(actual)

    error <- prediction[is.prediction] - price
    error2 <- error * error
    root.mean.squared.error <- sqrt(mean(error2))
    root.median.squared.error <- sqrt(median(error2))
    abs.relative.error <- abs(error / price)
    mean.price <- mean(price)
    median.price <- median(price)

    Fraction <- function(max.abs.relative.error) {
        is.within.x.percent <- abs.relative.error <= max.abs.relative.error
        fraction.within.x.percent <- sum(is.within.x.percent) / num.predictions
    }
        
    result <- list( rootMeanSquaredError = root.mean.squared.error
                   ,rootMedianSquaredError = root.median.squared.error
                   ,coverage = coverage
                   ,fraction.within.10.percent = Fraction(.10)
                   ,fraction.within.20.percent = Fraction(.20)
                   ,fraction.within.30.percent = Fraction(.30)
                   ,mean.price = mean.price
                   ,median.price = median.price
                   )
    result
}
MakeX <- function(data, control) {
    # return matrix containing just the predictor columns

    predictor.df <- data[, control$predictors]

    # stop if any columns have zero variance
    lapply( control$predictors
           ,function(predictor) {
               values <- predictor.df[,predictor]
               standard.deviation <- sd(values, na.rm = TRUE)
               Printf( 'predictor %40s has %d NA values out of %d; sd %f\n'
                      ,predictor
                      ,sum(is.na(values))
                      ,length(values)
                      ,standard.deviation
                      )
#               if (predictor == 'fireplace.number' || predictor == 'fireplace.indicator.flag')
#                   browser()
               if (standard.deviation == 0)
                   stop(sprintf('predictor %s has zero standard deviation', predictor))
           }
           )

    # convert data frame to matrix
    result <- data.matrix(predictor.df)
    result
}
MakeY <- function(data, control) {
    result <- data[, control$response]
    result
}
ListRemoveNulls <-function(lst) {
    # return lst with NULL elements removed
    # ref: R Cookbook, 5.12 Removing NULL elements from a list
    is.null.element <- sapply(lst, is.null)
    lst[is.null.element] <- NULL
    lst
}
EvaluateFeatures <- function(features, data, is.testing, is.training, control) {
    # reduce to call to ModelLinearLocal followed by call to evaluate preditions
    # determine which transactions will be the query transactions

    data.training <- data[is.training,]
    InTraining <- function(saleDate) {
        # return indicated of data.training transactions within num.training.days of saleDate
        last.transaction.date <- saleDate - 1
        first.transaction.date <- last.transaction.date - control$num.training.days
        in.training <- 
            data.training$saleDate <= last.transaction.date &
            data.training$saleDate >= first.transaction.date
        in.training
    }

    # determine query transactions
    data.testing <- data[is.testing,]
    num.test.transactions <- round(nrow(data.testing) * control$query.fraction)
    which.test <- sample.int( n = nrow(data.testing)
                             ,size = num.test.transactions
                             ,replace = FALSE)
    queries <- data.testing[which.test,]
    Printf('will sample %d of %d test transactions\n', nrow(queries), nrow(data.testing))

    mll <- ModelLinearLocal( InTraining = InTraining
                            ,queries = queries
                            ,data.training = data.training
                            ,formula = Formula( response = control$response
                                               ,predictors = features
                                               )
                            )
    predictions <- exp(mll$predictions)    # possible NA
    num.training <- mll$num.training       # probably not used
    
    actuals <- queries$price
    evaluate <- Evaluate( prediction = predictions
                         ,actual = actuals)
    evaluate
}
LCVAnalysis <- function(control, transaction.data) {
    # perform lasso regression using elasticnet function enet and predict.enet
    #cat('Analysis\n'); browser()

    clock <- Clock()
    # use lasso to determine the order in which variables should enter the model
    fitted <- enet( x = MakeX(transaction.data, control)
                   ,y = MakeY(transaction.data, control)
                   ,lambda = 0  # just lasso, not also ridge regression
                   ,trace = TRUE
                   )
    ordered.features.with.nulls <- sapply(fitted$action, function(x) attributes(x)$names)
    ordered.features <- ListRemoveNulls(ordered.features.with.nulls)
    str(ordered.features)

    # use cross validation to select from the models

    EvaluateModel <- sapply( 1:length(ordered.features)
                            ,function(n) {
                                # return function that implements the CrossValidate API
                                force(n)
                                function(data, is.testing, is.training, control)
                                    EvaluateFeatures( ordered.features[1:n]
                                                     ,data
                                                     ,is.testing
                                                     ,is.training
                                                     ,control
                                                     )
                            }
                            )
    model.name <- sapply( 1:length(ordered.features)
                         ,function(n) 
                             sprintf('using %d best features', n)
                         )

    cv.result <- CrossValidate( data = transaction.data
                               ,nfolds = control$nfolds
                               ,EvaluateModel = EvaluateModel
                               ,model.name = model.name
                               ,control = control
                               ,verbose = TRUE
                               )

    str(cv.result)
    str(ordered.features)

    elapsed.cpu <- clock$Cpu()
    elapsed.message <- sprintf( 'predicting with %f query fraction took %f CPU seconds\n'
                              ,control$query.fraction
                              ,elapsed.cpu
                              )
    Printf(elapsed.message)

    save( control
         ,cv.result
         ,ordered.features
         ,elapsed.cpu
         ,elapsed.message
         ,file = control$path.out.rdata
         )
}
LCVCharts <- function(my.control) {
    # produce all the charts
    # for now, there is only one
    #cat('starting Charts\n'); browser()

    # recover cetain values from the predictions
    cv.result <- NULL
    loaded <- load(file = my.control$path.out.rdata)
    str(loaded)  # NOTE: control has been replaced
    stopifnot(!is.null(cv.result))
    stopifnot(!is.null(ordered.features))

    # produce charts
    # use the controls from when data were created
    chart1 <- LCVChart1(control, cv.result, ordered.features)
                            
    writeLines( text = chart1
               ,con = my.control$path.out.chart1
               )
    print(chart1)

    print(elapsed.message)  # put the elapsed time from the Analysis phase in the final version of the log

    # save results from the analysis pass and this pass
    save( control
         ,cv.result
         ,ordered.features
         ,elapsed.cpu
         ,elapsed.message
         ,chart1
         ,file = my.control$path.out.rdata
         )
}
LCV <- function(control, transaction.data) {
    # approach is lasso regression then cross validation on derived models
    switch( control$which
           ,analysis = LCVAnalysis(control, transaction.data)
           ,charts = LCVCharts(control)
           ,both = {
               LCVAnalysis(control, transaction.data)
               LCVCharts(control)
           }
           ,stop('bad control$which')
           )
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
PCACharts <- function(my.control) {
    # create all charts for PCA
    Chart1 <- function(control, sdev) {
        num.components <- length(sdev)
        variance <- sdev * sdev
        total.variance <- sum(variance)
        
        Body <- function() {
            lapply( 1:num.components
                   ,function(component.num) {
                       sum.variance <- sum(variance[1:component.num])
                       sprintf('Principal Component %2d  Variance %15.0f  Cum Fraction Total Variance %8.6f'
                               ,component.num
                               ,variance[[component.num]]
                               ,sum.variance / total.variance
                               )
                   }
                   )
        }

        lines <- c(
                   'PCA Principal Component Variances'
                   ,' '
                   ,Body()
                   )
        lines
    }
    Chart2 <- function(control, rotation, component.number) {
        Body <- function() {
            feature.name <- dimnames(rotation)[[1]]
            weight <- rotation[,component.number]
            sapply(1:length(feature.name)
                   ,function(feature.index) {
                       sprintf('Feature %30s  Weight %+10.6f'
                               ,feature.name[[feature.index]]
                               ,weight[[feature.index]]
                               )
                   }
                   )
        }
        lines <- c(
                   sprintf('PCA Rotation for Principal Component %2d', component.number)
                   ,' '
                   ,Body()
                   )
        lines
    }


    prcomp.result <- NULL
    loaded <- load(file = my.control$path.out.rdata)
    str(loaded)  # NOTE: control has been replaced
    stopifnot(!is.null(prcomp.result))

    # produce charts

    chart1 <- sapply(Chart1(control, prcomp.result$sdev), function(x) x)
    writeLines(text = chart1, con = my.control$path.out.chart1)
    print(chart1)


    # create all the component chart analyses
    ComponentChart <- function(component.num) {
        # create one component chart analysis
        chart2 <- Chart2(control, prcomp.result$rotation, component.num)
        writeLines(text = chart2, con = my.control$path.out.chart2[[component.num]])
        print(chart2)
        chart2
    }
    chart2s <- lapply(1:length(control$path.out.chart2), ComponentChart)


    print(prcomp.result)
    print(chart2s[[1]])  # check first two principal components
    print(chart2s[[2]])

    # append charts to previously-saved info
    save( chart1
         ,chart2s
         ,control
         ,prcomp.result
         ,file = control$path.out.rdata
         )
}
PCA <- function(control, transaction.data) {
    switch( control$which
           ,analysis = PCAAnalysis(control, transaction.data)
           ,charts = PCACharts(control)
           ,both = {
               PCAAnalysis(control, transaction.data)
               PCACharts(control)
           }
           ,stop('bad control$which')
           )
}
Main <- function(control, transaction.data) {
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)

    PCAAnalysis(control, transaction.data)

    str(control)
    if (control$testing)
        cat('TESTING: DISCARD RESULTS\n')
    
}

#debug(Control)
default.args <- NULL  # synthesize the command line that will be used in the Makefile
default.args <- list('--predictors', 'always')

command.args <- if (is.null(default.args)) commandArgs(trailingOnly = TRUE) else default.args
control <- Control(command.args)


# cache transaction.data
if (!exists('transaction.data')) {
    transaction.data <- ReadTransactionSplits( path.in.base = control$path.in.splits
                                              ,split.names = control$split.names
                                              )
}

#debug(Main)
Main(control, transaction.data)
cat('done\n')
