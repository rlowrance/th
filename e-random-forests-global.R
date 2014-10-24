# e-random-forests-global.R
# main program
# est gen error for a single global random forest model
# for Jan 2003 (simulate a local model)
#
# command line
# --hpset: chr in 'a', 'b': determines values for hyperparameters mtry and ntree
# --year : int; ex: 2003; the year for the test period
# --month: chr in 'jan', 'feb', ..., 'dec'; the month for the test period

source('Directory.R')
source('Libraries.R')

source('After2002.R')
source('Predictors.R')
source('PredictorsBest.R')
source('RandomForestsGlobalParseCommandArgs.R')
source('ReadTransactionSplits.R')

library(memoise)
library(optparse)
library(randomForest)

Control <- function(default.args) {
    opt <- RandomForestsGlobalParseCommandArgs(default.args)

    me <- 'e-random-forests-global' 

    log <- Directory('log')
    splits <- Directory('splits')
    working <- Directory('working')

    # define the splits that we use
    predictors <- PredictorsBest()
    identification <- Predictors('identification')
    prices <- Predictors('prices')
    response <- 'price.log'

    testing <- FALSE
    #testing <- TRUE

    out.base <-
        sprintf( '%s--hpset-%s--year-%d--month-%s'
                ,me
                ,opt$hpset
                ,opt$year
                ,opt$month
                )

    # set ranges for tunable parameters: mtry and num.trees
    # follow kuhn-13 starting p 198

    # mtry: number of randomly-selected trees at each split
    # Kuhn's suggestion: select 5 evenly-spaced values in [2,P] where p + # predictors
    # Breiman's suggestion: set mtry to P/3 (here = 23 /3 ~= 8)
    # my idea: use Breiman's P/3 for sure

    # num.trees: number of trees in each random forest
    # Kuhn's suggestion: start with 1,000 and increase until performance levels out
    # Note: Breiman (2001) proved that random forests is protected from over fitting
    # code documentation for randomForest() suggests having at least one tree for every 
    # training sample, so that each training data point gets used
    # Observation: fitting all the training samples with ntree == 1 take 3.7 CPU minutes
    # For now, set ntree = 1
    # My idea: follow Kuhn

    control <- list( path.in.splits = splits
                    ,path.out.log = paste0(log, out.base, '.log')
                    ,path.out.rdata = paste0(working, out.base, '.RData')
                    ,num.training.days = 90
                    ,response = response
                    ,predictors = predictors
                    ,split.names = unique(c(predictors, identification, prices))
                    ,nfolds = if (testing) 2 else 10
                    ,testing = testing
                    ,debug = FALSE
                    ,opt = opt
                    ,training.days = 90
                    ,verbose.CrossValidate = TRUE
                    )
    control
}
EvaluatePredictions <- function(prediction, actual) {
    # return list of evaluations
    # ARGS
    # prediction: vector of predictions or NA
    # actual    : vector of actual values

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
EvaluateHps <- function(mtry, ntree, data, is.testing, is.training, control) {
    # evaluate hyperparamters mtry and num.trees
    # reduce to call to ModelRandomForestLocal followed by call to evaluate preditions
    # determine which transactions will be the query transactions

    str(control$opt)  # print command line args to stdout

    # split data into training and testing
    in.training <- is.training & data$in.training.period
    data.training <- data[in.training,]

    in.testing <-is.testing & data$in.testing.period
    data.testing <- data[in.testing,]

    # determine query transactions
    # for now, test everthing because testing is cheap and fitting is very expensive
    num.test.transactions <- nrow(data.testing)
    which.test <- sample.int( n = nrow(data.testing)
                             ,size = num.test.transactions
                             ,replace = FALSE)
    queries <- data.testing[which.test,]
    Printf('will sample %d of %d test transactions\n', nrow(queries), nrow(data.testing))

    clock.fit <- Clock()

    # fit the model
    formula <- Formula( response = control$response
                       ,predictors = control$predictors
                       )

    Printf('about to fit randomForest; ntree %d mtry %d nrow %d\n', ntree, mtry, nrow(data.training))
    print(formula)
    fitted <- randomForest( formula = formula
                           ,data = data.training
                           ,ntree = ntree
                           ,mtry = mtry
                           )

    print(fitted)
    print(summary(fitted))

    Printf( 'cpu minutes to fit model (mtry %d ntree %d) = %f\n'
           ,mtry
           ,ntree
           ,clock.fit$Cpu() / 60
           )

    # predict all the selected queries
    clock.predict <- Clock() 
    num.queries <- nrow(queries)
    predictions.log  <- as.double(rep(NA, num.queries))
    num.training <- as.double(rep(NA, num.queries))
    for (query.index in 1:num.queries) {
        prediction <- predict( object = fitted
                                   ,newdata = queries[query.index,]
                                   )
        predictions.log[[query.index]] <- prediction
    }
    Printf( 'cpu minutes to predict %d queries = %f\n'
           ,num.queries
           ,clock.predict$Cpu() / 60
           )

    # evaluate fold accuracy
    predictions <- exp(predictions.log)
    evaluate <- EvaluatePredictions( prediction = exp(predictions.log)
                                    ,actual = queries$price
                                    )
    evaluate 
}
SelectTransactionData <- function(data, test.year, test.month, test.training.days) {
    # return data frame with data from the test month and 90 days prior to the test month
    # add columns $in.testing.period and $in.training.period
    month <- switch( test.month
                    ,jan = 1
                    ,feb = 2
                    ,mar = 3
                    ,apr = 4
                    ,may = 5
                    ,jun = 6
                    ,jul = 7
                    ,aug = 8
                    ,sep = 9
                    ,oct = 10
                    ,nov = 11
                    ,dec = 12
                    )
    char.date <- sprintf('%d-%02d-01', test.year, month)
    first.test.date <- as.Date(char.date)
    last.test.date <- first.test.date + 30
    first.training.date <- first.test.date - test.training.days

    is.good.date <-
        data$saleDate >= first.training.date &
        data$saleDate <= last.test.date

    data.subset <- data[is.good.date,]

    # add columns
    data.subset$in.testing.period <- 
        data.subset$saleDate >= first.test.date & 
        data.subset$saleDate <= last.test.date
    data.subset$in.training.period <- !data.subset$in.testing.period

    data.subset
}
Main <- function(control, transaction.data.all.years) {
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)

    transaction.data <- SelectTransactionData( data = transaction.data.all.years
                                              ,test.year = control$opt$year
                                              ,test.month = control$opt$month
                                              ,test.training.days = control$training.days
                                              )

    # set hyperpameter values to test
    mtry.breiman <- round(length(control$predictors) / 3)
    mtry.values <- switch( control$opt$hpset
                          ,test = c(mtry.breiman)
                          ,a = c( round(mtry.breiman) / 2
                                 ,mtry.breiman
                                 ,min(length(control$predictors), mtry.breiman * 2)
                                 )
                          ,b = 1
                          ,stop('bad opt$hpset')
                          )

    nrow.training <- sum(transaction.data$in.training)
    ntree.values <- switch( control$opt$hpset
                           ,test = c(10)
                           ,a = c(1, 10, 1000, nrow.training)
                           ,b = c(nrow.training, 2 * nrow.training, 4 * nrow.training)
                           ,stop('bad opt$hpset')
                           )

    # build list of hyperparameter sets
    str(mtry.values)
    str(ntree.values)
    model.hp <- NULL
    for (mtry in mtry.values) {
        for (ntree in ntree.values) {
            model.hp <- ListAppend( model.hp
                                   ,list( mtry = mtry
                                         ,ntree = ntree
                                         )
                                   )
        }
    }
    str(model.hp)

    EvaluateModel <- sapply( model.hp
                            ,function(hp) {
                                # return function that implements CrossValidation API
                                force(hp)
                                function(data, is.testing, is.training, control)
                                    EvaluateHps( hp$mtry
                                                ,hp$ntree
                                                ,data
                                                ,is.testing
                                                ,is.training
                                                ,control
                                                )
                            }
                            )
    model.name <- sapply( model.hp
                         ,function(hp) {
                             print(hp)
                             sprintf( 'mtry %d ntree %d'
                                     ,hp$mtry
                                     ,hp$ntree
                                     )
                         }
                         )
    str(model.name)



    #cat('about to call CrossValidate\n'); browser()
    cv.result <- CrossValidate( data = transaction.data
                               ,nfolds = control$nfolds
                               ,EvaluateModel = EvaluateModel
                               ,model.name = model.name
                               ,control = control
                               ,verbose = control$verbose.CrossValidate
                               )

    save( control
         ,cv.result
         ,model.name
         ,file = control$path.out.rdata
         )

    str(control)
    if (control$testing)
        cat('TESTING: DISCARD RESULTS\n')
}

clock <- Clock()
#debug(Control)

# set default args to desired values to run from the R prompt
just.testing <- FALSE
default.args <-
    if (just.testing) {
        list( hpset = 'test'
             ,year = 2003
             ,month = 'jan'
             )
    } else {
        list( hpset = 'a'
             ,year = 2003
             ,month = 'jan'
             )
    }

control <- Control(default.args)


# cache transaction.data
if (!exists('transaction.data')) {
    transaction.data <- ReadTransactionSplits( path.in.base = control$path.in.splits
                                              ,split.names = control$split.names
                                              )
}

Main(control, transaction.data)
if (control$testing)
    cat('DISCARD: testing\n')
Printf('Took %f CPU minutes\n', clock$Cpu() / 60)
cat('done\n')
