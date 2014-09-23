# e-avm-variants-synthetic-data.R
# Determine if avm with assessment outperforms avm without assessment for synthetic data
# Test cases designed around the relative accuracies of the market and assessment:
# - assessment is more accurate than the market
# - assessment is exactly as accurate as the market
# - assessment is less accurate than the market
# 
# Accuracy here means the size of the standard deviation of the noise applied to the
# true value, as defined by the arguments market.sd.fraction and assessment.sd.fraction
# to the function DataSynthetic.

source('DirectoryLog.R')
source('DirectorySplits.R')
source('DirectoryWorking.R')

source('Libraries.R')
source('ReadTransactionSplits.R')


Control <- function() {
    # capture values from the command line
    me <- 'e-avm-variants-synthetic-data' 

    log <- DirectoryLog()
    splits <- DirectorySplits()
    working <- DirectoryWorking()

    testing <- FALSE
    testing <- TRUE

    # original control variables
#    market.sd.fraction = .2
#    control <- list( response = 'price'
#                    ,path.out.log = paste0(path.output, me, '.log')
#                    ,path.out.save = paste0(path.output, me, '.rsave')
#                    ,obs.per.day = 10
#                    ,inflation.annual.rate = .10
#                    ,testing.period = list( first.date = as.Date('2008-01-01')
#                                           ,last.date = as.Date('2008-01-31')
#                                           )
#                    ,market.bias = 1
#                    ,market.sd.fraction = market.sd.fraction
#                    ,assessment.bias.names = c('zero', 'lower', 'higher')
#                    ,assessment.bias.values = list( lower = .8
#                                                   ,zero  = 1
#                                                   ,higher = 1.2
#                                                   )
#                    ,assessment.relative.sd.names = c('nearzero', 'lower', 'same', 'higher')
#                    ,assessment.relative.sd.values = list( nearzero = .01
#                                                          ,lower = .5 * market.sd.fraction
#                                                          ,same = market.sd.fraction
#                                                          ,higher = 2 * market.sd.fraction
#                                                          )
#                    ,num.training.days = 60
#                    ,random.seed = 123
#                    ,testing = FALSE
#                    )
    control <- list( response = 'price.log'
                    ,path.in.splits = splits
                    ,path.out.log = paste0(log, me, '.log')
                    ,path.out.rdata = paste0(working, me, '.RData')
                    ,path.out.chart1 = paste0(working, me, '.txt')
                    ,split.names = c( 'land.square.footage'
                                     ,'median.household.income'
                                     ,'price'
                                     )
                    ,random.seed = 123
                    ,num.samples = 10000   # sample drawn from actual variables
                    ,fraction.testing = .2
                    ,assessment.error.mean = 0
                    ,assessment.error.sd = 50000
                    ,error.price.fraction = .1
                    ,error.assessment.fraction = .2
                    ,testing = TRUE
                    ,debug = FALSE
                    )
    control
}


MakeSyntheticData <- function(assessment.bias.name, assessment.relative.sd.name, control) {
    # return synthetic data in a data.frame and also the coefficients used to generate that data
    #cat('start MakeSyntheticData', assessment.bias.name, assessment.relative.sd.name, '\n'); browser()

    assessment.bias <- control$assessment.bias.values[[assessment.bias.name]]
    assessment.ds.fraction <- control$assessment.relative.sd.values[[assessment.relative.sd.name]]

    ds <- DataSynthetic( obs.per.day = control$obs.per.day
                        ,inflation.annual.rate = control$inflation.annual.rate
                        ,first.date = as.Date('2007-01-01')
                        ,last.date = as.Date('2008-01-31')
                        ,market.bias = control$market.bias
                        ,market.sd.fraction = control$market.sd.fraction
                        ,assessment.bias = assessment.bias
                        ,assessment.sd.fraction = assessment.ds.fraction
                        )

    debugging <- FALSE
    if (debugging) {
        browser()
        dataframe <- ds$data

        Hash <- function(dataframe) {
            element.sum <- 0
            for (column.name in names(dataframe)) {
                print(column.name)
                element.sum <- element.sum + sum(as.numeric(dataframe[[column.name]]))
            }

            element.sum
        }

        cat('synthetic data hash value', Hash(dataframe), '\n')
        head(dataframe)
    }
    ds
}

TestScenarios <- function(data, actual.coefficients, control) {
    # return a data.frame containing the results of testing on each of the 4 scenarios
    # The test is to determine the error on the January transaction after training for 60 days
    # The model formula is price ~ <predictors>

    Scenario <- function(case.name) {
        # translate local scenario name into name used by MakeModelLinear
        switch( case.name
               ,'assessor' = 'assessor'
               ,'avm w/o assessment' = 'avm'
               ,'avm w/ assessment' = 'avm'
               ,'mortgage' = 'mortgage'
               )
    }

    Predictors <- function(case.name) {
        # return the predictors that we use for the specified scenario
        switch( case.name
               ,'assessor' =
               ,'avm w/o assessment' = c('land.size', 'latitude', 'has.pool')
               ,'avm w/ assessment' =
               ,'mortgage' = c('land.size', 'latitude', 'has.pool', 'assessment')
               ,stop('bad scenario')
               )
    }

    ExtractCoefficients <- function(cv.result) {
        # return coefficients in the fitted model
        #cat('start ExtractCoefficients\n'); browser()
        result <- cv.result$fitted$coefficients
        result
    }

    CheckCoefficients <- function(fitted.coefficients, actual.coefficients) {
        # error if the predicted coefficients are not sufficiently close to the actual coefficients
        #cat('start CheckCoefficients\n'); browser()
        verbose <- FALSE
        if (verbose) {
            print(fitted.coefficients)
            print(actual.coefficients)
        }
        for (name in names(fitted.coefficients)) {
            if (name != '(Intercept)') {
                actual <- switch( name
                                 ,'has.poolTRUE' = actual.coefficients$has.pool
                                 ,actual.coefficients[[name]]
                                 )
                fitted <- fitted.coefficients[[name]]
                relative.abs.error <- (abs(actual - fitted) / actual) 
                limit <- .4
                if (relative.abs.error > limit) {
                    print(fitted.coefficients)
                    print(actual.coefficients)
                    print(relative.abs.error)
                    print(name)
                }
                stopifnot(relative.abs.error <= limit)
            }
        }
    }

    # BODY START HERE
    #cat('start TestScenarios', nrow(data), '\n'); browser()


    all.data.indices <- rep(TRUE, nrow(data))
    all <- NULL
    for (case.name in c('assessor', 'avm w/o assessment', 'avm w/ assessment', 'mortgage')) {
        cat('scenario', case.name, '\n')
        #browser()
        CvModel <- MakeModelLinear( scenario = Scenario(case.name)
                                   ,response = 'price'
                                   ,predictors = Predictors(case.name)
                                   ,testing.period = list( first.date = as.Date('2008-01-01')
                                                          ,last.date = as.Date('2008-01-31')
                                                          )
                                   ,data = data
                                   ,num.training.days = 60
                                   ,verbose.model = FALSE
                                   )
        cv.result <- CvModel( data = data
                             ,training.indices = all.data.indices
                             ,testing.indices = all.data.indices
                             )
        if (FALSE && case.name == 'assessor') {
            # the coefficients should be close to what was used to generate the data
            CheckCoefficients(ExtractCoefficients(cv.result), actual.coefficients)
        }
        assess <- Assess(cv.result)
        next.row <- data.frame( stringsAsFactors = FALSE
                               ,scenario = case.name
                               ,rmse = assess$rmse
                               )
        all <- IfThenElse(is.null(all), next.row, rbind(all, next.row))
    }
    all
}

Experiment <- function(assessment.bias.name, assessment.relative.sd.name, control) {
    # return data frame with these columns
    # $scenario = name of scenario, in 'assessor', 'avm w/o assessment', 'avm w/ assessment', 'mortgage'
    # $rmse = error from a log-log model, trained for 60 days and testing on Jan 2008 data

    cat('start Experiment', assessment.bias.name, assessment.relative.sd.name, '\n')
    #browser()

    sd <- MakeSyntheticData(assessment.bias.name, assessment.relative.sd.name, control)
    df <- TestScenarios(sd$data, sd$coefficients)
    df # return data frame containing result of test, one row per test
}


Sweep <- function(f, assessment.bias.names, assessment.relative.sd.names, control) {
    # return data.frame containing a row for each element in cross product of list1 and list2
    # and scenario and rmse for that scenario on appropriate synthetic data
    #cat('Sweep\n'); browser()

    # build up a data.frame
    all <- NULL
    for (assessment.bias.name in assessment.bias.names) {
        for (assessment.relative.sd.name in assessment.relative.sd.names) {
            one <- f(assessment.bias.name, assessment.relative.sd.name, control)
            new.row <- data.frame( stringsAsFactors = FALSE
                                  ,assessment.bias.name = assessment.bias.name
                                  ,assessment.relative.sd.name = assessment.relative.sd.name
                                  ,scenario = one$scenario
                                  ,rmse = one$rmse
                                  )
            all <- if(is.null(all)) new.row else rbind(all, new.row)
            if (control$testing) break
        }
        if (control$testing) break
    }
    all
}
Generate <- function(control, transaction.data) {
    # return data frame with price, land, income
    # where land and income are drawn from the actual data
    # and price ~ land + income

    str(transaction.data)
    fitted <- lm( data = transaction.data
                 ,formula = price ~ land.square.footage + median.household.income
                 )
    print(fitted)
    str(fitted)
    print(summary(fitted))
    intercept <- fitted$coefficients[['(Intercept)']]
    coef.land<- fitted$coefficients[['land.square.footage']]
    coef.income <- fitted$coefficients[['median.household.income']]

    sampled <- list()
    num.samples <- control$num.samples
    sampled$land <- sample( x= transaction.data$land.square.footage
                           ,size = num.samples
                           ,replace = TRUE
                           )
    sampled$income <- sample( x= transaction.data$median.household.income
                             ,size = num.samples
                             ,replace = TRUE
                             )
    Price <- function(land, income) {
        price <- intercept + coef.land * land + coef.income * income
        price
    }
    prices <- mapply(Price, sampled$land, sampled$income)
    Printf('mean actual price %f\n', mean(transaction.data$price))
    Printf('mean synthetic price %f\n', mean(prices))
    assessment <- prices + rnorm( length(prices)
                                 ,mean = control$assessment.error.mean
                                 ,sd = control$assessment.error.sd
                                 )
    mean.price <- mean(prices)
    errors.assessment <- rnorm( n = length(prices)
                          ,mean = 0
                          ,sd = control$error.assessment.fraction * mean.price
                          )
    errors.price <- rnorm( n = length(prices)
                          ,mean = 0
                          ,sd = control$error.price.fraction * mean.price
                          )

    result <- data.frame( stringAsFactor = FALSE
                         ,price = prices + errors.price
                         ,land = sampled$land
                         ,income = sampled$income
                         ,assessment = prices + errors.assessment
                         )
    result
}

Split <- function(df, fraction.testing) {
    ran <- runif( n = nrow(df)
                 ,min = 0
                 ,max = 1
                 )
    is.testing <- sapply(ran, function(x) x <= fraction.testing)
    result <- list( train = df[!is.testing, ]
                   ,test  = df[is.testing, ]
                   )
    result
}

Main <- function(control, transaction.data) {
    InitializeR( duplex.output.to = control$path.out.log
                ,random.seed = control$random.seed
                )
    print(control)

    generated <- Generate(control, transaction.data)
    split <- Split(generated, control$fraction.testing)
    train <- split$train
    test <- split$test

    # train both models on training data
    browser()
    fitted.m1 <- lm( data = train
                    ,formula = price ~ land + income
                    )
    print(summary(fitted.m1))

    fitted.m2 <- lm( data = train
                    ,formula = price ~ land + income + assessment
                    )
    print(summary(fitted.m2))
    browser()

    # fit model







    # OLD BELOW ME
    stop('old code')
    result.df <- Sweep( f = Experiment
                       ,assessment.bias.names = control$assessment.bias.names
                       ,assessment.relative.sd.names = control$assessment.relative.sd.names
                       ,control = control
                       )


    #cat('main, after Sweep\n'); browser()
    save(control, result.df, file = control$path.out.save)

    # write a report to the console
    print(result.df)
#    report.lines <- EAvmVariantsSyntheticDataReport(result.df)
#    writeLines( text = report.lines
#               ,sep  = '\n'
#               )

    print(control)
}

control <- Control()

# cache transaction.data
if (!exists('transaction.data')) {
    transaction.data <- ReadTransactionSplits( path.in.base = control$path.in.splits
                                              ,split.names = control$split.names
                                              )
}

debug(Main)
Main(control, transaction.data)
cat('done\n')
