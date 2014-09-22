# e-avm-variants.R
# main program to produce file WORKING/e-avm-variants.RData and *.txt
# issue resolved: The AVM model performs better than the assessor model. How much
# of the better performance is caused by using the assessment?
# Approach: Use k-fold cross validation to compare estimated generalization errors for
# model variants

source('DirectoryLog.R')
source('DirectorySplits.R')
source('DirectoryWorking.R')

source('Libraries.R')
source('ReadTransactionSplits.R')

library(memoise)

Control <- function(parsed.command.args) {
    # capture values from the command line
    num.training.days <- as.integer(parsed.command.args$training)

    me <- 'e-avm-variants' 
    out.base <- sprintf( '%s-training-%d'
                        ,me
                        ,num.training.days
                        )

    log <- DirectoryLog()
    splits <- DirectorySplits()
    working <- DirectoryWorking()

    # defines the splits that we use
    predictor.names = c(# continuous size positive
                        'land.square.footage'
                        ,'living.area'
                        # continuous size nonnegative
                        ,'bedrooms'
                        ,'bathrooms'
                        ,'parking.spaces'
                        # continuous non size
                        ,'median.household.income'
                        ,'year.built'
                        ,'fraction.owner.occupied'
                        ,'avg.commute.time'
                        # discrete
                        ,'factor.is.new.construction'
                        ,'factor.has.pool'
                        )
#    assessment.names = c( 'improvement.value'
#                         ,'land.value'
#                         ,'fraction.improvement.value'
#                         )
    assessment.names = c('total.assessment')
    other.names = c(# dates
                    'saleDate'
                    ,'recordingDate'
                    # prices
                    ,'price'
                    ,'price.log'
                    # apn
                    ,'apn'
                    )
    testing <- FALSE
    #testing <- TRUE
    control <- list( response = 'price.log'
                    ,path.in.splits = splits
                    ,path.out.log = paste0(log, out.base, '.log')
                    ,path.out.rdata = paste0(working, out.base, '.RData')
                    ,path.out.chart1 = paste0(working, out.base, '.txt')
                    ,predictors.without.assessment = c(predictor.names)
                    ,predictors.with.assessment = c( predictor.names
                                                    ,assessment.names)
                    ,response = 'price.log'
                    ,split.names = c( predictor.names
                                     ,assessment.names
                                     ,other.names)
                    ,nfolds = if (testing) 2 else 10
                    ,testing.period = list( first.date = as.Date('2008-11-01')
                                           ,last.date = as.Date('2009-03-31')
                                           )
                    ,num.training.days = num.training.days
                    ,chart1.format.header = '%27s | %20s %20s'
                    ,chart1.format.data =   '%27s | %20.0f %20.0f'
                    ,rich = 2.0
                    ,poor = 0.5
                    ,testing = testing
                    ,debug = FALSE
                    ,also.strata = FALSE
                    )
    control
}

CreateChart1Body <- function(control, gen.error) {
    # return a vector lines, the body of chart 1
    result <- sprintf(control$chart1.format.header, 'scenario', 'mean RMSE', 'median RMedianSE')

    for (row.index in 1:length(gen.error)) {
        name <- names(gen.error)[[row.index]]
        mean.RMSE <- gen.error[[row.index]]$mean.RMSE
        median.RMedianSE <- gen.error[[row.index]]$median.RMedianSE
        result <- c( result
                    ,sprintf( control$chart1.format.data
                             ,name
                             ,mean.RMSE
                             ,median.RMedianSE
                             )
                    )
    }

    result
}

CreateChart1 <- function(control, description, gen.error) {
    # return a vector of lines, the txt for chart 1
    #cat('start CreateChart1\n'); browser()

    result <- c( description
                ,' '
                ,CreateChart1Body(control, gen.error)
                )

    result
}


EstimateGeneralizationError <- function(cv.result) {
    # convert cross validation result into an estimate of the generalization error
    EstGenErrorForModel <- function(cv.result, model.name) {
        # esimate the generalization error for a model
        #cat('start EstGenError', model.name, '\n'); browser()
        this.result <- cv.result[[model.name]]

        Mean <- function(field.name) {
            elements <- sapply( this.result
                               ,function(x) x[[field.name]]
                               )
            result <- mean(elements)
            result
        }
        Median <- function(field.name) {
            elements <- sapply( this.result
                               ,function(x) x[[field.name]]
                               )
            result <- median(elements)
            result
        }
        Coverage <- function(field.name) {
            elements <- sapply( this.result
                               ,function(x) x[[field.name]]
                               )
            result <- mean(elements)
            result
        }

        est.gen.error <- list( mean.RMSE = Mean( 'rootMeanSquaredError')
                              ,median.RMedianSE = Median('rootMedianSquaredError')
                              ,mean.coverage = Coverage('coverage')
                              )
        est.gen.error
    }
    # for each model
    gen.error <- list()
    model.names <- names(cv.result)
    str(model.names)
    for (model.name in model.names) {
        gen.error[[model.name]] <- EstGenErrorForModel(cv.result, model.name)
    }
    gen.error
}

Stratify <- function(control, data){
    # break data.frame into MECE subsets (stratum)
    # for now, criteria is median.household.income and the 3 strata are wealth, poor, middle class
    # Returns list with these elements
    # $data : list of strata, each a data.frame
    # $name : chr vector of names for the corresponding strata

    # determine where to split
    #print(summary(data))
    median.median.household.income <- median(data$median.household.income)
    is.rich <- data$median.household.income > control$rich * median.median.household.income
    is.poor <- data$median.household.income < control$poor * median.median.household.income
    is.middle <- (!is.rich) & (!is.poor)

    result <- list( data = list( data[is.rich,]
                                ,data[is.poor,]
                                ,data[is.middle,]
                                )
                   ,name = list( ' wealthy neighborhoods'
                                ,'poor neighborhoods'
                                ,'middle class neighborhoods'
                                )
                   )
    result
}

ModelLinearLocal <- function(InTraining, queries, data.training, formula, num.training.days) {
    # return result of local linear regression
    # return a list $ok $result
    # if ($ok == true), the model was fit on each query point
    #   $result = a vector of predictions for the queries from a model trained on data.training
    # if ($ok == false), the model could not be fit on each query point
    #   $result is a list
    #      $query: query (in queries) for which model could not be fit
    #      $problem: char, description of problem
    #   
    # return vector of predictions from local assssor model trained for each query
    #cat('start ModelLinearLocal\n'); browser()
    verbose <- TRUE
    verbose <- FALSE

    TestForNoContrasts <- function(data, feature.name) {
        # this tests works only for factors with level FALSE and TRUE

        if (is.null(data[[feature.name]])) return(list(ok = TRUE))
        f <- data[[feature.name]]
        stopifnot(is.factor(f))
        my.levels <- levels(f)
        stopifnot(TRUE %in% my.levels)
        stopifnot(FALSE %in% my.levels)
        num.true <- sum(f == TRUE)
        if (num.true == length(f)) {
            return(list( ok = FALSE
                        ,problem = sprintf('factor %s is always TRUE', feature.name)
                        )

            )
        }
        if (num.true == 0) {
            return(list( ok = FALSE
                        ,problem = sprintf('factor %s is never TRUE', feature.name)
                        )

            )
        }
        return(list(ok = TRUE))
    }

    TestForInsufficientObservations <- function(data, formula) {
        # determine whether there are more observations the predictors
        #cat('start TestForInsufficentObservations\n'); browser()
        num.predictors <- length(labels(terms(formula)))
        num.observations <- nrow(data)
        if (num.observations < num.predictors) {
            return(list( ok = FALSE
                        ,problem = sprintf( '%d predictors, %d observations'
                                           ,num.predictors
                                           ,num.observations
                                           )

                        )
            )
        }
        return(list(ok = TRUE))
    }

    Fit <- function(saleDate) {
        # return list, either
        # $ok = TRUE, $fitted = lm object
        # $ok = FALSE, $feature = char, $problem = char
        #cat('start Fit', saleDate, '\n'); browser()
        in.training <- InTraining(saleDate)
        data <- data.training[in.training, ]

        # make sure the lm (below) is going to run
        test1 <- TestForNoContrasts(data, 'factor.is.new.construction')
        if (!test1$ok) return(test)

        test2 <- TestForNoContrasts(data, 'factor.has.pool')
        if (!test2$ok) return(test2)

        test3 <- TestForInsufficientObservations(data, formula)
        if (!test3$ok) return(test3)
        
        if (verbose) {
            print(formula)
            str(data)
            Printf('sum is new contruction %d\n', sum(data$factor.is.new.construction == TRUE))
            Printf('sum has pool  %d\n', sum(data$factor.has.pool == TRUE))
        }

        fitted <- lm( formula = formula
                     ,data = data
                     )
        return <- list(ok = TRUE, fitted = fitted)
    }

    FitMemoised <- memoise(Fit)

    FitPredict <- function(query.index) {
        # Fit only model and predict using it
        #cat('start FitPredict', query.index, '\n'); browser()
        saleDate <- queries$saleDate[[query.index]]

        fitted <- FitMemoised(saleDate)
        if (!fitted$ok) {
            return(list(ok = FALSE
                        ,problem = fitted$problem
                        ,query = queries[query.index,]
                        )
            )
        }
        prediction <- predict( object = fitted$fitted
                              ,newdata = queries[query.index, ]
                              )
        list(ok = TRUE, prediction = prediction)
    }

    # BODY STARTS HERE
    predictions <- NULL
    for(query.index in 1:nrow(queries)) {
        fit.predict <- FitPredict(query.index)
        if (!fit.predict$ok) {
            return(fit.predict)
        }
        predictions <- c(predictions, fit.predict$prediction)
    }

    predictions <- as.vector(predictions)
    list(ok = TRUE, predictions = predictions)
}

ModelAssessorLinearLocal <- function(queries, data.training, formula, num.training.days) {
    # return vector of predictions from local assssor model trained for each query
    #cat('start ModelAssessorLinearLocal\n'); browser()
    InTraining <- function(saleDate) {
        # return selector vector to identify training samples for the saleDate
        in.training <- data.training$recordingDate >= (saleDate - num.training.days) &
                       data.training$recordingDate <= (saleDate - 1)

    }
    ModelLinearLocal( InTraining
                     ,queries
                     ,data.training
                     ,formula
                     ,num.training.days
                     )
}
ModelAvmLinearLocal <- function(queries, data.training, formula, num.training.days) {
    # return vector of predictions from local assssor model trained for each query
    #cat('start ModelAvmLinearLocal\n'); browser()
    InTraining <- function(saleDate) {
        # return selector vector to identify training samples for the saleDate
        in.training <- data.training$saleDate >= (saleDate - num.training.days) &
                       data.training$saleDate <= (saleDate - 1)

    }
    ModelLinearLocal( InTraining
                     ,queries
                     ,data.training
                     ,formula
                     ,num.training.days
                     )
}
ModelMortgageLinearLocal <- function(queries, data.training, formula, num.training.days) {
    # return vector of predictions from local assssor model trained for each query
    #cat('start ModelMortgageLinearLocal\n'); browser()
    debug <- TRUE
    debug <- FALSE
    InTraining <- function(saleDate) {
        # return selector vector to identify training samples for the saleDate
        days.around <- num.training.days / 2
        in.training <- data.training$saleDate >= (saleDate - days.around) &
                       data.training$saleDate <= (saleDate + days.around)
        if (debug) {
            cat( 'debug ModelMortgageLinearLocal'
                ,'saleDate', as.character(as.Date(saleDate, origin = '1970-01-01'))
                ,'num.training.days', num.training.days
                ,'days.around', days.around
                ,'number training samples', sum(in.training)
                ,'\n'
                )
            if (sum(in.training) <= 2) browser()
        }
        in.training
    }
    ModelLinearLocal( InTraining
                     ,queries
                     ,data.training
                     ,formula
                     ,num.training.days
                     )
}
Queries <- function(data, control) {
    # return dataframe of queries from the testing data
    #cat('start Queries\n'); browser()
    ok.testing.first.date <- data$saleDate >=  control$testing.period$first.date
    ok.testing.last.date <- data$saleDate <= control$testing.period$last.date
    is.query <- ok.testing.first.date & ok.testing.last.date
    queries <- data[is.query, ]
}
CvModel<- function(Model, predictors, data, is.testing, is.training, control) {
    # fit cross-validation model from training Model on training data and testing
    # it on the testing data.
    # return either 
    # list $ok = TRUE $mean.error $median.error $coverage (fraction)
    # list $ok = FALSE $problem $query
    #cat('start CvModel\n'); browser()

    queries <- Queries(data[is.testing,], control)

    data.training <- data[is.training, ]

    formula <- Formula( predictors = predictors
                       ,response = control$response
                       )
    model.result <- Model( queries = queries
                          ,data.training = data.training
                          ,formula = formula
                          ,control$num.training.days
                          )
    if (!model.result$ok) return(model.result)
    prediction.raw <- model.result$prediction
    prediction <- if (control$response == 'price.log') exp(prediction.raw) else prediction.raw
    is.prediction <- !is.na(prediction)
    actual <- queries$price
    error <- (prediction - actual)[is.prediction]
    error2 <- error * error
    root.mean.squared.error <- sqrt(mean(error2))
    root.median.squared.error <- sqrt(median(error2))
    coverage <- length(error) / length(actual)
    result <- list( rootMeanSquaredError = root.mean.squared.error
                   ,rootMedianSquaredError = root.median.squared.error
                   ,coverage = coverage
                   )
    result
}
CvAssessorWithAssessment <- function(data, is.testing, is.training, control) {
    # return list $prediction $actual, as needed for CrossValidate
    #cat('start CvAssessWithAssessment\n'); browser()
    CvModel( ModelAssessorLinearLocal
            ,control$predictors.with.assessment
            ,data
            ,is.testing
            ,is.training
            ,control
            )
}
CvAssessorWithoutAssessment <- function(data, is.testing, is.training, control) {
    # return list $prediction $actual, as needed for CrossValidate
    #cat('start CvAssessorWithoutAssessment\n'); browser()
    CvModel( ModelAssessorLinearLocal
            ,control$predictors.without.assessment
            ,data
            ,is.testing
            ,is.training
            ,control
            )
}
CvAvmWithAssessment <- function(data, is.testing, is.training, control) {
    #cat('start CVAvmWithAssessment\n'); browser()
    CvModel( ModelAvmLinearLocal
            ,control$predictors.with.assessment
            ,data
            ,is.testing
            ,is.training
            ,control
            )
}
CvAvmWithoutAssessment <- function(data, is.testing, is.training, control) {
    #cat('start CvAvmWithoutAssessment\n'); browser()
    CvModel( ModelAvmLinearLocal
            ,control$predictors.without.assessment
            ,data
            ,is.testing
            ,is.training
            ,control
            )
}
CvMortgageWithAssessment <- function(data, is.testing, is.training, control) {
    #cat('start CVMortgageWithAssessment\n'); browser()
    CvModel( ModelMortgageLinearLocal
            ,control$predictors.with.assessment
            ,data
            ,is.testing
            ,is.training
            ,control
            )
}
CvMortgageWithoutAssessment <- function(data, is.testing, is.training, control) {
    #cat('start CVMortgageWithoutAssessment\n'); browser()
    CvModel( ModelMortgageLinearLocal
            ,control$predictors.without.assessment
            ,data
            ,is.testing
            ,is.training
            ,control
            )
}
Main <- function(control, transaction.data) {
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)


    if (control$testing) {
        nfolds <- 2 
        Models <- list( CvAvmWithoutAssessment
                       ,CvMortgageWithAssessment
                       )
        model.names <- list( 'assessor without assessment'
                            ,'mortgage with assessment'
                            )
    } else {
        nfolds <- control$nfolds
        Models <- list( CvAssessorWithoutAssessment
                       ,CvAssessorWithAssessment
                       ,CvAvmWithoutAssessment
                       ,CvAvmWithAssessment
                       ,CvMortgageWithAssessment
                       )
        model.names <- list( 'assessor without assessment'
                            ,'assessor with assessment'
                            ,'AVM without assessment'
                            ,'AVM with assessment'
                            ,'mortgage with assessment'
                            )
    }

    cv.result <- CrossValidate( data = transaction.data
                                ,nfolds = nfolds
                                ,Models = Models
                                ,model.names = model.names
                                ,control = control
                                ,verbose = TRUE
                                )

    gen.error <- EstimateGeneralizationError(cv.result)
    str(gen.error)

    # produce charts
    description <- c( 'Estimated Generalization Error'
                     ,sprintf('From %d-fold Cross Validation', control$nfolds)
                     ,'Model form: log-linear'
                     ,sprintf('Training period: %d days', control$num.training.days)
                     ,sprintf( 'Testing period: %s through %s'
                              ,control$testing.period$first.date
                              ,control$testing.period$last.date
                              )
                     )
                            
    chart1 <- CreateChart1( control = control
                           ,description = description
                           ,gen.error = gen.error
                           )
    writeLines( text = chart1
               ,con = control$path.out.chart1
               )
    print(chart1)

    # save results
    save(description, control, cv.result, chart1, gen.error, file = control$path.out.rdata)

    if (control$also.strata) {
        # see if we get better results stratifying the samples (clustering the data)
        stratify <- Stratify(control, data)

        StrataResult <- function(strata.index) {
            #cat('start StrataResult', strata.index, '\n'); browser()
            strata.name <- stratify$name[[strata.index]]
            strata.data <- stratify$data[[strata.index]]
            options(warn = 1)  # expect warning: no samples were selected for training
            experiment.result <- CvExperiment( control = control
                                              ,data = strata.data
                                              ,models.names = models.names
                                              )
            options(warn = 2)  # convert warnings into errors
            result <- list( strata.name = stratify$name[[strata.index]]
                           ,experiment.result = experiment.result
                           )

            result
        }


        strata.result <- Map(StrataResult, c(2))
        strata.results <- Map( StrataResult, 1:length(stratify$name))
        cat('strata.results\n')
        print(str(strata.results))
        PrintStrataResult <- function(strata.result) {
            cat('strata.name', strata.result$strata.name, '\n')
            cat('experiment results\n')
            print(strata.result$experiment.result)
            cat(' ')
        }

        Map(PrintStrataResult, strata.results)
    }

    print(control)
    if (control$testing) cat('DISCARD RESULTS: TESTING\n')
}

#debug(Control)
default.args <- NULL  # synthesize the command line that will be used in the Makefile
#default.args <- list('--training', '30')

command.args <- if (is.null(default.args)) CommandArgs() else default.args
parsed.command.args <- ParseCommandLine( cl = command.args
                                        ,keywords = c('training')
                                        ,ignoreUnexpected = TRUE
                                        )
control <- Control(parsed.command.args)


# cache transaction.data
if (!exists('transaction.data')) {
    transaction.data <- ReadTransactionSplits( path.in.base = control$path.in.splits
                                              ,split.names = control$split.names
                                              )
}

#debug(Main)
Main(control, transaction.data)
cat('done\n')
