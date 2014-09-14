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

Control <- function(parsed.command.args) {
    # capture values from the command line
    num.training.days <- as.integer(parsed.command.args$training)

    me <- 'e-avm-variants' 
    out.base <- sprintf( ' %s-training-%d'
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
                    ,also.strata = FALSE
                    )
    control
}

CreateChart1Body <- function(control, all.result) {
    # return a vector lines, the body of chart 1
    #cat('start CreateChart1Body\n'); browser()
    result <- sprintf(control$chart1.format.header, 'scenario', 'mean RMSE', 'median RMedianSE')

    for (row.index in 1:nrow(all.result)) {
        result <- c( result
                    ,sprintf( control$chart1.format.data
                             ,all.result$experiment.name[[row.index]]
                             ,all.result$mean.RMSE[[row.index]]
                             ,all.result$median.RMedianSE[[row.index]]
                             )
                    )
    }

    result
}

CreateChart1 <- function(control, description, all.result) {
    # return a vector of lines, the txt for chart 1
    #cat('start CreateChart1\n'); browser()

    result <- c( description
                ,' '
                ,CreateChart1Body(control, all.result)
                )

    result
}

DefineModelsNames <- function(control, data) {
    # build parallel arrays
    #cat('start DefineModelsNames\n'); browser()
    MakeModel <- function(scenario, predictors) {
        #cat('start MakeModel', scenario, '\n'); browser()
        Model <- MakeModelLinear( scenario = scenario
                                 ,predictors = predictors
                                 # other args are common
                                 ,response = 'price.log'
                                 ,testing.period = control$testing.period
                                 ,data = data
                                 ,num.training.days = control$num.training.days
                                 ,verbose = TRUE
                                 )
    }

    Models <- list( MakeModel('assessor', control$predictors.without.assessment)
                   ,MakeModel('assessor', control$predictors.with.assessment)
                   ,MakeModel('avm', control$predictors.without.assessment)
                   ,MakeModel('avm', control$predictors.with.assessment)
                   ,MakeModel('mortgage', control$predictors.with.assessment)
                   )
    names <- c( 'assessor without assessment'
               ,'assessor with assessment'
               ,'avm without assessment'
               ,'avm with assessment'
               ,'mortgage with assessment'
               )
             
    models.names <- list(Models = Models ,names = names)
    models.names
}
 
ExperimentResult <- function(cv.result, experiment.names) {
    # return data.frame
    #cat('start ExperimentResult\n'); browser()
    fold.assessment <- cv.result$fold.assessment

    MeanRmse <- function(model.index) {
        #cat('start MeanRmse', model.index, '\n'); browser()
        in.model <- fold.assessment$model.index == model.index
        fold.error <- fold.assessment$assessment.rmse[in.model]
        result <- mean(fold.error, na.rm = TRUE)
        if (is.nan(result) || is.na(result)) {
            cat('in MeanRmse: strange result', result, '\n')
            browser()
        }
        result
    }

    MedianRmse <- function(model.index) {
        #cat('start Median Rmse', model.index, '\n'); browser()
        in.model <- fold.assessment$model.index == model.index
        fold.error <- fold.assessment$assessment.root.median.squared.error[in.model]
        result <- median(fold.error, na.rm = TRUE)
        if (is.nan(result) || is.na(result)) {
            cat('in MedianRmse: strange result', result, '\n')
            browser()
        }
        result
    }

    num.models <- max(fold.assessment$model.index)
    experiment.result <- data.frame( stringsAsFactors = FALSE
                                    ,experiment.name = experiment.names
                                    ,mean.RMSE = sapply(1:num.models, MeanRmse)
                                    ,median.RMedianSE = sapply(1:num.models, MedianRmse)
                                    )

    experiment.result
}

Stratify <- function(control, data){
    # break data.frame into MECE subsets (stratum)
    # for now, criteria is median.household.income and the 3 strata are wealth, poor, middle class
    # Returns list with these elements
    # $data : list of strata, each a data.frame
    # $name : chr vector of names for the corresponding strata
    #cat('start Stratify', nrow(data), '\n'); browser()

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

CvExperiment <- function(control, data, models.names) { 
    # cross validate to determine expected generalization error, then tabulate experimental results
    #cat('start CvExperiment\n'); browser()

    cv.result <-  # 
        CrossValidate( data = data
                      ,nfolds = control$nfolds
                      ,Models = models.names$Models
                      ,Assess = Assess
                      ,experiment = models.names$experiment.names
                      )

    experiment.result <- ExperimentResult( cv.result = cv.result
                                          ,experiment.names = models.names$names
                                          )

    experiment.result
}

Main <- function(control, transaction.data) {
    #cat('start Main'); browser()


    InitializeR(duplex.output.to = control$path.out.log)
    str(control)

    models.names <- DefineModelsNames( control = control
                                      ,data = transaction.data)

    all.result <- CvExperiment( control = control
                               ,data = transaction.data
                               ,models.names = models.names
                               )

    cat('all.result\n')
    print(str(all.result))

    # produce charts
    description <- c( 'Estimated Generalization Error'
                     ,sprintf('From %d-fold Cross Validation', control$nfolds)
                     ,'Model form: log-linear'
                     ,sprintf('Training period: %d days before 2008-11-01', control$num.training.days)
                     ,sprintf( 'Testing period: %s through %s'
                              ,control$testing.period$first.date
                              ,control$testing.period$last.date
                              )
                     )
                            
    chart1 <- CreateChart1( control = control
                           ,description = description
                           ,all.result = all.result
                           )
    writeLines( text = chart1
               ,con = control$path.out.chart1
               )
    print(chart1)

    # save results
    save(description, control, all.result, chart1, file = control$path.out.rdata)

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
default.args <- list('--training', '30')

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
