Chart1And2 <- function(my.control) {
    # class object
    # methods
    # $FileDependencies()
    # $Txt()

    # these format lines must be edited as a group (15 columns)
    # line fields: scenario / response / predictorsForm / 12 x ndays
    case   <- '%8s %8s %5s'
    header.format <- paste0(case, paste0(rep(' %6s', 12), collapse = ''))
    data.format   <- paste0(case, paste0(rep(' %6.0f', 12), collapse = ''))

    ndays.range <- c('30', '60', '90', '120', '150', '180', '210', '240', '270', '300', '330', '360')

    PathIn <- function(scenario, response, predictorsName, predictorsForm, ndays) {
        path.in <- paste0( my.control$path.in.base
                          ,'_global'
                          ,'_linear'
                          ,'_2009'
                          ,'_', scenario
                          ,'_', response
                          ,'_', predictorsName
                          ,'_', predictorsForm
                          ,'_', ndays
                          ,'_1'
                          ,'_0'
                          ,'_0'
                          ,'_0'
                          ,'.RData'
                          )
        path.in
    }

    Accumulate <- function(f) {
        # accumulate lines across relevant combination of scenario,response, predictorsForm, predictorsName
        accumulator <- Lines()
        for (scenario in c('avm', 'mortgage')) {
            for (response in c('price', 'logprice')) {
                for (predictorsForm in c('level', 'log')) {
                    f( scenario = scenario
                      ,response = response
                      ,predictorsForm = predictorsForm
                      ,accumulator
                      )
                }
            }
        }
        result <- accumulator$Get()
        result
    }

    FileDependencies <- function(predictorsName) {
        FileDependency <- function(scenario, response, predictorsForm, accumulator) {
            #cat('FileDependency', scenario, response, predictorsForm, '\n'); browser()
            for (ndays in ndays.range) {
                line <- PathIn( scenario = scenario
                               ,response = response
                               ,predictorsName = predictorsName
                               ,predictorsForm = predictorsForm
                               ,ndays = ndays
                               )
                accumulator$Append(line)
            }
        }

        result <- Accumulate(FileDependency)
        result
    }

    DataRecords <- function(predictorsName) {
        DataRecord <- function(scenario, response, predictorsForm, accumulator) {
            M_RMSE <- function(ndays) {
                path.in <- PathIn( scenario = scenario
                                  ,response = response
                                  ,predictorsName = predictorsName
                                  ,predictorsForm = predictorsForm
                                  ,ndays = ndays
                                  )
                load(path.in)
                stopifnot(!is.null(cv.result))
                stopifnot(length(cv.result) == 1)
                median.RMSE <- MedianRMSE(cv.result[[1]])
                median.RMSE
            }
            line <- sprintf( data.format
                            ,scenario
                            ,response
                            ,predictorsForm
                            ,M_RMSE(30)
                            ,M_RMSE(60)
                            ,M_RMSE(90)
                            ,M_RMSE(120)
                            ,M_RMSE(150)
                            ,M_RMSE(180)
                            ,M_RMSE(210)
                            ,M_RMSE(240)
                            ,M_RMSE(270)
                            ,M_RMSE(300)
                            ,M_RMSE(330)
                            ,M_RMSE(360)
                            )
            accumulator$Append(line)
        }

        result <- Accumulate(DataRecord)
        result
    }

    HeaderRecords <- function(predictorsName) {
        lines <- Lines()
        lines$Append('Median of Root Median Squared Errors from 10 Fold Cross Validation')
        lines$Append('For global linear model')
        lines$Append('Data from late 2008 and 2009')
        lines$Append('Features Present in Every Transaction')
        lines$Append(switch( predictorsName
                            ,always = 'Using Assessments'
                            ,alwaysNoAssessment = 'Not Using Assessment'
                            ,stop('bad predictorsName')
                            )
        )
        lines$Append(' ')
        lines$Append(sprintf( header.format
                             ,' '
                             ,' '
                             ,'preds'  # abbreviate 'predictors' to fit into 6 columns
                             ,'ndays'
                             ,' '
                             ,' '
                             ,' '
                             ,' '
                             ,' '
                             ,' '
                             ,' '
                             ,' '
                             ,' '
                             ,' '
                             ,' '
                             )
        )
        lines$Append(sprintf( header.format
                             ,'scenario'
                             ,'response'
                             ,'Form'
                             ,'30'
                             ,'60'
                             ,'90'
                             ,'120'
                             ,'150'
                             ,'180'
                             ,'210'
                             ,'240'
                             ,'270'
                             ,'300'
                             ,'330'
                             ,'360'
                             )
        )
        result <- lines$Get()
        result
    }

    Txt <- function(predictorsName) {
        lines <- Lines()

        for (line in HeaderRecords(predictorsName)) {
            lines$Append(line)
        }

        for (line in DataRecords(predictorsName)) {
            lines$Append(line)
        }

        result <- lines$Get()
        result
    }

    list( Txt = Txt
         ,FileDependencies = FileDependencies
         )
}

