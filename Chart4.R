Chart4 <- function(my.control) {
    # class object
    # methods
    # $FileDependencies()
    # $Txt()

    # these format lines must be edited as a group (15 columns)
    # line fields: scenario / response / predictorsForm / predictorsName / 12 x ndays

    # MAYBE: cut off after ndays that are no longer informative
    case   <- '%8s %8s %5s %3s'
    header.format <- paste0(case, paste0(rep(' %6s', 12), collapse = ''))
    data.format   <- paste0(case, paste0(rep(' %6.0f', 12), collapse = ''))

    ndays.range <- c('30', '60', '90', '120', '150', '180', '210', '240', '270', '300', '330', '360')

    PathIn <- function(scenario, response, predictorsName, predictorsForm, ndays) {
        path.in <- paste0( my.control$path.in.base
                          ,'_global'
                          ,'_linear'
                          ,'_2008'
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
                    for (predictorsName in c('alwaysNoAssessment', 'alwaysNoCensus')) {
                        f( scenario = scenario
                          ,response = response
                          ,predictorsName = predictorsName
                          ,predictorsForm = predictorsForm
                          ,accumulator
                          )
                    }
                }
            }
        }
        result <- accumulator$Get()
        result
    }

    FileDependencies <- function() {
        FileDependency <- function(scenario, response, predictorsName, predictorsForm, accumulator) {
            cat('FileDependency', scenario, response, predictorsName, predictorsForm, '\n'); browser()
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

    DataRecords <- function() {
        DataRecord <- function(scenario, response, predictorsName, predictorsForm, accumulator) {
            #cat('DataRecord', scenario, response, predictorsName, predictorsForm, '\n'); browser()
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
                            ,switch(predictorsName
                                    ,alwaysNoAssessment = 'yes'
                                    ,alwaysNoCensus     = 'no'
                                    ,stop('bad predictorsName')
                                    )
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


    HeaderRecords <- function() {
        lines <- Lines()
        lines$Append('Median of Root Median Squared Errors from 10 Fold Cross Validation')
        lines$Append('For global linear model')
        lines$Append('Data from 2003 Onward')
        lines$Append('Non Tax Assessment Features Present in Every Transaction')
        lines$Append('With and Without Census Tract Features (Column Use Cen)')
        lines$Append(' ')
        lines$Append(sprintf( header.format
                             ,' '
                             ,' '
                             ,'preds'  # abbreviate 'predictors' to fit into 6 columns
                             ,'Use'
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
                             ,'Cen'
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

    Txt <- function() {
        lines <- Lines()

        for (line in HeaderRecords()) {
            lines$Append(line)
        }

        for (line in DataRecords()) {
            lines$Append(line)
        }

        result <- lines$Get()
        result
    }

    list( Txt = Txt
         ,FileDependencies = FileDependencies
         )
}
