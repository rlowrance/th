source('TryCatchWE.R')
PredictLocal <- function(scenario, ndays, data.training, query.transactions
                         ,scope, response, predictorsForm, predictorsName, control
                         ,TrainingData, MakeFormula
                         ,FitModel, fit.model.data
                         ,PredictModel) {
    # Return evaluation of the specified model on the given training and test data
    # Return vector of predictions for the query.transactions using a local model

    verbose.prediction <- FALSE
    verbose.memory <- FALSE

    Fit <- function(saleDate) {
        # return list $ok $value (maybe) $problem (maybe)
        training.data <- TrainingData( data.training = data.training
                                      ,scenario = scenario
                                      ,ndays = ndays
                                      ,saleDate = saleDate
                                      )
        if (nrow(training.data) == 0) {
            return(list(ok = FALSE, problem = 'no training samples'))
        }
        # convert year.built to age and age2
        # convert effective.year.built to effective.age and effective.age2
        converted.data <- ConvertYearFeatures( data = training.data
                                              ,saleDate = saleDate
                                              )

        stopifnot(scope == 'global')  # must take city-sample for submarket scope
        # drop feature that are non-informative; otherwise, lm sets coefficient to NA and predict fails
        maybe.formula <- MakeFormula( data = converted.data
                                     ,scope = scope
                                     ,response = response
                                     ,predictorsForm = predictorsForm
                                     ,predictorsName = predictorsName
                                     ,control = control
                                     )
        if (!maybe.formula$ok)
            return(list(ok = false, problem = maybe.formula$problem))
        maybe.fitted <-
            TryCatchWE(FitModel( formula = maybe.formula$value
                                ,data = converted.data
                                ,fit.model.data = fit.model.data
                               )
                       )
        if (!is.null(maybe.fitted$warning)) {
          list(ok = FALSE, problem = paste0('Fit warning: ', maybe.fitted$warning))
        } else if (!is.null(maybe.fitted$error)) {
          list(ok = FALSE, problem = paste0('Fit warning: ', maybe.fitted$error))
        } else
          list(ok = TRUE, value = maybe.fitted$value)
#        if (inherits(maybe.fitted, 'lm'))
#            list(ok = TRUE, value = maybe.fitted)
#        else
#            list(ok = FALSE, problem = paste0('Fit: ', maybe.fitted))
    }
    Predict <- function(fitted, query.transaction) {
        # return list $ok $value (maybe) $problem (maybe)
        maybe.predict <- TryCatchWE(PredictModel( object = fitted
                                                 ,newdata = query.transaction
                                                 )
        )
        if (!is.null(maybe.predict$warning)) {
          list(ok = FALSE, problem = paste0('Predict warning: ', maybe.predict$warning))
        } else if (!is.null(maybe.predict$error)) {
          list(ok = FALSE, problem = paste0('Predict error: ', maybe.predict$error))
        } else {
          list(ok = TRUE, value = maybe.predict$value)
        }
#        maybe.predict <-
#            tryCatch( predict(object = fitted, newdata = query.transaction)
#                     ,warning = function(w) w
#                     ,error = function(e) e
#                     )
#        if (inherits(maybe.predict, 'numeric')) 
#            list(ok = TRUE, value = maybe.predict)
#        else
#            list(ok = FALSE, problem = paste0('Predict: ', maybe.predict))
    }
    FitPredict <- function(query.transaction) {
        saleDate <- query.transaction$saleDate
        #maybe.fitted <- FitMemoised(saleDate)
        maybe.fitted <- Fit(saleDate)
        if (maybe.fitted$ok) {
            maybe.prediction <- Predict( fitted = maybe.fitted$value
                                        ,query.transaction = ConvertYearFeatures( data = query.transaction
                                                                                 ,saleDate = saleDate
                                                                                 )
                                        )
            return(maybe.prediction)
        } else {
            return(maybe.fitted)
        }
    }

    predictions <- as.double(rep(NA), nrow(query.transactions))

    # group queries by sale date, and fit one model for each sale date
    saleDates <- query.transactions$saleDate
    unique.saleDates <- unique(saleDates)
    for (saleDate.number in unique.saleDates) {
      saleDate <- as.Date(saleDate.number, origin = as.Date('1970-01-01'))
      maybe.fitted <- Fit(saleDate)
      if (maybe.fitted$ok) {
        # predict for each query for the sale date
        query.indices <- which(query.transactions$saleDate == saleDate)
        for (query.index in query.indices) {
          query.transaction <- query.transactions[query.index, ]
          query.transaction.converted <- ConvertYearFeatures( data = query.transaction
                                                             ,saleDate = saleDate)
          maybe.prediction <- Predict( fitted = maybe.fitted$value
                                      ,query.transaction = query.transaction.converted
                                      )
          if (maybe.prediction$ok) {
            if (verbose.prediction) 
              Printf( 'prediction query.index %d sale date %s value %f\n'
                     ,query.index
                     ,as.character(query.transactions[query.index,]$saleDate)
                     ,maybe.prediction$value
                     )
            predictions[[query.index]] <- maybe.prediction$value
          } else {
            Printf( 'prediction query.index %d sale date %s failed: %s\n'
                   ,query.index
                   ,as.character(query.transactions[query.index,]$saleDate)
                   ,as.character(maybe.prediction$problem)
                   )
          }
          if (verbose.memory)
            Printf( 'memory used %f GB prediction size %f GB \n'
                   ,mem_used() / 1e9
                   ,object_size(predictions) / 1e9
                   )
        }
      }
    }
#    for (query.index in 1:nrow(query.transactions)) {
#      maybe.prediction <- FitPredict(query.transactions[query.index,])
#      if (maybe.prediction$ok) {
#        if (verbose) 
#          Printf( 'prediction query.index %d sale date %s value %f\n'
#                 ,query.index
#                 ,as.character(query.transactions[query.index,]$saleDate)
#                 ,maybe.prediction$value
#                 )
#        predictions[[query.index]] <- maybe.prediction$value
#      } else {
#        Printf( 'prediction query.index %d sale date %s failed: %s\n'
#               ,query.index
#               ,as.character(query.transactions[query.index,]$saleDate)
#               ,as.character(maybe.prediction$problem)
#               )
#      }
#      if (verbose)
#        Printf( 'memory used %f GB prediction size %f GB \n'
#               ,mem_used() / 1e9
#               ,object_size(predictions) / 1e9
#               )
#    }
    predictions
}
