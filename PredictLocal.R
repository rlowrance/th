source('TryCatchWE.R')
PredictLocal <- function( scenario, ndays, data.training, query.transactions
                         ,response, predictorsForm, predictorsName, control
                         ,TrainingData, MakeFormula
                         ,FitModel, fit.model.data
                         ,PredictModel
                         ,model.name) {
    # Return evaluation of the specified model on the given training and test data
    # Return vector of predictions for the query.transactions using a local model

    verbose.prediction <- FALSE
    verbose.memory <- FALSE

    Fit <- function(saleDate, training.data) {
        # return list $ok $value (maybe) $problem (maybe)
#        training.data <- TrainingData( data.training = data.training
#                                      ,scenario = scenario
#                                      ,ndays = ndays
#                                      ,saleDate = saleDate
#                                      )
        if (nrow(training.data) == 0) {
            return(list(ok = FALSE, problem = 'no training samples'))
        }
        if (nrow(training.data) == 1) {
            return(list(ok = FALSE, problem = '1 training sample'))
        }

        # drop feature that are non-informative; otherwise, lm sets coefficient to NA and predict fails
        maybe.formula <- MakeFormula( data = training.data
                                     ,response = response
                                     ,predictorsForm = predictorsForm
                                     ,predictorsName = predictorsName
                                     ,control = control
                                     )
        if (!maybe.formula$ok)
            return(list(ok = FALSE, problem = maybe.formula$problem))
        maybe.fitted <-
            TryCatchWE(FitModel( formula = maybe.formula$value
                                ,data = training.data
                                ,fit.model.data = fit.model.data
                               )
                       )
        #cat('test maybe.fitted\n'); browser()
        if (!is.null(maybe.fitted$warning)) {
          list(ok = FALSE, problem = paste0('Fit warning: ', maybe.fitted$warning))
        } else if (!is.null(maybe.fitted$error)) {
          list(ok = FALSE, problem = paste0('Fit warning: ', maybe.fitted$error))
        } else
          list(ok = TRUE, value = maybe.fitted$value)
    }
    Predict <- function(fitted, query.transaction) {
        # return list $ok $value (maybe) $problem (maybe)
        #cat('starting Predict\n'); browser()
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
    }
    RemoveZeroOccurrenceLevels <- function(data) {
      # remove levels in location factors that have no values
      if (!is.null(data$zip5))
        data$zip5 <- factor(data$zip5)
      if (!is.null(data$census.tract))
        data$census.tract = factor(data$census.tract)
      if (!is.null(data$property.city))
        data$property.city = factor(data$property.city)
      data
    }
    ResetFactors <- function(training.data ,query.transaction) {
      # reset any location factor levels in the query.transaction
      if (!is.null(query.transaction$zip5))
        query.transaction$zip5 <- factor( x = query.transaction$zip5
                                        ,levels = levels(training.data$zip5)
                                        )
      if (!is.null(query.transaction$census.tract))
        query.transaction$census.tract <- factor( x = query.transaction$census.tract
                                                 ,levels = levels(training.data$census.tract)
                                                 )
      if (!is.null(query.transaction$property.city))
        query.transaction$property.city <- factor( x = query.transaction$property.city
                                                  ,levels = levels(training.data$census.tract)
                                                  )
      query.transaction
    }

    predictions <- as.double(rep(NA, nrow(query.transactions)))

    # group queries by sale date, and fit one model for each sale date
    saleDates <- query.transactions$saleDate
    unique.saleDates <- unique(saleDates)
    for (unique.saleDate.index in 1:length(unique.saleDates)) {
      saleDate.as.number <- unique.saleDates[[unique.saleDate.index]]
      #cat('looping on saleDate\n'); browser()
      saleDate <- as.Date(saleDate.as.number, origin = as.Date('1970-01-01'))

      # derive training data, convert years to ages, remove factors levels with 0 occurences
      training.data.in.period <- TrainingData( data.training = data.training
                                              ,scenario = scenario
                                              ,ndays = ndays
                                              ,saleDate = saleDate)
      training.data.age <- ConvertYearFeatures( data = training.data.in.period
                                               ,saleDate = saleDate
                                               )
      training.data.factors <- RemoveZeroOccurrenceLevels(training.data.age)
      Printf('saleDate %s %d of %d fitting on %d samples model %s\n'
             ,saleDate
             ,unique.saleDate.index
             ,length(unique.saleDates)
             ,nrow(training.data.factors)
             ,model.name
             )

      # attempt to fit using revised training data
      maybe.fitted <- Fit( saleDate = saleDate
                          ,training.data = training.data.factors
                          )
      #cat('just maybe.fitted\n'); browser()

      if (maybe.fitted$ok) {
        # predict for each query for the sale date
        query.indices <- which(query.transactions$saleDate == saleDate)
        for (query.index in query.indices) {
          query.transaction <- query.transactions[query.index, ]

        
          query.transaction.converted <- ConvertYearFeatures( data = query.transaction
                                                             ,saleDate = saleDate)
          query.transaction.factors <- ResetFactors( training.data = training.data.factors
                                                    ,query.transaction = query.transaction.converted
                                                    )

          # attempt to predict
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
    predictions
}
