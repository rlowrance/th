PredictLocalRandomForests <- function( scenario
                                      ,ndays
                                      ,data.training
                                      ,query.transactions
                                      ,response
                                      ,predictorsForm
                                      ,predictorsName
                                      ,control
                                      ,fit.model.data  # hyperparameters
                                      ,TrainingData
                                      ,model.name
                                      ,fold.number
                                      ) {
    # return vector of predictions, one for each query.transaction
    HasZip5 <- function(char.vector) {
        # return TRUE iff char.vector has zip5 has one value
        for (element in char.vector) {
            if (element == 'zip5')
                return(TRUE)
        }
        FALSE
    }
    predictors <- Predictors2(predictorsName, predictorsForm)
    has.zip <- HasZip5(predictors)
    if (has.zip) {
        # replace zip5 feature with zip5.is.special
        predictors['zip5'] <- 'zip5.is.special'
        data.training$zip5.is.special <- rep(FALSE, nrow(data.training))
        query.transactions$zip5.is.special <- rep(TRUE, nrow(query.transactions))
    }
    formula <- Formula( if (response == 'logprice') 'price.log' else 'price'
                       ,predictors = predictors
                       )

    predictions <- as.double(rep(NA, nrow(query.transactions)))
    for (query.index in 1:nrow(query.transactions)) {
        # NOTE: Could create a cache based on the zip codes that actually occur
        query <- query.transactions[query.index,]
        data.training.in.period <- TrainingData( data.training = data.training
                                                ,scenario = scenario
                                                ,ndays = ndays
                                                ,saleDate = query$saleDate
                                                )
        cannot.fit <- FALSE
        if (has.zip) {
            data.training.in.period$zip5.is.special <- data.training.in.period$zip5 == query$zip5
            sum.zip5.is.special <- sum(data.training.in.period$zip5.is.special)
            cannot.fit <- sum.zip5.is.special == 0 || sum.zip5.is.special == nrow(data.training.in.period)
        }
        if (cannot.fit) {
            Printf( 'query index %d only 1 value for zip5 %f\n'
                   ,query.index
                   ,query$zip5
                   )
        } else {
            fitted.model <- randomForest( formula = formula
                                         ,data = data.training.in.period
                                         ,ntree = fit.model.data$ntree
                                         ,mtry = fit.model.data$mtry
                                         )
            prediction <- predict( object = fitted.model
                                  ,newdata = query
                                  )
            predictions[query.index] <- prediction
        }
        Printf('rf query.index %d of %d training.samples %d %s %d\n'
               ,query.index
               ,nrow(query.transactions)
               ,nrow(data.training)
               ,model.name
               ,fold.number
               )
    }
    result <- predictions
    result
}
