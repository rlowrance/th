ModelLinearLocal <- function(InTraining, queries, data.training, formula) {
    # return result of local linear regression
    # return list
    # $predictions : vector of predictions, some of which may be NA
    #                NA means that the prediction could not be carried out
    #                For now, throw away the error reason
    # $num.training: if prediction[[i]] is not NA, then number of training samples used
    #                to fit the model

    #cat('start ModelLinearLocal\n'); browser()
    verbose <- TRUE
    verbose <- FALSE

    TestForNoContrasts <- function(data, feature.name) {
        # this tests works only for factors with level FALSE and TRUE

        f <- data[[feature.name]]
        if (is.null(f)) return(list(ok = TRUE))
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
        # $ok = TRUE, $fitted = lm object $num.training
        # $ok = FALSE, $feature = char, $problem = char
        #cat('start Fit', saleDate, '\n'); browser()
        in.training <- InTraining(saleDate)
        data <- data.training[in.training, ]

        # make sure the lm (below) is going to run
        test1 <- TestForNoContrasts(data, 'factor.is.new.construction')
        if (!test1$ok) return(test1)

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
        return <- list(ok = TRUE, fitted = fitted, num.training = nrow(data))
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
        list(ok = TRUE, prediction = prediction, num.training = fitted$num.training)
    }

    # BODY STARTS HERE
    num.queries <- nrow(queries)
    predictions <- as.double(rep(NA, num.queries))
    num.training <- as.double(rep(NA, num.queries))
    for(query.index in 1:num.queries) {
        fit.predict <- FitPredict(query.index)
        if (fit.predict$ok) {
            predictions[[query.index]] <- fit.predict$prediction
            num.training[[query.index]] <- fit.predict$num.training
        } 
    }

    if (verbose) {print('ModelLinearLocal prediction'); print(predictions)}

    list( predictions = predictions
         ,num.training = num.training
         )
}
