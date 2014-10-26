library(randomForest)  # use randomForest() to implement random forests
#library(lubridate)
ModelRandomForestsLocal <- function(InTraining, queries, data.training, formula, 
                                    mtry, ntree) {
    # return result of local linear regression
    # model.each: chr in {'day', 'month', 'year'}
    # return list
    # $predictions : vector of predictions, some of which may be NA
    #                NA means that the prediction could not be carried out
    #                For now, throw away the error reason
    # $num.training: if prediction[[i]] is not NA, then number of training samples used
    #                to fit the model


    #cat('start ModelRandomForestLocal\n'); browser()

    verbose <- TRUE
    #verbose <- FALSE
    
    debugging <- TRUE 
    debugging <- FALSE


    Fit <- function(saleDate) {
        # return list, either
        # $ok = TRUE, $fitted = lm object $num.training
        # $ok = FALSE, $feature = char, $problem = char
        #cat('start Fit\n'); print(saleDate); browser()
        in.training <- InTraining(saleDate)
        data <- data.training[in.training, ]

        
        #cat('Fit fitted\n'); browser()
        # ntree := number of tree (ntree / nrow(data) is number of times each training sample is used)
        clock <- Clock()
        fitted <- randomForest( formula = formula
                               ,data = data
                               ,ntree = ntree
                               ,mtry = mtry
                               )
        if (verbose) {
            Printf( 'cpu minutes to fit (%s ntree %d mtry %d)= %f\n'
                   ,as.character(saleDate)
                   ,ntree
                   ,mtry
                   ,clock$Cpu() / 60
                   )
        }

        return <- list( ok = TRUE
                       ,fitted = fitted
                       ,num.training = nrow(data)
                       )
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

        prediction <- 
            tryCatch( {
                     #cat('FitPredict prediction\n'); browser()
                     prediction <- predict( object = fitted$fitted
                                           ,newdata = queries[query.index,]
                                           )
                     }
                     ,warning = function(w) {
                         cat('warning in ModelRandomForestLocal:\n')
                         print(w)
                         w
                     }
                     ,error = function(e) {
                         cat('error in ModelRandomForestLocal:\n')
                         print(e)
                         e
                     }
                     )
        if (is.numeric(prediction) )
            list(ok = TRUE, prediction = prediction, num.training = fitted$num.training)
        else
            list(ok = FALSE, problem = prediction)
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

    if (verbose) {print('ModelRandomForestLocal prediction'); print(predictions)}

    list( predictions = predictions
         ,num.training = num.training
         )
}
