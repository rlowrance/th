CvApplyAllPossibilities <- function(FUN, possible, one.arg = FALSE) {
    # FUN(possible$scope, possible$model, ..., possible$mtry), for all combinations, returning nothing
    # f should accumulate its results
    # ARGS:
    # FUN     :  function(scope,model,timePeriod,scenario,response,predictorsName,predictorsForm
    #                    ,ndays,query,C,ntree,mtry); OR
    # FUN     :  function(arg) where arg is a named list
    # possible: named list with elements scope, model, ... mtry
    #           each element is a vector of possible values
    
    for (scope          in possible$scope) {
    for (model          in possible$model) {
    for (timePeriod     in possible$timePeriod) {
    for (scenario       in possible$scenario) {
    for (response       in possible$response) {
    for (predictorsName in possible$predictorsName) {
    for (predictorsForm in possible$predictorsForm) {
    for (ndays          in possible$ndays) {
    for (query          in possible$query) {
    for (c              in possible$c) {
    for (ntree          in possible$ntree) {
    for (mtry           in possible$mtry) {
        if (one.arg) {
            arg <- list( scope = scope
                        ,model = model
                        ,timePeriod = timePeriod
                        ,scenario = scenario
                        ,response = response
                        ,predictorsName = predictorsName
                        ,predictorsForm = predictorsForm
                        ,ndays = ndays
                        ,query = query
                        ,c = c
                        ,ntree = ntree
                        ,mtry = mtry
                        )
            FUN(arg)
        } else {
            FUN( scope = scope
                ,model = model
                ,timePeriod = timePeriod
                ,scenario = scenario
                ,response = response
                ,predictorsName = predictorsName
                ,predictorsForm = predictorsForm
                ,ndays = ndays
                ,query = query
                ,c = c
                ,ntree = ntree
                ,mtry = mtry
                )
        }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
}
