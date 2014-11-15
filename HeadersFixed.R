HeadersFixed <- function(fixed, lines) {
    # append fixed header to lines, a Lines() object
    HeaderScope <- function(s) {
        paste0( 'Scope: '
               ,switch( s
                       ,global = 'entire market'
                       ,stop('bad scope')
                       )
               )
    }
    HeaderModel <- function(s) {
        paste0( 'Model: '
               ,switch( s
                       ,linear = 'linear'
                       ,linL2  = 'linear with L2 regularizer'
                       ,rf     = 'random forest'
                       ,stop('bad model')
                       )
               )
    }
    HeaderNdays <- function(s) {
        paste0( 'Number of days in training period: ', s)
    }
    HeaderPredictorsForm <- function(s) {
        paste0( 'Predictors form: '
               ,switch(s
                       ,level = 'level (natural units)'
                       ,log   = 'log(natural units)'
                       ,stop('bad predictorsForm')
                       )
               )
    }
    HeaderPredictorsName <- function(s) {
        paste0( 'Predictors name: '
               ,switch(s
                       ,always = 'always in every transaction'
                       ,alwaysNoAssessment = 'always in every transaction and not in the tax assessment'
                       ,alwaysNoCensus = 'always in every transaction and not in the tax assessment or census'
                       ,stop('bad predictorsName')
                       )
               )
    }
    HeaderResponse <- function(s) {
        paste0( 'Response: '
               ,switch( s
                       ,price = 'price'
                       ,logprice = 'log(price)'
                       ,stop('bad response')
                       )
               )
    }
    HeaderTimePeriod <- function(s) {
        paste0( 'Time period: '
               ,switch( s
                       ,'2003on' = '2003 on'
                       ,'2008'   = '2008'
                       ,stop('bad time period')
                       )
               )
    }
    HeaderQuery <- function(s) {
        paste0( 'Percent of queries in each fold that were estimated: '
               ,switch( s
                       ,'100' = '1'
                       ,'1'   = '100'
                       ,stop('bad query')
                       )
               )
    }
    HeaderLambda <- function(s) {
        if (s == '0')
            NULL
        else
            sprintf('Lambda: %.2f', as.numeric(s) / 100)
    }
    
    stopifnot(!is.null(lines))
    lines$Append(' ')
    for (header.name in names(fixed)) {

        if (header.name == 'scenario') stopifnot(fixed$scenario == 'avm')
        else if (header.name == 'ntree') stopifnot(fixed$ntree == '0')
        else if (header.name == 'mtry') stopifnot(fixed$mtry == '0')
        else {
            value <- switch( header.name
                            ,scope = HeaderScope(fixed$scope)
                            ,model = HeaderModel(fixed$model)
                            ,timePeriod = HeaderTimePeriod(fixed$timePeriod)
                            ,response = HeaderResponse(fixed$response)
                            ,predictorsForm = HeaderPredictorsForm(fixed$predictorsForm)
                            ,predictorsName = HeaderPredictorsName(fixed$predictorsName)
                            ,ndays = HeaderNdays(fixed$ndays)
                            ,query = HeaderQuery(fixed$query)
                            ,lambda = HeaderLambda(fixed$lambda)
                            ,stop(paste('bad header.name', header.name))
                            )
            if (!is.null(value))
                lines$Append(value)
        }
    }
}
