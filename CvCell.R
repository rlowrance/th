CvCell <- function() {
  # list of functions for working with e-cv-cells
  source('Directory.R')

  Is.Valid.Scope <- function(s)          s %in% c('global')
  Is.Valid.Model <- function(s)          s %in% c('linear')
  Is.Valid.TimePeriod <- function(s)     s %in% c('2003on', '2008')
  Is.Valid.Scenario <- function(s)       s %in% c('assessor', 'avm', 'mortgage')
  Is.Valid.Response <- function(s)       s %in% c('logprice', 'price')
  Is.Valid.PredictorsName <- function(s) s %in% Possible.PredictorsNames()
  Is.Valid.PredictorsForm <- function(s) s %in% c('level', 'log')
  Is.Valid.Ndays <- function(s)          s %in% Possible.Ndays()
  Is.Valid.Query <- function(s)          s %in% c('1', '100')
  Is.Valid.C <- function(s)              s %in% c('0')
  Is.Valid.Ntree <- function(s)          s %in% c('0')
  Is.Valid.Mtry <- function(s)           s %in% c('0')


  Path <- function( scope, model, timePeriod, scenario
                   ,response, predictorsName, predictorsForm, ndays
                   ,query, c, ntree, mtry) {
    # return path in file system to a particular cell

    stopifnot(Is.Valid.Scope(scope))
    stopifnot(Is.Valid.Model(model))
    stopifnot(Is.Valid.TimePeriod(timePeriod))
    stopifnot(Is.Valid.Scenario(scenario))
    stopifnot(Is.Valid.Response(response))
    stopifnot(Is.Valid.PredictorsName(predictorsName))
    stopifnot(Is.Valid.PredictorsForm(predictorsForm))
    stopifnot(Is.Valid.Ndays(ndays))
    stopifnot(Is.Valid.Query(query))
    stopifnot(Is.Valid.C(c))
    stopifnot(Is.Valid.Ntree(ntree))
    stopifnot(Is.Valid.Mtry(mtry))

    path <- paste0( Directory('working')
                   ,'e-cv-cells/' ,scope
                   ,'_', model
                   ,'_', timePeriod
                   ,'_', scenario
                   ,'_', response
                   ,'_', predictorsName
                   ,'_', predictorsForm
                   ,'_', ndays
                   ,'_', query
                   ,'_', c
                   ,'_', ntree
                   ,'_', mtry
                   ,'.RData'
                   )
    path
  }

  Possible.Ndays <- function() {
    # return vector of all possible values for ndays ('30', '60', ..., '360')
    ndays = c('30', '60', '90', '120', '150', '180', '210', '240', '270', '300', '330', '360')
    ndays
  }

  Possible.PredictorsNames <- function() {
    # return vector of all possible values for predictorsName
    predictors.names <- c( 'always', 'alwaysNoAssessment', 'alwaysNoCensus'
                          ,'best01', 'best02', 'best03', 'best04', 'best05', 'best06'
                          ,'best07', 'best08', 'best09', 'best10', 'best11', 'best12'
                          ,'best13', 'best14', 'best15', 'best16', 'best17', 'best18'
                          ,'best19', 'best20', 'best21', 'best22', 'best23', 'best24'
                          )
    predictors.names
  }
                          

  list( Path                     = Path
       ,Possible.Ndays           = Possible.Ndays
       ,Possible.PredictorsNames = Possible.PredictorsNames
       )
}
