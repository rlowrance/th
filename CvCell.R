CvCell <- function(validate.cell.specifiers = TRUE) {
  # list of functions for working with e-cv-cells
  # $Command(scope, model, ..., mtry) --> chr, Rscript -e-cv.R --scope SCOPE ... --mtry MTRY
  # $Path(scope, model, ..., mtry) --> chr, path to WORKING/e-cv-cells/FILE.Rdata
  # $PossibleNdays() --> chr vector, all possible values for ndays, namely '30' .. '360'
  # $PossiblePredictorsNamess() --> chr vector, all possible values for predictorsName, 'always', ...
  # $FixedCellValues(chart.name) --> list of any 12 cell attributes that are common

  Is.Valid.Scope <- function(s)          is.character(s)
  Is.Valid.Model <- function(s)          s %in% c('linear', 'linL2', 'rf')
  Is.Valid.TimePeriod <- function(s)     s %in% c('2003on', '2008')
  Is.Valid.Scenario <- function(s)       s %in% c('assessor', 'avm', 'mortgage')
  Is.Valid.Response <- function(s)       s %in% c('logprice', 'price')
  Is.Valid.PredictorsName <- function(s) s %in% PossiblePredictorsNamess()
  Is.Valid.PredictorsForm <- function(s) s %in% c('level', 'log')
  Is.Valid.Ndays <- function(s)          s %in% PossibleNdays()
  Is.Valid.Query <- function(s)          s %in% c('1', '20', '100')
  Is.Valid.Lambda <- function(s)         is.character(s) && as.integer(s) >= 0
  Is.Valid.Ntree <- function(s)          is.character(s) && as.integer(s) >= 0
  Is.Valid.Mtry <- function(s)           is.character(s) && as.integer(s) >= 0

  Command <- function( scope, model, timePeriod, scenario
                      ,response, predictorsName, predictorsForm, ndays
                      ,query, lambda, ntree, mtry) {
    # return command to build a particular cell
    if (validate.cell.specifiers) 
      Validate.Cell.Specifiers( scope, model, timePeriod, scenario
                               ,response, predictorsName, predictorsForm, ndays
                               ,query, lambda, ntree, mtry)
    command <- paste0( 'Rscript e-cv.R'
                      ,' --scope ', scope
                      ,' --model ', model
                      ,' --timePeriod ', timePeriod
                      ,' --scenario ', scenario
                      ,' --response ', response
                      ,' --predictorsName ', predictorsName
                      ,' --predictorsForm ', predictorsForm
                      ,' --ndays ', ndays
                      ,' --query ', query
                      ,' --lambda ', lambda
                      ,' --ntree ', ntree
                      ,' --mtry ', mtry
                      )
    command
  }

  Path <- function( scope, model, timePeriod, scenario
                   ,response, predictorsName, predictorsForm, ndays
                   ,query, lambda, ntree, mtry) {
    # return path in file system to a particular cell

    if (validate.cell.specifiers)
      Validate.Cell.Specifiers( scope, model, timePeriod, scenario
                               ,response, predictorsName, predictorsForm, ndays
                               ,query, lambda, ntree, mtry)

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
                   ,'_', lambda
                   ,'_', ntree
                   ,'_', mtry
                   ,'.RData'
                   )
    path
  }

  PossibleNdays <- function() {
    # return vector of all possible values for ndays ('30', '60', ..., '360')
    ndays = c('30', '60', '90', '120', '150', '180', '210', '240', '270', '300', '330', '360')
    ndays
  }

  PossiblePredictorsNamess <- function() {
    # return vector of all possible values for predictorsName
    predictors.names <- c( 'always', 'alwaysNoAssessment', 'alwaysNoCensus'
                          ,'best01', 'best02', 'best03', 'best04', 'best05', 'best06'
                          ,'best07', 'best08', 'best09', 'best10', 'best11', 'best12'
                          ,'best13', 'best14', 'best15', 'best16', 'best17', 'best18'
                          ,'best19', 'best20', 'best21', 'best22', 'best23', 'best24'
                          ,'pca01',  'pca02',  'pca03',  'pca04'
                          #,'best20census', 'best20city', 'best20zip'
                          ,'best15census', 'best15city', 'best15zip'
                          )
    predictors.names
  }

  Validate.Cell.Specifiers <- function( scope, model, timePeriod, scenario
                                       ,response, predictorsName, predictorsForm, ndays
                                       ,query, lambda, ntree, mtry) {
    # stop if any cell specifier is invalide

    stopifnot(Is.Valid.Scope(scope))
    stopifnot(Is.Valid.Model(model))
    stopifnot(Is.Valid.TimePeriod(timePeriod))
    stopifnot(Is.Valid.Scenario(scenario))
    stopifnot(Is.Valid.Response(response))
    stopifnot(Is.Valid.PredictorsName(predictorsName))
    stopifnot(Is.Valid.PredictorsForm(predictorsForm))
    stopifnot(Is.Valid.Ndays(ndays))
    stopifnot(Is.Valid.Query(query))
    stopifnot(Is.Valid.Lambda(lambda))
    stopifnot(Is.Valid.Ntree(ntree))
    stopifnot(Is.Valid.Mtry(mtry))
  }
                          

  
  list( Command                  = Command
       ,Path                     = Path
       ,PossibleNdays            = PossibleNdays
       ,PossiblePredictorsNamess = PossiblePredictorsNamess
       )
}
