CvCell <- function(validate.cell.specifiers = TRUE) {
  # list of functions for working with e-cv-cells
  # $Command(scope, model, ..., mtry) --> chr, Rscript -e-cv.R --scope SCOPE ... --mtry MTRY
  # $Path(scope, model, ..., mtry) --> chr, path to WORKING/e-cv-cells/FILE.Rdata
  # $PossibleNdays() --> chr vector, all possible values for ndays, namely '30' .. '360'
  # $PossiblePredictorsNamess() --> chr vector, all possible values for predictorsName, 'always', ...
  # $FixedCellValues(chart.name) --> list of any 12 cell attributes that are common

  InRange <- function(s, low, high) {
    num <- as.numeric(s)

    if (num >= low && num <= high) {
      #cat('in range', s, '\n')
      TRUE
    } else {
      cat('not in range', s, '\n')
      FALSE
    }
  }

  IsValidScope <- function(s)          is.character(s)
  IsValidModel <- function(s)          s %in% c('linear', 'linL2', 'rf')
  IsValidTimePeriod <- function(s)     s %in% c('2003on', '2008')
  IsValidScenario <- function(s)       s %in% c('assessor', 'avm', 'mortgage')
  IsValidResponse <- function(s)       s %in% c('logprice', 'price')
  IsValidPredictorsName <- function(s) s %in% PossiblePredictorsNamess()
  IsValidPredictorsForm <- function(s) s %in% c('level', 'log')
  IsValidNdays <- function(s)          InRange(s, 1, 360)
  IsValidQuery <- function(s)          s %in% c('1', '20', '100')
  IsValidLambda <- function(s)         is.character(s) && as.integer(s) >= 0
  IsValidNtree <- function(s)          is.character(s) && as.integer(s) >= 0
  IsValidMtry <- function(s)           is.character(s) && as.integer(s) >= 0

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

  Parse <- function(file.name) {
    # return list of components
    str.split <- strsplit( x = file.name
                          ,split = '.'
                          ,fixed = TRUE
                          )
    base.name <- str.split[[1]][[1]]
    components <- strsplit( x = base.name
                           ,split = '_'
                           ,fixed = TRUE
                           )
    component <- components[[1]]
    result <-
      list( scope = component[[1]]
           ,model = component[[2]]
           ,timePeriod = component[[3]]
           ,scenario = component[[4]]
           ,response = component[[5]]
           ,predictorsName = component[[6]]
           ,predictorsForm = component[[7]]
           ,ndays = component[[8]]
           ,query = component[[9]]
           ,lambda = component[[10]]
           ,ntree = component[[11]]
           ,mtry = component[[12]]
           )
    result
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

    stopifnot(IsValidScope(scope))
    stopifnot(IsValidModel(model))
    stopifnot(IsValidTimePeriod(timePeriod))
    stopifnot(IsValidScenario(scenario))
    stopifnot(IsValidResponse(response))
    stopifnot(IsValidPredictorsName(predictorsName))
    stopifnot(IsValidPredictorsForm(predictorsForm))
    stopifnot(IsValidNdays(ndays))
    stopifnot(IsValidQuery(query))
    stopifnot(IsValidLambda(lambda))
    stopifnot(IsValidNtree(ntree))
    stopifnot(IsValidMtry(mtry))
  }
                          

  
  list( Command                  = Command
       ,Parse                    = Parse
       ,Path                     = Path
       ,PossibleNdays            = PossibleNdays
       ,PossiblePredictorsNamess = PossiblePredictorsNamess
       )
}
