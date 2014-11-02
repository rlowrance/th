Predictors2 <- function(predictors.name, predictors.form) {
    # return character vector of predictor names
    # predictor.name : chr, of one always | alwaysNoassessment
    # predictor.form : chr, one of level | log
    #                  where log means use the log of size variables

    # taxonomy of features
    # house
    # - from the assessment
    # - not from the assessment
    # location
    # - derived from a decenial census
    # - derived from a zip code

    # features are further divided into
    # - size (log(price) ~ log(size) could reasonably be linear)
    #   . always positive (so that log(x) is always defined
    #   . always non-negative (so that log1p(x) is always defined)
    # - not size

    # always := the feature is present in every transaction in subset1

    always.house.assessment.size.positive <- 
        c( 'improvement.value'
          ,'land.value'
          )
    always.house.assessment.size.non.negative <- 
        c(
          )
    always.house.assessment.not.size <- 
        c( 'fraction.improvement.value')
    always.house.not.assessment.size.positive <- 
        c( 'land.square.footage'
          ,'living.area')
    always.house.not.assessment.size.non.negative <- 
        c( 'basement.square.feet'
          ,'bathrooms'
          ,'bedrooms'
          ,'fireplace.number'
          ,'parking.spaces'
          ,'stories.number'
          ,'total.rooms'
          )
    always.house.not.assessment.not.size <- 
        c( 'effective.year.built'
          ,'factor.has.pool'
          ,'factor.is.new.construction'
          ,'year.built'
          )
    always.location.census <- 
        c( 'avg.commute.time'
          ,'census.tract.has.industry'
          ,'census.tract.has.park'
          ,'census.tract.has.retail'
          ,'census.tract.has.school'
          ,'fraction.owner.occupied'
          ,'median.household.income'
          )
    always.location.zip <- 
        c( 'zip5.has.industry'
          ,'zip5.has.park'
          ,'zip5.has.retail'
          ,'zip5.has.school'
          )
    identification <- 
        c( 'recordingDate'
          ,'saleDate'
          ,'apn'
          ,'census.tract'
          ,'zip5'
          )
    price <- 
        c( 'price'
          ,'price.log'
          )



    Log <- function(v) {
        # list of names in log space
        sapply(v, function(name) sprintf('%s.log', name))
    }
    Log1p <- function(v) {
        # list of names in log1p space
        sapply(v, function(name) sprintf('%s.log1p', name))
    }

    #cat('Predictors2 args:', predictors.name, predictors.form, '\n')
    result.named <-
        if (predictors.name == 'price') {
            price
        } else if (predictors.name == 'identification') {
            identification
        } else if (predictors.name == 'always' && predictors.form == 'level') {
            c( always.house.not.assessment.size.positive
              ,always.house.not.assessment.size.non.negative
              ,always.house.not.assessment.not.size
              ,always.house.assessment.size.positive
              ,always.house.assessment.size.non.negative
              ,always.house.assessment.not.size
              ,always.location.census
              ,always.location.zip
              )
        } else if (predictors.name == 'always' && predictors.form == 'log') {
            c( Log(always.house.not.assessment.size.positive)
              ,Log1p(always.house.not.assessment.size.non.negative)
              ,always.house.not.assessment.not.size
              ,Log(always.house.assessment.size.positive)
              ,Log1p(always.house.assessment.size.non.negative)
              ,always.house.assessment.not.size
              ,always.location.census
              ,always.location.zip
              )
        } else if (predictors.name == 'alwaysNoAssessment' && predictors.form == 'level') {
            c( always.house.not.assessment.size.positive
              ,always.house.not.assessment.size.non.negative
              ,always.house.not.assessment.not.size
              ,always.location.census
              ,always.location.zip
              )
        } else if (predictors.name == 'alwaysNoAssessment' && predictors.form == 'log') {
            c( Log(always.house.not.assessment.size.positive)
              ,Log1p(always.house.not.assessment.size.non.negative)
              ,always.house.not.assessment.not.size
              ,always.location.census
              ,always.location.zip
              )
        } else if (predictors.name == 'alwaysNoCensus' && predictors.form == 'level') {
            c( always.house.not.assessment.size.positive
              ,always.house.not.assessment.size.non.negative
              ,always.house.not.assessment.not.size
              ,always.location.zip
              )
        } else if (predictors.name == 'alwaysNoCensus' && predictors.form == 'log') {
            c( Log(always.house.not.assessment.size.positive)
              ,Log1p(always.house.not.assessment.size.non.negative)
              ,always.house.not.assessment.not.size
              ,always.location.zip
              )
        } else {
            print(predictors.name)
            print(predictors.form)
            stop('bad arguments')
        }
    result.list <- unname(result.named)
    result.vector <- sapply(result.list, function(x) x)
} 

Predictors2Test <- function() {
    # unit test
    # for now, simply test that everything runs to completion
    verbose <- FALSE
    Test <- function(predictors.name, predictors.form = NULL) {
        value <- Predictors2(predictors.name, predictors.form)
        if (verbose) {
            cat(predictors.name, predictors.form, '\n')
            cat(sprintf( 'predictors.name %s predictors.form %s\n'
                        ,predictors.name
                        ,as.character(predictors.form)
                        )
            )
            print(value)
            cat('number of features:', length(value), '\n')
            cat('\n')
            browser()
        }
    }
    Test('price')
    Test('identification')
    Test('always', 'level')
    Test('always', 'log')
    Test('alwaysNoAssessment', 'level')
    Test('alwaysNoAssessment', 'log')
    Test('alwaysNoCensus', 'level')
    Test('alwaysNoCensus', 'log')
}

Predictors2Test()
