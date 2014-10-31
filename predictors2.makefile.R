# predictors2.makefile.R
# generate the makefile predictors2.makefile

source('Predictors2.R')

Lines <- function() {
    # class object
    lines <- rep('', 1000)
    last.line.index <- 0

    Append <- function(line) {
        last.line.index <<- last.line.index + 1
        lines[[last.line.index]] <<- line
    }

    Get <- function() {
        lines[1:last.line.index]
    }

    list( Append = Append
         ,Get = Get
         )
}

lines <- Lines()

AppendLine <- function(line) {
    lines$Append(line)
}
AppendPair <- function(name, file.stem) {
    line <- sprintf('predictors2.%s += $(splits)/%s.RData', name, file.stem)
    lines$Append(line)
}
AppendTriple <- function(name, form, file.stem) {
    line <- sprintf('predictors2.%s.%s += $(splits)/%s.RData', name, form, file.stem)
    lines$Append(line)
}
AppendPredictors1 <- function(predictors.name) {
    predictors <- Predictors2(predictors.name)
    for (predictor in predictors) {
        AppendPair(predictors.name, predictor)
    }
}
AppendPredictors2 <- function(predictors.name, predictors.form) {
    predictors <- Predictors2(predictors.name, predictors.form)
    for (predictor in predictors) {
        AppendTriple(predictors.name, predictors.form, predictor)
    }
}

AppendPredictors1('identification')
AppendLine(' ')
AppendPredictors1('price')
AppendLine(' ')
AppendPredictors2('always', 'level')
AppendLine(' ')
AppendPredictors2('always', 'log')
AppendLine(' ')
AppendPredictors2('alwaysNoAssessment', 'level')
AppendLine(' ')
AppendPredictors2('alwaysNoAssessment', 'log')
AppendLine(' ')
AppendLine('predictors2.always += $(predictor2.always.level)')
AppendLine(' ')
AppendLine('predictors2.always += $(predictor2.always.log)')
AppendLine(' ')
AppendLine('predictors2.alwaysNoAssessment += $(predictor2.alwaysNoAssessment.level)')
AppendLine(' ')
AppendLine('predictors2.alwaysNoAssessment += $(predictor2.alwaysNoAssessment.log)')
AppendLine(' ')
AppendLine('predictors2.price += $(splits)/price.RData')
AppendLine('predictors2.price.log += $(splits)/price.log.RData')

browser()
writeLines( text = lines$Get()
           ,con = 'predictors2.makefile'
           ,sep = '\n'
           )

cat('done\n')
