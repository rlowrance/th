# e-cv-chart-generated.R  
# main program
# Create file e-cv-generated.makefile
# containing
# - rules to make all the cells (but without any dependencies)
# - targets to allow manual rebuilding of all of the rules
#   the targets are for now written to run on multiple systems
#
# Command line arguments: NONE

source('Directory.R')
source('Libraries.R')

source('CvCell.R')
source('Lines.R')

#library(boot)
#library(ggplot2)
#library(optparse)
#library(memoise)

Control <- function(default.args) {
    # parse command line arguments in command.args
    stopifnot(is.null(default.args))

    me <- 'e-cv-chart-generated' 

    log <- Directory('log')
    working <- Directory('working')
    cells <- paste0(working, 'e-cv-cells/')

    control <- list( path.out.log = paste0(log, me, '.log')
                    ,path.out.makefile = paste0(me, '.makefile')
                    ,path.in.chart9.features = paste0(working, 'e-features-lcv2.txt')
                    ,path.in.submarkets = paste0(working, 'submarkets.RData')
                    ,testing = FALSE
                    ,debug = FALSE
                    ,me = me
                    )
    control
}
Chart.5.FileDependencies <- function(my.control) {
    # return list of file names used to construct chart 5 
    # predictorsName in {always, alwaysNoAssesment}
    # timePeriod 2008

    fixed <- CvCell()$FixedCellValues('Chart5')
    result <- NULL
    for (response in c('price', 'logprice')) {
        for (predictorsName in c('always', 'alwaysNoAssessment')) {
            for (predictorsForm in c('level', 'log')) {
                for (ndays in CvCell()$Possible.Ndays()) {
                    element <- list( scope = fixed$scope
                                    ,model = fixed$model
                                    ,timePeriod = fixed$timePeriod
                                    ,scenario = fixed$scenario
                                    ,response = response
                                    ,predictorsName = predictorsName
                                    ,predictorsForm = predictorsForm
                                    ,ndays = ndays
                                    ,query = fixed$query
                                    ,lambda = fixed$lambda
                                    ,ntree = fixed$ntree
                                    ,mtry = fixed$mtry
                                    )
                    result[[length(result) + 1]] <- element
                }
            }
        }
    }
    result
}
Chart.6.FileDependencies <- function(my.control) {
    # return list of file names used to construct chart 6
    # predictorsName in {alwaysNoAssesment, alwaysNoCensus}
    # timePeriod 2003on

    fixed <- CvCell()$FixedCellValues('Chart6')

    result <- NULL
    for (response in c('price', 'logprice')) {
        for (predictorsName in c('alwaysNoAssessment', 'alwaysNoCensus')) {
            for (predictorsForm in c('level', 'log')) {
                for (ndays in CvCell()$Possible.Ndays()) {
                    element <- list( scope = fixed$scope
                                    ,model = fixed$model
                                    ,timePeriod = fixed$timePeriod
                                    ,scenario = fixed$scenario
                                    ,response = response
                                    ,predictorsName = predictorsName
                                    ,predictorsForm = predictorsForm
                                    ,ndays = ndays
                                    ,query = fixed$query
                                    ,lambda = fixed$lambda
                                    ,ntree = fixed$ntree
                                    ,mtry = fixed$mtry
                                    )
                    result[[length(result) + 1]] <- element
                }
            }
        }
    }
    result
}
Chart.7.FileDependencies <- function(my.control) {
    # return list of file names used to construct chart 7 

    fixed <- CvCell()$FixedCellValues('Chart7')

    result <- NULL
    for (response in c('price', 'logprice')) {
        for (predictorsName in 'alwaysNoAssessment') {
            for (predictorsForm in c('level', 'log')) {
                for (ndays in CvCell()$Possible.Ndays()) {
                    element <- list( scope = fixed$scope
                                    ,model = fixed$model
                                    ,timePeriod = fixed$timePeriod
                                    ,scenario = fixed$scenario
                                    ,response = response
                                    ,predictorsName = predictorsName
                                    ,predictorsForm = predictorsForm
                                    ,ndays = ndays
                                    ,query = fixed$query
                                    ,lambda = fixed$lambda
                                    ,ntree = fixed$ntree
                                    ,mtry = fixed$mtry
                                    )
                    result[[length(result) + 1]] <- element
                }
            }
        }
    }
    result
}
Chart.8.FileDependencies <- function(my.control) {
    result <- Chart.7.FileDependencies(my.control)
    result
}
Chart.9.FileDependencies <- function(my.control) {
    # return list of file names used to construct chart 9
    # this is just for the best form and ndays
    # best for is log-level
    # best ndays is 60 

    Chart.9.PredictorsNames <- function(control) {
        # return list of predictor names for chart 9
        # these have the form best01, best02, ..., best24
        ordered.features <- readLines(con = control$path.in.chart9.features)
        cvcell.predictors.names <- CvCell()$Possible.PredictorsNames()
        stopifnot(length(ordered.features) != length(cvcell.predictors.names))
        predictorsNames <- sapply( 1:length(ordered.features)
                                  ,function(n) 
                                      sprintf('best%02d', n)
                                  )
        # verify that each of the predictors was expected
        lapply( predictorsNames
               ,function(predictorsName) is.element(predictorsName, cvcell.predictors.names)
               )
        predictorsNames
    }

    fixed <- CvCell()$FixedCellValues('Chart9')

    result <- NULL
    for (predictorsName in Chart.9.PredictorsNames(my.control)) {
        element <- list( scope = fixed$scope
                        ,model = fixed$model
                        ,timePeriod = fixed$timePeriod
                        ,scenario = fixed$scenario
                        ,response = fixed$response
                        ,predictorsName = predictorsName
                        ,predictorsForm = fixed$predictorsForm
                        ,ndays = fixed$ndays
                        ,query = fixed$query
                        ,lambda = fixed$lambda
                        ,ntree = fixed$ntree
                        ,mtry = fixed$mtry
                        )
        result[[length(result) + 1]] <- element
    }
    result
}
Chart.10.FileDependencies <- function(my.control) {
    # return list of file names used to construct chart 10

    Chart.10.PredictorsNames <- function() {
        # return vector of predictor names used in chart 10
        result <- sprintf('pca%02d', 1:4)
        result
    }

    fixed <- CvCell()$FixedCellValues('Chart10')

    result <- NULL
    for (predictorsName in Chart.10.PredictorsNames()) {
        element <- list( scope = fixed$scope
                        ,model = fixed$model
                        ,timePeriod = fixed$timePeriod
                        ,scenario = fixed$scenario
                        ,response = fixed$response
                        ,predictorsName = predictorsName
                        ,predictorsForm = fixed$predictorsForm
                        ,ndays = fixed$ndays
                        ,query = fixed$query
                        ,lambda = fixed$lambda
                        ,ntree = fixed$ntree
                        ,mtry = fixed$mtry
                        )
        result[[length(result) + 1]] <- element
    }
    result
}
Chart.11.FileDependencies <- function(my.control) {
    # return list of file names used to construct chart 11

    Chart.11.PredictorsNames <- function() {
        # return vector of predictor names used in chart 10
        result <- c( 'best08'
                    ,'best19'
                    ,'best20'
                    ,'best24'
                    ,'pca01'
                    ,'pca02'
                    ,'pca03'
                    ,'pca04'
                    )
        result
    }

    fixed <- CvCell()$FixedCellValues('Chart11')

    result <- NULL
    for (predictorsName in Chart.11.PredictorsNames()) {
        element <- list( scope = fixed$scope
                        ,model = fixed$model
                        ,timePeriod = fixed$timePeriod
                        ,scenario = fixed$scenario
                        ,response = fixed$response
                        ,predictorsName = predictorsName
                        ,predictorsForm = fixed$predictorsForm
                        ,ndays = fixed$ndays
                        ,query = fixed$query
                        ,lambda = fixed$lambda
                        ,ntree = fixed$ntree
                        ,mtry = fixed$mtry
                 )
        result[[length(result) + 1]] <- element
    }
    result
}
Chart.12.FileDependencies <- function(my.control) {
    # return list of file names used to construct chart 11

    Chart.12.Lambda.Values <- function() {
        # return vector of lambda values that are used for regularization
        # lambda on command lines is 100 * lambda in regression
        lambda.in.regression <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 30)
        lambda.on.command.line <- as.character(100 * lambda.in.regression)
        lambda.on.command.line
    }

    fixed <- CvCell()$FixedCellValues('Chart12')

    result <- NULL
    for (lambda in Chart.12.Lambda.Values()) {
        element <- list( scope = fixed$scope
                        ,model = fixed$model
                        ,scenario = fixed$scenario
                        ,timePeriod = fixed$timePeriod
                        ,response = fixed$response
                        ,predictorsName = fixed$predictorsName
                        ,predictorsForm = fixed$predictorsForm
                        ,ndays = fixed$ndays
                        ,query = fixed$query
                        ,lambda = lambda
                        ,ntree = fixed$ntree
                        ,mtry = fixed$mtry
                        )
        result[[length(result) + 1]] <- element
    }
    result
}
Chart.13.FileDependencies <- function(control) {
    # return list of all combinations

    fixed <- CvCell()$FixedCellValues('Chart13')

    result <- NULL
    GenerateIndicatorElements <- function() {
        for (predictorsName in c( 'best20zip'
                                 ,'best20census'
                                 ,'best20city'
                                 )) {
            element <-list( scope = 'global'
                           ,model = fixed$model
                           ,timePeriod = fixed$timePeriod
                           ,scenario = fixed$scenario
                           ,response = fixed$response
                           ,predictorsName = predictorsName
                           ,predictorsForm = fixed$predictorsForm
                           ,ndays = fixed$ndays
                           ,query = fixed$query
                           ,lambda = fixed$lambda
                           ,ntree = fixed$ntree
                           ,mtry = fixed$mtry
                           )
            result[[length(result) + 1]] <<- element
        }
    }
    GenerateIndicatorElements()

    GenerateSubmarketElements <- function() {
        loaded <- load(file = control$path.in.submarkets)
        for (scope in c(codes.census.tract, codes.property.city, codes.zip5)) {
            element <-list( scope = scope
                           ,model = fixed$model
                           ,timePeriod = fixed$timePeriod
                           ,scenario = fixed$scenario
                           ,response = fixed$response
                           ,predictorsName = 'best20'
                           ,predictorsForm = fixed$predictorsForm
                           ,ndays = fixed$ndays
                           ,query = '1'  # 100 percent sample (each scope tends to be small)
                           ,lambda = fixed$lambda
                           ,ntree = fixed$ntree
                           ,mtry = fixed$mtry
                           )
            result[[length(result) + 1]] <<- element
        }
    }
    GenerateSubmarketElements()
    result
}
Chart.14.FileDependencies <- function(control) {
    # return list of all combinations

    fixed <- CvCell()$FixedCellValues('Chart14')

    result <- NULL
    # generate in order so that the longest to run are specified first
    # that's because the makefile runs the jobs in parallel, first to last
    for (predictorsName in c('always', 'best20')) {
        for (ntree in c('1000', '300', '100', '1')) {
            for (mtry in c('4', '3', '2', '1')) {
                element <-list( scope = fixed$scope
                               ,model = fixed$model
                               ,scenario = fixed$scenario
                               ,timePeriod = fixed$timePeriod
                               ,response = fixed$response
                               ,predictorsName = predictorsName
                               ,predictorsForm = fixed$predictorsForm
                               ,ndays = fixed$ndays
                               ,query = fixed$query
                               ,lambda = fixed$lambda
                               ,ntree = ntree
                               ,mtry = mtry
                               )
                result[[length(result) + 1]] <- element
            }
        }
    }
    result
}
MakeMakefiles <- function(control) {
    # write a makefile

    Path <- CvCell()$Path
    Command <- CvCell()$Command

    targets.lines <- Lines()
    rules.lines <- Lines()

    
    num.threads <- 20  # 8 on J's system, 12 on R's system
    thread.number <- 0
    targets.lines <- Lines()
    all.dependency.file.names <- NULL
    M <- function(target.variable.name, dependency.file.names) {
        # append to all.dependency.file.names
        # build targets.lines
        for (dfn in dependency.file.names) {
            all.dependency.file.names[[length(all.dependency.file.names) + 1]] <<- dfn
            path <- Path( scope = dfn$scope
                         ,model = dfn$model
                         ,timePeriod = dfn$timePeriod
                         ,scenario = dfn$scenario
                         ,response = dfn$response
                         ,predictorsName = dfn$predictorsName
                         ,predictorsForm = dfn$predictorsForm
                         ,ndays = dfn$ndays
                         ,query = dfn$query
                         ,lambda = dfn$lambda
                         ,ntree = dfn$ntree
                         ,mtry = dfn$mtry
                         )

            # distribute the jobs over the threads
            thread.number <- thread.number + 1 
            if (thread.number > num.threads)
                thread.number <- 1 
            targets.lines$Append(sprintf('%s-thread-%d += %s'
                                         ,target.variable.name
                                         ,thread.number
                                         ,path
                                         ))
        }
        # assign 8 threads to J's system and 12 threads to R's
        CreateTarget <- function(tag, threads) {
            system.target.name <- sprintf('%s-target-%s'
                                          ,target.variable.name
                                          ,tag
                                          )
            targets.lines$Append(sprintf('.PHONY: %s'
                                         ,system.target.name
                                         ))
            uses <- ''
            for (thread.index in threads) {
                uses <- sprintf('%s $(%s)'
                                ,uses
                                ,sprintf('%s-thread-%d'
                                         ,target.variable.name
                                         ,thread.index
                                         )
                                )
            }
            targets.lines$Append(sprintf('%s : %s'
                                         ,system.target.name
                                         ,uses
                                         ))
        }
        CreateTarget('r', 1:12)  # R runs threads 1 - 12
        CreateTarget('j', 13:20) # J runs threads 13 - 20
        CreateTarget('all', 1:20)
        targets.lines
        # return nothing
        NULL
    }

    M( target.variable.name = 'e-cv-chart-chart5'
      ,dependency.file.names = Chart.5.FileDependencies(control)
      )
    M( target.variable.name = 'e-cv-chart-chart6'
      ,dependency.file.names = Chart.6.FileDependencies(control)
      )
    M( target.variable.name = 'e-cv-chart-chart7'
      ,dependency.file.names = Chart.7.FileDependencies(control)
      )
    M( target.variable.name = 'e-cv-chart-chart8'
      ,dependency.file.names = Chart.8.FileDependencies(control)
      )
    M( target.variable.name = 'e-cv-chart-chart9'
      ,dependency.file.names = Chart.9.FileDependencies(control)
      )
    M( target.variable.name = 'e-cv-chart-chart10'
      ,dependency.file.names = Chart.10.FileDependencies(control)
      )
    M( target.variable.name = 'e-cv-chart-chart11'
      ,dependency.file.names = Chart.11.FileDependencies(control)
      )
    M( target.variable.name = 'e-cv-chart-chart12'
      ,dependency.file.names = Chart.12.FileDependencies(control)
      )
    M( target.variable.name = 'e-cv-chart-chart13'
      ,dependency.file.names = Chart.13.FileDependencies(control)
      )
    M( target.variable.name = 'e-cv-chart-chart14'
      ,dependency.file.names = Chart.14.FileDependencies(control)
      )

    RulesRecipes <- function(all.dependency.file.names) {
        # return Lines object containing unique rules and recipes
        # NOTE: all.dependency.file.name may contain the same value many times
        # because the same cell is used in many charts
        u <- unique(all.dependency.file.names)  # does this work on lists?
        #print(length(all.dependency.file.names))
        #print(length(u))
        stopifnot(length(u) < length(all.dependency.file.names))
        rules.recipes <- Lines()
        for (dfn in u) {
            path <- Path( scope = dfn$scope
                         ,model = dfn$model
                         ,timePeriod = dfn$timePeriod
                         ,scenario = dfn$scenario
                         ,response = dfn$response
                         ,predictorsName = dfn$predictorsName
                         ,predictorsForm = dfn$predictorsForm
                         ,ndays = dfn$ndays
                         ,query = dfn$query
                         ,lambda = dfn$lambda
                         ,ntree = dfn$ntree
                         ,mtry = dfn$mtry
                         )
            command <- Command( scope = dfn$scope
                               ,model = dfn$model
                               ,timePeriod = dfn$timePeriod
                               ,scenario = dfn$scenario
                               ,response = dfn$response
                               ,predictorsName = dfn$predictorsName
                               ,predictorsForm = dfn$predictorsForm
                               ,ndays = dfn$ndays
                               ,query = dfn$query
                               ,lambda = dfn$lambda
                               ,ntree = dfn$ntree
                               ,mtry = dfn$mtry
                               )
            # don't depend on anything, so RUN THESE RULES MANUUALLY
            rules.recipes$Append(paste0(path ,':'))
            #rules.recipes$Append(paste0(path ,': e-cv.R $(e-cv-source) $(e-cv-data)'))
            rules.recipes$Append(paste0('\t', command))
        }
        rules.recipes
    }

    # targets.lines and all.dependency.file.names
    # now combine them into one makefile

    all.lines <- Lines()
    # comments for human readers
    all.lines$Append('# rules and targets to make cv cells needed by charts produced by e-cv-chart.r')
    all.lines$Append('# to generate files needed for Chart 12, execute')
    all.lines$Append('#   make -j 12 e-cv-chart-chart12')
    all.lines$Append(' ')
    all.lines$Append('# generated by Rscript e-cv-chart.R --makefile')
    all.lines$Append(paste0('# run ', Sys.time()))
    
    # rules and recipes
    all.lines$Append(' ')
    all.lines$Append('# rules and recipes')
    rules.recipes <- RulesRecipes(all.dependency.file.names)
    lapply(rules.recipes$Get(), function(line) all.lines$Append(line))
    
    # targets (to allow data for individual charts to be created and recreated)
    all.lines$Append(' ')
    all.lines$Append('# targets')
    lapply(targets.lines$Get(), function(line) all.lines$Append(line))

    writeLines( text = all.lines$Get()
               ,con = control$path.out.makefile
               )
}
Main <- function(control) {
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)

    MakeMakefiles(control)

    str(control)
}


### Execution starts here

default.args <- NULL

control <- Control(default.args)

Main(control)
if (control$testing)
    cat('DISCARD RESULTS: TESTING\n')
cat('done\n')
