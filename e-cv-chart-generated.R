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

    cv.cell <- CvCell(validate.cell.specifiers = if(control$testing) FALSE else TRUE)
    Path <- cv.cell$Path
    Command <- cv.cell$Command

    PathWithFold <- function(path, fold) {
        result <- paste0( path
                         ,'.fold_'
                         ,as.character(fold)
                         )
        result
    }
    CommandWithFold <- function(command, fold) {
        result <- paste0( command
                         ,' --fold '
                         ,as.character(fold)
                         )
        result
    }
    DefineCells <- function(targets, target.variable.name, system.id, path, num.folds) {
        if (num.folds == 0) {
            line <- paste0( target.variable.name
                            ,'-cells-'
                            ,system.id
                            ,' += '
                            ,path
                            )
            targets$Append(line)
        } else {
            for (fold in 1:num.folds) {
                path.with.fold <- PathWithFold(path, fold)
                line <- paste0( target.variable.name
                               ,'-cells-',
                               system.id
                               ,' += '
                               ,path.with.fold
                               )
                targets$Append(line)
            }
        }
    }
    DefineTarget <- function(lines, target.variable.name, system.id) {
        target.variable.name.with.system.id <- paste0( target.variable.name
                                                      ,'-'
                                                      ,system.id
                                                      )
        line <- paste0( '.PHONY : '
                       ,target.variable.name.with.system.id
                       )
        line <- paste0(target.variable.name.with.system.id
                       ,': $('
                       ,target.variable.name
                       ,'-cells-'
                       ,system.id
                       ,')'
                       )
        lines$Append(line)
    }
    OneTarget <- function(target.variable.name, dependency.file.names, num.folds) {
        # return list
        # $path.command: list() with name $path and value command
        # $targets: Lines () object

        if (FALSE && num.folds > 0) {
            cat('target.variable.name', target.variable.name, '\n'); browser()
        }

        targets <- Lines()
        thread.number <- 0
        path.command.nfold <- NULL
        for (dfn in dependency.file.names) {
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

            path.command.nfold[[path]] <- list(command = command, nfold = num.folds)

            # 12 threads on R's system, 8 on J's
            # assign all folds for one path/command to the same system
            thread.number <-
                if (thread.number == 20)
                    1
                else
                    thread.number + 1
            #cat('thread.number', thread.number, '\n'); browser()
            if (thread.number <= 12) {
                DefineCells(targets, target.variable.name, 'r', path, num.folds)
            } else {
                DefineCells(targets, target.variable.name, 'j', path, num.folds)
            }
            DefineCells(targets, target.variable.name, 'all', path, num.folds)
            #print(tail(rules$Get())); print(tail(targets$Get())); browser()
        }
        DefineTarget(targets, target.variable.name, 'r')
        DefineTarget(targets, target.variable.name, 'j')
        DefineTarget(targets, target.variable.name, 'all')
        result <- list( path.command = path.command.nfold
                       ,targets = targets
                       )
        result
    }


    all.path.command.nfold <- NULL
    all.targets <- Lines()
    M <- function(target.variable.name, dependency.file.names, num.folds = 0) {
        one.target <- OneTarget(target.variable.name, dependency.file.names, num.folds)
        if (FALSE && num.folds > 0) {
            cat('in M examine one.target\n'); browser()
        }
        all.path.command.nfold <<- c(all.path.command.nfold, one.target$path.command)
        lapply(one.target$targets$Get(), function(line) all.targets$Append(line))
    }
    if (control$testing) {
        browser()
        fixed <- list( model = 'a'
                      ,timePeriod = 'b'
                      ,scenario = 'c'
                      ,response = 'd'
                      ,predictorsName = 'e'
                      ,predictorsForm = 'f'
                      ,ndays = 'g'
                      ,query = 'h'
                      ,lambda = 'i'
                      ,ntree = 'j'
                      ,mtry = 'k'
                      )
        TestNoFolds <- function(control) {
            element <- fixed
            element[['scope']] <- 'nofolds'
            result <- list(element)
            result
        }
        TestFolds <- function(control) {
            element <- fixed
            element[['scope']] <- 'folds'
            result <- list(element)
            result
        }
        M( target.variable.name = 'target-no-folds',
          ,dependency.file.names = TestNoFolds(control)
          )
        M( target.variable.name = 'target-folds'
          ,dependency.file.name = TestFolds(control)
          ,num.folds = 2
          )
    } else {
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
          ,num.folds = 10
          )
        M( target.variable.name = 'e-cv-chart-chart14'
          ,dependency.file.names = Chart.14.FileDependencies(control)
          ,num.folds = 10
          )
    }

    GenerateMakefile <- function(all.path.command.nfold, all.targets) {
        # return a Lines() object containing all lines in the makefile
        all.lines <- Lines()

        # comments for human readers
        all.lines$Append('# rules and targets to make cv cells needed by charts produced by e-cv-chart.r')

        all.lines$Append('# to generate files needed for Chart 12, execute')
        all.lines$Append('#   make -j 12 e-cv-chart-chart12-all')
        all.lines$Append('# to generate files needed for Chart 12 using judith"s system, execute')
        all.lines$Append('#   make -j 8 e-cv-chart-chart12-j')

        all.lines$Append(' ')
        all.lines$Append('# generated by Rscript e-cv-chart.R --makefile')
        all.lines$Append(paste0('# run ', Sys.time()))
    
        # rules and recipes
        all.lines$Append(' ')
        all.lines$Append('# rules and recipes')
        unique.paths <- unique(names(all.path.command.nfold))
        for (path in unique.paths) {
            command.nfold <- all.path.command.nfold[[path]]
            command <- command.nfold$command
            nfold <- command.nfold $nfold
            if (nfold == 0) {
                dependency <- paste0(path, ' :')
                recipe     <- paste0('\t', command)
                all.lines$Append(dependency)
                all.lines$Append(recipe)
            }
            else {
                for (fold in 1:nfold) {
                    path.with.fold <- PathWithFold(path, fold)
                    command.with.fold <- CommandWithFold(command, fold)
                    dependency <- paste0(path.with.fold, ' :')
                    recipe <- paste0('\t', command.with.fold)
                    all.lines$Append(dependency)
                    all.lines$Append(recipe)
                }
                # now recreate rule and recipe for the combination
                dependency.first <- paste0(path, ' : \\')  # end with backslash, to continue
                all.lines$Append(dependency.first)
                for (fold in 1:nfold) {
                    dependency.next <- paste0(' '
                                              ,PathWithFold(path, fold)
                                              ,' \\'
                                              )
                    all.lines$Append(dependency.next)
                }
                recipe <- paste0('\t', command, ' --fold combine')
                all.lines$Append(recipe)
            }
        }
    
        # targets (to allow data for individual charts to be created and recreated)
        all.lines$Append(' ')
        all.lines$Append('# targets')
        lapply(all.targets$Get(), function(line) all.lines$Append(line))

        all.lines
    }
    
    makefile.lines <- GenerateMakefile(all.path.command.nfold, all.targets)

    writeLines( text = makefile.lines$Get()
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
