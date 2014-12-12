# main program to generate file chart14-target.makefile
# containing two targets:
#  chart14-targets-R, designed to run on system R
#  chart14-targets-J, designed to run on system J
# for now, just generated random targets

source('Directory.R')
source('Libraries.R')

source('Chart14.R')
source('CvCell.R')
source('Lines.R')

Control <- function() {
    me <- 'chart14-targets'

    log <- Directory('log')
    working <- Directory('working')
    src <- Directory('source')

    control <-
        list( path.out.log = paste0(log, me, '.log')
             ,path.out = paste0(src, '/', me, '.makefile')
             ,random.seed  = 123
             ,num.hyperparameter.sets = 100
             ,testing      = FALSE
             ,bestN.range = list(low = 1, high = 24)
             ,ntree.range = list(low = 100, high = 5000)
             ,ndays.range = list(low = 1, high = 100)
             )
    control
}

Exp10 <- function(x) 10 ^ x

RandomTarget <- function(control) {
    # return function Next() --> list $bestN $mtry $tree $ndays
    force(control)
    Next <- function() {
        # return list $bestN $mtry $tree $ndays
        bestN <- round(runif( n = 1
                             ,min = control$bestN$low
                             ,max = control$bestN$high
                             )
        )

        # test mtry as sqrt(bestN), plus half and double
        mtry.1 <- round(sqrt(bestN))
        mtry.2 <- round(sqrt(bestN) / 2)
        mtry.3 <- min(2 * mtry.1, bestN)

        mtry <- sample( x = c(mtry.1, mtry.2, mtry.3)
                       ,size = 1
                       )
                    

        ntree <- round(Exp10(runif( n = 1
                                   ,min = log10(control$ntree$low)
                                   ,max = log10(control$ntree$high)
                                   )
        )
        )

        ndays <- round(Exp10(runif( n = 1
                                   ,min = log10(control$ndays$low)
                                   ,max = log10(control$ndays$high)
                                   )
        )
        )

        result <- list( bestN = bestN
                       ,mtry = mtry
                       ,ntree = ntree
                       ,ndays = ndays
                       )
        result
    }

    Next
}

Main <- function(control) {
    InitializeR( duplex.output.to = control$path.out.log
                ,random.seed      = control$random.seed
                )
    str(control)

    Path <- CvCell()$Path
    fixed <- Chart14()$Fixed()
    
    ThePath <- function(vars) {
        path <- Path( scope = fixed$scope
                     ,model = fixed$model
                     ,timePeriod = fixed$timePeriod
                     ,scenario = fixed$scenario
                     ,response = fixed$response
                     ,predictorsName = sprintf('best%02d', vars$bestN)
                     ,predictorsForm = fixed$predictorsForm
                     ,ndays = as.character(vars$ndays)
                     ,query = '100'  # 1% sample
                     ,lambda = fixed$lambda
                     ,ntree = as.character(vars$ntree)
                     ,mtry = as.character(vars$mtry)
                     )
        path
    }

    Next <- RandomTarget(control)
    AppendNDependencies <- function(lines, target, n) {
        # append N dependencies
        lines$Append(paste0('.PHONY: ', target))
        lines$Append(paste0( target
                            ,': \\'
                            )
        )
        for (i in 1:n) {
            lines$Append(paste0( '  '
                                ,ThePath(Next())
                                ,if (i == n) '' else ' \\'
                                )
            )
        }
    }

    lines <- Lines()

    # first set of jobs
    AppendNDependencies(lines, 'chart14-targets-R', 12)
    AppendNDependencies(lines, 'chart14-targets-J', 8)

    # second set of jobs
    AppendNDependencies(lines, 'chart14-targets-R2', 7)
    AppendNDependencies(lines, 'chart14-targets-J2', 7)
    
    # write the makefile
    txt <- lines$Get()
    print(txt)

    writeLines( text = txt
               ,con = control$path.out
               )
}

control <- Control()
Main(control)
if (control$testing)
    cat('DISCARD RESULTS: TESTING')
cat('done\n')