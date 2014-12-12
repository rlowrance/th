# main program to generate hyperparameters to test with the randomforests model.
#

source('Directory.R')
source('Libraries.R')

Control <- function() {
    me <- 'randomforests-hyperparameters'

    log <- Directory('log')
    working <- Directory('working')

    control <-
        list( path.out.log = paste0(log, me, '.log')
             ,path.out.csv = paste0(working, me, '.csv')
             ,random.seed  = 123
             ,num.hyperparameter.sets = 100
             ,testing      = FALSE
             )
    control
}

Main <- function(control) {
    InitializeR( duplex.output.to = control$path.out.log
                ,random.seed      = control$random.seed
                )
    str(control)

    # set limits for hyperparameters
    bestN.low <- 1
    bestN.high <- 24

    ntree.low <- 100
    ntree.high <- 5000

    ndays.low <- 1
    ndays.high <- 100

    Exp10 <- function(x) 10 ^ x


    # build up a data frame with a row for each set of hyperparameters
    all <- NULL
    id <- 0
    for (hyperparameter.set.number in 1:control$num.hyperparameter.sets) {
        id <- id + 1  
        # sample bestN
        bestN <- round(runif( n = 1
                             ,min = bestN.low
                             ,max = bestN.high
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
                                   ,min = log10(ntree.low)
                                   ,max = log10(ntree.high)
                                   )
        )
        )

        ndays <- round(Exp10(runif( n = 1
                                   ,min = log10(ndays.low)
                                   ,max = log10(ndays.high)
                                   )
        )
        )

        next.row <- data.frame( stringsAsFactors = FALSE
                               ,id = id
                               ,bestN = bestN
                               ,mtry = mtry
                               ,ntree = ntree
                               ,ndays = ndays
                               )
        all <- rbind(all, next.row)
    }
    str(all)
    print(all)
    print(summary(all))


    write.csv( all
              ,file = control$path.out.csv
              ,row.names = FALSE
              )
}

control <- Control()
Main(control)
if (control$testing)
    cat('DISCARD RESULTS: TESTING')
cat('done\n')




