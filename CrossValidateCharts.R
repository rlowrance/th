CrossValidateCharts <- function(control, cv.result, model.name) {
    # return list of objects, each a chart
    library(ggplot2)
    Heading.1.and.2 <- function(control) {
        result <- 
            c( sprintf( 'cv.result of %f Percent Random Sample of Training Transactions'
                       ,control$query.sample * 100
                       )
              ,'AVM scenario'
              ,'Log-linear form'
              ,sprintf( 'Testing period: %s through %s'
                       ,control$testing.period$first.date
                       ,control$testing.period$last.date
                       )
              ,sprintf( 'Number of training days: %d'
                       ,control$num.training.days
                       )
              )
        result
    }

    Chart1 <- function(control, cv.result, model.name) {
        # text file with variables in order
        result <- c( Heading.1.and.2(control)
                    ,' '
                    ,'Model descriptions'
                    ,sapply( 1:length(model.name)
                            ,function(index)
                                sprintf(' %2d: %s', index, model.name[[index]])
                            )
                    )
        result
    }

    Chart2 <- function(control, cv.result, model.name) {
        # text file with median of rootMedianSquaredErrors
        RMedianSE <- function(model.index) {
            model.result <- cv.result[[model.index]]
            rMedianSE.values <- sapply( 1:length(model.result)
                                       ,function(fold.index) {
                                           evaluate <- model.result[[fold.index]]
                                           evaluate$rootMedianSquaredError
                                       }
                                       )
            result <- median(rMedianSE.values)
            result
        }
        result <- c( Heading.1.and.2(control)
                    ,' '
                    ,sapply( 1:length(cv.result)
                            ,function(model.index)
                                 sprintf( ' RMedianSE for model %d:%-40s %0.f'
                                         ,model.index
                                         ,model.name[[model.index]]
                                         ,RMedianSE(model.index)
                                         )
                            )
                    )
        result
    }

    Chart3 <- function(control, cv.result) {
        # box chart showing all rMedianSE values in each fold
        df <- NULL
        for (cv.result.index in 1:length(cv.result)) {
            one.cv <- cv.result[[cv.result.index]]
            for (fold.index in 1:length(one.cv)) {
                next.df <- data.frame( num.vars = cv.result.index
                                      ,rootMedianSquaredError = one.cv[[fold.index]]$rootMedianSquaredError
                                      )
                df <- rbind(df, next.df)
            }
        }
        df$num.vars <- factor(df$num.vars)
        gg <- ggplot( df
                     ,aes( x = num.vars
                          ,y = rootMedianSquaredError
                          )
                     )
        g <- gg + geom_boxplot()
        g
    }

    Chart4 <- function(control, cv.result) {
        # median of RMedianSE
        # line graph showing rMedianSE values in each fold with error bars
        # ref: R Graphics Cookbook p158 (adding error bars to line graph)
        # ref: R Graphics Cookbook, recipe 15.18 (calculated standard errors)
        # NOTE: se is the standard deviation / sqrt(sample size)
        # NOTE: a 95% confidence interval for the mean is +- 1.96 * se
        df <- NULL
        for (cv.result.index in 1:length(cv.result)) {
            one.cv <- cv.result[[cv.result.index]]
            for (fold.index in 1:length(one.cv)) {
                next.df <- data.frame( num.vars = cv.result.index
                                      ,rootMedianSquaredError = one.cv[[fold.index]]$rootMedianSquaredError
                                      )
                df <- rbind(df, next.df)
            }
        }

        # compute median results and standard errors
        ResultsForModel <- function(model.index) {
            values <- df[df$num.vars == model.index, 'rootMedianSquaredError']
            values
        }
        MedianRMedianSE <- function(model.index) {
            values <- ResultsForModel(model.index)
            values.median <- median(values)
            values.median
        }
        SE <- function(model.index) {
            # standard error of the mean
            values <- ResultsForModel(model.index)
            values.sd <- sd(values)
            values.se <- values.sd / length(values)
            values.se
        }

        model.index <- 1:length(cv.result)
        medianRMedianSE <- sapply(1:length(cv.result), MedianRMedianSE)
        se <- sapply(1:length(cv.result), SE)

        df2 <- data.frame( model.index = model.index
                          ,medianRMedianSE = medianRMedianSE
                          ,se = se
                          )


        gg <- ggplot( df2
                     ,aes( x = model.index
                          ,y = medianRMedianSE
                          )
                     )
        g <- 
            gg + 
            geom_line(aes(group = 1)) + 
            geom_point(size = 4) + 
            geom_errorbar( aes( ymin = medianRMedianSE - se
                               ,ymax = medianRMedianSE + se
                               )
            ,width = 0.2
            )



            g
    }

    Chart5 <- function(control, cv.result) {
        # mean of rootMedianSquaredError
        # line graph showing rMedianSE values in each fold with error bars
        # ref: R Graphics Cookbook p158 (adding error bars to line graph)
        # ref: R Graphics Cookbook, recipe 15.18 (calculated standard errors)
        # NOTE: se is the standard deviation / sqrt(sample size)
        # NOTE: a 95% confidence interval for the mean is +- 1.96 * se
        df <- NULL
        for (cv.result.index in 1:length(cv.result)) {
            one.cv <- cv.result[[cv.result.index]]
            for (fold.index in 1:length(one.cv)) {
                next.df <- data.frame( num.vars = cv.result.index
                                      ,rootMedianSquaredError = one.cv[[fold.index]]$rootMedianSquaredError
                                      )
                df <- rbind(df, next.df)
            }
        }

        # compute median results and standard errors
        ResultsForModel <- function(model.index) {
            values <- df[df$num.vars == model.index, 'rootMedianSquaredError']
            values
        }
        MeanRMedianSE <- function(model.index) {
            values <- ResultsForModel(model.index)
            values.mean <- mean(values)
            values.mean
        }
        SE <- function(model.index) {
            # standard error of the mean
            values <- ResultsForModel(model.index)
            values.sd <- sd(values)
            values.se <- values.sd / length(values)
            values.se
        }

        model.index <- 1:length(cv.result)
        meanRMedianSE <- sapply(1:length(cv.result), MeanRMedianSE)
        se <- sapply(1:length(cv.result), SE)

        df2 <- data.frame( model.index = model.index
                          ,meanRMedianSE = meanRMedianSE
                          ,se = se
                          )


        gg <- ggplot( df2
                     ,aes( x = model.index
                          ,y = meanRMedianSE
                          )
                     )
        g <- 
            gg + 
            geom_line(aes(group = 1)) + 
            geom_point(size = 4) + 
            geom_errorbar( aes( ymin = meanRMedianSE - se
                               ,ymax = meanRMedianSE + se
                               )
            ,width = 0.2
            )



            g
    }

    # BODY STARTS HERE

    charts <- list( chart1 = Chart1(control, cv.result, model.name)
                   ,chart2 = Chart2(control, cv.result, model.name)
                   ,chart3 = Chart3(control, cv.result)
                   ,chart4 = Chart4(control, cv.result)
                   ,chart5 = Chart5(control, cv.result)
                   )
    charts
}
