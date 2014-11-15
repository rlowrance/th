Chart5 <- function(my.control) {
    # return list of lines, the txt table for chart 5

    cv.cell <- CvCell()
    fixed <- cv.cell$FixedCellValues('Chart5')
    Path <- cv.cell$Path

    Header <- function() {
        lines <- Lines()
        lines$Append('Median of Root Median Squared Errors from 10 Fold Cross Validation')

        stopifnot(fixed$scope == 'global')
        stopifnot(fixed$model == 'linear')
        stopifnot(fixed$timePeriod == '2008')
        stopifnot(fixed$query == '1')

        HeadersFixed(fixed, lines)

        lines$Append(' ')
        lines$Append('Column Use Tax (yes ==> use tax assessment)')
        result <- lines$Get()
        result
    }

    table <- Table5And6()
    table$Formatted(Header())
    table$Blank()
    table$Header1('preds', 'Use', 'ndays')
    table$Header2( 'response', 'Form', 'Tax'
                  ,'30', '60', '90', '120', '150', '180', '210', '240', '270', '300', '330', '360'
                  )

    DetailLine <- function(response, predictorsName, predictorsForm) {
        M <- function(ndays) {
            path.in <- Path( scope = fixed$scope
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
            load(path.in)
            stopifnot(!is.null(cv.result))
            stopifnot(length(cv.result) == 1)
            median.RMSE <- MedianRMSE(cv.result[[1]])
            median.RMSE
        }

        use.assessment <- switch( predictorsName
                                 ,always = 'yes'
                                 ,alwaysNoAssessment = 'no'
                                 )
        table$Detail( response, predictorsForm, use.assessment
                     ,M(30), M(60), M(90), M(120), M(150), M(180)
                     ,M(210), M(240), M(270), M(300), M(330), M(360)
                     )
    }

    for (response in c('price', 'logprice')) {
        for (predictorsForm in c('level', 'log')) {
            for (predictorsName in c('always', 'alwaysNoAssessment')) {
                DetailLine( response = response
                           ,predictorsName = predictorsName
                           ,predictorsForm = predictorsForm
                           )
            }
        }
    }

    result <- table$Get()
    result
}
