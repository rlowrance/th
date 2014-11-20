Table7A <- function(lines) {
    # return table function object $Header1() $Header2() $Detail() $Formated() $Blank() $Get()
    # each of which appends to Lines object line
    case   <- '%8s %8s %8s '
    header.format             <- paste0(case, paste0(rep(' %6s', 12), collapse = ''))

    o      <- ' %6.0f' # other
    lowest <- '*%6.0f'
    data.format.whole.numbers <-
        list( paste0(case, lowest, o, o, o, o, o, o, o, o, o, o, o)
             ,paste0(case, o, lowest, o, o, o, o, o, o, o, o, o, o)
             ,paste0(case, o, o, lowest, o, o, o, o, o, o, o, o, o)
             ,paste0(case, o, o, o, lowest, o, o, o, o, o, o, o, o)
             ,paste0(case, o, o, o, o, lowest, o, o, o, o, o, o, o)
             ,paste0(case, o, o, o, o, o, lowest, o, o, o, o, o, o)
             ,paste0(case, o, o, o, o, o, o, lowest, o, o, o, o, o)
             ,paste0(case, o, o, o, o, o, o, o, lowest, o, o, o, o)
             ,paste0(case, o, o, o, o, o, o, o, o, lowest, o, o, o)
             ,paste0(case, o, o, o, o, o, o, o, o, o, lowest, o, o)
             ,paste0(case, o, o, o, o, o, o, o, o, o, o, lowest, o)
             ,paste0(case, o, o, o, o, o, o, o, o, o, o, o, lowest)
             )

    o      <- ' %6.3f' # other
    lowest <- '*%6.3f'
    data.format.fractions <-
        list( paste0(case, lowest, o, o, o, o, o, o, o, o, o, o, o)
             ,paste0(case, o, lowest, o, o, o, o, o, o, o, o, o, o)
             ,paste0(case, o, o, lowest, o, o, o, o, o, o, o, o, o)
             ,paste0(case, o, o, o, lowest, o, o, o, o, o, o, o, o)
             ,paste0(case, o, o, o, o, lowest, o, o, o, o, o, o, o)
             ,paste0(case, o, o, o, o, o, lowest, o, o, o, o, o, o)
             ,paste0(case, o, o, o, o, o, o, lowest, o, o, o, o, o)
             ,paste0(case, o, o, o, o, o, o, o, lowest, o, o, o, o)
             ,paste0(case, o, o, o, o, o, o, o, o, lowest, o, o, o)
             ,paste0(case, o, o, o, o, o, o, o, o, o, lowest, o, o)
             ,paste0(case, o, o, o, o, o, o, o, o, o, o, lowest, o)
             ,paste0(case, o, o, o, o, o, o, o, o, o, o, o, lowest)
             )

    Header1 <- function(predictorsForm, ndays30) {
        # append a header record with mostly blank columns
        Header2( response = ' '
                ,predictorsForm = predictorsForm
                ,metric = ' '
                ,ndays30 = ndays30
                ,ndays60 = ' '
                ,ndays90 = ' '
                ,ndays120 = ' '
                ,ndays150 = ' '
                ,ndays180 =  ' '
                ,ndays210 = ' '
                ,ndays240 = ' '
                ,ndays270 =  ' '
                ,ndays300 = ' '
                ,ndays330 =  ' '
                ,ndays360 = ' '
                )
    }

    Header2 <- function( response, predictorsForm, metric
                        ,ndays30, ndays60, ndays90, ndays120, ndays150, ndays180
                        ,ndays210, ndays240, ndays270, ndays300, ndays330, ndays360
                        ) {
        line <- sprintf( header.format
                        ,response, predictorsForm, metric
                        ,ndays30, ndays60, ndays90, ndays120, ndays150, ndays180
                        ,ndays210, ndays240, ndays270, ndays300, ndays330, ndays360
                        )
        lines$Append(line)
    }
    
    Blank <- function() {
        lines$Append(' ')
    }

    DetailEither <- function( response, predictorsForm, metricName
                             ,ndays30, ndays60, ndays90, ndays120, ndays150, ndays180
                             ,ndays210, ndays240, ndays270, ndays300, ndays330, ndays360
                             ,data.format.family
                             ,min.or.max
                             ) {
        # put brackets [] around the minimizer
        special.index <-
            if (min.or.max == 'min') {
                which.min(c( ndays30, ndays60, ndays90, ndays120, ndays150, ndays180
                            ,ndays210, ndays240, ndays270, ndays300, ndays330, ndays360
                            )
                )
            } else {
                which.max(c( ndays30, ndays60, ndays90, ndays120, ndays150, ndays180
                            ,ndays210, ndays240, ndays270, ndays300, ndays330, ndays360
                            )
                )
            }
        data.format <- data.format.family[[special.index]]

        line <- sprintf( data.format
                        ,response, predictorsForm, metricName
                        ,ndays30, ndays60, ndays90, ndays120, ndays150, ndays180
                        ,ndays210, ndays240, ndays270, ndays300, ndays330, ndays360
                        )
        lines$Append(line)
    }

    DetailWholeNumbers <- function( response, predictorsForm, metricName
                       ,ndays30, ndays60, ndays90, ndays120, ndays150, ndays180
                       ,ndays210, ndays240, ndays270, ndays300, ndays330, ndays360
                       ) {
        #cat('DetailWholeNumber\n'); browser()
        DetailEither( response, predictorsForm, metricName
                     ,ndays30, ndays60, ndays90, ndays120, ndays150, ndays180
                     ,ndays210, ndays240, ndays270, ndays300, ndays330, ndays360
                     ,data.format.whole.numbers
                     ,min.or.max = 'min'
                     )
    }

    DetailFractions <- function( response, predictorsForm, metricName
                                ,ndays30, ndays60, ndays90, ndays120, ndays150, ndays180
                                ,ndays210, ndays240, ndays270, ndays300, ndays330, ndays360
                                ) {
        #cat('DetailFractions\n'); browser()
        DetailEither( response, predictorsForm, metricName
                     ,ndays30, ndays60, ndays90, ndays120, ndays150, ndays180
                     ,ndays210, ndays240, ndays270, ndays300, ndays330, ndays360
                     ,data.format.fractions
                     ,min.or.max = 'max'
                     )
    }

    Append <- function(line) {
        lines$Append(line)
    }


    Get <- function() {
       lines$Get()
    }

    list( Header1            = Header1
         ,Header2            = Header2
         ,Blank              = Blank
         ,DetailWholeNumbers = DetailWholeNumbers
         ,DetailFractions    = DetailFractions
         ,Append             = Append
         ,Get                = Get
         )
}
