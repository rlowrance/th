Table7A <- function(lines) {
    # return table function object $Header1() $Header2() $Detail() $Formated() $Blank() $Get()
    # each of which appends to Lines object line
    case   <- '%8s %8s %8s'
    header.format             <- paste0(case, paste0(rep(' %6s', 12), collapse = ''))
    data.format.whole.numbers <- paste0(case, paste0(rep(' %6.0f', 12), collapse = ''))
    data.format.fractions     <- paste0(case, paste0(rep(' %6.3f', 12), collapse = ''))

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
                             ,data.format
                             ) {
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
