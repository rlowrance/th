Table7B <- function(lines) {
    # return table function object $Header1() $Header2() $Detail() $Formated() $Blank() $Get()
    # each of which appends to Lines object line
    case   <- '%8s %8s %8s'
    header.format            <- paste0(case, ' %5s', ' %6s')
    data.format.whole.number <- paste0(case, ' %5s', ' %6.f')
    data.format.fraction     <- paste0(case, ' %5s', ' %6.3f')

    Header <- function(response, predictorsForm, metric, ndays, value) {
        lines$Append(sprintf( header.format
                             ,response
                             ,predictorsForm
                             ,metric
                             ,ndays
                             ,value
                             )
        )
    }
    
    Blank <- function() {
        lines$Append(' ')
    }

    Detail <- function( response, predictorsForm, metricName
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

    DetailWholeNumber <- function(response, predictorsForm, metricName, ndays, value) {
        lines$Append(sprintf( data.format.whole.number
                             ,response
                             ,predictorsForm
                             ,metricName
                             ,ndays
                             ,value
                             )
        )
    }

    DetailFraction <- function(response, predictorsForm, metricName, ndays, value) {
        lines$Append(sprintf( data.format.fraction
                             ,response
                             ,predictorsForm
                             ,metricName
                             ,ndays
                             ,value
                             )
        )
    }

    Append <- function(line) {
        lines$Append(line)
    }


    Get <- function() {
       lines$Get()
    }

    list( Header            = Header
         ,Blank             = Blank
         ,DetailWholeNumber = DetailWholeNumber
         ,DetailFraction    = DetailFraction
         ,Append             = Append
         ,Get                = Get
         )
}
