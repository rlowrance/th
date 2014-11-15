Table_8 <- function(lines) {
    # return function object $Header1() $Header2() $Detail() $Get()
    header.format <- '%8s %8s %6s %6s %6s %6s %6s %6s %6s %6s %6s %6s %6s %6s'
    data.format   <- '%8s %8s %6.0f %6.0f %6.0f %6.0f %6.0f %6.0f %6.0f %6.0f %6.0f %6.0f %6.0f %6.0f'

    Header1 <- function(predictorsForm, ndays30) {
        Header2( predictorsForm = predictorsForm
                ,ndays30 = ndays30
                )
    }
    Header2 <- function( response = ' ', predictorsForm = ' '
                        ,ndays30 = ' ', ndays60 = ' ', ndays90 = ' '
                        ,ndays120 = ' ', ndays150 = ' ', ndays180 = ' '
                        ,ndays210 = ' ', ndays240 = ' ', ndays270 = ' '
                        ,ndays300 = ' ', ndays330 = ' ', ndays360 = ' ') {
        line <- sprintf( header.format
                        ,response, predictorsForm
                        ,ndays30, ndays60, ndays90
                        ,ndays120, ndays150, ndays180
                        ,ndays210, ndays240, ndays270
                        ,ndays300, ndays330, ndays360
                        )
        lines$Append(line)
    }
    Detail <- function( response, predictorsForm
                       ,ndays30, ndays60, ndays90
                       ,ndays120, ndays150, ndays180
                       ,ndays210, ndays240, ndays270
                       ,ndays300, ndays330, ndays360
                       ) {
        line <- sprintf( data.format
                        ,response, predictorsForm
                        ,ndays30, ndays60, ndays90
                        ,ndays120, ndays150, ndays180
                        ,ndays210, ndays240, ndays270
                        ,ndays300, ndays330, ndays360
                        )
        lines$Append(line)
    }
    Get <- function() {
        lines$Get()
    }
    list( Header1 = Header1
         ,Header2 = Header2
         ,Detail  = Detail
         ,Get     = Get
         )
}
