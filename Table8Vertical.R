Table8Vertical <- function(lines) {
    # return function object $Header()  $Detail() $Get()
    header.format <- '%15s %8s %8s %8s %8s'
    data.format   <- '%15s %8.0f %8.0f %8.0f %8.0f'

    Header <- function(col1, col2, col3, col4, col5) {
        line <- sprintf(header.format, col1, col2, col3, col4, col5)
        lines$Append(line)
    }

    Detail <- function(ndays, value1, value2, value3, value4) {
        line <- sprintf(data.format, ndays, value1, value2, value3, value4)
        lines$Append(line)
    }

    Get <- function() {
        lines$Get()
    }

    list( Header  = Header
         ,Detail  = Detail
         ,Get     = Get
         )
}
