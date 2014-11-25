Table5And6Vertical <- function(lines) {
    # return list $Header() $Detail()
    format.header <- '%12s %8s %8s %8s %8s %8s %8s %8s %8s'
    format.detail <- '%12s %8.0f %8.0f %8.0f %8.0f %8.0f %8.0f %8.0f %8.0f'

    Header <- function(col1, col2, col3, col4, col5, col6, col7, col8, col9) {
        line <- sprintf(format.header, col1, col2, col3, col4, col5, col6, col7, col8, col9)
        lines$Append(line)
    }

    Detail <- function(ndays, value1, value2, value3, value4, value5, value6, value7, value8) {
        line <- sprintf(format.detail, ndays, value1, value2, value3, value4, value5, value6, value7, value8)
        lines$Append(line)
    }

    result <- list( Header = Header
                   ,Detail = Detail
                   )
    result
}
