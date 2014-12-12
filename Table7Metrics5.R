Table7Metrics5 <- function(lines) {
    # return function object $Header() $Detail() , for 5 metrics
    header.format <- '%15s %8s %8s %8s %8s %8s'
    data.format   <- '%15s %8.0f %8.0f %8.3f %8.3f %8.3f'
    data.format.large.value.1     <- '%15s %8.0g %8.0f %8.3f %8.3f %8.3f'
    data.format.large.value.1.4   <- '%15s %8.0g %8.0f %8.3f %8.0g %8.3f'
    panel.format  <- 'Panel %s: %s'

    Header <- function( col1
                       ,col2
                       ,col3
                       ,col4
                       ,col5
                       ,col6
                       ) {
        line <- sprintf( header.format
                        ,col1
                        ,col2
                        ,col3
                        ,col4
                        ,col5
                        ,col6
                        )
        lines$Append(line)
    }


    Detail <- function( ndays
                       ,value1
                       ,value2
                       ,value3
                       ,value4
                       ,value5
                       ) {
        format <-
            if (value1 > 1e6) {
                if (value4 > 1000)
                    data.format.large.value.1.4
                else
                    data.format.large.value.1
            }
            else
                data.format
        line <- sprintf( format
                        ,ndays
                        ,value1
                        ,value2
                        ,value3
                        ,value4
                        ,value5
                        )
        lines$Append(line)
    }


    list( Header  = Header
         ,Detail  = Detail
         )
}
