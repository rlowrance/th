Table7Metrics3 <- function(lines) {
    # return function object $Header() $Panel() $Detail() $Get(), for 3 metrics
    header.format <- '%15s %8s %8s %8s %8s %8s %8s'
    data.format   <- '%15s %8.0f %8.0f %8.3f %8.0f %8.0f %8.3f'
    data.format.large.value1   <- '%15s %8.0g %8.0f %8.3f %8.0f %8.0f %8.3f'
    panel.format  <- 'Panel %s: %s'

    Header <- function( col1
                       ,col2, col3, col4
                       ,col5, col6, col7
                       ) {
        line <- sprintf( header.format
                        ,col1
                        ,col2, col3, col4
                        ,col5, col6, col7
                        )
        lines$Append(line)
    }

    panel.code <- 'A'
    Panel <- function( panel.description) {
        line <- sprintf( panel.format
                        ,panel.code
                        ,panel.description
                        )
        lines$Append(line)
        panel.code <<- 'B'  # only 2 panels
    }

    Detail <- function( ndays
                       ,value1, value2, value3
                       ,value4, value5, value6
                       ) {
        format <-
            if (value1 > 1e6) 
                data.format.large.value1
            else
                data.format
        line <- sprintf( format
                        ,ndays
                        ,value1, value2, value3
                        ,value4, value5, value6
                        )
        lines$Append(line)
    }


    list( Header  = Header
         ,Detail  = Detail
         ,Panel   = Panel
         )
}
