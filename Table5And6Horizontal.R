Table5And6Horizontal <- function(lines) {
    # return table function object $Header1() $Header2() $Detail() $Formated() $Blank() $Get()
    case   <- '%8s %5s %3s'
    header.format <- paste0(case, paste0(rep(' %6s', 12), collapse = ''))
    data.format   <- paste0(case, paste0(rep(' %6.0f', 12), collapse = ''))

    Header1 <- function(col2, col3, col4) {
        # append a header record with mostly blank columns
        line <- sprintf( header.format 
                        ,' '
                        ,col2, col3, col4   
                        ,' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' 
                        )
        lines$Append(line)
    }

    Header2 <- function(col1, col2, col3, col4, col5, col6, col7, col8,
                        col9, col10, col11, col12, col13, col14, col15) {
        line <- sprintf(header.format,
                         col1, col2, col3, col4, col5, col6, col7, col8,
                         col9, col10, col11, col12, col13, col14, col15
                         )
        lines$Append(line)
    }
    
    Blank <- function() {
        lines$Append(' ')
    }

    Detail <- function(col1, col2, col3, col4, col5, col6, col7, col8,
                       col9, col10, col11, col12, col13, col14, col15) {
        line <- sprintf(data.format,
                        col1, col2, col3, col4, col5, col6, col7, col8,
                        col9, col10, col11, col12, col13, col14, col15
                        )
        lines$Append(line)
    }

    Formatted <- function(additional.lines) {
        # append already-formatted lines (usually in the header)
        for (line in additional.lines) {
            lines$Append(line)
        }
    }

    Get <- function() {
       lines$Get()
    }

    list( Header1   = Header1
         ,Header2   = Header2
         ,Blank     = Blank
         ,Detail    = Detail
         ,Formatted = Formatted
         ,Get       = Get
         )
}
