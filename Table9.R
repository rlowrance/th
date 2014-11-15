Table9 <- function(lines) {
    # append to Lines object lines
    header.format <- '%15s %30s %15s  %15s'
    data.format   <- '%15.0f %30s %15.0f  [%6.0f,%6.0f]'

    Header <- function(num.features, name, median, ci) {
        line <- sprintf(header.format, num.features, name, median, ci)
        lines$Append(line)
    }

    Detail <- function(num.features, name, median, ci.low, ci.high) {
        line <- sprintf(data.format, num.features, name, median, ci.low, ci.high)
        lines$Append(line)
    }

    list( Header = Header
         ,Detail = Detail
         )
}
