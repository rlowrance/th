CITable <- function(axis.names, axis.values, names, values, values.low, values.high) { # return list of chr, a table
    format.header <- '%20s %15s %30s'
    format.data   <- '%20s %15.0f        [%6.0f,%6.0f]'
    lines <- Lines()
    lines$Append(axis.values)
    lines$Append(' ')
    lines$Append(sprintf(format.header, axis.names, 'median', '95% confidence interval'))
    lines$Append(' ')
    for (index in 1:length(values)) {
        lines$Append(sprintf( format.data
                             ,names[[index]]
                             ,values[[index]]
                             ,values.low[[index]]
                             ,values.high[[index]]))
    }
    result <- lines$Get()
    result
}
