CIChart <- function(axis.names, axis.values, names, values, values.low, values.high
                     ,show.zero.value) {
    # Cleveland dot plot showing confidence intervals
    # ref: G Graphics Cookbook, p 42 and following
    df <- data.frame( stringsAsFactors = FALSE
                     ,names = names
                     ,values = values
                     ,values.low = values.low
                     ,values.high = values.high
                     )
    gg <- ggplot( df
                 ,aes( x = values
                      ,y = reorder(names, length(names):1)
                      )
                 )
    g1 <-
        gg +
        xlab(axis.values) +
        ylab(axis.names) +
        geom_point(aes(x = values), size = 3) +
        geom_point(aes(x = values.low), size = 2) +
        geom_point(aes(x = values.high), size = 2) +
        theme_bw() +
        theme( panel.grid.major.x = element_blank()
              ,panel.grid.minor.x = element_blank()
              ,panel.grid.major.y = element_line( colour = 'grey60'
                                                 ,linetype = 'dashed'
                                                 )
              )
    g <- if (show.zero.value) g1 + coord_cartesian(xlim = c(0, 1.1 * max(values.high))) else g1 
    g
}
