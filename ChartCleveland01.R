ChartCleveland01 <- function(labels, values, label.axis.name, value.axis.name) {
    # return ggplot2 object, a Cleveland Dot plot
    # df$value: num, the plot point
    # df$label: chr name for the plot point
    # ref: R Graphics Cookbook p.42 and following
    y.factor <- factor( labels
                       ,levels = sort(labels, decreasing = TRUE)
                       )
    df <- data.frame( x = values
                     ,y = y.factor
                     )
    gg <- ggplot( df
                 ,aes( x = x
                      ,y = y
                      )
                 )
    g <-
        gg +
        geom_point(size = 3) +
        xlim(0, max(df$x)) +
        xlab(value.axis.name) +
        ylab(label.axis.name) +
        theme_bw() +
        theme( panel.grid.major.x = element_blank()
              ,panel.grid.minor.y = element_blank()
              ,panel.grid.major.x = element_blank()
              )
    g
}
