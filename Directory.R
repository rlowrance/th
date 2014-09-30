Directory <- function(name) {
    # convert the name of a directory into a path to that directory
    # a path is relative to the source directory
    # a path is a character that ends in a slash
    data <- '../../los-angeles/'
    path <- switch( name
                   ,drawings = '../drawings/'
                   ,log = paste0(data, 'log/')
                   ,output = paste0(data, 'output/')
                   ,raw = paste0(data, 'raw/')
                   ,source = '.'
                   ,splits = paste0(data, 'working/transactions-subset1-train-splits/')
                   ,utilities = '../../lowranceutilitiesr'
                   ,working = paste0(data, 'working/')
                   )
    path
}

