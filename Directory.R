# Directory.R
# determine directory for a given set of files

Directory <- function(description) {
    # return path to the described directory, as a string
    # ARGS
    # directory: chr
    data.dir = '../../los-angeles/'
    switch(
        description
        ,source  = './'
        ,log = paste0(data.dir, 'log/')
        ,output  = paste0(data.dir, 'output/')
        ,raw     = paste0(data.dir, 'raw/')
        ,working = paste0(data.dir, 'working/')
        ,realestate = '../../lowrancerealestater'
        ,utilities  = '../../lowranceutilitiesr'
        ,stop(paste('bad description', description))
    )
}