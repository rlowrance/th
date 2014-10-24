GlobalParseCommandArgsERandomForestsGlobal <- function(default.args) {
    # return name list of values from the command args
    opt.hpset <- make_option( opt_str = c('--hpset')
                             ,action = 'store'
                             ,type = 'character'
                             ,help = 'hyperparmeter set in a, b'
                             ,default = default.args$hpset
                             )
    opt.year <- make_option( opt_str = c('--year')
                            ,action = 'store'
                            ,type = 'integer'
                            ,help = 'year for test data'
                            ,default = default.args$year
                            )
    opt.month <- make_option( opt_str = c('--month')
                             ,action = 'store'
                             ,type = 'character'
                             ,help = 'month in jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec'
                             ,default = default.args$month
                             )
    option.list <- list( opt.hpset
                        ,opt.year
                        ,opt.month
                        )
    command.args <- commandArgs(trailingOnly = TRUE)
    opt <- parse_args( object = OptionParser(option_list = option.list)
                      ,args = command.args
                      ,positional_arguments = FALSE
                      )
    opt
}
