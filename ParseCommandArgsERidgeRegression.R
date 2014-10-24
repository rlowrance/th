ParseCommandArgsERidgeRegression <- function(command.args, default.args) {
    # return name list of values from the command args
    opt.query <- make_option( opt_str = c('--query')
                             ,action = 'store'
                             ,type = 'double'
                             ,default = default.args$query
                             ,help = '1 / fraction of samples used as queries'
                             )
    opt.lambdaSet <- make_option( opt_str = c('--lambdaSet')
                                 ,action = 'store'
                                 ,type = 'character'
                                 ,default = default.args$lambdaSet
                                 ,help = 'for now, "one"'
                                 )

    option.list <- list( opt.query
                        ,opt.lambdaSet
                        )

    opt <- parse_args( object = OptionParser(option_list = option.list)
                      ,args = command.args
                      ,positional_arguments = FALSE
                      )
}
