IsRandomForests <- function(model.name) {
    # return TRUE iff we are fitting a random forests model
    str.split <- strsplit( model.name
                          ,split = '_'
                          ,fixed = TRUE
                          )
    model <- str.split[[1]][[2]]
    result <- model == 'rf'
    result
}
IsRandomForestsTest <- function() {
    # unit tests
    stopifnot(IsRandomForests('global_rf_2003on_avm_logprice_best15zip_level_60_100_0_300_1'))
    stopifnot(!IsRandomForests('global_lin_2003on_avm_logprice_best15zip_level_60_100_0_300_1'))
}
IsRandomForestsTest()
