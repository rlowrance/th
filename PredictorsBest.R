PredictorsBest <- function() {
    # return vector of the best preditor names
    # these names are discoved in e-reduced-features

    # for now, just return all the features that are always present
    result <- c( 'living.area'
                ,'median.household.income'
                ,'avg.commute.time'
                ,'fireplace.number'
                ,'year.built'
                ,'fraction.owner.occupied'
                )
    result
}
