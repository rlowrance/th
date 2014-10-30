CvPossible <- function() {
    # return list of all possible command line arguments to e-cv program
    # list only values actually used or definitely planned for use
    result <-
        list( scope = c('global', 'submarket', 'submarketIndicator')
             ,model = c('linear', 'linearReg', 'randomForest')
             ,timePeriod = c('2003', '2009')
             ,scenario = c('assessor', 'avm', 'mortgage')
             ,response = c('price', 'logprice')
             ,predictorsForm = c('level', 'log')
             ,predictorsName = c('always', 'alwaysNoAssessment', 'alwaysNoCensus')
             ,ndays = c('30', '60', '90', '120', '150', '180', '210', '240', '270', '300', '330', '360')
             ,query = c(1)
             ,c = c(0)
             ,ntree = c(0)
             ,mtry = c(0)
             )
    result
}
