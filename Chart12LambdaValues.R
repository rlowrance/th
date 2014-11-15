Chart12LambdaValues <- function() {
    # return vector of lambda values that are used for regularization
    # lambda on command lines is 100 * lambda in regression
    lambda.in.regression <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 30)
    lambda.on.command.line <- as.character(100 * lambda.in.regression)
    lambda.on.command.line
}
