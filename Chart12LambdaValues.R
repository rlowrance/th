Chart12LambdaValues <- function() {
    # return vector of lambda values that are used for regularization
    # lambda on command lines is 100 * lambda in regression
    lambda.in.regression <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 30
                              ,100, 300, 1000, 3000, 10000
                              ,41, 55, 74    # beween 30 and 100, exponentially
                              ,132, 173, 228 # between 100 and 300, exponentially
                              )
    lambda.on.command.line <- sprintf('%d', 100 * lambda.in.regression)
    lambda.on.command.line
}
