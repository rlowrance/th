TryCatchWE <- function(expr) {
  # evalute expr
  # if expr succeeed with no warning nor error
  #   return $value $warning == NULL
  # if expr generates a warning but not an error
  #   return $value $warning (a list, not NULL)
  # if expr generates an error
  #   return $value (a list with element #message), $warning == NULL
  # ref: author martin Maechler, R-help mailing list Dec 8, 2010
  # ref: at the R prompt, type demo(error.catching)
  # NOTE: this version catches both warning and error and has an 
  # easier to use API than the original

  W <- NULL
  w.handler <- function(w) {
    #warning handler
    W <<- w
    invokeRestart("muffleWarning")  # obtain the expr value in the restart
  }

  E <- NULL
  e.handler <- function(e) {
    E <<- e
  }

  list( value = withCallingHandlers(tryCatch( expr
                                             ,error = e.handler
                                             )
                                    ,warning = w.handler
                                    )
       ,warning = W
       ,error = E
       )
}

TryCatchWE.test <- function() {
  # unit test
  result <- TryCatchWE(log(2))
  stopifnot(result$value == log(2))
  stopifnot(is.null(result$warning))
  stopifnot(is.null(result$error))

  # detect there is no error
  if (!is.null(result$warning)) {
    stop('bad: no warning')
  } else if (!is.null(result$error)) {
    stop('bad: no error')
  } else {
    # no warning nor error
  }

  result <- TryCatchWE(log(-1))
  stopifnot(is.nan(result$value))
  stopifnot(!is.null(result$warning))
  stopifnot(is.null(result$error))

  # detect warning
  if (!is.null(result$warning)) {
    # we go here
  } else {
    stop('bad')
  }

  result <- TryCatchWE(log('a'))
  stopifnot(is.null(result$warning))
  stopifnot(!is.null(result$error))
  stopifnot(is.null(result$warning))

  # detect error
  if (!is.null(result$error)) {
    #ok
  } else {
    stop('bad')
  }
}

TryCatchWE.test()
