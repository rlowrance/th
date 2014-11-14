Counter <- function(initial.value = 0) {
    # stateful function object that counts; returns list
    # $Increment(by = 1) --> current.value (after incrementing)
    # $Get() --> current value
    current.value <- initial.value

    Increment <- function(by = 1) {
        current.value <<- current.value + by
    }

    Get <- function() {
        current.value
    }

    list( Increment = Increment
         ,Get       = Get
         )
}

Counter.test <- function() {
    # unit test
    counter <- Counter()
    stopifnot(counter$Get() == 0)
    stopifnot(counter$Increment() == 1)
    stopifnot(counter$Increment(by = -1) == 0)
    stopifnot(counter$Get() == 0)

    counter <- Counter(10)
    stopifnot(counter$Get() == 10)
    stopifnot(counter$Increment(20) == 30)
    stopifnot(counter$Get() == 30)
}

Counter.test()
