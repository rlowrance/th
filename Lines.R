Lines <- function(max.size = 1000) {
  # return function object for a list of lines
  # methods:
  # $Append(line) --> append to lines
  # $Get()        --> return all lines

    lines <- rep('', max.size)
    last.index <- 0

    Append <- function(line, debug = FALSE) {
        if (debug) browser()
        last.index <<- last.index + 1
        lines[[last.index]] <<- line
    }

    Get <- function() {
        if (last.index == 0)
          as.character()
       else
         lines[1:last.index]
    }


    list( Append = Append
         ,Get    = Get
         )
}
