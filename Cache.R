Cache <- function() {
    # return a new cache and access functions for it:
    # cache <- Cache()
    # cache$reset() : empty the cache
    # cache$set(key, value): mutate the cache
    # cache$get(key): return value at key
    # cache$has_key(key): return TRUE or FALSE
    # ref: github.com/hadley/memoise
    cache <- NULL
    
    cache_reset <- function() {
        cache <<- new.env(TRUE, emptyenv())
    }

    cache_set <- function(key, value) {
        assign(key, value, envir = cache)
    }

    cache_get <- function(key) {
        get(key, envir = cache, inherits = FALSE)
    }

    cache_has_key <- function(key) {
        exists(key, envir = cache, inherits = FALSE)
    }
    
    cache_keys <- function() {
        ls(envir = cache)
    }

    cache_reset()
    list( reset   = cache_reset
         ,set     = cache_set
         ,get     = cache_get
         ,has_key = cache_has_key
         ,keys    = cache_keys
         )
}
