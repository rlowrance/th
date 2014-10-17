# nhgis.R
# main program to process census data files downloaded from nhgis.org
# produce output file containing one data frame in "melted" format
# with these columns for just Los Angeles County census tracts
# $year
# $census.tract: number, 6 digits
# $variable    : chr in {'median.hh.income', 'avg.commute.minutes', 'fraction.owner.occupied'}
# $value

source('Directory.R')
source('Libraries.R')

Control <- function() {
    me <- 'nhgis'

    log <- Directory('log')
    working <- Directory('working')

    dir.nhgis <- '../../nhgis/'  # FIXME: should call Directory('nhgis') (not raw, as not Los Angeles)
    dir.nhgis.1980 <- paste0(dir.nhgis, '1980/nhgis0003_csv/')

    control <- list( path.in.1980.ds104 = paste0(dir.nhgis.1980, 'nhgis0003_ds104_1980_tract.csv')
                    ,path.out.log = paste0(log, me, '.log')
                    ,path.out.rdata = paste0(working, me, '.RData')
                    )
    control
}

Read1980DS104 <- function(path.in) {
    # read data set 104 for year 1980
    # return data frame in melted format
    d <- read.csv( file = path.in
                  ,header = TRUE
                  ,skip = 1  # skip first header with short names
                  )
    str(d)
    browser()
    stopifnot(all(d$Data.File.Year == 1980))
    is.california <- d$State.Name == 'California'
    is.los.angeles.county <- d$County.Name == 'Los Angeles'
    is.both <- is.california & is.los.angeles.country
    d.subset <- d[is.both,]
    owner <- d.subset$Owner.occupied
    renter <- d.subset$Renter.occupied
    fraction.owner.occupied <- owner / (owner + renter)
    stopifnot(all(fraction.owner.occupied >= 0))
    stopifnot(all(fraction.owner.occupied <= 1))
    result <- data.frame( year = d.subset$Data.File.Year
                         ,census.tract = d.subset$Census.Tract.Code
                         ,variable = 'fraction.owner.occupied'
                         ,value = fraction.owner.occupied
                         )
    result
}

Read1980DS107 <- function(path.in) {
    stop('write me')
}

Read1980 <- function(control) {
    df <- rbind( Read1980DS104(control$path.in.1980.ds104)
                ,Read1980DS107(control$path.in.1980.ds107)
                )
    df
}

Main <- function(control) {
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)
    df.1980 <- Read1980(control)
    df.1990 <- Read1990(control)
    df.2000 <- Read2000(control)

    df <- rbind(df.1980, df.1990, df.2000)

    save( control
         ,df
         ,file = control$path.out.rdata
         )
}

control <- Control()
Main(control)
cat('done\n')
