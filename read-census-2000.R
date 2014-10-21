# main program: read files in census-2000
 
source('Directory.R')
source('Libraries.R')

source('DataframeRenameColumn.R')

Control <- function() {
    me <- 'read-census-2000'
    dir.in <- '../../census-us/census_2000_sf3/California/'
    log <- Directory('log')
    control <- list( path.in.geo = paste0(dir.in, 'cageo.uf3')
                    ,path.in.03 = paste0(dir.in, 'ca00003.uf3')
                    ,path.in.06 = paste0(dir.in, 'ca00006.uf3')
                    ,path.in.56 = paste0(dir.in, 'ca00056.uf3')
                    ,path.out.log = paste0(log, me)
                    )
    control
}
Range <- function(prefix, count) {
    # return vector of char (variable names)
    result <- sprintf('%s%03d', prefix, 1:count)
    result
}
AverageTravelTime <- function(df) {
    # return average of travel times in data frame
    # ref: page 8-14
    num.trips <- 
        df$travel.less.than.5 +
        df$travel.5.to.9 +
        df$travel.10.to.14 +
        df$travel.15.to.19 + 
        df$travel.20.to.24 +
        df$travel.25.to.29 +
        df$travel.30.to.35 +
        df$travel.36.to.39 +
        df$travel.40.to.44 + 
        df$travel.45.to.59 +
        df$travel.60.to.89 +
        df$travel.90.or.more
    weighted.sum -
          2.5 * df$travel.less.than.5 +
          7.0 * df$travel.5.to.9 +
         12.0 + df$travel.10.to.14 +
         17.0 * df$travel.15.to.19 + 
         22.0 * df$travel.20.to.24 +
         27.0 * df$travel.25.to.29 +
         32.0 * df$travel.30.to.34 +
         37.0 * df$travel.35.to.39 +
         42.0 * df$travel.40.to.44 + 
         52.0 * df$travel.45.to.59 +
         74.5 * df$travel.60.to.89 +
        135.0 * df$travel.90.or.more  # use (3/2) of 90
    avg <- weighted.sum / num.trips
}
ReadData03 <- function(path) {
    # return data frame with correctly named columns for data file 03
    df <- read.csv( file = path
                   ,header = FALSE
                   )
    # names start on page  7-38
    names(df) <- c( # file linking fields
                    'fileid'
                   ,'stusab'
                   ,'chariter'
                   ,'cifsn'
                   ,'logrecno'
                   # p25: residence in 1995
                   , Range('P025', 35)
                   # p26: place of work state and county level
                   ,Range('P026', 5)
                   # p27: place of work place level
                   ,Range('P027', 5)
                   # p28: plae of work MSA/PMSA level
                   ,Range('P028', 25)
                   # p29: place of work minor civil division level
                   ,Range('P029', 5)
                   # p30: means of transportation
                   ,Range('P30', 16)
                   # p 31: travel time to work
                   ,'travel.total'
                   ,'travel.not.worked.at.home'
                   ,'travel.less.than.5'
                   ,'travel.5.to.9'
                   ,'travel.10.to.14'
                   ,'travel.15.to.19'
                   ,'travel.20.to.24'
                   ,'travel.25.to.29'
                   ,'travel.30.to.34'
                   ,'travel.35.to.39'
                   ,'travel.40.to.44'
                   ,'travel.45.to.59'
                   ,'travel.60.to.89'
                   ,'travel.90.or.more'
                   ,'travel.worked.at.home'
                   )
    travel.indices <- 97:111
    str(df[,travel.indices])
    df
}
ReadData06 <- function(path) {
    # return data frame with correctly named columns for data file 06
    df <- read.csv( file = path
                   ,header = FALSE
                   )
    # names start on page  7-59
    names(df) <- c( # file linking fields
                    'fileid'
                   ,'stusab'
                   ,'chariter'
                   ,'cifsn'
                   ,'logrecno'
                   # p51: sex by industry by class of worker
                   ,Range('P051', 65)
                   # p52: household income in 1999
                   ,Range('P052', 17)
                   # p53: median household income in 1999
                   ,'median.household.income'
                   )
    income.indices <- 88:88
    str(df[,income.indices])
    df
}
ReadData56 <- function(path) {
    # return data frame with correctly named colums for data file 56
    df <- read.csv( file = path
                   ,header = FALSE
                   )
    # names start on page 7-394
    names(df) <- c( # file linking fields
                    'fileid'
                   ,'stusab'
                   ,'chariter'
                   ,'cifsn'
                   ,'logrecno'
                   # h1: housing units
                   ,'housing.units.total'
                   # h2: unweighted sample housing unit
                   ,'sample.total'
                   ,'sample.occupied'
                   ,'sample.vacant'
                   # h3: 100-percent count of housing units
                   ,'housing.100percent.total'
                   # h4: percent of housing units in sample
                   ,'percent.sample.occupied'
                   ,'percent.sample.vacant'
                   # h5: urban and rural
                   ,'ur.total'
                   ,'ur.urban'
                   ,'ur.urban.inside.urbanized.areas'
                   ,'ur.urban.inside.urban.clusters'
                   ,'ur.rural'
                   ,'ur.rural.farm'
                   ,'ur.rural.nonfarm'
                   # h6: occupancy status
                   ,'occupancy.total'
                   ,'occupancy.occupied'
                   ,'occupancy.vacant'
                   # h7: tenure
                   ,'occupied.total'
                   ,'occupied.owner'
                   ,'occupied.renter'
                   )
    tenure.indices <- 23:25
    str(df[,tenure.indices])
    df
}
ReadGeo <- function(path) {
    # return data frame with correctly named columns

    # define all fields for
    #  geographic header record
    #  summary file 3 state file
    # ref: figure 2-5 page 2-8
    # FORMAT: FIELD.NAME, FIELD.SIZE, ...
    defs <- c(# record codes
              'fileid', 6
              ,'stusab', 2
              ,'sumlev', 3
              ,'geocomp', 2
              ,'chariter', 3
              ,'cifsn', 2
              ,'logrecno', 7
              # geographic area codes
              ,'region', 1
              ,'division', 1
              ,'statece', 2
              ,'state', 2
              ,'county', 3
              ,'countysc', 2
              ,'cousub', 5
              ,'cousubcc', 2
              ,'cousubsc', 2
              ,'place', 5
              ,'placecc', 2
              ,'placedc', 1
              ,'placesc', 2
              ,'tract', 6
              ,'blkgrp', 1
              ,'block', 4
              ,'iuc', 2
              ,'concit', 5
              ,'concitcc', 2
              ,'concitsc', 2
              ,'aianhh',4
              ,'aianhhfp', 5
              ,'aianhhcc', 2
              ,'aihhtli', 1
              ,'aitsce', 3
              ,'aits', 5
              ,'aitscc', 2
              ,'anrc', 5
              ,'anrccc', 2
              ,'mascmsa', 4
              ,'masc', 2
              ,'cmsa', 2
              ,'macci', 1
              ,'pmsa', 4
              ,'necma', 4
              ,'necmacci', 1
              ,'ncemasc', 2
              ,'exi', 1
              ,'ua', 5
              ,'uasc', 2
              ,'uatype', 1
              ,'ur', 1
              ,'cd106', 2
              ,'cd108', 2
              ,'cd109', 2
              ,'cd110', 2
              ,'sldu', 3
              ,'sldl', 3
              ,'vtd', 6
              ,'vtdi', 1
              ,'zcta3', 3
              ,'zcta5', 5
              ,'submcd', 5
              ,'submcdcc', 2
              # area characteristics
              ,'arealand', 14
              ,'areawatr', 14
              ,'name', 90
              ,'funcstat', 1
              ,'gcuni', 1
              ,'pop100', 9
              ,'hu100', 9
              ,'intptlat', 9
              ,'intptlon', 10
              ,'lsadc', 2
              ,'partflag', 1
              # special area codes
              ,'sdelm', 5
              ,'sdsec', 5
              ,'sduni', 5
              ,'taz', 6
              ,'uga', 5
              ,'puma5', 5
              ,'puma1', 5
              ,'reserve2', 15
              ,'macc', 5
              ,'uacp', 5
              ,'reserved', 7
              )
    # all the fields are alpha numeric (A/N) except for these 
    numeric.field <- c('sumlev', 'logrecno')
    alpha.field <- c('stusab')

    # build vector of widths needed for read.fxf
    stopifnot(length(defs) %% 2 == 0)
    field.widths <- as.numeric(defs[seq(2, length(defs), 2)])
    field.names <- defs[seq(1, length(defs), 2)]
    stopifnot(length(field.widths) == length(field.names))

    # check that defs table is correct
    if (TRUE) {
        StartingPosition <- function(index) {
            if (index == 1) 1
            else            1 + sum(field.widths[1:(index - 1)])
        }

        starting.position <- sapply(1:length(field.widths)
                                    ,StartingPosition
                                    )
        stopifnot(length(starting.position) == length(field.widths))
        stopifnot(starting.position[[1]] == 1)
        stopifnot(starting.position[[2]] == 7)
        stopifnot(starting.position[[length(starting.position)]] == 394)
    }



    # read the header record
    df <- read.fwf( file = path
                   ,widths = field.widths 
                   ,comment.char = ''  # turn off comments altogether
                   #,n = 1
                   #,skip = 1390
                   )
    names(df) <- field.names

    # convert numeric fields to numeric values
    lapply(numeric.field
           ,function(field.name) {
               df[[field.name]] <- as.numeric(df[[field.name]])
           }
           )

    str(df)
    df
}
Main <- function(control, geo) {
    InitializeR(duplex.output.to = control$path.out.log)
    str(control)
    # Q: which summary level do I want? A: 140
    # Q: how do I idetify records in Los Angeles County?
    lac <- sprintf('%-90s', 'Los Angeles County')  # pad with spaces on right
    in.lac<- substr(geo$name, 1, nchar(lac)) == lac
    is.county <- geo$sumlev == 50
    is.census.tract <- geo$sumlev == 140
    is.lac.census.tract <- in.lac & is.census.tract

    cat('in Main\n'); browser()
    debug(ReadData03)
    debug(ReadData06)
    debug(ReadData56)
    if (FALSE ) geo <- ReadGeo(control$path.in.geo)
    if (FALSE) d03 <- ReadData03(control$path.in.03)
    if (FALSE) d06 <- ReadData06(control$path.in.06)
    if (FALSE) d56 <- ReadData56(control$path.in.56)
    browser()
    geo.tracts <- geo[geo$sumlevel == 140,]
    list(geo, d03, d06, d56)
}

control <- Control()
if (!exists('geo'))
    geo <- ReadGeo(control$path.in.geo)


df <- Main(control, geo)
