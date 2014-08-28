ReadRawParcels <- function(nrows, path.to.raw.directory, verbose = FALSE) {
    # return data frame containing all are first nrows of all parcels
    #cat('start ReadRawParcels\n'); browser()

    ReadParcelsFile <- function(num) {
        # return dataframe containing all features in a taxroll file
        #cat('start ReadParcelsFile', num, '\n'); browser()

        Volume <- function(num) {
            paste0( 'corelogic-deeds-090402_'
                   ,if (num <= 4) '07' else '09'
                   ,'/'
                   )
        }

        path <- paste0( path.to.raw.directory
                       ,Volume(num)
                       ,sprintf('CAC06037F%d.zip', num)
                       )

        # The path is to a zip file. It has one file in it. Construct the file name.
        len <- nchar(path)
        filename <- paste0(substr(path, len - 13, len - 4), '.txt')

        df <- read.table( file = unz(path, filename)
                         ,header=TRUE
                         ,sep="\t"
                         ,quote=""
                         ,comment=""
                         ,stringsAsFactors=FALSE
                         ,na.strings=""
                         ,nrows=nrows
                         )

        if (verbose) {
            Printf('Read %d observations\n from file %s\n in zip %s\n', nrow(df), filename, path)
        }

        # track original source
        df$parcel.file.number <- rep(num,nrow(df))
        df$parcel.record.number <- 1:nrow(df)

        df
    }

    # BODY STARTS HERE

    df <- NULL
    for (file.number in 1:8) {
        parcels <- ReadParcelsFile(file.number)
        df <- rbind(df, parcels)
    }
    df
}
