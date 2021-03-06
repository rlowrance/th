Predictors <- function(which) {
    # return predictor set as a vector of character, where which is oneof
    # 'chopra.level'
    # 'all.level'
    # 'always.level'
    # 'always.level.census'
    # 'always.level.no.census'
    all.level =    c( # build list fro
                     'air.conditioning.code'
                     ,'avg.commute.time'
                     ,'basement.square.feet'
                     ,'bathrooms'
                     ,'bedrooms'
                     ,'census.tract.has.industry'
                     ,'census.tract.has.park'
                     ,'census.tract.has.retail'
                     ,'census.tract.has.school'
                     ,'condition.code'
                     ,'construction.type.code'
                     ,'effective.year.built'
                     ,'exterior.walls.code'
                     ,'factor.foundation.type'
                     ,'factor.has.pool'
                     ,'factor.heating.code'
                     ,'factor.is.new.construction'
                     ,'factor.parking.type'
                     ,'factor.roof.type'
                     ,'fireplace.indicator.flag'
                     ,'fireplace.number'
                     ,'foundation.code'
                     ,'fraction.owner.occupied'
                     ,'garage.code'
                     ,'garage.parking.square.feet'
                     ,'heating.code'
                     ,'land.square.footage'
                     ,'living.area'
                     ,'median.household.income'
                     ,'parking.spaces'
                     ,'parking.type.code'
                     ,'pool.code'
                     ,'quality.code'
                     ,'roof.cover.code'
                     ,'sewer.code'
                     ,'stories.number'
                     ,'total.rooms'
                     ,'water.code'
                     ,'year.built'
                     ,'zip5.has.industry'
                     ,'zip5.has.park'
                     ,'zip5.has.retail'
                     ,'zip5.has.school'
                     )
    always.level.census <-
        c( # subset that always occurs and is derived from census tract data
           'avg.commute.time'
          ,'census.tract.has.industry'
          ,'census.tract.has.park'
          ,'census.tract.has.retail'
          ,'census.tract.has.school'
          ,'fraction.owner.occupied'
          ,'median.household.income'
          )
    always.level.no.census <-
        c( # subset that always occurs
           'basement.square.feet'
          ,'bathrooms'
          ,'bedrooms'
          ,'effective.year.built'
          ,'factor.has.pool'
          ,'factor.is.new.construction'
          #,'fireplace.indicator.flag'  # use fireplace.number instead 
          ,'fireplace.number'    #  possibly zero
          ,'land.square.footage'
          ,'living.area'
          ,'parking.spaces'
          ,'stories.number'
          ,'total.rooms'
          ,'year.built'
          ,'zip5.has.industry'
          ,'zip5.has.park'
          ,'zip5.has.retail'
          ,'zip5.has.school'
          )
    always.level.most.important = c( 'living.area'
                                    ,'median.household.income'
                                    ,'fireplace.number'
                                    ,'avg.commute.time'
                                    ,'fraction.owner.occupied'
                                    ,'effective.year.built'
                                    ,'zip5.has.industry'
                                    ,'total.rooms'
                                    ,'census.tract.has.industry'
                                    ,'parking.spaces'
                                    ,'land.square.footage'
                                    ,'factor.has.pool'
                                    ,'zip5.has.school'
                                    ,'stories.number'
                                    ,'census.tract.has.retail'
                                    ,'zip5.has.park'
                                    ,'bedrooms'
                                    ,'bathrooms'
                                    ,'factor.is.new.construction'
                                    ,'census.tract.has.school'
                                    ,'year.built'
                                    ,'census.tract.has.park'
                                    ,'basement.square.feet'
                                    )

    chopra.level = c( 'land.square.footage'
                     ,'living.area'
                     ,'bedrooms'
                     ,'bathrooms'
                     ,'parking.spaces'
                     ,'median.household.income'
                     ,'year.built'
                     ,'fraction.owner.occupied'
                     ,'avg.commute.time'
                     ,'factor.is.new.construction'
                     ,'factor.has.pool'
                     )
    identification = c( 'recordingDate'
                       ,'saleDate'
                       ,'apn'
                       ,'census.tract'
                       ,'zip5'
                       )
    prices = c( 'price'
               ,'price.log'
               )


    result <-
        switch( which
               # used by Sumit Chopra in his thesis
               ,chopra.level = chopra.level

               # all possible predictors (list build from the splits directory)
               ,all.level = all.level

               # predictors that are present in every transaction (always present)
               ,always.level.no.census = always.level.no.census  # excludes census data
               ,always.level = c(always.level.census, always.level.no.census)

               # identification of transactions
               ,identification = identification

               # prices for transactions
               ,prices = prices
               ,stop('bad which')
               )
    result
}
