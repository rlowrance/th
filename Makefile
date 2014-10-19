# Makefile
# debug with --debug=b  (basic debugging)

# disable the built-in rules
.SUFFIXES:

data = ../../los-angeles

drawings = ../drawings
output   = $(data)/output
raw      = $(data)/raw
working  = $(data)/working

splits = $(working)/transactions-subset1-train-splits
tex    = $(data)/tex-generated-from-Rnw

raw-deeds-volume-1 = $(raw)/corelogic-deeds-090402_07
raw-deeds-volume-2 = $(raw)/corelogic-deeds-090402_09
 
raw-deeds += $(raw-deeds-volume-1)/CAC06037F1.zip
raw-deeds += $(raw-deeds-volume-1)/CAC06037F2.zip
raw-deeds += $(raw-deeds-volume-1)/CAC06037F3.zip
raw-deeds += $(raw-deeds-volume-1)/CAC06037F4.zip
raw-deeds += $(raw-deeds-volume-2)/CAC06037F5.zip
raw-deeds += $(raw-deeds-volume-2)/CAC06037F6.zip
raw-deeds += $(raw-deeds-volume-2)/CAC06037F7.zip
raw-deeds += $(raw-deeds-volume-2)/CAC06037F8.zip

raw-parcels-volume = $(raw)/corelogic-taxrolls-090402_05

raw-parcels += $(raw-parcels-volume)/CAC06037F1.zip
raw-parcels += $(raw-parcels-volume)/CAC06037F2.zip
raw-parcels += $(raw-parcels-volume)/CAC06037F3.zip
raw-parcels += $(raw-parcels-volume)/CAC06037F4.zip
raw-parcels += $(raw-parcels-volume)/CAC06037F5.zip
raw-parcels += $(raw-parcels-volume)/CAC06037F6.zip
raw-parcels += $(raw-parcels-volume)/CAC06037F7.zip
raw-parcels += $(raw-parcels-volume)/CAC06037F8.zip

raw-census.csv = $(raw)/neighborhood-data/census.csv

thesis-input-processing.pdf = $(working)/thesis-input-processing.pdf
thesis-linear-models.pdf    = $(working)/thesis-linear-models.pdf

# PREDICTORS (see Predictors.R)
predictors.chopra.level += $(splits)/land.square.footage.RData
predictors.chopra.level += $(splits)/living.area.RData
predictors.chopra.level += $(splits)/bedrooms.RData
predictors.chopra.level += $(splits)/bathrooms.RData
predictors.chopra.level += $(splits)/parking.spaces.RData
predictors.chopra.level += $(splits)/median.household.income.RData
predictors.chopra.level += $(splits)/year.built.RData
predictors.chopra.level += $(splits)/fraction.owner.occupied.RData
predictors.chopra.level += $(splits)/avg.commute.time.RData
predictors.chopra.level += $(splits)/factor.is.new.construction.RData
predictors.chopra.level += $(splits)/factor.has.pool.RData

predictors.always.level += $(splits)/avg.commute.time.RData
predictors.always.level += $(splits)/basement.square.feet.RData
predictors.always.level += $(splits)/bathrooms.RData
predictors.always.level += $(splits)/bedrooms.RData
predictors.always.level += $(splits)/census.tract.has.industry.RData
predictors.always.level += $(splits)/census.tract.has.park.RData
predictors.always.level += $(splits)/census.tract.has.retail.RData
predictors.always.level += $(splits)/census.tract.has.school.RData
predictors.always.level += $(splits)/effective.year.built.RData
predictors.always.level += $(splits)/factor.has.pool.RData
predictors.always.level += $(splits)/factor.is.new.construction.RData
predictors.always.level += $(splits)/fireplace.number.RData
predictors.always.level += $(splits)/fraction.owner.occupied.RData
predictors.always.level += $(splits)/garage.parking.square.feet.RData
predictors.always.level += $(splits)/land.square.footage.RData
predictors.always.level += $(splits)/living.area.RData
predictors.always.level += $(splits)/median.household.income.RData
predictors.always.level += $(splits)/parking.spaces.RData
predictors.always.level += $(splits)/stories.number.RData
predictors.always.level += $(splits)/total.rooms.RData
predictors.always.level += $(splits)/year.built.RData
predictors.always.level += $(splits)/zip5.has.industry.RData
predictors.always.level += $(splits)/zip5.has.park.RData
predictors.always.level += $(splits)/zip5.has.retail.RData
predictors.always.level += $(splits)/zip5.has.school.RData



# EXPERIMENT TARGETS

#targets += $(working)/e-adjust-training-period--query.fraction-0.001000.RData
#targets += $(working)/e-adjust-training-period--query.fraction-0.001000.txt
#targets += $(working)/e-adjust-training-period--query.fraction-0.010000.RData
#targets += $(working)/e-adjust-training-period--query.fraction-0.010000.txt
#
#targets += $(working)/e-avm-variants--training-30.RData
#targets += $(working)/e-avm-variants--training-30.txt
#targets += $(working)/e-avm-variants--training-60.RData
#targets += $(working)/e-avm-variants--training-60.txt
#targets += $(working)/e-avm-variants--training-90.RData
#targets += $(working)/e-avm-variants--training-90.txt
#$(warning e-avm-variants targets is $(targets))

#targets += $(working)/e-forms--trainingDays-30--testSample-0.001000.RData
#targets += $(working)/e-forms--trainingDays-30--testSample-0.001000.txt
#targets += $(working)/e-forms--trainingDays-30--testSample-0.010000.RData
#targets += $(working)/e-forms--trainingDays-30--testSample-0.010000.txt
#targets += $(working)/e-forms--trainingDays-90--testSample-0.001000.RData
#targets += $(working)/e-forms--trainingDays-90--testSample-0.001000.RData
#targets += $(working)/e-forms--trainingDays-90--testSample-0.010000.RData
#targets += $(working)/e-forms--trainingDays-90--testSample-0.010000.RData
#targets += $(working)/e-forms--trainingDays-120--testSample-0.001000.txt
#targets += $(working)/e-forms--trainingDays-120--testSample-0.001000.txt
#targets += $(working)/e-forms--trainingDays-120--testSample-0.010000.txt
#targets += $(working)/e-forms--trainingDays-120--testSample-0.010000.txt
#
#targets += $(working)/e-median-price-by-month-from-2006-to-2009.pdf
#targets += $(working)/e-median-price-by-month-from-2006-to-2009.RData
#targets += $(working)/e-median-price-by-year-from-1984-to-2009.pdf
#targets += $(working)/e-median-price-by-year-from-1984-to-2009.RData

#targets += $(working)/e-penalized-regression--query.fraction-0.001000.RData
#targets += $(working)/e-penalized-regression--query.fraction-0.001000.txt
#targets += $(working)/e-penalized-regression--query.fraction-0.010000.RData
#targets += $(working)/e-penalized-regression--query.fraction-0.010000.txt
#$(warning targets is $(targets))

#targets += $(working)/e-training-period--testSampleFraction-0.001000.RData
#targets += $(working)/e-training-period--testSampleFraction-0.001000.txt
#targets += $(working)/e-training-period--testSampleFraction-0.010000.RData
#targets += $(working)/e-training-period--testSampleFraction-0.010000.txt
#
#
## SPLITS actually used
#targets += $(splits)/apn.RData
#targets += $(splits)/avg.commute.time.RData
#targets += $(splits)/bathrooms.RData
#targets += $(splits)/bedrooms.RData
#targets += $(splits)/factor.has.pool.RData
#targets += $(splits)/factor.is.new.construction.RData
#targets += $(splits)/fraction.owner.occupied.RData
#targets += $(splits)/land.square.footage.RData
#targets += $(splits)/living.area.RData
#targets += $(splits)/median.household.income.RData
#targets += $(splits)/parking.spaces.RData
#targets += $(splits)/price.RData
#targets += $(splits)/price.log.RData
#targets += $(splits)/recordingDate.RData
#targets += $(splits)/saleDate.RData
#targets += $(splits)/sale.month.RData
#targets += $(splits)/sale.year.RData
#targets += $(splits)/total.assessment.RData
#targets += $(splits)/year.built.RData
#
#
## transactions RData targets
#targets += $(working)/census.RData
#targets += $(working)/deeds-al-g.RData
#targets += $(working)/parcels-derived-features.RData 
#targets += $(working)/parcels-sample.RData 
#targets += $(working)/parcels-sfr.RData 
#targets += $(working)/transactions.RData 
#targets += $(working)/transactions-subset1.RData
#targets += $(working)/transactions-subset1-train.RData
#targets += $(working)/transactions-subset1-test.RData
#
#
# thesis targets
targets += $(working)/thesis-input-processing.pdf
targets += $(working)/thesis-linear-models.pdf

#$(warning targets is $(targets))

# default rule
.PHONY: all
all: $(targets)

# dependencies in R source files for functions
DirectoryLog.R         : DirectoryData.R
DirectoryOutput.R      : DirectoryData.R
DirectoryRaw.R         : DirectoryData.R
DirectoryWorking.R     : DirectoryData.R
ReadTransactionSplits.R: ReadSplit.R
ZipN.R                 : EvaluateWithoutWarnings.R

# dependencies in R source files for main programs
lrwl = DirectoryLog.R DirectoryRaw.R                    DirectoryWorking.R Libraries.R
lwl  = DirectoryLog.R                                   DirectoryWorking.R Libraries.R
lswl = DirectoryLog.R                DirectorySplits.R  DirectoryWorking.R Libraries.R
w    =                                                  DirectoryWorking.R
dl   = Directory.R                                                         Libraries.R

census.R                            : $(lrwl)
deeds-al-sample.R                   : $(lwl)  ReadDeedsAl.R
deeds-al-g.R                        : $(lrwl) DEEDC.R PRICATCODE.R
e-adjust-training-period.R          : $(dl)   ModelLinearLocal.R ReadTransactionSplits.R
e-avm-variants.R                    : $(dl)   ReadTransactionSplits.R
e-features-pca.R                    : $(dl)   Predictors.R ReadTransactionSplits.R
e-features-pca-chart.R              : $(dl)   
e-features-lcv.R                    : $(dl)   ModelLinearLocal.R Predictors.R ReadTransactionSplits.R
e-features-lcv-chart.R              : $(dl)   
e-forms.R                           : $(lswl) ReadTransactionSplits.R
e-median-price.R                    : $(dl)   ReadTransactionsSubset1.R
e-penalized-regression.R            : $(dl)   ModelLinearLocal.R ReadTransactionSplits.R
e-training-period.R                 : $(dl)   ModelLinearLocal.R ReadTransactionSplits.R
parcels-coded.R                     : $(lrwl) LUSEI.R PROPN.R ReadRawParcels.R
parcels-derived-features.R          : $(dl)   LUSEI.R PROPN.R ReadParcelsCoded.R ZipN.R
parcels-sample.R                    : $(lrwl) LUSEI.R ReadRawParcels.R
parcels-sfr.R                       : $(lrwl) LUSEI.R ReadRawParcels.R
parcels-sfr-sample.R                : $(lwl)  ReadParcelsSfr.R
transactions.R                      : $(dl)   BestApns.R ReadCensus.R ReadDeedsAlG.R \
                                              ReadParcelsSfr.R ZipN.R
transactions-subset1.R              : $(lwl)  ReadTransactions.R DEEDC.R SCODE.R TRNTP.R
transactions-subset1-train.R        : $(dl)   ReadTransactionsSubset1.R
transactions-subset1-train-splits.R : $(dl)   ReadTransactionsSubset1Train.R
thesis-input-processing.Rnw         : $(w)    


# experiment-driven RData files

# E-ADJUST-TRAINING-PERIOD

e-adjust-training-period-dependencies += e-adjust-training-period.R
e-adjust-training-period-dependencies += $(splits)/land.square.footage.RData
e-adjust-training-period-dependencies += $(splits)/living.area.RData
e-adjust-training-period-dependencies += $(splits)/bedrooms.RData
e-adjust-training-period-dependencies += $(splits)/bathrooms.RData
e-adjust-training-period-dependencies += $(splits)/parking.spaces.RData
e-adjust-training-period-dependencies += $(splits)/median.household.income.RData
e-adjust-training-period-dependencies += $(splits)/year.built.RData
e-adjust-training-period-dependencies += $(splits)/fraction.owner.occupied.RData
e-adjust-training-period-dependencies += $(splits)/avg.commute.time.RData
e-adjust-training-period-dependencies += $(splits)/factor.is.new.construction.RData
e-adjust-training-period-dependencies += $(splits)/factor.has.pool.RData
e-adjust-training-period-dependencies += $(splits)/saleDate.RData
e-adjust-training-period-dependencies += $(splits)/recordingDate.RData
e-adjust-training-period-dependencies += $(splits)/price.RData
e-adjust-training-period-dependencies += $(splits)/price.log.RData
e-adjust-training-period-dependencies += $(splits)/apn.RData
#$(warning e-adjust-training-period-dependencies is $(e-adjust-training-period-dependencies))

# the stem is query.fraction
$(working)/e-adjust-training-period--%-0.001000.RData \
$(working)/e-adjust-training-period--%-0.001000.txt \
: $(e-adjust-training-period-dependencies)
	Rscript e-adjust-training-period.R --query.fraction 0.001000

$(working)/e-adjust-training-period--%-0.010000.RData \
$(working)/e-adjust-training-period--%-0.010000.txt \
: $(e-adjust-training-period-dependencies)
	Rscript e-adjust-training-period.R --query.fraction 0.010000



# E-AVM-VARIANTS

e-avm-variants-dependencies += e-avm-variants.R
e-avm-variants-dependencies += $(splits)/land.square.footage.RData
e-avm-variants-dependencies += $(splits)/living.area.RData
e-avm-variants-dependencies += $(splits)/bedrooms.RData
e-avm-variants-dependencies += $(splits)/bathrooms.RData
e-avm-variants-dependencies += $(splits)/parking.spaces.RData
e-avm-variants-dependencies += $(splits)/median.household.income.RData
e-avm-variants-dependencies += $(splits)/year.built.RData
e-avm-variants-dependencies += $(splits)/fraction.owner.occupied.RData
e-avm-variants-dependencies += $(splits)/avg.commute.time.RData
e-avm-variants-dependencies += $(splits)/factor.is.new.construction.RData
e-avm-variants-dependencies += $(splits)/factor.has.pool.RData
e-avm-variants-dependencies += $(splits)/total.assessment.RData
e-avm-variants-dependencies += $(splits)/saleDate.RData
e-avm-variants-dependencies += $(splits)/recordingDate.RData
e-avm-variants-dependencies += $(splits)/price.RData
e-avm-variants-dependencies += $(splits)/price.log.RData
e-avm-variants-dependencies += $(splits)/apn.RData
#$(warning e-avm-variants-dependencies is $(e-avm-variants-dependencies))
#$(warning working is $(working))

# the stem is 'training'
$(working)/e-avm-variants--%-30.RData \
$(working)/e-avm-variants--%-30.txt \
: $(e-avm-variants-dependencies)
	Rscript e-avm-variants.R --training 30

$(working)/e-avm-variants--%-60.RData \
$(working)/e-avm-variants--%-60.txt \
: $(e-avm-variants-dependencies)
	Rscript e-avm-variants.R --training 60

$(working)/e-avm-variants--%-90.RData \
$(working)/e-avm-variants--%-90.txt \
: $(e-avm-variants-dependencies)
	Rscript e-avm-variants.R --training 90

# e-features-lcv-chart always stem is query
$(working)/e-features-lcv-chart--predictors-always--%-100_1.txt \
$(working)/e-features-lcv-chart--predictors-always--%-100_2.txt \
$(working)/e-features-lcv-chart--predictors-always--%-100_3.pdf \
$(working)/e-features-lcv-chart--predictors-always--%-100_4.pdf \
$(working)/e-features-lcv-chart--predictors-always--%-100_5.pdf \
: \
e-features-lcv-chart.R \
$(working)/e-features-lcv--predictors-always--query-100.RData
	Rscript e-features-lcv-chart.R --predictors always --query 100

# e-features-lcv
$(working)/e-features-lcv--predictors-always--query-100.RData \
: \
e-features-lcv.R \
$(predictors.always.level)	
	Rscript e-features-lcv.R --predictor always --query 100

# e-features-lcv-chart chopra stem is query
$(working)/e-features-lcv-chart--predictors-chopra--%-100_1.txt \
$(working)/e-features-lcv-chart--predictors-chopra--%-100_2.txt \
$(working)/e-features-lcv-chart--predictors-chopra--%-100_3.pdf \
$(working)/e-features-lcv-chart--predictors-chopra--%-100_4.pdf \
$(working)/e-features-lcv-chart--predictors-chopra--%-100_5.pdf \
: \
e-features-lcv-chart.R \
$(working)/e-features-lcv--predictors-chopra--query-100.RData
	Rscript e-features-lcv-chart.R --predictors chopra --query 100

# e-features-lcv
$(working)/e-features-lcv--predictors-chopra--query-100.RData \
: \
e-features-lcv.R \
$(predictors.chopra.level)	
	Rscript e-features-lcv.R --predictor chopra --query 100

# e-features-pca-chart always
# stem is txt
$(working)/e-features-pca-chart--predictors-always_1.% \
$(working)/e-features-pca-chart--predictors-always_2_01.% \
$(working)/e-features-pca-chart--predictors-always_2_02.% \
$(working)/e-features-pca-chart--predictors-always_2_03.% \
$(working)/e-features-pca-chart--predictors-always_2_04.% \
$(working)/e-features-pca-chart--predictors-always_2_05.% \
$(working)/e-features-pca-chart--predictors-always_2_06.% \
$(working)/e-features-pca-chart--predictors-always_2_07.% \
$(working)/e-features-pca-chart--predictors-always_2_08.% \
$(working)/e-features-pca-chart--predictors-always_2_09.% \
$(working)/e-features-pca-chart--predictors-always_2_10.% \
: \
e-features-pca-chart.R \
$(working)/e-features-pca--predictors-always.RData
	Rscript e-features-pca-chart.R --predictors always

# e-features-pca always
$(working)/e-features-pca--predictors-always.RData \
: \
e-features-pca.R \
$(predictors.always.level)
	Rscript e-features-pca.R --predictors always


# e-features-pca-chart chopra
# stem is txt
$(working)/e-features-pca-chart--predictors-chopra_1.% \
$(working)/e-features-pca-chart--predictors-chopra_2_01.% \
$(working)/e-features-pca-chart--predictors-chopra_2_02.% \
$(working)/e-features-pca-chart--predictors-chopra_2_03.% \
$(working)/e-features-pca-chart--predictors-chopra_2_04.% \
$(working)/e-features-pca-chart--predictors-chopra_2_05.% \
$(working)/e-features-pca-chart--predictors-chopra_2_06.% \
$(working)/e-features-pca-chart--predictors-chopra_2_07.% \
$(working)/e-features-pca-chart--predictors-chopra_2_08.% \
$(working)/e-features-pca-chart--predictors-chopra_2_09.% \
$(working)/e-features-pca-chart--predictors-chopra_2_10.% \
: \
e-features-pca-chart.R \
$(working)/e-features-pca--predictors-chopra.RData
	Rscript e-features-pca-chart.R --predictors chopra

# e-features-pca chopra
$(working)/e-features-pca--predictors-chopra.RData \
: \
e-features-pca.R \
$(predictors.chopra.level)
	Rscript e-features-pca.R --predictors chopra

# E-FORMS
# the stem is trainingDays
e-forms-dependencies += e-forms.R
e-forms-dependencies += $(working)/apn.RData
e-forms-dependencies += $(working)/avg.commute.time.RData
e-forms-dependencies += $(working)/bathrooms.RData
e-forms-dependencies += $(working)/bathrooms.log1p.RData
e-forms-dependencies += $(working)/bedrooms.RData
e-forms-dependencies += $(working)/bedrooms.log1p.RData
e-forms-dependencies += $(working)/factor.has.pool.RData
e-forms-dependencies += $(working)/factor.is.new.construction.RData
e-forms-dependencies += $(working)/fraction.owner.occupied.RData
e-forms-dependencies += $(working)/land.square.footage.RData
e-forms-dependencies += $(working)/land.square.footage.log.RData
e-forms-dependencies += $(working)/living.area.RData
e-forms-dependencies += $(working)/living.area.log.RData
e-forms-dependencies += $(working)/median.household.income.RData
e-forms-dependencies += $(working)/parking.spaces.RData
e-forms-dependencies += $(working)/parking.spaces.log1p.RData
e-forms-dependencies += $(working)/price.RData
e-forms-dependencies += $(working)/price.log.RData
e-forms-dependencies += $(working)/recordingDate.RData
e-forms-dependencies += $(working)/saleDate.RData
e-forms-dependencies += $(working)/year.built.RData
#$(warning e-forms-dependencies is $(e-forms-dependencies))

$(working)/e-forms--%-30--testSample-0.001000.RData \
$(working)/e-forms--%-30--testSample-0.001000.txt \
: $(e-forms-depndencies)
	Rscript e-forms.R --trainingDays 30 --testSample 0.001000

$(working)/e-forms--%-30--testSample-0.010000.RData \
$(working)/e-forms--%-30--testSample-0.010000.txt \
: $(e-forms-depndencies)
	Rscript e-forms.R --trainingDays 30 --testSample 0.010000

$(working)/e-forms--%-90--testSample-0.001000.RData \
$(working)/e-forms--%-90--testSample-0.001000.txt \
: $(e-forms-depndencies)
	Rscript e-forms.R --trainingDays 90 --testSample 0.001000

$(working)/e-forms--%-90--testSample-0.010000.RData \
$(working)/e-forms--%-90--testSample-0.010000.txt \
: $(e-forms-depndencies)
	Rscript e-forms.R --trainingDays 90 --testSample 0.010000

$(working)/e-forms--%-120--testSample-0.001000.RData \
$(working)/e-forms--%-120--testSample-0.001000.txt \
: $(e-forms-depndencies)
	Rscript e-forms.R --trainingDays 120 --testSample 0.001000

$(working)/e-forms--%-120--testSample-0.01000.RData \
$(working)/e-forms--%-120--testSample-0.01000.txt \
: $(e-forms-depndencies)
	Rscript e-forms.R --trainingDays 120 --testSample 0.01000

# E-MEDIAN-PRICE

e-median-price-dependencies += e-median-price.R
e-median-price-dependencies += ReadTransactionsSubset1.R
e-median-price-dependencies += $(working)/transactions-subset1.RData
e-median-price-dependencies += $(splits)/price.RData
e-median-price-dependencies += $(splits)/sale.year.RData
e-median-price-dependencies += $(splits)/sale.month.RData
#$(warning e-median-price-dependencies is $(e-median-price-dependencies))

# stem is 2009
$(working)/e-median-price-by-year-from-1984-to-%.pdf \
$(working)/e-median-price-by-year-from-1984-to-%.RData \
: $(e-median-price-dependencies)
	RScript e-median-price.R --by year --from 1984 --to 2009

$(working)/e-median-price-by-month-from-2006-to-%.pdf \
$(working)/e-median-price-by-month-from-2006-to-%.Rdata \
: $(e-median-price-dependencies)
	RScript e-median-price.R --by month --from 2006 --to 2009

# E-PENALIZED-REGRESSION

e-penalized-regression-dependenceis += e-penalized-regression.R
# predictors used
e-penalized-regression-dependencies += $(splits)/air.conditioning.code.RData
e-penalized-regression-dependencies += $(splits)/avg.commute.time.RData
e-penalized-regression-dependencies += $(splits)/basement.square.feet.RData
e-penalized-regression-dependencies += $(splits)/bathrooms.RData.RData
e-penalized-regression-dependencies += $(splits)/bedrooms.RData.RData
e-penalized-regression-dependencies += $(splits)/census.tract.has.industry.RData
e-penalized-regression-dependencies += $(splits)/census.tract.has.park.RData
e-penalized-regression-dependencies += $(splits)/census.tract.has.retail.RData
e-penalized-regression-dependencies += $(splits)/census.tract.has.school.RData
e-penalized-regression-dependencies += $(splits)/condition.code.RData
e-penalized-regression-dependencies += $(splits)/construction.type.code.RData
e-penalized-regression-dependencies += $(splits)/effective.year.built.RData
e-penalized-regression-dependencies += $(splits)/exterior.walls.code.RData
e-penalized-regression-dependencies += $(splits)/factor.foundation.type.RData
e-penalized-regression-dependencies += $(splits)/factor.has.pool.RData
e-penalized-regression-dependencies += $(splits)/factor.heating.code.RData
e-penalized-regression-dependencies += $(splits)/factor.is.new.construction.RData
e-penalized-regression-dependencies += $(splits)/factor.parking.type.RData
e-penalized-regression-dependencies += $(splits)/factor.roof.type.RData
e-penalized-regression-dependencies += $(splits)/fireplace.indicator.flag.RData
e-penalized-regression-dependencies += $(splits)/fireplace.number.RData
e-penalized-regression-dependencies += $(splits)/fireplace.type.code.RData
e-penalized-regression-dependencies += $(splits)/floor.code.RData
e-penalized-regression-dependencies += $(splits)/foundation.code.RData
e-penalized-regression-dependencies += $(splits)/fraction.owner.occupied.RData
e-penalized-regression-dependencies += $(splits)/garage.code.RData
e-penalized-regression-dependencies += $(splits)/garage.parking.square.feet.RData
e-penalized-regression-dependencies += $(splits)/heating.code.RData
e-penalized-regression-dependencies += $(splits)/land.square.footage.RData
e-penalized-regression-dependencies += $(splits)/living.area.RData
e-penalized-regression-dependencies += $(splits)/median.household.income.RData
e-penalized-regression-dependencies += $(splits)/parking.spacies.RData
e-penalized-regression-dependencies += $(splits)/parking.type.code.RData
e-penalized-regression-dependencies += $(splits)/pool.code.RData
e-penalized-regression-dependencies += $(splits)/quality.code.RData
e-penalized-regression-dependencies += $(splits)/roof.cover.code.RData
e-penalized-regression-dependencies += $(splits)/sewer.code.RData
e-penalized-regression-dependencies += $(splits)/stories.number.RData
e-penalized-regression-dependencies += $(splits)/total.rooms.RData
e-penalized-regression-dependencies += $(splits)/water.code.RData
e-penalized-regression-dependencies += $(splits)/year.built.RData
e-penalized-regression-dependencies += $(splits)/zip5.has.industry.RData
e-penalized-regression-dependencies += $(splits)/zip5.has.park.RData
e-penalized-regression-dependencies += $(splits)/zip5.has.retail.RData
e-penalized-regression-dependencies += $(splits)/zip5.has.school.RData
# other splits
e-penalized-regression-dependencies += $(splits)/apn.RData
e-penalized-regression-dependencies += $(splits)/price.RData
e-penalized-regression-dependencies += $(splits)/price.log.RData
e-penalized-regression-dependencies += $(splits)/recordingDate.RData
e-penalized-regression-dependencies += $(splits)/saleDate.RData

# TODO: add for all splits (above is for chopra's splits)

# E-REDUCED-FEATURES

e-reduced.features.R : Directory.R Libraries.R ModelLinearLocal.R Predictors.R ReadTransactionSplits.R

$(working)/e-reduced-features--query-100.RData \
: \
e-reduced-features.R \
$(predictors.all.level) \
$(splits)/saleDate.RData \
$(splits)/recordingDate.RData \
$(splits)/price.RData \
$(splits)/price.log.RData \
$(splits)/apn.RData
	Rscript e-reduced-features.R --query 100

# E-REDUCED-FEATURES-CHART

e-reduced-features-chart.R : Directory.R Libraries.R CrossValidateCharts.R

# stem is _

$(working)/e-reduced-features-chart--query-100%1.txt \
$(working)/e-reduced-features-chart--query-100%2.txt \
$(working)/e-reduced-features-chart--query-100%3.pdf \
$(working)/e-reduced-features-chart--query-100%4.pdf \
$(working)/e-reduced-features-chart--query-100%5.pdf \
: \
e-reduced-features-chart.R \
$(working)/e-reduced-features--query-100.RData
	Rscript e-reduced-features-chart.R --query 100


# E-TRAINING-PERIOD; stem is testSampleFraction

e-training-period-dependencies += e-training-period.R
e-training-period-dependencies += $(splits)/apn.RData
e-training-period-dependencies += $(splits)/avg.commute.time.RData
e-training-period-dependencies += $(splits)/bathrooms.RData
e-training-period-dependencies += $(splits)/bedrooms.RData
e-training-period-dependencies += $(splits)/factor.has.pool.RData
e-training-period-dependencies += $(splits)/factor.is.new.construction.RData
e-training-period-dependencies += $(splits)/fraction.owner.occupied.RData
e-training-period-dependencies += $(splits)/land.square.footage.RData
e-training-period-dependencies += $(splits)/living.area.RData
e-training-period-dependencies += $(splits)/median.household.income.RData
e-training-period-dependencies += $(splits)/parking.spaces.RData
e-training-period-dependencies += $(splits)/price.log.RData
e-training-period-dependencies += $(splits)/recordingDate.RData
e-training-period-dependencies += $(splits)/saleDate.RData
e-training-period-dependencies += $(splits)/year.built.RData
#$(warning e-training-period-dependencies is $(e-training-period-dependencies))

$(working)/e-training-period--%-0.001000.RData \
$(working)/e-training-period--%-0.001000.txt \
: $(e-training-period-dependencies)
	Rscript e-training-period.R --testSampleFraction 0.001000

$(working)/e-training-period--%-0.010000.RData \
$(working)/e-training-period--%-0.010000.txt \
: $(e-training-period-dependencies)
	Rscript e-training-period.R --testSampleFraction 0.010000



# PDF files (and accompanying tex files)

# THESIS-INPUT-PROCESSING

$(working)/thesis-input-processing.pdf: thesis-input-processing.Rnw \
	$(working)/transactions.RData \
	$(working)/transactions-subset1.RData \
	$(working)/deeds-al-g.RData \
	$(working)/parcels-sfr.RData
	Rscript -e "library('knitr'); knit('thesis-input-processing.Rnw')"
	pdflatex thesis-input-processing.tex
	mv thesis-input-processing.pdf $(working)/
	mv thesis-input-processing.tex $(tex)/

# THESIS-LINEAR-MODELS

$(working)/thesis-linear-models.pdf: thesis-linear-models.Rnw \
	$(drawings)/scenarios.pdf \
	$(working)/e-adjust-training-period--query.fraction-0.001000.txt \
	$(working)/e-adjust-training-period--query.fraction-0.010000.txt \
	$(working)/e-avm-variants--training-30.RData \
	$(working)/e-avm-variants--training-60.RData \
	$(working)/e-avm-variants--training-90.RData \
	$(working)/e-avm-variants--training-30.txt \
	$(working)/e-avm-variants--training-60.txt \
	$(working)/e-avm-variants--training-90.txt \
	$(working)/e-features-lcv-chart--predictors-always--query-100_1.txt \
	$(working)/e-features-lcv-chart--predictors-always--query-100_2.txt \
	$(working)/e-features-lcv-chart--predictors-always--query-100_3.pdf \
	$(working)/e-features-lcv-chart--predictors-always--query-100_4.pdf \
	$(working)/e-features-lcv-chart--predictors-always--query-100_5.pdf \
	$(working)/e-features-lcv-chart--predictors-chopra--query-100_1.txt \
	$(working)/e-features-lcv-chart--predictors-chopra--query-100_2.txt \
	$(working)/e-features-lcv-chart--predictors-chopra--query-100_3.pdf \
	$(working)/e-features-lcv-chart--predictors-chopra--query-100_4.pdf \
	$(working)/e-features-lcv-chart--predictors-chopra--query-100_5.pdf \
	$(working)/e-features-pca-chart--predictors-always_1.txt \
	$(working)/e-features-pca-chart--predictors-always_2_01.txt \
	$(working)/e-features-pca-chart--predictors-always_2_02.txt \
	$(working)/e-features-pca-chart--predictors-always_2_03.txt \
	$(working)/e-features-pca-chart--predictors-chopra_1.txt \
	$(working)/e-features-pca-chart--predictors-chopra_2_01.txt \
	$(working)/e-features-pca-chart--predictors-chopra_2_02.txt \
	$(working)/e-features-pca-chart--predictors-chopra_2_03.txt \
	$(working)/e-forms--trainingDays-30--testSample-0.001000.txt \
	$(working)/e-forms--trainingDays-30--testSample-0.010000.txt \
	$(working)/e-forms--trainingDays-90--testSample-0.001000.txt \
	$(working)/e-forms--trainingDays-90--testSample-0.010000.txt \
	$(working)/e-forms--trainingDays-120--testSample-0.001000.txt \
	$(working)/e-forms--trainingDays-120--testSample-0.010000.txt \
	$(working)/e-median-price-by-month-from-2006-to-2009.pdf \
	$(working)/e-median-price-by-year-from-1984-to-2009.pdf \
	$(working)/e-penalized-regression--query.fraction-0.001000.txt \
	$(working)/e-penalized-regression--query.fraction-0.010000.txt \
	$(working)/e-training-period--testSampleFraction-0.001000.txt \
	$(working)/e-training-period--testSampleFraction-0.010000.txt \
	$(working)/e-reduced-features-chart--query-100_1.txt \
	$(working)/e-reduced-features-chart--query-100_2.txt \
	$(working)/e-reduced-features-chart--query-100_3.pdf \
	$(working)/e-reduced-features-chart--query-100_4.pdf \
	$(working)/e-reduced-features-chart--query-100_5.pdf 
	Rscript -e "library('knitr'); knit('thesis-linear-models.Rnw')"
	pdflatex thesis-linear-models.tex
	bibtex thesis-linear-models
	mv thesis-linear-models.pdf $(working)/
	mv thesis-linear-models.tex $(tex)/

# make all the splits that we use simultaeously
# requires a pattern rule
# here the stem is the RData file name suffix
$(splits)/apn.% \
$(splits)/avg.commute.time.% \
$(splits)/bathrooms.% \
$(splits)/bedrooms.% \
$(splits)/factor.has.pool.% \
$(splits)/factor.is.new.construction.% \
$(splits)/fraction.owner.occupied.% \
$(splits)/land.square.footage.% \
$(splits)/living.area.% \
$(splits)/median.household.income.% \
$(splits)/parking.spaces.% \
$(splits)/price.% \
$(splits)/price.log.% \
$(splits)/recordingDate.% \
$(splits)/saleDate.% \
$(splits)/sale.month.% \
$(splits)/sale.year.% \
$(splits)/total.assessment.% \
$(splits)/year.built.% \
: transactions-subset1-train-splits.R $(working)/transactions-subset1-train.RData
	Rscript transactions-subset1-train-splits.R

$(working)/census.RData: census.R \
	$(raw-census.csv)
	Rscript census.R

$(working)/deeds-al-g.RData: deeds-al-g.R \
	$(raw-deeds)
	Rscript deeds-al-g.R

$(working)/parcels-coded.RData: parcels-coded.R \
	$(raw-parcels)
	RScript parcels-coded.R

$(working)/parcels-derived-features.RData: parcels-derived-features.R \
	$(working)/parcels-coded.RData
	Rscript parcels-derived-features.R

$(working)/parcels-sample.RData: parcels-sample.R \
	$(raw-parcels)
	Rscript parcels-sample.R

$(working)/parcels-sfr.RData: parcels-sfr.R \
	$(raw-parcels)
	Rscript parcels-sfr.R

$(working)/transactions.RData: transactions.R \
	$(working)/census.RData \
	$(working)/deeds-al-g.RData \
	$(raw)/geocoding.tsv \
	$(working)/parcels-sfr.RData \
	$(working)/parcels-derived-features.RData
	Rscript transactions.R

$(working)/transactions-subset1.RData: transactions-subset1.R \
	$(working)/transactions.RData
	Rscript transactions-subset1.R

# the stem is RData
transactions-subset1-train-dependencies += transactions-subset1-train.R
transactions-subset1-train-dependencies += $(working)/transactions-subset1.RData
$(working)/transactions-subset1-train.% \
$(working)/transactions-subset1-test.% \
: $(transactions-subset1-train-dependencies)
	Rscript transactions-subset1-train.R

