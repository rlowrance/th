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

# always.level includes always.level.no.census
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

predictors.best += $(splits)/living.area.RData
predictors.best += $(splits)/median.household.income.RData
predictors.best += $(splits)/avg.commute.time.RData
predictors.best += $(splits)/fireplace.number.RData
predictors.best += $(splits)/year.built.RData
predictors.best += $(splits)/fraction.owner.occupied.RData

predictors.identification += $(splits)/apn.RData
predictors.identification += $(splits)/census.tract.RData
predictors.identification += $(splits)/recordingDate.RData
predictors.identification += $(splits)/saleDate.RData
predictors.identification += $(splits)/zip5.RData

predictors.prices += $(splits)/price.RData
predictors.prices += $(splits)/price.log.RData

# predictors2 (based on Predictors2.R; generated by program predictors2.makefile.R

include predictors2.makefile


# default rule
.PHONY: all
#all: $(working)/defense.pdf $(working)/experiments.pdf $(working)/thesis.pdf
#all:  $(working)/experiments.pdf $(working)/thesis.pdf
all:  $(working)/thesis.pdf

# dependencies within this makefile
predictors2.makefile: Predictors2.R predictors2.makefile.R
	Rscript predictors2.makefile.R

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

# E-CENSUS-VALUE

#$(warning predictors.always.level $(predictors.always.level))
#$(warning predictors.prices $(predictors.prices))
#$(warning predictors.identification $(predictors.identification))

$(working)/e-census-value--query-100.RData \
: \
e-census-value.R \
Directory.R \
Libraries.R \
After2002.R \
ModelLinearLocal.R \
Predictors.R \
ReadTransactionSplits.R \
$(predictors.always.level) \
$(predictors.identification) \
$(predictors.prices)
	Rscript e-census-value.R --query 100

# E-CENSUS-VALUE-CHART

$(working)/e-census-value-chart--query-100_1.txt \
$(working)/e-census-value-chart--query-100_2.txt \
$(working)/e-census-value-chart--query-100_3.pdf \
$(working)/e-census-value-chart--query-100_4.pdf \
$(working)/e-census-value-chart--query-100_5.pdf \
: \
e-census-value-chart.R \
$(working)/e-census-value--query-100.RData
	Rscript e-census-value-chart.R --query 100



# E-CITY
$(working)/e-city.RData \
: \
e-city.R \
$(splits)/living.area.RData \
$(splits)/median.household.income.RData \
$(splits)/fireplace.number.RData \
$(splits)/avg.commute.time.RData \
$(splits)/fraction.owner.occupied.RData \
$(splits)/effective.year.built.RData \
$(splits)/zip5.has.industry.RData \
$(splits)/total.rooms.RData \
$(splits)/census.tract.has.industry.RData \
$(splits)/parking.spaces.RData \
$(splits)/land.square.footage.RData \
$(splits)/factor.has.pool.RData \
$(splits)/zip5.has.school.RData \
$(splits)/stories.number.RData \
$(splits)/census.tract.has.retail.RData \
$(splits)/zip5.has.park.RData \
$(splits)/bedrooms.RData \
$(splits)/bathrooms.RData \
$(splits)/factor.is.new.construction.RData \
$(splits)/census.tract.has.school.RData \
$(splits)/year.built.RData \
$(splits)/census.tract.has.park.RData \
$(splits)/basement.square.feet.RData \
$(splits)/saleDate.RData \
$(splits)/recordingDate.RData \
$(splits)/price.RData \
$(splits)/price.log.RData \
$(splits)/apn.RData \
$(splits)/property.city.RData 
	Rscript e-city.R

# E-CITY-CHART stem is _

$(working)/e-city-chart%1.txt \
$(working)/e-city-chart%2.txt \
$(working)/e-city-chart%3.txt \
$(working)/e-city-chart%4.txt \
: \
e-city-chart.R \
$(working)/e-city.RData
	Rscript e-city-chart.R

# E-CV

e-cv-data += $(predictors2.always.level)
e-cv-data += $(predictors2.always.log)
e-cv-data += $(predictors2.identification)
e-cv-data += $(predictors2.prices)

e-cv-source += Directory.R
e-cv-source += Libraries.R
e-cv-source += PredictLinear.R
e-cv-source += PredictLinearReg.R
e-cv-source += PredictRandomForest.R
e-cv-source += Predictors2.R
e-cv-source += ReadTransactionSplits.R
e-cv-source += Directory.R

e-cv-generated.makefile: e-cv.R
	Rscript e-cv-generated.R

# this makefile is generated by the program e-cv-generated.R
# define when and how to remake files $(working)/e-cv_SCOPE_MODEL_TIMEPERIOD_ ... .RData
include e-cv-generated.makefile

# E-CV-CHART
# for now, all charts are produced by one program

## create and include make file to state dependencies in e-cv-chart output files
#e-cv-chart-generated.makefile: e-cv-chart.R
#	Rscript e-cv-chart.R --makefile
#
## define symbols e-cv-chart_chart5_data
##                e-cv-chart_chart6_data
##include e-cv-chart-generated.makefile

e-cv-chart-source += Directory.R
e-cv-chart-source += Libraries.R
e-cv-chart-source += CvApplyAllPossibilities.R

e-cv-chart-chart5-generated%makefile \
e-cv-chart-chart6-generated%makefile \
e-cv-chart-chart7-generated%makefile \
e-cv-chart-chart8-generated%makefile \
e-cv-chart-chart9-generated%makefile \
: $(e-cv-chart-source)
	Rscript e-cv-chart.R --makefile

# define variables e-cv-chart-chartN
# define targets   e-cv-chart-chartN-target
include e-cv-chart-chart5-generated.makefile  
include e-cv-chart-chart6-generated.makefile 
include e-cv-chart-chart7-generated.makefile
include e-cv-chart-chart8-generated.makefile
include e-cv-chart-chart9-generated.makefile

#$(warning e-cv-chart-chart5 is $(e-cv-chart-chart5))

# stem is .
$(working)/e-cv-chart_chart5%txt \
$(working)/e-cv-chart_chart6%txt \
$(working)/e-cv-chart_chart7%txt \
$(working)/e-cv-chart_chart8%txt \
$(working)/e-cv-chart_chart9%txt \
: \
$(e-cv-chart-chart5) \
$(e-cv-chart-chart6) \
$(e-cv-chart-chart7) \
$(e-cv-chart-chart8) \
$(e-cv-chart-chart9) \
e-cv-chart.R $(e-cv-chart-source)
	Rscript e-cv-chart.R

#$(working)/e-cv-chart_chart5.txt: e-cv-chart.R
#	Rscript e-cv-chart.R
#
#$(working)/e-cv-chart_chart6.txt: e-cv-chart.R
#	Rscript e-cv-chart.R

# E-FEATURES-LCV

$(working)/e-features-lcv--query-100.RData \
: \
e-features-lcv.R \
Directory.R \
Libraries.R \
After2002.R \
ModelLinearLocal.R \
Predictors.R \
ReadTransactionSplits.R \
$(predictors.alwaysl.level) \
$(predictors.identification) \
$(predictors.prices)
	Rscript e-features-lcv.R --query 100

# E-FEATURES-LCV-CHART stem is _

$(working)/e-features-lcv-chart--query-100%1.txt \
$(working)/e-features-lcv-chart--query-100%2.txt \
$(working)/e-features-lcv-chart--query-100%3.pdf \
$(working)/e-features-lcv-chart--query-100%4.pdf \
$(working)/e-features-lcv-chart--query-100%5.pdf \
: \
e-features-lcv-chart.R \
$(working)/e-features-lcv--query-100.RData
	Rscript e-features-lcv-chart.R --query 100

# E-FEATURES-LCV2

e-features-lcv2.R: Directory.R Libraries.R Predictors.R ReadTransactionSplits.R

e-features-lcv2-data += $(predictors.always.level)
e-features-lcv2-data += $(predictors.identification)
e-features-lcv2-data += $(predictors.price)

$(working)/e-features-lcv2.txt: e-features-lcv2.R $(e-features-lcv2-data)
	Rscript e-features-lcv2.R

e-featues-lcv2-chart.R : Directory.R Libraries.R Lines.R

$(working)/e-features-lcv2-chart_1.txt : e-features-lcv2-chart.R $(working)/e-features-lcv2.txt
	Rscript e-features-lcv2-chart.R

# E-FEATURES-PCA

$(working)/e-features-pca.RData \
: \
e-features-pca.R \
$(predictors.always.level) \
$(predictors.identification) \
$(predictors.prices)
	Rscript e-features-pca.R 


# E-FEATURES-PCA-CHART stem is _
$(working)/e-features-pca-chart%1.txt \
$(working)/e-features-pca-chart%2_01.txt \
$(working)/e-features-pca-chart%2_02.txt \
$(working)/e-features-pca-chart%2_03.txt \
$(working)/e-features-pca-chart%2_04.txt \
$(working)/e-features-pca-chart%2_05.txt \
$(working)/e-features-pca-chart%2_06.txt \
$(working)/e-features-pca-chart%2_07.txt \
$(working)/e-features-pca-chart%2_08.txt \
$(working)/e-features-pca-chart%2_09.txt \
$(working)/e-features-pca-chart%2_10.txt \
: \
e-features-pca-chart.R \
$(working)/e-features-pca.RData
	Rscript e-features-pca-chart.R

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

# E-PRICE

$(working)/e-price.RData : e-price.R
	Rscript e-price.R

e-price.R : Directory.R Libraries.R ReadTransactionsSubset1.R $(working)/transactions-subset1.RData

# E-PRICE-CHART

# stem is .
$(working)/e-price-chart_chart1%pdf \
$(working)/e-price-chart_chart2%pdf \
: \
e-price-chart.R 
	Rscript e-price-chart.R
	
e-price-chart.R : Directory.R Libraries.R $(working)/e-price.RData


# E-RANDOM-FORESTS-GLOBAL 

$(working)/e-random-forests-global--hpset-a--year-2003-month-jan.RData \
: \
e-random-forests-global.R \
Directory.R \
Libraries.R \
After2002.R \
ParseCommandArgsERandomForestsGlobal.R \
Predictors.R \
PredictorsBest.R \
ReadTransactionSplits.R \
$(predictors.best) \
$(predictors.identification) \
$(predictors.prices)
	Rscript e-random-forests-global.R --hpset a --year 2003 --month jan

# E-RANDOM-FORESTS-GLOBAL-CHART stem is %

$(working)/e-random-forests-global-chart--hpset-a--year-2003--month-jan%1.txt \
$(working)/e-random-forests-global-chart--hpset-a--year-2003--month-jan%2.txt \
$(working)/e-random-forests-global-chart--hpset-a--year-2003--month-jan%3.pdf \
$(working)/e-random-forests-global-chart--hpset-a--year-2003--month-jan%4.pdf \
$(working)/e-random-forests-global-chart--hpset-a--year-2003--month-jan%5.pdf \
$(working)/e-random-forests-global-chart--hpset-a--year-2003--month-jan%6.pdf \
: \
e-random-forests-global-chart.R \
Directory.R \
Libraries.R \
CrossValidateCharts.R \
ParseCommandArgsERandomForestsGlobal.R \
$(working)/e-random-forests-global--query-1.RData
	Rscript e-random-forests-global-chart.R --hpset a --year 2003 --month jan

# E-REDUCED-FEATURES

$(working)/e-reduced-features--query-100.RData \
: \
e-reduced-features.R \
Directory.R \
Libraries.R \
After2002.R \
ModelLinearLocal.R \
Predictors.R \
ReadTransactionSplits.R \
$(predictors.all.level) \
$(predictors.identification) \
$(predictors.prices) 
	Rscript e-reduced-features.R --query 100

# E-REDUCED-FEATURES-CHART stem is _

$(working)/e-reduced-features-chart--query-100%1.txt \
$(working)/e-reduced-features-chart--query-100%2.txt \
$(working)/e-reduced-features-chart--query-100%3.pdf \
$(working)/e-reduced-features-chart--query-100%4.pdf \
$(working)/e-reduced-features-chart--query-100%5.pdf \
: \
e-reduced-features-chart.R \
Directory.R \
Libraries.R \
CrossValidateCharts.R \
$(working)/e-reduced-features--query-100.RData
	Rscript e-reduced-features-chart.R --query 100

# E-RIDGE-REGRESSION

$(working)/e-ridge-regression--query-100--lambdaSet-a.RData \
: \
e-ridge-regression.R \
Directory.R \
Libraries.R \
After2002.R \
ParseCommandArgsERidgeRegression.R \
ModelLinearRidgeLocal.R \
Predictors.R \
PredictorsBest.R \
ReadTransactionSplits.R \
$(predictors.best) \
$(predictors.identification) \
$(predictors.prices) 
	Rscript e-ridge-regression.R --query 100 --lambdaSet a




# E-RIDGE-REGRESSION-CHART step is _

$(working)/e-ridge-regression-chart--query-100--lambdaSet-a%1.txt \
$(working)/e-ridge-regression-chart--query-100--lambdaSet-a%2.txt \
$(working)/e-ridge-regression-chart--query-100--lambdaSet-a%3.pdf \
$(working)/e-ridge-regression-chart--query-100--lambdaSet-a%4.pdf \
$(working)/e-ridge-regression-chart--query-100--lambdaSet-a%5.pdf \
$(working)/e-ridge-regression-chart--query-100--lambdaSet-a%6.pdf \
$(working)/e-ridge-regression-chart--query-100--lambdaSet-a%7.pdf \
: \
e-ridge-regression-chart.R \
Directory.R \
Libraries.R \
CrossValidateCharts.R \
ParseCommandArgsERidgeRegression.R \
$(working)/e-ridge-regression--query-100--lambdaSet-a.RData
	Rscript e-ridge-regression-chart.R --query 100 --lambdaSet a



# E-SUBMARKETS

$(working)/e-submarkets--query-100.RData \
: \
e-submarkets.R \
Directory.R \
Libraries.R \
After2002.R \
ModelLinearLocal.R \
ModelLinearSubmarketLocal.R \
Predictors.R \
PredictorsBest.R \
ReadTransactionSplits.R \
$(predictors.best) \
$(predictors.identification) \
$(predictors.price) \
$(splits)/census.tract.RData \
$(splits)/property.city.RData \
$(splits)/zip5.RData 
	Rscript e-submarkets.R --query 100



# E-SUBMARKETS-CHARTS; stem is _

$(working)/e-submarkets-chart--query-100%1.txt \
$(working)/e-submarkets-chart--query-100%2.txt \
$(working)/e-submarkets-chart--query-100%3.pdf \
$(working)/e-submarkets-chart--query-100%4.pdf \
$(working)/e-submarkets-chart--query-100%5.pdf \
$(working)/e-submarkets-chart--query-100%6.pdf \
: \
e-submarkets-charts.R \
$(working)/e-submarkets--query-100.RData
	Rscript e-submarkets-chart.R --query 100



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

# E-VERIFY-ASSESSMENT

e-verify-assessment-data += $(dependencies.identification)
e-verify-assessment-data += $(splits)/total.assessment.RData
e-verify-assessment-data += $(splits)/price.RData

e-verify-assessment-source += e-verify-assessment.R
e-verify-assessment-source += Directory.R
e-verify-assessment-source += Predictors2.R
e-verify-assessment-source += ReadTransactionSplits.R

$(working)/e-verify-assessment.RData : $(e-verify-assessment-source) $(e-verify-assessment-data)
	Rscript e-verify-assessment.R


# E-VERIFY-ASSESSMENT-CHART

e-verify-assessment-chart-data += $(working)/e-verify-assessment.RData

e-verify-assessment-chart-source += e-verify-assessment-chart.R
e-verify-assessment-chart-source += Directory.R
e-verify-assessment-chart-source += Libraries.R
e-verify-assessment-chart-source += ChartCleveland01.R

$(working)/e-verify-assessment-chart_chart1.pdf \
: \
$(e-verify-assessment-chart-source) \
$(e-verify-assessment-chart-data)
	Rscript e-verify-assessment-chart.R


#e-verify-assessment-chart.R : \
#	Directory.R Libraries.R ChartCleveland01.R $(working)/e-verify-assessment.RData


# PDF files (and accompanying tex files)

# DEFENSE

#.PHONY: defense
#defense: $(working)/defense.pdf
#
#$(working)/defense.pdf : \
#defense.Rnw
#	Rscript -e "library('knitr'); knit('defense.Rnw')"
#	pdflatex defense.tex
#	mv defense.pdf $(working)/
#	mv defense.tex $(tex)/
	
# THESIS

.PHONY: thesis
thesis: $(working)/thesis.pdf

thesis-chapters += thesis-chapter-introduction.Rnw
thesis-chapters += thesis-chapter-literature-review.Rnw
thesis-chapters += thesis-chapter-data-munging.Rnw
#thesis-chapters += thesis-chapter-taxonomy.Rnw
thesis-chapters += thesis-chapter-data-selection.Rnw
thesis-chapters += thesis-chapter-best-linear.Rnw

thesis-data-data-munging += $(working)/transactions.RData
thesis-data-data-munging += $(working)/transactions-subset1.RData
thesis-data-data-munging += $(working)/deeds-al-g.RData
thesis-data-data-munging += $(working)/parcels-sfr.RData

thesis-data-data-selection += $(working)/e-cv-chart_chart5.txt
thesis-data-data-selection += $(working)/e-cv-chart_chart6.txt
thesis-data-data-selection += $(working)/e-price-chart_chart1.pdf
thesis-data-data-selection += $(working)/e-price-chart_chart2.pdf
thesis-data-data-selection += $(working)/e-verify-assessment-chart_chart1.pdf

thesis-data-best-linear += $(working)/e-cv-chart_chart7.txt
thesis-data-best-linear += $(working)/e-cv-chart_chart8.txt
thesis-data-best-linear += $(working)/e-cv-chart_chart9_1.pdf
thesis-data-best-linear += $(working)/e-cv-chart_chart9_2.pdf
thesis-data-best-linear += $(working)/e-cv-chart_chart9.txt
thesis-data-best-linear += $(working)/e-features-lcv2-chart_1.txt
thesis-data-best-linear += $(working)/e-features-pca-chart_1.txt
thesis-data-best-linear += $(working)/e-features-pca-chart_2_01.txt
thesis-data-best-linear += $(working)/e-features-pca-chart_2_02.txt
thesis-data-best-linear += $(working)/e-features-pca-chart_2_03.txt

thesis-data += $(thesis-data-data-munging)
thesis-data += $(thesis-data-data-selection)
thesis-data += $(thesis-data-best-linear)

$(working)/thesis.pdf : thesis.Rnw $(thesis-chapters) $(thesis-data)
	Rscript -e "library('knitr'); knit('thesis.Rnw')"
	pdflatex thesis.tex
	mv thesis.pdf $(working)/
	cp thesis.tex $(tex)/
	
.PHONY: thesis-only
thesis-only:
	Rscript -e "library('knitr'); knit('thesis.Rnw')"
	pdflatex thesis.tex
	mv thesis.pdf $(working)/
	cp thesis.tex $(tex)/
	
.PHONY : thesis-final
thesis-final:   # incorporate all bibliography changes
	Rscript -e "library('knitr'); knit('thesis.Rnw')"
	pdflatex thesis.tex
	mv thesis.pdf $(working)/
	cp thesis.tex $(tex)/
	bibtex thesis
	pdflatex thesis.tex
	pdflatex thesis.tex
	mv thesis.pdf $(working)/
	cp thesis.tex $(tex)

.PHONY : bibtex
bibtex:
	bibtex thesis



# EXPERIMENTS (ONE DOCUMENT HAS THEM ALL; USE FOR INTERNAL REVIEWS)

.PHONY: experiments
experiments: $(working)/experiments.pdf

$(working)/experiments.pdf: experiments.Rnw \
	$(drawings)/scenarios.pdf \
	$(working)/e-adjust-training-period--query.fraction-0.001000.txt \
	$(working)/e-adjust-training-period--query.fraction-0.010000.txt \
	$(working)/e-avm-variants--training-30.RData \
	$(working)/e-avm-variants--training-60.RData \
	$(working)/e-avm-variants--training-90.RData \
	$(working)/e-avm-variants--training-30.txt \
	$(working)/e-avm-variants--training-60.txt \
	$(working)/e-avm-variants--training-90.txt \
	$(working)/e-census-value-chart--query-100_4.pdf \
	$(working)/e-city-chart_1.txt \
	$(working)/e-city-chart_3.txt \
	$(working)/e-city-chart_4.txt \
	$(working)/e-features-lcv-chart--query-100_1.txt \
	$(working)/e-features-lcv-chart--query-100_4.pdf \
	$(working)/e-features-pca-chart_1.txt \
	$(working)/e-features-pca-chart_2_01.txt \
	$(working)/e-features-pca-chart_2_02.txt \
	$(working)/e-features-pca-chart_2_03.txt \
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
	$(working)/e-random-forests-global-chart--hpset-a--year-2003--month-jan_1.txt \
	$(working)/e-random-forests-global-chart--hpset-a--year-2003--month-jan_2.txt \
	$(working)/e-random-forests-global-chart--hpset-a--year-2003--month-jan_6.pdf \
	$(working)/e-reduced-features-chart--query-100_1.txt \
	$(working)/e-reduced-features-chart--query-100_4.pdf \
	$(working)/e-ridge-regression-chart--query-100--lambdaSet-a_2.txt \
	$(working)/e-ridge-regression-chart--query-100--lambdaSet-a_7.pdf \
	$(working)/e-submarkets-chart--query-100_1.txt \
	$(working)/e-submarkets-chart--query-100_6.pdf \
	$(working)/e-training-period--testSampleFraction-0.001000.txt \
	$(working)/e-training-period--testSampleFraction-0.010000.txt
	Rscript -e "library('knitr'); knit('experiments.Rnw')"
	pdflatex experiments.tex
	bibtex experiments
	mv experiments.pdf $(working)/
	mv experiments.tex $(tex)/

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

