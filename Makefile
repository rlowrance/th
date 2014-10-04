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

# EXPERIMENT TARGETS

targets += $(working)/e-avm-variants--training-30.RData
targets += $(working)/e-avm-variants--training-30.txt
targets += $(working)/e-avm-variants--training-60.RData
targets += $(working)/e-avm-variants--training-60.txt
targets += $(working)/e-avm-variants--training-90.RData
targets += $(working)/e-avm-variants--training-90.txt
#$(warning e-avm-variants targets is $(targets))

targets += $(working)/e-forms--trainingDays-30--testSample-0.001000.RData
targets += $(working)/e-forms--trainingDays-30--testSample-0.001000.txt
targets += $(working)/e-forms--trainingDays-30--testSample-0.010000.RData
targets += $(working)/e-forms--trainingDays-30--testSample-0.010000.txt
targets += $(working)/e-forms--trainingDays-90--testSample-0.001000.RData
targets += $(working)/e-forms--trainingDays-90--testSample-0.001000.RData
targets += $(working)/e-forms--trainingDays-90--testSample-0.010000.RData
targets += $(working)/e-forms--trainingDays-90--testSample-0.010000.RData
targets += $(working)/e-forms--trainingDays-120--testSample-0.001000.txt
targets += $(working)/e-forms--trainingDays-120--testSample-0.001000.txt
targets += $(working)/e-forms--trainingDays-120--testSample-0.010000.txt
targets += $(working)/e-forms--trainingDays-120--testSample-0.010000.txt

targets += $(working)/e-median-price-by-month-from-2006-to-2009.pdf
targets += $(working)/e-median-price-by-month-from-2006-to-2009.RData
targets += $(working)/e-median-price-by-year-from-1984-to-2009.pdf
targets += $(working)/e-median-price-by-year-from-1984-to-2009.RData

targets += $(working)/e-training-period--testSampleFraction-0.001000.RData
targets += $(working)/e-training-period--testSampleFraction-0.001000.txt
targets += $(working)/e-training-period--testSampleFraction-0.010000.RData
targets += $(working)/e-training-period--testSampleFraction-0.010000.txt

# SPLITS actually used
targets += $(splits)/apn.RData
targets += $(splits)/avg.commute.time.RData
targets += $(splits)/bathrooms.RData
targets += $(splits)/bedrooms.RData
targets += $(splits)/factor.has.pool.RData
targets += $(splits)/factor.is.new.construction.RData
targets += $(splits)/fraction.owner.occupied.RData
targets += $(splits)/land.square.footage.RData
targets += $(splits)/living.area.RData
targets += $(splits)/median.household.income.RData
targets += $(splits)/parking.spaces.RData
targets += $(splits)/price.RData
targets += $(splits)/price.log.RData
targets += $(splits)/recordingDate.RData
targets += $(splits)/saleDate.RData
targets += $(splits)/sale.month.RData
targets += $(splits)/sale.year.RData
targets += $(splits)/total.assessment.RData
targets += $(splits)/year.built.RData


# transactions RData targets
targets += $(working)/census.RData
targets += $(working)/deeds-al-g.RData
targets += $(working)/parcels-derived-features.RData 
targets += $(working)/parcels-sample.RData 
targets += $(working)/parcels-sfr.RData 
targets += $(working)/transactions.RData 
targets += $(working)/transactions-subset1.RData
targets += $(working)/transactions-subset1-train.RData
targets += $(working)/transactions-subset1-test.RData

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
e-avm-variants.R                    : $(lswl) ReadTransactionSplits.R
e-forms.R                           : $(lswl) ReadTransactionSplits.R
e-median-price.R                    : $(lswl)
e-training-period.R                 : $(dl)   ModelLinearLocal.R ReadTransactionSplits.R
parcels-coded.R                     : $(lrwl) LUSEI.R PROPN.R ReadRawParcels.R
parcels-derived-features.R          : $(lwl)  LUSEI.R PROPN.R ReadParcelsCoded.R ZipN.R
parcels-sample.R                    : $(lrwl) LUSEI.R ReadRawParcels.R
parcels-sfr.R                       : $(lrwl) LUSEI.R ReadRawParcels.R
parcels-sfr-sample.R                : $(lwl)  ReadParcelsSfr.R
transactions.R                      : $(lrwl) BestApns.R ReadCensus.R ReadDeedsAlG.R \
                                              ReadParcelsSfr.R ZipN.R
transactions-subset1.R              : $(lwl)  ReadTransactions.R DEEDC.R SCODE.R TRNTP.R
transactions-subset1-train.R        : $(lwl)  ReadTransactionsSubset1.R
transactions-subset1-train-splits.R : $(lswl) ReadTransactionsSubset1Train.R
thesis-input-processing.Rnw         : $(w)    

# dependencies for data files
#$(splits)/price.RData     : $(working)/transactions-al-sfr-subset.RData
#$(splits)/sale.month.RData: $(working)/transactions-al-sfr-subset.RData
#$(splits)/sale.year.RData : $(working)/transactions-al-sfr-subset.RData

# experiment-driven RData files

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

# E-TRAINING-PERIOD; stem is testSampleFraction

e-training-period-dependencies += e-training-period.R
e-training-period-dependencies += $(splits)/apn.RData
e-training-period-dependencies += $(splits)/avg.commute.time.RData
e-training-period-dependencies += $(splits)/bathrooms.log1p.RData
e-training-period-dependencies += $(splits)/bedrooms.log1p.RData
e-training-period-dependencies += $(splits)/factor.has.pool.RData
e-training-period-dependencies += $(splits)/factor.is.new.construction.RData
e-training-period-dependencies += $(splits)/fraction.owner.occupied.RData
e-training-period-dependencies += $(splits)/land.square.footage.log.RData
e-training-period-dependencies += $(splits)/living.area.log.RData
e-training-period-dependencies += $(splits)/median.household.income.RData
e-training-period-dependencies += $(splits)/parking.spaces.log1p.RData
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
	$(working)/e-avm-variants--training-30.RData \
	$(working)/e-avm-variants--training-60.RData \
	$(working)/e-avm-variants--training-90.RData \
	$(working)/e-avm-variants--training-30.txt \
	$(working)/e-avm-variants--training-60.txt \
	$(working)/e-avm-variants--training-90.txt \
	$(working)/e-forms--trainingDays-30--testSample-0.001000.txt \
	$(working)/e-forms--trainingDays-30--testSample-0.010000.txt \
	$(working)/e-forms--trainingDays-90--testSample-0.001000.txt \
	$(working)/e-forms--trainingDays-90--testSample-0.010000.txt \
	$(working)/e-forms--trainingDays-120--testSample-0.001000.txt \
	$(working)/e-forms--trainingDays-120--testSample-0.010000.txt \
	$(working)/e-median-price-by-month-from-2006-to-2009.pdf \
	$(working)/e-median-price-by-year-from-1984-to-2009.pdf \
	$(working)/e-training-period--testSampleFraction-0.001000.txt \
	$(working)/e-training-period--testSampleFraction-0.010000.txt 
	Rscript -e "library('knitr'); knit('thesis-linear-models.Rnw')"
	pdflatex thesis-linear-models.tex
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
	Rscript transactions-al-sfr.R

$(working)/transactions-subset1.RData: transactions-subset1.R \
	$(working)/transactions.RData
	Rscript transactions-subset1.R

# the stem is RData
$(working)/transactions-subset1-train.% \
$(working)/transactions-subset1-test.% \
: transactions-subset1-train.R
	Rscript transactions-subset1-train.R
