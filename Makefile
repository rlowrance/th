# Makefile
data = ../../los-angeles

output  = $(data)/output
raw     = $(data)/raw
working = $(data)/working

splits = $(working)/transactions-al-sfr-subset1-splits

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

e-median-price-ALL.RData += $(working)/e-median-price_by_year_from_1984_to_2009.RData
e-median-price-ALL.RData += $(working)/e-median-price_by_month_from_2006_to_2009.RData
#$(warning e-median-price-ALL.RData is $(e-median-price-ALL.RData))

# experiment targets (all produce .RData files)
targets += $(e-median-price-ALL.RData)

# RData targets
targets += $(splits)/apn.RData
targets += $(working)/census.RData
targets += $(working)/deeds-al.RData
targets += $(working)/deeds-al-sample.RData 
targets += $(working)/parcels-derived-features.RData 
targets += $(working)/parcels-sample.RData 
targets += $(working)/parcels-sfr.RData 
targets += $(working)/parcels-sfr-sample.RData 
targets += $(working)/transactions-al-sfr.RData 
targets += $(working)/transactions-al-sfr-subset1.RData
# thesis targets
targets += $(working)/thesis-linear-models.pdf
targets += $(working)/thesis-input-processing.pdf

$(warning targets is $(targets))

# default rule
.PHONY: all
all: $(targets)

# dependencies in R source files for functions
DirectoryLog.R    : DirectoryData.R
DirectoryOutput.R : DirectoryData.R
DirectoryRaw.R    : DirectoryData.R
DirectoryWorking.R: DirectoryData.R
ZipN.R            : EvaluateWithoutWarnings.R

# dependencies in R source files for main programs
lrwl = DirectoryLog.R DirectoryRaw.R                    DirectoryWorking.R Libraries.R
lwl  = DirectoryLog.R                                   DirectoryWorking.R Libraries.R
lwsl = DirectoryLog.R                DirectorySplits.R  DirectoryWorking.R Libraries.R
w    =                                                  DirectoryWorking.R

census.R                            : $(lrwl)
deeds-al-sample.R                   : $(lwl)  ReadDeedsAl.R
deeds-al.R                          : $(lrwl) BestApns.R PRICATCODE.R
e-median-price.R                    : $(lswl)
parcels-coded.R                     : $(lrwl) LUSEI.R PROPN.R ReadRawParcels.R
parcels-derived-features.R          : $(lwl)  LUSEI.R PROPN.R ReadParcelsCoded.R ZipN.R
parcels-sample.R                    : $(lrwl) LUSEI.R ReadRawParcels.R
parcels-sfr.R                       : $(lrwl) LUSEI.R ReadRawParcels.R
parcels-sfr-sample.R                : $(lwl)  ReadParcelsSfr.R
transactions-al-sfr.R               : $(lrwl) BestApns.R ReadCensus.R ReadDeedsAl.R ReadDeedsAlSample.R \
                                              ReadParcelsSfr.R ReadParcelsSfrSample.R ZipN.R
transactions-al-sfr-subset1.R       : $(lwl)  ReadTransactionsAlSfr.R DEEDC.R SCODE.R TRNTP.R
transactions-al-sfr-subset1-splits.R: $(lwsl) ReadTransactionsAlSfrSubset1.R
thesis-input-processing.Rnw         : $(w)    


# experiment-driven RData files

e-median-price-dependencies += e-median-price.R
e-median-price-dependencies += $(splits)/price.RData
e-median-price-dependencies += $(splits)/sale.month.RData
e-median-price-dependencies += $(splits)/sale.year.RData

$(working)/e-median-price_by_year_from_1984_to_2009.RData: $(e-median-price-dependencies)
	RScript e-median-price.R --by year --from 1984 --to 2009

$(working)/e-median-price_by_month_from_2006_to_2009.RData: $(e-median-price-dependencies)
	RScript e-median-price.R --by month --from 2006 --to 2009

# PDF files (and accompanying tex files)
thesis-input-processing.tex: thesis-input-processing.Rnw \
	$(working)/transactions-al-sfr.RData \
	$(working)/transactions-al-sfr-subset1.RData \
	$(working)/deeds-al.RData \
	$(working)/parcels-sfr.RData
	Rscript -e "library('knitr'); knit('thesis-input-processing.Rnw')"

$(working)/thesis-input-processing.pdf: thesis-input-processing.tex
	pdflatex thesis-input-processing.tex
	mv thesis-input-processing.pdf $(working)/

thesis-linear-models.tex: thesis-linear-models.Rnw
	Rscript -e "library('knitr'); knit('thesis-linear-models.Rnw')"

$(working)/thesis-linear-models.pdf: thesis-linear-models.tex
	pdflatex thesis-linear-models.tex
	mv thesis-linear-models.pdf $(working)/


# the apn.RData target represents all the files in the splits directory
# this recipe creates all of them
$(splits)/apn.RData: transactions-al-sfr-subset1-splits.R \
	$(working)/transactions-al-sfr-subset1.RData
	Rscript transactions-al-sfr-subset1-splits.R

$(working)/census.RData: census.R \
	$(raw-census.csv)
	Rscript census.R

$(working)/deeds-al.RData: deeds-al.R \
	$(raw-deeds)
	Rscript deeds-al.R

$(working)/deeds-al-sample.RData: deeds-al-sample.R \
	$(working)/deeds-al.RData
	Rscript deeds-al-sample.R

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

$(working)/parcels-sfr-sample.RData: parcels-sfr-sample.R \
	$(working)/parcels-sfr.RData
	Rscript parcels-sfr-sample.R

$(working)/transactions-al-sfr.RData: transactions-al-sfr.R \
	$(working)/census.RData \
	$(working)/deeds-al.RData \
	$(working)/deeds-al-sample.RData \
	$(raw)/geocoding.tsv \
	$(working)/parcels-sfr.RData \
	$(working)/parcels-sfr-sample.RData \
	$(working)/parcels-derived-features.RData
	Rscript transactions-al-sfr.R

$(working)/transactions-al-sfr-subset1.RData: transactions-al-sfr-subset1.R \
	$(working)/transactions-al-sfr.RData
	Rscript transactions-al-sfr-subset1.R
