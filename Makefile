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

targets += $(working)/thesis-input-processing.pdf
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
