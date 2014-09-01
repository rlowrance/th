# Makefile
data = ../../los-angeles

output = $(data)/output
raw = $(data)/raw
working = $(data)/working

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

targets = $(working)/transactions-al-sfr-subset1.RData \
          $(working)/transactions-al-sfr.RData \
		  $(working)/deeds-al-sample.RData \
		  $(working)/parcels-derived-features.RData \
		  $(working)/parcels-sample.RData \
		  $(working)/parcels-sfr-sample.RData \
		  $(working)/deeds-al.RData \
		  $(working)/parcels-sfr.RData \
		  $(working)/census.RData

$(warning targets is $(targets))

.PHONY: all
all: $(targets)

# dependencies in R source files for functions
DirectoryLog.R    : DirectoryData.R
DirectoryOutput.R : DirectoryData.R
DirectoryRaw.R    : DirectoryData.R
DirectoryWorking.R: DirectoryData.R
ZipN.R            : EvaluateWithoutWarnings.R

# dependencies in R source files for main programs
lrwl = DirectoryLog.R DirectoryRaw.R DirectoryWorking.R Libraries.R
lrl  = DirectoryLog.R                DirectoryWorking.R Libraries.R
census.R                  : $(lrwl)
deeds-al-sample.R         : $(lrl)  ReadDeedsAl.R
deeds-al.R                : $(lrwl) BestApns.R PRICATCODE.R
parcels-coded.R           : $(lrl)  LUSEI.R PROPN.R ReadRawParcels.R
parcels-derived-features.R: $(lrl)  LUSEI.R PROPN.R ReadParcelsCoded.R ZipN.R
parcels-sample.R          : $(lrwl) LUSEI.R ReadRawParcels.R
parcels-sfr.R             : $(lrwl) LUSEI.R ReadRawParcels.R
parcels-sfr-sample.R      : $(lrl)  ReadParcelsSfr.R
transactions-al-sfr.R     : $(lrwl) BestApns.R ReadCensus.R ReadDeedsAl.R ReadDeedsAlSample.R \
	                                ReadParcelsSfr.R ReadParcelsSfrSample.R ZipN.R
transactions-al-sfr-subset1.R:      \
	                        $(lwl)  ReadTransactionsAlSfr.R \
	                                DEEDC.R LUSEI.R PRICATCODE.R PROPN.R SCODE.R SLMLT.R TRNTP.R


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

$(working)/transactions-al-sfr-subset1.R: transactions-al-sfr-subset1.R \
	$(working)/transactions-al-sfr.RData
	Rscript transactions-al-sfr-subset1.R
