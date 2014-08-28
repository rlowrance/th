# Makefile
data = ../../los-angeles

output = $(data)/output
raw = $(data)/raw
working = $(data)/working

deeds-volume-1 = $(raw)/corelogic-deeds-090402_07
deeds-volume-2 = $(raw)/corelogic-deeds-090402_09

deeds-zip1 = $(deeds-volume-1)/CAC06037F1.zip
deeds-zip2 = $(deeds-volume-1)/CAC06037F2.zip
deeds-zip3 = $(deeds-volume-1)/CAC06037F3.zip
deeds-zip4 = $(deeds-volume-1)/CAC06037F4.zip
deeds-zip5 = $(deeds-volume-2)/CAC06037F5.zip
deeds-zip6 = $(deeds-volume-2)/CAC06037F6.zip
deeds-zip7 = $(deeds-volume-2)/CAC06037F7.zip
deeds-zip8 = $(deeds-volume-2)/CAC06037F8.zip

deeds-al.RData = $(working)/deeds-al.RData

parcels-volume = $(raw)/corelogic-taxrolls-090402_05

parcels-zip1 = $(parcels-volume)/CAC06037F1.zip
parcels-zip2 = $(parcels-volume)/CAC06037F2.zip
parcels-zip3 = $(parcels-volume)/CAC06037F3.zip
parcels-zip4 = $(parcels-volume)/CAC06037F4.zip
parcels-zip5 = $(parcels-volume)/CAC06037F5.zip
parcels-zip6 = $(parcels-volume)/CAC06037F6.zip
parcels-zip7 = $(parcels-volume)/CAC06037F7.zip
parcels-zip8 = $(parcels-volume)/CAC06037F8.zip

parcels-sfr.RData = $(working)/parcels-sfr.RData

parcels-sfr-sample.RData = $(working)/parcels-sfr-sample.RData

census.csv = $(raw)/neighborhood-data/census.csv
census.RData = $(working)/census.RData

targets = $(parcels-sfr-sample.RData) $(deeds-al.RData) $(parcels-sfr.RData) $(census.RData)
$(warning targets is $(targets))

.PHONY: all
all: $(targets)

$(census.RData): census.R Directory.R \
	$(census.csv)
	Rscript census.R

$(deeds-al.RData): deeds-al.R Directory.R PRICATCODE.R \
  $(deeds-zip1) $(deeds-zip2) $(deeds-zip3) $(deeds-zip4) \
  $(deeds-zip5) $(deeds-zip6) $(deeds-zip7) $(deeds-zip8)
	Rscript deeds-al.R

$(parcels-sfr.RData): parcels-sfr.R Directory.R LUSEI.R ReadRawParcels.Rt  \
  $(parcels-zip1) $(parcels-zip2) $(parcels-zip3) $(parcels-zip4) \
  $(parcels-zip5) $(parcels-zip6) $(parcels-zip7) $(parcels-zip8)
	Rscript parcels-sfr.R

$(parcels-sfr-sample.RData): parcels-sfr-sample.R Directory.R ReadParcelsSfr.R \
	$(parcels-sfr.RData)
	Rscript parcels-sfr-sample.R

