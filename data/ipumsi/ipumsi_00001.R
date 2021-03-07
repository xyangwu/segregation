# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).
setwd("C:/Users/WXY/Downloads/ipumsi_00001")
list.files()

library(R.utils)
gunzip("ipumsi_00002.dat.gz", remove=FALSE)

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi <- read_ipums_ddi("data/ipumsi_00001/ipumsi_00001.xml")
data1 <- read_ipums_micro(ddi)

ipums_val_labels(data1, SAMPLE)
