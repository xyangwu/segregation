# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi4 <- read_ipums_ddi("data/ipumsi/ipumsi_00004.xml")
ddi6 <- read_ipums_ddi("data/ipumsi/ipumsi_00006.xml")
ddi7 <- read_ipums_ddi("data/ipumsi/ipumsi_00007.xml")

data4 <- read_ipums_micro(ddi4)
data6 <- read_ipums_micro(ddi6)
data7 <- read_ipums_micro(ddi7)

data_90 <- cbind(data4, data6, data7)

ipums_view(data_90)

data_90 = data_90 %>% mutate(age = 1990-(CN1990A_BIRTHY+1000),
                             agegroup = cut(age, breaks = c(seq(0, 120, 5)), right = FALSE, include.lowest = TRUE),
                             sex = ifelse(CN1990A_SEX == 1, "m", "f"))
data_90[, c(58)] %>% head()
data_90 = data_90[, -c(58, 60)]
saveRDS(data_90, "data/data1990.rds")
