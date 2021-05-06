# install.packages("yarrr")
library("yarrr")
# install.packages('tidyverse')
library(tidyverse)
# getwd()
# ls()
# read csv

rm(list=ls())
# data_01 <- read.table("data/6_bd-ai.csv", header = TRUE, sep = ",")


# List all files ending with csv in directory
csv_files = list.files(path = 'data', pattern = "csv$", full.names = TRUE)
# Read each csv file into a list
data_01 <- map_dfr(csv_files, read_csv)


#rename columns

# add roka column
data_01$roka <- "nepareizs"
data_01$roka[data_01$target_x == data_01$hand_x] <- "pareizs"

# add kustiba column
data_01$kustiba <- "nepareizs"
data_01$kustiba[data_01$target_y == data_01$movement_y] <- "pareizs"

# drop incorrect responses
# rm(data_1_filtered)
data_1_filtered_correct <- data_01[data_01$key_resp.corr == 1, ]

# percent of incorrect responses
correct_key_responses <- nrow(data_1_filtered_correct) / nrow(data_01) * 100

# drop responses > 1 sec
data_1_filtered <- data_1_filtered_correct[data_1_filtered_correct$key_resp.rt <= 1, ]

# mean
mean(data_1_filtered$key_resp.rt)

# means of response times vs cue correctness
aggregate(formula = key_resp.rt ~ roka * kustiba,
          data = data_1_filtered,
          FUN = mean,
          na.rm = TRUE,
          na.action=NULL)


# create linear regression model
response.rt.lm <- lm (formula = key_resp.rt ~ roka * kustiba,
                        data = data_1_filtered)
ano <- anova(response.rt.lm)

xtable(ano)


# regression coefficients
summary(response.rt.lm)

# raw data, means, density and 95% CI plot
yarrr::pirateplot(formula = key_resp.rt ~ roka * kustiba, # dv is weight, iv is Diet
                  data = data_1_filtered,
                  theme = 3,
                  main = "Reakcijas laika vidējās vērtības, sadalījums un 95% ticamības intervāls atkarībā no signālu pareizības",
                  xlab = "Reakcijas laiks",
                  ylab = "rokas un kustibas signāls (pareizs/nepareizs)")

# install.packages("gtsummary")
library(gtsummary)

resu <- data_1_filtered %>% select(participant, key_resp.rt)

table1 <- 
  tbl_summary(
    resu,
    by = participant, # split table by group
    missing = "no" # don't list missing data separately
  ) %>%
  modify_header(label = "**Dalībnieks**") %>% # update the column header
  bold_labels() 
table1











