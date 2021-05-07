# install.packages("yarrr")
library("yarrr")
# install.packages('tidyverse')
library(tidyverse)
# install.packages('plyr')
library(plyr)
library(dplyr)
# getwd()
# ls()
# read csv

rm(list=ls())
# data_01 <- read.table("data/6_bd-ai.csv", header = TRUE, sep = ",")


# List all files ending with csv in directory
csv_files = list.files(path = 'data', pattern = "csv$", full.names = TRUE)
# Read each csv file into a list
data_01 <- map_dfr(csv_files, read_csv)

# 


#rename columns

# add roka column
data_01$roka <- "nepareizs"
data_01$roka[data_01$target_x == data_01$hand_x] <- "pareizs"

# add kustiba column
data_01$kustiba <- "nepareizs"
data_01$kustiba[data_01$target_y == data_01$movement_y] <- "pareizs"

data_01$atbilde <- 0
data_01$atbilde[data_01$correct_key == data_01$key_resp.keys] <-1
mean(data_01$atbilde)

# drop incorrect responses
# rm(data_1_filtered)
# data_1_filtered_correct <- data_01[data_01$key_resp.corr == 1, ]
data_1_filtered_correct <- data_01[data_01$atbilde == 1, ]
data_1_filtered_incorrect <- data_01[data_01$atbilde == 0, ]

table(data_1_filtered_incorrect$roka, data_1_filtered_incorrect$kustiba)/nrow(data_1_filtered_incorrect)*100
table(data_1_filtered_incorrect$roka, data_1_filtered_incorrect$kustiba)
table(data_1_filtered_incorrect$atbilde)/nrow(data_1_filtered_incorrect)*100
chisq.test(table(data_01$atbilde, data_01$kustiba, data_01$roka), correct = FALSE)
both <- 43.418590 / 0.49
hand <- 21.64691 / 0.21
movement <- 20.77355 / 0.21
none <- 14.16095 / 0.09
total <- both + hand + movement + none
both/total
hand/total
movement/total
none/total

# percent of incorrect responses
correct_key_responses <- nrow(data_1_filtered_correct) / nrow(data_01) * 100
incorrect_key_responses <- 100 - correct_key_responses

# mean & SD
mean(data_1_filtered_correct$key_resp.rt)
sd(data_1_filtered_correct$key_resp.rt)

mean_3_sd <- mean(data_1_filtered_correct$key_resp.rt) + 3*sd(data_1_filtered_correct$key_resp.rt)
data_1_filtered_graph <- data_1_filtered_correct[data_1_filtered_correct$key_resp.rt <= mean_3_sd, ]

data_1_filtered <- data_1_filtered_correct
# dropped_outlayers <- 100 - nrow(data_1_filtered)/nrow(data_1_filtered_correct)*100

# mean & SD
mean(data_1_filtered$key_resp.rt)
sd(data_1_filtered$key_resp.rt)


# means and SD of response times vs cue correctness
aggregate(formula = key_resp.rt ~ roka * kustiba,
          data = data_1_filtered,
          FUN = mean,
          na.rm = TRUE,
          na.action=NULL)

aggregate(formula = key_resp.rt ~ roka * kustiba,
          data = data_1_filtered,
          FUN = sd,
          na.rm = TRUE,
          na.action=NULL)

# create linear regression model
response.rt.lm <- lm (formula = key_resp.rt ~ roka * kustiba,
                        data = data_1_filtered)
ano <- anova(response.rt.lm)


# regression coefficients
summary(response.rt.lm)

# raw data, means, density and 95% CI plot


yarrr::pirateplot(formula = key_resp.rt ~ roka * kustiba, # dv is weight, iv is Diet
                  data = data_1_filtered_graph,
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

# demographics
mean(data_1_filtered$age)
min(data_1_filtered$age)
max(data_1_filtered$age)
sd(data_1_filtered$age)
table(data_1_filtered$gender) / nrow(data_1_filtered) * 7






