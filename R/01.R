# install.packages("yarrr")
# library("yarrr")
# install.packages('tidyverse')
library(tidyverse)
# install.packages('plyr')
library(plyr)
library(dplyr)
# getwd()
# ls()
# read csv

rm(list=ls())

# List all files ending with csv in directory
csv_files = list.files(path = 'data', pattern = "csv$", full.names = TRUE)

# Read each csv file into a list
data_01 <- map_dfr(csv_files, read_csv)


# add roka column
data_01$roka <- "nepareizs"
data_01$roka[data_01$target_x == data_01$hand_x] <- "pareizs"

# add kustiba column
data_01$kustiba <- "nepareizs"
data_01$kustiba[data_01$target_y == data_01$movement_y] <- "pareizs"

# add atbilde column (correct or not)
data_01$atbilde <- 0
data_01$atbilde[data_01$correct_key == data_01$key_resp.keys] <-1
mean(data_01$atbilde)

data_01$pareizi <- "abi"
data_01$pareizi[data_01$roka == "pareizs" & data_01$kustiba == "nepareizs"] <- "roka"
data_01$pareizi[data_01$roka == "nepareizs" & data_01$kustiba == "pareizs"] <- "kustiba"
data_01$pareizi[data_01$roka == "nepareizs" & data_01$kustiba == "nepareizs"] <- "neviens"

# drop incorrect responses
# data_1_filtered_correct <- data_01[data_01$key_resp.corr == 1, ]
data_1_filtered_correct <- data_01[data_01$atbilde == 1, ]
data_1_filtered_incorrect <- data_01[data_01$atbilde == 0, ]

# percent of incorrect responses
correct_key_responses <- nrow(data_1_filtered_correct) / nrow(data_01) * 100
incorrect_key_responses <- 100 - correct_key_responses

#
table(data_1_filtered_incorrect$roka, data_1_filtered_incorrect$kustiba)/nrow(data_1_filtered_incorrect)*100
table(data_1_filtered_incorrect$roka, data_1_filtered_incorrect$kustiba)
table(data_1_filtered_incorrect$atbilde)/nrow(data_1_filtered_incorrect)*100


chisq.test(table(data_01$atbilde, data_01$pareizi), correct = FALSE)

both <- 43.418590 / 0.49
hand <- 21.64691 / 0.21
movement <- 20.77355 / 0.21
none <- 14.16095 / 0.09
total <- both + hand + movement + none
both/total
hand/total
movement/total
none/total

# drop outliers
data_1_filtered_correct$outlier <- is_outlier(data_1_filtered_correct$key_resp.rt, coef = 1.5)

data_01_outliers$outlier <- is_outlier(data_01_outliers$key_resp.rt, coef = 1.5)
mean(data_01_outliers$outlier)

data_1_filtered_correct_outliers <- data_1_filtered_correct[data_1_filtered_correct$outlier == FALSE, ]
data_1_filtered_correct_outliers_out <- data_1_filtered_correct[data_1_filtered_correct$outlier == TRUE, ]
dropped_outlayers <- 100 - nrow(data_1_filtered_correct_outliers)/nrow(data_1_filtered_correct)*100


max(data_1_filtered_correct_outliers$key_resp.rt)
max(data_1_filtered_correct_outliers_out$key_resp.rt)
min(data_1_filtered_correct_outliers$key_resp.rt)
min(data_1_filtered_correct_outliers_out$key_resp.rt)

ggqqplot(data=data_1_filtered_correct_outliers, 'key_resp.rt')
ggqqplot(data=data_1_filtered_correct_outliers_out, 'key_resp.rt')


# mean & SD
mean(data_1_filtered_correct$key_resp.rt)
mean(data_1_filtered_correct_outliers$key_resp.rt)
sd(data_1_filtered_correct$key_resp.rt)
sd(data_1_filtered_correct_outliers$key_resp.rt)

# data_1_filtered <- data_1_filtered_correct_outliers




# means and SD of response times vs cue correctness
aggregate(formula = key_resp.rt ~ roka * kustiba,
          data = data_1_filtered_correct,
          FUN = mean,
          na.rm = TRUE,
          na.action=NULL)

aggregate(formula = key_resp.rt ~ roka * kustiba,
          data = data_1_filtered_correct,
          FUN = sd,
          na.rm = TRUE,
          na.action=NULL)

aggregate(formula = key_resp.rt ~ roka * kustiba,
          data = data_1_filtered_correct_outliers,
          FUN = mean,
          na.rm = TRUE,
          na.action=NULL)

aggregate(formula = key_resp.rt ~ roka * kustiba,
          data = data_1_filtered_correct_outliers,
          FUN = sd,
          na.rm = TRUE,
          na.action=NULL)



# create linear regression model
response.rt.lm <- lm (formula = key_resp.rt ~ roka * kustiba,
                        data = data_1_filtered_correct)
ano <- anova(response.rt.lm)
ano
summary(response.rt.lm)

residuals <- residuals(object = ano)

shapiro.test()

stdres = rstandard(response.rt.lm)
qqnorm(stdres)
qqline(stdres)


response.rt.lm_outliers <- lm (formula = key_resp.rt ~ roka * kustiba,
                      data = data_1_filtered_correct_outliers)
ano_outliers <- anova(response.rt.lm_outliers)
ano_outliers
summary(response.rt.lm_outliers)

stdres_outliers = rstandard(response.rt.lm_outliers)

install.packages("nortest")
library(nortest)
ad.test(stdres_outliers)

qqnorm(stdres_outliers,
       ylab="Standartizēts atlikums", 
       xlab="Teorētiskās kvantiles", 
       main="Standartizētā regresijas atlikuma kvantiļu sadalījums")
qqline(stdres_outliers)


# raw data, means, density and 95% CI plot
yarrr::pirateplot(formula = key_resp.rt ~ roka * kustiba,
                  data = data_1_filtered_graph,
                  theme = 3,
                  main = "Reakcijas laika vidējās vērtības, sadalījums un 95% ticamības intervāls atkarībā no signālu pareizības",
                  xlab = "Reakcijas laiks",
                  ylab = "rokas un kustibas signāls (pareizs/nepareizs)")

# per participant
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

Sys.setlocale("LC_TIME", "Latvian")




