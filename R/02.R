# install.packages('apaTables')
library(apaTables)
library(tidyverse)


#
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

# drop incorrect responses
# rm(data_1_filtered)
data_1_filtered_correct <- data_01[data_01$key_resp.corr == 1, ]

# percent of incorrect responses
correct_key_responses <- nrow(data_1_filtered_correct) / nrow(data_01) * 100

# drop responses > 1 sec
data_1_filtered <- data_1_filtered_correct[data_1_filtered_correct$key_resp.rt <= 1, ]

#rename columns
# names(df)[names(df) == 'old.var.name'] <- 'new.var.name'

# 2-way anova table
apa.2way.table(iv1=roka,iv2=kustiba,dv=key_resp.rt,data=data_1_filtered,landscape=TRUE, filename="ex2_desc_table.doc")

response.rt.lm <- lm (formula = key_resp.rt ~ roka * kustiba,
                      data = data_1_filtered)

apa.reg.table(response.rt.lm, filename="2.doc")


# mean
mean(data_1_filtered$age)
