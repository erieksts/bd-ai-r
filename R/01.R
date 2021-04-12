# install.packages("yarrr")
# library("yarrr")
# getwd()
# ls()

#rm(data_1)

# read csv
data_01 <- read.table("data/p_01.csv", header = TRUE, sep = ",")

# add correct_key column

data_01_less$correct_key <- "up"
data_01_less$correct_key[data_01_less$target_x == 0.5 & 
               data_01_less$target_y == -0.25] <- "down"
data_01_less$correct_key[data_01_less$target_x == -0.5 & 
                           data_01_less$target_y == 0.25] <- "left"
data_01_less$correct_key[data_01_less$target_x == -0.5 & 
                           data_01_less$target_y == -0.25] <- "right"

# add roka column
data_01_less$roka <- "nepareizs"
data_01_less$roka[data_01_less$target_x == data_01_less$hand_x] <- "pareizs"

# add kustība column
data_01_less$kustība <- "nepareizs"
data_01_less$kustība[data_01_less$target_y == data_01_less$movement_y] <- "pareizs"

# drop incorrect responses
rm(data_1_filtered)
data_1_filtered <- data_01_less[data_01_less$response.keys == data_01_less$correct_key, ]

# percent of incorrect responses
correct_key_responses <- nrow(data_1_filtered) / nrow(data_01_less) * 100

# means of response times vs cue correctness
aggregate(formula = response.rt ~ roka * kustība,
          data = data_1_filtered,
          FUN = mean)


# create linear regression model
response.rt.lm <- lm (formula = response.rt ~ roka * kustība,
                        data = data_1_filtered)
anova(response.rt.lm)

# anova
  # response.rt.aov <- aov(formula = response.rt ~ roka * kustība,
  #     data = data_1_filtered)
  # summary(response.rt.aov)

# regression coefficients
summary(response.rt.lm)

# raw data, means, density and 95% CI plot
yarrr::pirateplot(formula = response.rt ~ roka * kustība, # dv is weight, iv is Diet
                  data = data_1_filtered,
                  theme = 3,
                  main = "Reakcijas laika vidējās vērtības, sadalījums un 95% ticamības intervāls atkarībā no signālu pareizības",
                  xlab = "Reakcijas laiks",
                  ylab = "rokas un kustības signāls (pareizs/nepareizs)")

















