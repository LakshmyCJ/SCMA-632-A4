library(dplyr)
library(MASS)

pizza=read.csv("D:/Bootcamp VCU datasets/pizza_data.csv")

head(pizza)

conjoint_model <- lm(ranking ~ price + size + crust + cheese + spicy, data = pizza)
summary(conjoint_model)
