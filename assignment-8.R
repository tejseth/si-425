library(readxl)
library(tidyverse)
library(gt)
library(ltm)
library(caret)
library(InformationValue)
library(ISLR)

StarDigital_data <- read_excel("StarDigital_data.xlsx")

gt_1 <- StarDigital_data |> 
  group_by(Test) |> 
  summarize(Count = n(),
            Proportion = round(n() / nrow(StarDigital_data),3)) |> 
  gt() |> 
  cols_align(align = "center") |> 
  gtExtras::gt_theme_538() |> 
  gtExtras::gt_hulk_col_numeric(Proportion) |> 
  tab_header(title = "Fraction of Customers in Control vs. Test Conditions",
             subtitle = "Test condition (Test = 1), control condition (Test = 0)")
gtsave(gt_1, "gt_1.png")

gt_2 <- StarDigital_data |> 
  group_by(Test) |>
  mutate(test_count = n()) |> 
  ungroup() |> 
  group_by(Test, Purchase) |> 
  summarize(Count = n(),
            Proportion = round(n() / test_count,3)) |> 
  distinct() |> 
  ungroup() |> 
  gt() |> 
  cols_align(align = "center") |> 
  gtExtras::gt_theme_538() |> 
  gtExtras::gt_hulk_col_numeric(Proportion) |> 
  tab_header(title = "How Often Each Condition in Control and Test Purchase",
             subtitle = "Test condition (Test = 1), control condition (Test = 0)")
gtsave(gt_2, "gt_2.png")

gt_3 <- StarDigital_data |> 
  group_by(Test) |> 
  summarize(Count = n(),
            `Average Impressions` = round(mean(`Total Imp`), 2)) |> 
  gt() |> 
  cols_align(align = "center") |> 
  gtExtras::gt_theme_538() |> 
  gtExtras::gt_hulk_col_numeric(`Average Impressions` ) |> 
  tab_header(title = "Average Total Impressions in Each Condition",
             subtitle = "Test condition (Test = 1), control condition (Test = 0)")
gtsave(gt_3, "gt_3.png")

logit_1 <- glm(Purchase ~ Imp_1, family = "binomial", data = StarDigital_data)
logit_2 <- glm(Purchase ~ Imp_2, family = "binomial", data = StarDigital_data)
logit_3 <- glm(Purchase ~ Imp_3, family = "binomial", data = StarDigital_data)
logit_4 <- glm(Purchase ~ Imp_4, family = "binomial", data = StarDigital_data)
logit_5 <- glm(Purchase ~ Imp_5, family = "binomial", data = StarDigital_data)
logit_6 <- glm(Purchase ~ Imp_6, family = "binomial", data = StarDigital_data)

imp_1 <- sum(round(predict(logit_1, type = "response")) == as.numeric(StarDigital_data$Purchase)) / nrow(StarDigital_data)
imp_2 <- sum(round(predict(logit_2, type = "response")) == as.numeric(StarDigital_data$Purchase)) / nrow(StarDigital_data)
imp_3 <- sum(round(predict(logit_3, type = "response")) == as.numeric(StarDigital_data$Purchase)) / nrow(StarDigital_data)
imp_4 <- sum(round(predict(logit_4, type = "response")) == as.numeric(StarDigital_data$Purchase)) / nrow(StarDigital_data)
imp_5 <- sum(round(predict(logit_5, type = "response")) == as.numeric(StarDigital_data$Purchase)) / nrow(StarDigital_data)
imp_6 <- sum(round(predict(logit_6, type = "response")) == as.numeric(StarDigital_data$Purchase)) / nrow(StarDigital_data)

paste0("Website 1 Accuracy: ", 100*round(imp_1, 3), "%")
paste0("Website 2 Accuracy: ", 100*round(imp_2, 3), "%")
paste0("Website 3 Accuracy: ", 100*round(imp_3, 3), "%")
paste0("Website 4 Accuracy: ", 100*round(imp_4, 3), "%")
paste0("Website 5 Accuracy: ", 100*round(imp_5, 3), "%")
paste0("Website 6 Accuracy: ", 100*round(imp_6, 3), "%")

StarDigital_data |> 
  pivot_longer(cols = starts_with('Imp')) |> 
  ggplot(aes(x = name, y = `Total Imp`)) +
  geom_boxplot(aes(fill = Purchase)) +
  theme_minimal()

StarDigital_data <- StarDigital_data |> 
  mutate(Impression_1 = ifelse(Imp_1 > 0, 1, 0),
         Impression_2 = ifelse(Imp_2 > 0, 1, 0),
         Impression_3 = ifelse(Imp_3 > 0, 1, 0),
         Impression_4 = ifelse(Imp_4 > 0, 1, 0),
         Impression_5 = ifelse(Imp_5 > 0, 1, 0),
         Impression_6 = ifelse(Imp_6 > 0, 1, 0))

misClassError(StarDigital_data$Purchase, StarDigital_data$Impression_1)
misClassError(StarDigital_data$Purchase, StarDigital_data$Impression_2)
misClassError(StarDigital_data$Purchase, StarDigital_data$Impression_3)
misClassError(StarDigital_data$Purchase, StarDigital_data$Impression_4)
misClassError(StarDigital_data$Purchase, StarDigital_data$Impression_5)
misClassError(StarDigital_data$Purchase, StarDigital_data$Impression_6)

