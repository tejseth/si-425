library(readxl)
library(tidyverse)
library(gt)

double_click <- read_excel("Assignment 7 - airfrance-data.xlsx", 2)
kayak <- read_excel("Assignment 7 - airfrance-data.xlsx", 3)
publisher_id_lookups <- read_excel("Assignment 7 - airfrance-data.xlsx", 4)

names(kayak) <- kayak[2,]
kayak <- kayak[-2,]
kayak <- kayak[2,]

kayak_adj <- kayak |> 
  mutate(Impressions = NA) |> 
  dplyr::select(Campaign = `Search Engine`, Clicks, Impressions, 
                `Total Cost of Clicks` = `Media Cost`, 
                `Number of Bookings` = `Total Bookings`,
                `Total Revenue from Bookings` = `Total Revenue`)

double_click$Clicks <- as.numeric(double_click$Clicks)
double_click$Impr. <- as.numeric(double_click$Impr.)
double_click$`Total Cost of Clicks` <- as.numeric(double_click$`Total Cost of Clicks`)
double_click$`# of Bookings` <- as.numeric(double_click$`# of Bookings`)
double_click$`Total Revenue from Bookings` <- as.numeric(double_click$`Total Revenue from Bookings`)

dc_grouped <- double_click |> 
  group_by(Campaign) |> 
  summarize(Clicks = sum(Clicks),
            Impressions = sum(Impr.),
            `Total Cost of Clicks` = sum(`Total Cost of Clicks`),
            `Number of Bookings` = sum(`# of Bookings`),
            `Total Revenue from Bookings` = sum(`Total Revenue from Bookings`)) 

dc_grouped_2 <- rbind(dc_grouped, kayak_adj)

dc_grouped_2$Clicks <- as.numeric(dc_grouped_2$Clicks)
dc_grouped_2$Impressions <- as.numeric(dc_grouped_2$Impressions)
dc_grouped_2$`Total Cost of Clicks` <- as.numeric(dc_grouped_2$`Total Cost of Clicks`)
dc_grouped_2$`Number of Bookings` <- as.numeric(dc_grouped_2$`Number of Bookings`)
dc_grouped_2$`Total Revenue from Bookings` <- as.numeric(dc_grouped_2$`Total Revenue from Bookings`)

dc_campaign <- dc_grouped_2 |> 
  mutate(CTR = ifelse(Campaign == "Kayak", NA, round(`Clicks`/`Impressions`, 3)),
         `Conversion Rate` = round(`Number of Bookings`/`Clicks`, 3),
         ROI = round(`Total Revenue from Bookings`/`Total Cost of Clicks`, 2)) |> 
  gt() |> 
  cols_align(align = "center") |> 
  gtExtras::gt_hulk_col_numeric(CTR) |> 
  gtExtras::gt_hulk_col_numeric(`Conversion Rate`) |> 
  gtExtras::gt_hulk_col_numeric(ROI) |> 
  gtExtras::gt_theme_538() |> 
  tab_header(title = "DoubleClick Data by Campaign")
gtsave(dc_campaign, "dc_campaign.png")

publisher_grouped <- double_click |> 
  left_join(publisher_id_lookups, by = c("Publisher ID"))  |> 
  group_by(`Publisher Name`) |> 
  summarize(Clicks = sum(Clicks),
            Impressions = sum(Impr.),
            `Total Cost of Clicks` = sum(`Total Cost of Clicks`),
            `Number of Bookings` = sum(`# of Bookings`),
            `Total Revenue from Bookings` = sum(`Total Revenue from Bookings`)) 

kayak_adj_2 <- kayak_adj |> 
  rename(`Publisher Name` = Campaign)

publisher_grouped_2 <- rbind(publisher_grouped, kayak_adj_2)

publisher_grouped_2$Clicks <- as.numeric(publisher_grouped_2$Clicks)
publisher_grouped_2$Impressions <- as.numeric(publisher_grouped_2$Impressions)
publisher_grouped_2$`Total Cost of Clicks` <- as.numeric(publisher_grouped_2$`Total Cost of Clicks`)
publisher_grouped_2$`Number of Bookings` <- as.numeric(publisher_grouped_2$`Number of Bookings`)
publisher_grouped_2$`Total Revenue from Bookings` <- as.numeric(publisher_grouped_2$`Total Revenue from Bookings`)

dc_pub <- publisher_grouped_2 |> 
  mutate(CTR = round(`Clicks`/`Impressions`, 3),
         `Conversion Rate` = round(`Number of Bookings`/`Clicks`, 3),
         ROI = round(`Total Revenue from Bookings`/`Total Cost of Clicks`, 2)) |> 
  gt() |> 
  cols_align(align = "center") |> 
  gtExtras::gt_hulk_col_numeric(CTR) |> 
  gtExtras::gt_hulk_col_numeric(`Conversion Rate`) |> 
  gtExtras::gt_hulk_col_numeric(ROI) |> 
  gtExtras::gt_theme_538() |> 
  tab_header(title = "DoubleClick Data by Publisher Name")
gtsave(dc_pub, "dc_pub.png")

############################################################################

auction_experiments <- read_excel("SI 425 Auction experiments 2022.xlsx")

