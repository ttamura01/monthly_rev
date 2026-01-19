rm(list = ls())
library(tidyverse)
library(dplyr)
library(ggtext)
library(scales)

#detach("package:MASS", unload = TRUE)

setwd("/Users/takayukitamura/Documents/R_Computing/monthly_rev")
rev <- read.csv("rev.csv", 
                sep = ",", header = TRUE, stringsAsFactors = FALSE) #%>% 
  # rename(cat_1 = cat.1, cat_2 = cat.2)

sapply(rev, class)
colnames(rev)

# rev <- rev %>% 
#  select(-networking)

# Remove commas and convert the column to numeric
# rev$revenue <- as.numeric(gsub(",", "", rev$revenue))

# write_csv(rev, "rev.csv")

#rev$revenue <- as.integer(rev$revenue)

print(rev)

rev <- rev %>% 
  mutate(networking = cat_1 + cat_2) 

ggplot(rev, aes(x = networking, y = revenue)) +
  geom_point() +
  geom_smooth(se = TRUE, method = "lm" )


# ggplot(rev, aes(x = networking, y = revenue)) +
#   geom_boxplot()

model <- lm(revenue~networking, rev)
lm(model)
summary(model)

predict(model, data.frame(networking = c(275, 300, 350)))

# print(rev_network)
# colnames(rev_network)
# 
# rev_variable <- rev_network %>% 
#   filter(month > "2022-05-01")

rev %>% 
  ggplot(aes(x = networking, y=revenue)) +
  geom_point() +
  geom_smooth(method = "lm")+
  labs(title = "The correlation between my networking and mothly revenue<br> is consistently high",
       x = "network",
       y = "revenue") +
  theme_classic() +
  theme(
    plot.title.position = "plot",
        plot.title = element_markdown())

ggsave("my_network_vs_revenue.png")
    



month <- month %>% 
  select(month, network, revenue)

monthly <- rev %>% 
  mutate(
    residuals = residuals(model),
    std_redisuals = residuals/sd(residuals(model))
  )

ggplot(monthly, aes(x = networking, y = revenue)) +
  geom_point(aes(color = std_redisuals), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(
    title = "Revenue vs. Networking",
    subtitle = "Points colored by standardized residuals",
    x = "Networking",
    y = "Revenue",
    color = "Standardized Residuals"
  )

ggsave("monthly_revenue_network.png", height = 8, width = 5)

