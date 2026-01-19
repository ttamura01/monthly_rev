rm(list = ls())
library(tidyverse)
library(dplyr)
library(ggtext)
library(scales)
library(glue)

setwd("/Users/takayukitamura/Documents/R_Computing/monthly_rev")
getwd()

# ds <- read.csv("/Users/takayukitamura/Desktop/revenue_realtor.csv", header = TRUE)

ds <- read.csv("revenue_realtor.csv", header = TRUE) %>% 
  select(-X) %>% 
  filter(realtor > 70) 

updates <- tribble(~"realtor", ~"monthly",
                  112, 10000)

sapply(updates, class)

ds <- rbind(ds, updates)

# ds <- ds[-c(10, 11, 13, 17), ] 

write.csv(ds,"/Users/takayukitamura/Desktop/revenue_realtor.csv")

sapply(ds, class)

ds %>% 
  ggplot(aes(x = realtor, y = monthly)) +
  geom_point() +
  geom_smooth(method = "lm")

model <- lm(monthly~realtor, ds)
coef(model)
summary(model)
intercept <- model$coefficients[1]
slope <- model$coefficients[2]

intercept <- format(model$coefficients[1], digits = 7)
slope <- format(model$coefficients[2], digits = 4)

formula <- glue("y={slope}X + {intercept}\n R^2 = 0.6025")

monthly <- ds %>% 
  mutate(
    residuals = residuals(model),
    std_redisuals = residuals/sd(residuals(model))
  )

ggplot(monthly, aes(x = realtor, y = monthly)) +
  geom_point(aes(color = std_redisuals), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  annotate(geom = "text",
           x = 75,
           y = 9000,
           label = formula ) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  scale_y_continuous(breaks = seq(5000, 12500, 2500),
                     labels = scales::label_comma()) +
  theme_minimal() +
  labs(
    title = "Monthly revenue vs. Number of Realtors",
    subtitle = "Points colored by standardized residuals",
    x = "# of Realtors with Business",
    y = "Monthly Revenue",
    color = "Standardized Residuals"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold")
  )

ggsave("monthly_revenue_realtors.png")

#Revenue masked 
ggplot(monthly, aes(x = realtor, y = monthly)) +
  geom_point(aes(color = std_redisuals), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  annotate(geom = "text",
           x = 75,
           y = 9000,
           label = formula ) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  scale_y_continuous(breaks = seq(5000, 12500, 2500),
                     labels = scales::label_comma()) +
  theme_minimal() +
  labs(
    title = "Monthly revenue vs. Number of Realtors",
    subtitle = "Points colored by standardized residuals",
    x = "# of Realtors with Business",
    y = "Monthly Revenue",
    color = "Standardized Residuals"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.y = element_blank()
  )
