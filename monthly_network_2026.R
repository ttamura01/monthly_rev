rm(list = ls())
library(tidyverse)
library(dplyr)
library(ggtext)
library(scales)
library(glue)

setwd("/Users/takayukitamura/Documents/R_Computing/monthly_rev")
getwd()

ds <- read.csv("realtor_monthly.csv", header = TRUE) %>% 
  select(-X) 

# ds <- read.csv("revenue_realtor.csv", header = TRUE) %>% 
#   select(-X) %>% 
#   filter(realtor > 70) 

# updates <- tribble(~"realtor", ~"monthly",
#                    112, 10000)

# sapply(updates, class)
# 
# ds <- rbind(ds, updates)

# ds <- ds[-c(10, 11, 13, 17), ] 

# write.csv(ds,"realtor_monthly.csv")

sapply(ds, class)
ds$date <- as.Date(ds$date, format = "%Y-%m-%d")

# set filter

# ds <- ds %>%
#   mutate(month = month(date)) %>% 
#   filter(month %in% c(4,5,6,7,8,9,10))
# 
# ds <- ds %>%
#   filter(revenue >= 5000)
# 
# ds <- ds %>%
#   filter(date >= as.Date("2025-02-01"))

ds %>% 
  ggplot(aes(x = date, y = revenue)) +
  geom_col()

ds %>% 
  ggplot(aes(x = date, y = revenue)) +
  geom_point()
  
ds %>% 
  ggplot(aes(x = realtors, y = revenue)) +
  geom_point() +
  geom_smooth(method = "lm")

model <- lm(revenue ~ realtors, ds)
coef(model)
summary(model)

#how to extract p-value
summary(model)$coefficients["realtors", "Pr(>|t|)"]
coef(summary(model))[2,4]
f <- summary(model)$fstatistic
pf(f[1], f[2], f[3], lower.tail = FALSE)
s <- summary(model)
p_coef <- coef(s)[2,4]
p_model <- pf(s$fstatistic[1],
              s$fstatistic[2],
              s$fstatistic[3],
              lower.tail = FALSE)
p_value <- round((p_coef), 4)

plot(model)



new_realtors <- data.frame(realtors = c(130, 140, 150))
predict (model, new_realtors)

intercept <- model$coefficients[1]
slope <- model$coefficients[2]
r2 <- summary(model)$r.squared
r2_rounded <- round(r2, 2)

intercept <- format(model$coefficients[1], digits = 7)
slope <- format(model$coefficients[2], digits = 4)

# formula <- glue("y={slope}X + {intercept}\n R^2 = {r2_rounted}")
formula <- glue("y={slope}X + {intercept}\n p-value = {p_value}")

monthly <- ds %>% 
  mutate(
    residuals = residuals(model),
    std_redisuals = residuals/sd(residuals(model))
  )

ggplot(monthly, aes(x = realtors, y = revenue)) +
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



