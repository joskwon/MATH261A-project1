## 00_clean-data.R

library(tidyverse)
library(ggplot2)
library(ggpubr)

#cleaning data
raw_data <- read.csv("data/nfl-team-statistics.csv")
summary(raw_data)

sum(is.na(raw_data))

clean_data <- raw_data[c("season", "offense_completion_percentage", "defense_completion_percentage", "points_scored","points_allowed", "score_differential")]
summary(clean_data)

write.csv(clean_data, "data/nfl_clean.csv")
#------------
#visualization of data 

#scatter plot of data score differential vs offense completion percentage
ggplot(clean_data, aes(x = offense_completion_percentage, y = score_differential)) +
  geom_point() + 
  theme_pubr()

#regression with score differential as outcome and offense completion percentage as predictor
lm_fit <- lm(score_differential ~ offense_completion_percentage, data = clean_data)

#visualization of regression
summary(lm_fit)

b0 <- coef(lm_fit)[1] #intercept coefficient
b1 <- coef(lm_fit)[2] #slope coefficient

#95% confidence interval
confint(lm_fit)

#scatter with linear regression line
ggplot(clean_data, aes(x = offense_completion_percentage, y = score_differential)) +
  geom_abline(slope = coef(lm_fit)[2], 
              intercept = coef(lm_fit)[1], color = "blue") + 
  geom_point() +
  xlab("Offense Pass Completion(%)")+
  ylab("Score Differential")+
  ggtitle("Higher offense completion percentage leads to higher score differential",
          subtitle = "Data from NFL") +
  theme_pubr() 


#residual vs predictor plot
plot(clean_data$offense_completion_percentage, resid(lm_fit),
     main = "Residual vs. Predictor Plot for NFL Model",
     xlab = "Offense completion percentage",
     ylab = "Residuals",
)+
  abline(h=0)

#quantile-quantile plot 
qqnorm(resid(lm_fit),
       main = "Q-Q Plot of Residuals")
qqline(resid(lm_fit), col = "blue")

#note the heavy tails
