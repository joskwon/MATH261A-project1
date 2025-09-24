## 00_clean-data.R

#cleaning data
raw_data <- read.csv("data/nfl-team-statistics.csv")
summary(raw_data)

sum(is.na(raw_data))

clean_data <- raw_data[c("season", "offense_completion_percentage", "defense_completion_percentage", "points_scored","points_allowed", "score_differential")]
summary(clean_data)

#regression with score differential as outcome and offense completion percentage as predictor
lm_fit <- lm(score_differential ~ offense_completion_percentage, data = clean_data)
summary(lm_fit)

b0 <- coef(lm_fit)[1] #intercept coefficient
b1 <- coef(lm_fit)[2] #slope coefficient

confint(lm_fit)

#reisudal vs predictor plot
plot(clean_data$offense_completion_percentage, resid(lm_fit),
     main = "Residual vs. Predictor Plot for NFL Model",
     xlab = "Offense completion percentage",
     ylab = "Residuals",
)+
  abline(h=0)

#quantile-quantile plot 
qqnorm(resid(lm_fit),
       main = "Q-Q Plot of Residuals")+
  qqline(resid(lm_fit), col = "blue")

#note the heavy tails
