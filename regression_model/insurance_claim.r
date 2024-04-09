#--- Install and load required packages
install.packages(c("ggplot2", "dplyr", "statmod", "MASS"))
library(ggplot2)
library(dplyr)
library(statmod)
library(MASS)

#--- Load the insurance claim dataset
data(Insurance, package = "MASS")

#--- Explore the dataset
str(Insurance)
summary(Insurance)

#--- Visualize the relationship between age and claim amount
ggplot(Insurance, aes(x = Age, y = Claims)) +
  geom_point() +
  labs(x = "Age", y = "Claim Amount", title = "Age vs. Claim Amount")

#--- Fit a Poisson regression model
poisson_model <- glm(Claims ~ District + Group + Age + offset(log(Holders)), 
                     data = Insurance, family = poisson)

#--- Print model summary
summary(poisson_model)

#--- Check for overdispersion
dispersion <- sum(residuals(poisson_model, type = "pearson")^2) / poisson_model$df.residual
cat("Dispersion:", dispersion, "\n")

#--- Fit a negative binomial regression model
nb_model <- glm.nb(Claims ~ District + Group + Age + offset(log(Holders)), 
                   data = Insurance)

#--- Print model summary
summary(nb_model)

#--- Compare the models using AIC
AIC(poisson_model, nb_model)

#--- Perform model diagnostics
par(mfrow = c(2, 2))
plot(nb_model)

#--- Make predictions using the negative binomial model
new_data <- data.frame(
  District = "1",
  Group = "1",
  Age = 40,
  Holders = 1000
)

predicted_claims <- predict(nb_model, newdata = new_data, type = "response")
cat("Predicted number of claims:", round(predicted_claims), "\n")

#--- Estimate the average marginal effect of age on claims
age_effect <- coef(nb_model)["Age"] * mean(Insurance$Claims)
cat("Average marginal effect of age on claims:", round(age_effect, 2), "\n")