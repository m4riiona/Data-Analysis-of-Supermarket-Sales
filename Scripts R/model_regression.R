install.packages("samplingbook") #install the library
library("samplingbook") #load the library 


install.packages("readxl")
library("readxl")


dd <- read_excel("C:/Users/sambr/Downloads/cluster_sample.xlsx")

attach(dd)
names(dd)


initial_model <- lm(`gross income` ~ Total, data=dd)
summary(initial_model)

# Check the structure of the object produced
class(initial_model)
attributes(initial_model)

# Access coefficients and fitted values
initial_model$coefficients
initial_model$fitted.values


# Check the fit visually for gross income
plot(dd$Total, dd$`gross income`)
lines(dd$Total, initial_model$fitted.values, col="red")

# Residuals vs Fitted values
plot(initial_model$fitted.values, residuals(initial_model), 
     main = "Residuals vs Fitted", xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "red")

# Normal Q-Q plot
qqnorm(residuals(initial_model))
qqline(residuals(initial_model), col = "red")

# Scale-Location plot
plot(initial_model$fitted.values, sqrt(abs(residuals(initial_model))), 
     main = "Scale-Location", xlab = "Fitted values", ylab = "√|Residuals|")
abline(h = 0, col = "red")

# Residuals vs Leverage plot
plot(initial_model, which = 5)

dd <- read_excel("C:/Users/sambr/Downloads/200quenosondelclustersample.xlsx")
prediccio <- predict(initial_model, newdata = dd)
plot(prediccio)
diferencia <- sqrt((dd$`gross income`-prediccio)^2)
summary(diferencia)


plot(dd$`gross income`, prediccio, 
     main = "Actual vs Predicted Gross Income",
     xlab = "Actual Gross Income", ylab = "Predicted Gross Income")
abline(a = 0, b = 1, col = "red") # Add a y=x line for reference



