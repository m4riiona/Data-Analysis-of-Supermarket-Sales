install.packages("readxl")
install.packages("vcd")
library(readxl)
library(ggplot2)

dd <- read_excel("C:/Users/pilar/Downloads/Basededades.xlsx")

#qualitative numerical
ggplot(dd, aes(x = `Time of the month`, y = `Quantity`, fill = `Time of the month`)) +
  geom_boxplot(position = "dodge") +
  labs(title = "Quantitative and Qualitative", x = "Time of the month", y = "Quantity of products", fill = "Time of the month")

ggplot(dd, aes(x = `Quantity`, fill = `Time of the month`)) +
  geom_density(alpha = 0.5) +
  labs(title = "Quantitative and Qualitative", x = "Quantity of products", fill = "Time of the month")

mean_beginning <- mean(dd$Quantity[dd$`Time of the month` == "Beginning"])
mean_end <- mean(dd$Quantity[dd$`Time of the month` == "End"])

#qualitative qualitative

ggplot(dd, aes(x = `Gender`, fill = `Product line`)) +
  geom_bar(position = "stack") +
  labs(title = "Qualitative and qualitative", x = "Gender", fill = "Product line")


ggplot(dd, aes(x = `Gender`, fill = `Product line`)) +
  geom_bar(position = "dodge") +
  labs(title = "Qualitative and qualitative", x = "Gender", fill = "Product line")

health_beauty_male <- subset(dd, Gender == "Male" & `Product line` == "Health and beauty")
health_beauty_female <- subset(dd, Gender == "Female" & `Product line` == "Health and beauty")
fashion_acc_male <- subset(dd, Gender == "Male" & `Product line` == "Fashion accessories")
fashion_acc_female <- subset(dd, Gender == "Female" & `Product line` == "Fashion accessories")


#numerical numerical
ggplot(dd, aes(x = `Quantity`, y = `Tax 5%`)) +
  geom_point() +
  labs(title = "Quantitative and Quantitative", x = "Quantity of products", y = " Tax")



ggplot(dd, aes(x = `Total`, y = `Tax 5%`)) +
  geom_point() +
  labs(title = "Quantitative and Quantitative", x = "Total", y = " Tax")

