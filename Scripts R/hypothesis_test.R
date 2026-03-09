

install.packages("read.xl")
library(readxl)

#SCENARIO 1

#The store manager of a supermarket chain has been in 
#his position for more than 10 years. He suspects that
#the average number of products that customers buy is
#greater than 5. To check his prediction, he tracks the 
#number of products that each customer buys over a certain 
#period of time.

x<-dd$`Quantity`
t.test(x,alternative="greater", mu=5)

#Since the p.value is excessively low, we can reject the
#null hypothesis.There is strong evidence that the true 
#mean of products purchased is greater than 5, thus confirming
#the alternative hypothesis and supporting the manager's suspicion.


#_______________________________________________________________________________

#SCENARIO 2

#The store manager of a supermarket chain has been
#in his position for more than 10 years. He suspects
#that the average number of products that customers
#buy is greater than 5. To check his prediction, he
#tracks the number of products that each customer 
#buys over a certain period of time.

x<-dd$'Product line'
y<-dd$Gender

x<-as.factor(x)
y<-as.factor(y)

table(x,y)
chisq.test(x,y,correct = FALSE)

#The p-value is greater than the significance level (0.05), 
#which indicates that there is insufficient evidence to refute
#the null hypothesis. There is sufficient evidence to refute the
#null hypothesis which, in this context, would be that no
#There is no significant association between gender and product line.

#_______________________________________________________________________________

#SCENARIO 3

#Two roommates are talking about their expenses and a specific
#topic comes up in the conversation. One of the roommates believes
#that people tend to buy more products at the beginning of the 
#month than at the end of the month because they have just been
#paid. The other says that there is no significant relationship
#between the two. They want to verify whose statement is true,
#so they do some research and find a database with data on
#purchases in a famous supermarket chain.


x<-dd$Quantity
y<-dd$`Moment del mes`

y<-as.factor(y)

aov(x~y)
summary(aov(x~y))

#The p-value is greater than the significance level (0.05), 
#which indicates that there is insufficient evidence to refute
#the null hypothesis. There is sufficient evidence to refute the
#null hypothesis which, in this context, would be that no
#There is no significant association between quantity and time of the month.


