
#recalling the R basics|practice
Price = 3:10
Cost = 1.8
Sales = 12-1.1*Price
Revenue = Sales*Price
Profit = Sales*(Price - Cost)
Data = data.frame(vPrice = Price,
                  vProfit = Profit,
                  Profit_Level = c("L", "M", "H", "H", "H", "M", "L", "L"))
max(Revenue)
max(Profit)
plot(x = Price, y = Revenue)
plot(x = Price, y = Profit)

length(vProfit)
length(Profit)
View(Data)

length(Data$Profit_Level)
mean(Data$Profit_Level)
Data$vCost = 1.8
Data$vMargin = Data$vPrice - Data$vCost
Data  

vMargin = vPrice - vCost  
Data$Var = Data$vPrice - Data$Profit_Level

Data$Recommendation = ifelse(Data$vProfit >22, "Do it!", "Forget about it!")
Data

#Create a Data set by deleting Profit Level
DataNew = Data[,-3]
DataNew

#Create a Data set with only 2 variables
DataNew = Data[, c("vPrice", "vProfit")]
DataNew

DataNew2 = Data[, 1:2]
DataNew2

#Create a Data set when profit level "M
DataM = Data[Data$Profit_Level == "M",]
DataNew2

#Regression 1 - Orange Juice Data
#we want to run the regression for the weekly sales data for orange juice from a given chain of grocery stores
#we have the independent variables as follows:
#brand: orange juice brand
#sales: weekly sales volume
#prive: price per carton
#feat: whether the product is featured (if featured, feat=1; otherwise 0)
#modePrice: the mode price - the most frequent price for the brand, which can be considered as the regular price
#discount: If the price is below the modePrice, it is considered as the regular price, otherwise it is not having a discount (when having discount, discount=1; otherwise 0)

oj = read.csv(file = "oj.csv")
View(oj)
head(oj)
dim(oj)
summary(oj)

#Average sales when it is not featured is 10,071.17, this number increases by 4x when it is featured
aggregate(sales~feat, data=oj, mean)
Difference = 40590.47 - 10071.17
Difference
40590.47/10071.17

#we can predict that beta(0) will be 10,071.17 and beta(1) will equal the difference = 30,519.3
#the first regression model is E[Sales|Feature] = beta(0) + beta(1)*Feature
reg1 = lm(sales~feat, data = oj)
summary(reg1)

#the regression shows that the sales volume is over four times higher when the product is featured than when it is not.

aggregate(sales~feat, data = oj, mean)
Difference = 40590.47 - 10071.17
Difference  

# now run the regression to see the effect of discounts on sales
reg2 = lm(sales~discount, data = oj)  
summary(reg2)
#the result shows that the discount results in higher volumes. Beta(1) is 19,882.8

reg3 = lm(sales~price, data = oj)
summary(reg3)
#as expected the price and sales have inverse relationship. Beta(1) is negative

#We can plot this relationship as below
plot(oj$price, oj$sales, main = "Price and sales", xlab = "Price", ylab = "Sales")
abline(lm(sales~price, data = oj), col = "blue")

#given the product is featured, how likely is it to also have a price discount?
aggregate(discount~feat, data = oj, mean)
#when the product is not featured the price discount is given 25% of the time, while when the product is featured the price discount was given 44% of the time
#this shows the relationship between the product having a higher discount when being featured and when not. This means that in order to obtain more clear regression results, we need to include both variables into our sales regression analysis.
reg4 = lm(sales~feat + discount, data =oj)
summary(reg4)
#when we compare the results with the initial results with only one variable, we can see that their impact was "inflated" or "overvalued". 
#the impact of the product being featured is now lower, when keeping the discount contant and vise versa.
summary(lm(sales~price, data=oj[oj$feat ==0,]))
summary(lm(sales~price, data=oj[oj$feat ==1,]))
#if we run for the price sensitivity given the product is featured and not featured, we can see that when the product is featured, the consumers are less elastic to the price.

#Incorporating Interaction for further investigation
oj$feat_price = oj$feat*oj$price
reg6 = lm(sales~feat + price + feat_price, data = oj)
summary(reg6)

#here we see how the impact of the first variable on sales volume may depend on the value of the second variable. When the price and the feat are combines, their impact is less than the sum of the individual effects.
