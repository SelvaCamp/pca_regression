
library(tidyverse)
library(ggplot2)
library(corrplot)

head(home_sales)
str(home_sales, give.attr=FALSE)

#Let's visualize the target variable
ggplot(home_sales, aes(selling_price))+
  geom_density()

summary(home_sales$selling_price)

home_sales%>%
  cor()%>%
  corrplot()

cor_sqft_linving_selling_price=cor(home_sales$sqft_living, home_sales$bedrooms)

#CPA Computation
lapply(home_sales, var)


home_sales_1=home_sales%>%
  select(-selling_price)

home_sales_1=home_sales_1%>%
  scale()%>%
  as.data.frame()

pca_home_sales=prcomp(home_sales_1)

#Variance eigenvalues
pca_home_sales$sdev^2%>%
  round(2)

#Proportion explained
(pca_home_sales$sdev^2/length(pca_home_sales$sdev))%>%
  round(2)

summary(pca_home_sales) #the minimum number for which cuulative proportio exceeds 70% is CP4

#Correlations btw original variables and components
round(pca_home_sales$rotation[ , 1:7], 2)

#Kaiser-Guttman Criterion

sum(pca_home_sales$sdev^2 >1) #the kaiser-Guttman criterion we extract 3 components

#screenplot

screeplot(pca_home_sales, type="lines")
box()
abline(h=1, lty=2) #the screenplot suggest 3 components

#biplot
biplot(pca_home_sales, choices = 1:2, cex=0.5)

#Let's compare models
library(car)

mdl1=lm(selling_price ~., data=home_sales)
vif(mdl1)

summary(mdl1)$adj.r.squared

#Let's combine the target variable (it is not scaled) and pca

data_Home_Sales_Components=cbind(home_sales[ , "selling_price"],
                                 pca_home_sales$x[ , 1:3])%>%
  as.data.frame()

mdl2=lm(selling_price~., data=data_Home_Sales_Components)
vif(mdl2)

summary(mdl2)$adj.r.squared

summary(mdl2)


