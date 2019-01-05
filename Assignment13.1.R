cobra = read.csv("COBRA-YTD2017.csv")
summary(cobra)
str(cobra)


summary(is.na(cobra))
table(cobra$MaxOfnum_victims)
table(cobra$loc_type)

cobra$MaxOfnum_victims = as.numeric(as.character(cobra$MaxOfnum_victims))
cobra$loc_type = as.numeric(as.character(cobra$loc_type ))

cobra[!complete.cases(cobra$MaxOfnum_victims),'MaxOfnum_victims'] = mean(cobra$MaxOfnum_victims,na.rm = TRUE)
cobra[!complete.cases(cobra$loc_type),'loc_type'] = mean(cobra$loc_type,na.rm = TRUE)

#a. Find out top 5 attributes having highest correlation (select only Numeric features).
library(dplyr)
cor_crime <- cobra %>% select (c('MI_PRINX','offense_id','beat','MinOfucr','x','y','loc_type','MaxOfnum_victims'))
cor_crime
summary(cor_crime)
str(cor_crime)

library(gclus)
high_cor <- cor_crime# get data
high_cor
high_cor.r <- abs(cor(high_cor)) # get correlations
high_cor.col <- dmat.color(high_cor.r) # get colors
# reorder variables so those with highest are closest to the diagonal
high_cor.o <- order.single(high_cor.r)
cpairs(high_cor, high_cor.o, panel.colors=high_cor.col, gap=.5,
       main="Variables Ordered and Colored by Correlation" )

#b. Find out top 3 reasons for having more crime in a city.






crimetype  = table (cobra$UC2.Literal)
barplot(height = crimetype[order(crimetype,decreasing = TRUE)] ,col = 'Red')
#  LARCENY-FROM VEHICLE, LARCENY-NON VEHICLE , AUTO THEFT  , BURGLARY-RESIDENCE , AGG ASSAULT
# logistic regression model:
str(cobra)
fit <- lm(MaxOfnum_victims~.,data = cobra,family = binomial)
summary(fit)

library(MASS)
step_fit <- stepAIC(fit,method='backward')


step_fit <- stepAIC(fit,method='forward')
step_fit <- stepAIC(fit,method='both')
summary(step_fit)
confint(step_fit)

 





#c. Which all attributes have correlation with crime rate?
install.packages('corrr')
library(corrr)
correl = cor_crime %>% correlate() %>% focus(MaxOfnum_victims)

corrplot(correl, method="color")






