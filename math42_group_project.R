library(nnet)
library(reshape2)
library(foreign)
library(dplyr)
library(ggplot2)

DataSet <- na.omit(flavors_of_cacao[5:9])
newDataSet <- DataSet[,!(names(DataSet) %in% "Bean\nType")]
newDataSet <- newDataSet %>%
           rename("cocoa_percent" = "Cocoa\nPercent",
                  "company_location" = "Company\nLocation",
                 "rating"="Rating",
                 "bean_origin" = "Broad Bean\nOrigin")
# newDataSet
newDataSet$company_location[newDataSet$company_location != "U.S.A." &
                              newDataSet$company_location != "France" &
                              newDataSet$company_location != "U.K." &
                              newDataSet$company_location != "Italy"] <- "Other"

newDataSet$bean_origin[newDataSet$bean_origin != "Venezuela" &
                          newDataSet$bean_origin != "Ecuador" &
                          newDataSet$bean_origin != "Peru" &
                          newDataSet$bean_origin != "Madagascar"] <- "Other"
#
 temp1 = c("42%","46%","50%", "53%","55%","56%","57%","58%")
 temp2 = c("60%","60.5%","61%","62%","63%","64%","65%","66%","67%","68%","69%")
 temp3 = c("70%","72.5%","71%","72%","73%","74%","75%","76%","77%","78%","79%","73.5%")
 temp4 = c("80%","81%","82%","83%","84%","85%","86%","87%","88%","89%")
 temp5 = c("90%","91%", "99%","100%")
#
 newDataSet$cocoa_percent[newDataSet$cocoa_percent %in% temp1] <- "below 60%"
 newDataSet$cocoa_percent[newDataSet$cocoa_percent %in% temp2] <- "60%-70%"
 newDataSet$cocoa_percent[newDataSet$cocoa_percent %in% temp3] <- "70%-80%"
 newDataSet$cocoa_percent[newDataSet$cocoa_percent %in% temp4] <- "80%-90%"
 newDataSet$cocoa_percent[newDataSet$cocoa_percent %in% temp5] <- "over 90%"
#
#  write.csv(newDataSet,file = "newDataSet.csv", row.names = FALSE)
# 
 with(newDataSet, table(cocoa_percent, rating))
 with(newDataSet, table(company_location, rating))
 with(newDataSet, table(bean_origin, rating))

## 1. Analyze cocoa_percent
with(newDataSet, do.call(rbind, tapply(rating, cocoa_percent, function(x) c(M = mean(x), SD = sd(x)))))

test <- multinom(cocoa_percent ~ rating, data = newDataSet)

exp(coef(test))
# the relative risk ratio for a one-unit increase in the variable
# rate is 1.054 for being in 70%-80% vs 60%-70%

#get the predicted probabilities for the observations
head(pp <- fitted(test))

dwrite <- data.frame(newDataSet$cocoa_percent, newDataSet$rating)


dwrite <- dwrite %>%
          rename("cocoa_percent" = "newDataSet.cocoa_percent",
                 "rating"="newDataSet.rating")

pp.write <- cbind(dwrite, predict(test, newdata = dwrite, type = "probs", se=TRUE))

lpp <- melt(pp.write, id.vars = c("cocoa_percent", "rating"), 
            value.name = "probability")

head(lpp)

ggplot(lpp, aes(x = rating, y = probability)) + geom_line() + facet_grid(variable ~., scales = "free")



## 2. Analyze company location
with(newDataSet, do.call(rbind, tapply(rating, company_location, function(x) c(M = mean(x), SD = sd(x)))))

test2 <- multinom(company_location ~ rating, data = newDataSet)

exp(coef(test2))
# the relative risk ratio for a one-unit increase in the variable
# rate is xxx for being in xxx vs xxx

#get the predicted probabilities for the observations
head(pp2 <- fitted(test2))

dwrite2 <- data.frame(newDataSet$company_location, newDataSet$rating)

# dwrite2
dwrite2 <- dwrite2 %>%
  rename("company_location" = "newDataSet.company_location",
         "rating"="newDataSet.rating")

pp.write2 <- cbind(dwrite2, predict(test2, newdata = dwrite2, type = "probs", se=TRUE))

lpp2 <- melt(pp.write2, id.vars = c("company_location", "rating"), 
            value.name = "probability")

head(lpp2)

ggplot(lpp2, aes(x = rating, y = probability)) + geom_line() + facet_grid(variable ~., scales = "free")



## 3. Analyze bean origin
with(newDataSet, do.call(rbind, tapply(rating, bean_origin, function(x) c(M = mean(x), SD = sd(x)))))

test3 <- multinom(bean_origin ~ rating, data = newDataSet)

exp(coef(test3))
# the relative risk ratio for a one-unit increase in the variable
# rate is xxx for being in xxx vs xxx

#get the predicted probabilities for the observations
head(pp3 <- fitted(test3))

dwrite3 <- data.frame(newDataSet$bean_origin, newDataSet$rating)

# dwrite3
dwrite3 <- dwrite3 %>%
  rename("bean_origin" = "newDataSet.bean_origin",
         "rating"="newDataSet.rating")

pp.write3 <- cbind(dwrite3, predict(test3, newdata = dwrite3, type = "probs", se=TRUE))

lpp3 <- melt(pp.write3, id.vars = c("bean_origin", "rating"), 
             value.name = "probability")

head(lpp3)

ggplot(lpp3, aes(x = rating, y = probability)) + geom_line() + facet_grid(variable ~., scales = "free")

