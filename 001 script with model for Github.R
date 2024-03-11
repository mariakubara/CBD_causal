library(tidyverse)
library(sf)
library(grf) #regression_forest

### Acknowledgment: 
# Codes for this analysis are adapted from 
# dr Kevin Credit's codes, presented during classes "Causal Machine Learning for Spatial Data"
# conducted in October 2023 at the Faculty of Economic Sciences, University of Warsaw


# load warsaw districs
waw.sf <- st_read("data/dzielnice_Warszawy/dzielnice_Warszawy.shp")
waw.sf <- st_transform(waw.sf, crs = "+proj=longlat +datum=NAD83")

waw.sf.line <- st_cast(waw.sf,"MULTILINESTRING")
waw.total <- st_union(waw.sf)

#save(X, Y, checkCBD, file = "data/startup github.RData")
load("data/startup github.RData")



### X - presents the variables included in the model
summary(X)

### Description of the dataset:

# V1 - `Total assets first`, - how much total assets a company had at the time of foundation
# V2 - `Employees first`, - how many employees within first year of operation
# V3 - ownershipForeign, - whether a company is dominantly controlled by foreign capital
# V4 - virtual, - whether a company uses a virtual office
# V5 - metro500, - whether there is a metro station in 500m radius from the office location
# V6 - catering500Index, - whether there is a catering facility in 500m radius (insensitivity index)
# V7 - accomodation500Index, - whether there is an accomodation facility in 500m radius (insensitivity index)
# V8 - transportAll500Index, - whether there is a public transport facility in 500m radius (insensitivity index)
# V9 - shopping500Index, - whether there is a shopping facility in 500m radius (insensitivity index)
# V10 - neighbors - how many investors a company has (formal neighbours)



### Y - presents the dependent variable 

# Total assets after 5 years of operation. If a company did not survived - assets are set to 0. 


### checkCBD - consists of location based data on startups

# main treatment variable CBD - whether a company is in the CBD location
# additional controls: CBD_500 and CBD_1000 - CBD buffer extended by 500m and 1000m
# additionally: lng, lat - coordinates of the office 


### Gather data in one set

data <- cbind(X,Y, checkCBD)
colnames(data) <- c("Total assets first", 
                    "Employees first", 
                    "ownershipForeign",
                    "virtual",
                    "metro500", 
                    "catering500Index",
                    "accomodation500Index",
                    "transportAll500Index",
                    "shopping500Index",
                    "neighbors", 
                    "Total assets last",
                    "CBD",
                    "CBD_500",
                    "CBD_1000",
                    "lng", "lat")

# Define treatment variable

W <- checkCBD$CBD
# W <- checkCBD$CBD_500 # run for robustness checks
# W <- checkCBD$CBD_1000 # run for robustness checks





# Estimate baseline outcomes for no test/train split
# m(X) = E[Y | X] 
forest.Y <- regression_forest(X, Y)
Y.hat_train <- predict(forest.Y)$predictions


# Estimate propensity score of treatment E[W | X]
forest.W <- regression_forest(X, W)
W.hat_train <- predict(forest.W)$predictions


# Train the causal forest (weighted by propensity score)
c.forest <- causal_forest(X, Y, W, Y.hat_train, W.hat_train)
tau.hat <- predict(c.forest, X)$predictions

data$tau.hat <- tau.hat


# Find ATE
average_treatment_effect(c.forest, target.sample = "overlap") 
# ATE for all observations (treated and untreated)


# What's the nature of the heterogeneity? 
# What variables are useful for targeting based on treatment effects?
imp <- c.forest %>% 
  variable_importance() %>% 
  as.data.frame() %>% 
  mutate(variable = colnames(c.forest$X.orig)) 

imp$V2 <- c("Total assets first", "Employees first", 
            "ownershipForeign", 'virtual', 'metro500', 
            "catering500Index","accomodation500Index","transportAll500Index","shopping500Index",
            'neighbors')

imp[order(imp$V1, decreasing = TRUE),]



#Plot linear relationships between CATE and the most important variables
ggplot(data, aes(x = `Total assets first`, y = tau.hat, color=as.factor(CBD))) +
  scale_color_brewer(palette="Paired") +
  geom_point(alpha = 0.4 ) +
  geom_smooth(method = "lm", fullrange=TRUE) +
  ylab("Treatment effect") +
  theme_light()

ggplot(data, aes(x = `Employees first`, y = tau.hat, color=as.factor(CBD))) +
  scale_color_brewer(palette="Paired") +
  geom_point(alpha = 0.4 ) +
  geom_smooth(method = "lm", fullrange=TRUE) +
  ylab("Treatment effect") +
  theme_light()

ggplot(data, aes(x = `neighbors`, y = tau.hat, color=as.factor(CBD))) +
  scale_color_brewer(palette="Paired") +
  geom_point(alpha = 0.4 ) +
  geom_smooth(method = "lm", fullrange=TRUE) +
  ylab("Treatment effect") +
  theme_light()

ggplot(data, aes(x = `accomodation500Index`, y = tau.hat, color=as.factor(CBD))) +
  scale_color_brewer(palette="Paired") +
  geom_point(alpha = 0.4 ) +
  geom_smooth(method = "lm", fullrange=TRUE) +
  ylab("Treatment effect") +
  theme_light()

ggplot(data, aes(x = `transportAll500Index`, y = tau.hat, color=as.factor(CBD))) +
  scale_color_brewer(palette="Paired") +
  geom_point(alpha = 0.4 ) +
  geom_smooth(method = "lm", fullrange=TRUE) +
  ylab("Treatment effect") +
  theme_light()

ggplot(data, aes(x = `virtual`, y = tau.hat, color=as.factor(CBD))) +
  scale_color_brewer(palette="Paired") +
  geom_point(alpha = 0.4 ) +
  geom_smooth(method = "lm", fullrange=TRUE) +
  ylab("Treatment effect") +
  theme_light()

ggplot(data, aes(x = `ownershipForeign`, y = tau.hat, color=as.factor(CBD))) +
  scale_color_brewer(palette="Paired") +
  geom_point(alpha = 0.4 ) +
  geom_smooth(method = "lm", fullrange=TRUE) +
  ylab("Treatment effect") +
  theme_light()

ggplot(data, aes(x = `transportAll500Index`, y = tau.hat, color=as.factor(CBD))) +
  scale_color_brewer(palette="Paired") +
  geom_point(alpha = 0.4 ) +
  geom_smooth(method = "lm", fullrange=TRUE) +
  ylab("Treatment effect") +
  theme_light()

ggplot(data, aes(x = `virtual`, y = tau.hat, color=as.factor(CBD))) +
  scale_color_brewer(palette="Paired") +
  geom_point(alpha = 0.4 ) +
  geom_smooth(method = "lm", fullrange=TRUE) +
  ylab("Treatment effect") +
  theme_light()



#Best linear projection - statistical significance for individual CATE
best_linear_projection(c.forest, X, debiasing.weights= W.hat_train, 
                       target.sample = "overlap")


##Compute a prioritization rule based on estimated treatment effects
priority.cate <- predict(c.forest, X)$predictions


#Estimate AUTOC (in this case, no held out data)
cf.eval <- causal_forest(X, Y, W, Y.hat_train, W.hat_train) 
#Priority rule = units with biggest positive treatment effects (CATE) are highest-priority


#"We evaluate prioritization rules via rank-weighted average treatment effects (RATEs), which
#capture the extent to which individuals who are highly ranked by the prioritization rule are
#more responsive to treatment than the average treatment effect"
rate <- rank_average_treatment_effect(cf.eval, priority.cate, debiasing.weights = W.hat_train)


# In case of 0 or 1 extreme propensities -> add minor modifier to overcome the function error
# cf.eval$W.hat[114] <- cf.eval$W.hat[114] -0.0000001
# W.hat_train %in% 0
# W.hat_train[114] <- W.hat_train[114]-0.0001
# W.hat_train[113]

rate <- rank_average_treatment_effect(cf.eval, priority.cate, debiasing.weights = W.hat_train)

plot(rate)



