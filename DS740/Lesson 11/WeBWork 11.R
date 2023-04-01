setwd("C:/Users/jeffe/Documents/MSDS/GitHub/MSDS/DS740/lesson 11")

library(dplyr)
library(arules)



ames <- read.csv("AmesSimple.csv")

ames <- ames %>%
  mutate(Lot.Shape = ifelse(Lot.Shape =="Reg", 1, 0))
ames$Bedroom.AbvGr = discretize(ames$Bedroom.AbvGr, breaks=2, ordered=T, method="interval")

ames_count <- ames %>%
  filter(Bedroom.AbvGr == "[0,4)")

ames$Full.Bath = discretize(ames$Full.Bath, breaks = 2, ordered = T, method = "interval")
LivArea = discretize(ames$Gr.Liv.Area, breaks = 3, ordered = T, method = "interval")

ames$Gr.Liv.Area = discretize(ames$Gr.Liv.Area, method = "fixed", 
                              breaks=c(334, 1126, 1743, 5642), 
                              ordered=T)

summary(ames$SalePrice)
ames$SalePrice = discretize(ames$SalePrice, method = "fixed", 
                              breaks=c(12789, 129500, 213500, 755000), 
                              ordered=T)
summary(ames$SalePrice)


ames <- ames %>%
  mutate(across(where(is.character), factor)) %>%
  mutate(across(where(is.double), factor)) %>%
  mutate(across(where(is.integer), factor))

ames_trans = as(ames, "transactions")

rules = apriori(ames_trans, 
                parameter = list(support = .05, confidence = 0.5, maxlen = 12))
summary(rules)
rules2 = apriori(ames_trans, parameter = list(support = .05, confidence = 0.5, maxlen = 12), 
                 appearance = list(rhs = c("SalePrice=[2.14e+05,7.55e+05]"), default = "lhs") )
non_redundant = !is.redundant(rules2)
rules3 = rules2[non_redundant]
summary(rules3)
rules4 = subset( rules3, subset = lhs %in% c("Bldg.Type.simple=1Fam", "Bldg.Type.simple=2Fam") )
high_lift = subset(rules3, subset = lift > 3.5 & confidence > .95)
summary(high_lift)
mylhs = lhs(rules3)
singleAnt = which( size(mylhs) == 1 )
inspect( rules3[singleAnt] )
inspect(head(mylhs, n = 5, by = "lift"))
mylhs_mat = as(mylhs, Class = "matrix")
mylhs_mat        
