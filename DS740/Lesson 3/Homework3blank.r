### Load packages
library(ggformula)

### Load data
wisc_income <- read.csv("Wisconsin_income-1.csv")
wisc_income <- wisc_income %>% 
  mutate(CIT2 = as.factor(CIT2),
        COW = as.factor(COW),
        LANX = as.factor(LANX),
        MAR = as.factor(MAR),
        SEX = as.factor(SEX),
        DIS = as.factor(DIS),
        RAC = as.factor(RAC),
        Hispanic = as.factor(Hispanic),
# add log transformation of JWMP and PERNP
        JWMNP.log = log(JWMNP),
        PERNP.log = log(PERNP))

