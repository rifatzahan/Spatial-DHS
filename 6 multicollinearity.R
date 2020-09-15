###############################
### Check Multicollinearity ###
###############################

library(carData)
library(car)

vif(glm(status2~ District + education + religion + wealth + residence + totalChildrenEverBorn + cohabAge + maritalStatus + 
        occupation + occupation_partner + education_partner + aliveChildren, data=dfaddress2))

vif(glm(status2~ totalChildrenEverBorn + wealth + occupation + maritalStatus + cohabAge + residence, data=dfaddress2))
