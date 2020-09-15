##########################
### Variable Selection ###
##########################
dfaddress2 <- subset(dfaddress, select = c(status, District, education, religion, wealth, residence,
  totalChildrenEverBorn, cohabAge, maritalStatus, occupation, occupation_partner, education_partner, 
  aliveChildren, currentAgeGroup)) 

dfaddress2 <- as.data.frame(dfaddress2)
dfaddress2 <- na.omit(dfaddress2)
dfaddress2$status2 <- as.numeric(dfaddress2$status)

stack <- INLA::inla.stack(data = list(y = dfaddress2$status2),
                           A = list(1),
                           effects = list(data.frame(Intercept = 1, dfaddress2[,3:13])))

result <- INLAstep(fam1 = "binomial", 
                     dfaddress2,
                     in_stack = stack,
                     invariant = "0 + Intercept",
                     direction = 'forwards',
                     include = 3:13,
                     y = 'y',
                     powerl = 2,
                     inter = 1,
                     thresh = 1)

result$best_formula


autoplot(result$best_model, which = c(1, 5), CI = TRUE)

