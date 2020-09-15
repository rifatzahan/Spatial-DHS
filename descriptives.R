
#################################
#### Frequency and Percentage####
#################################

summary(is.na(dfaddress$wealth))
summary(is.na(dfaddress$maritalStatus))
summary(is.na(dfaddress$occupation))
summary(is.na(dfaddress$cohabAge))
summary(is.na(dfaddress$totalChildrenEverBorn))
summary(is.na(dfaddress$residence))


dfaddress <- dfaddress[!is.na(dfaddress$occupation),]



round(wtd.table(x = dfaddress$status, weights = dfaddress$weightsdf, normwt = TRUE)*100/
        (sum(wtd.table(x = dfaddress$status, weights = dfaddress$weightsdf, normwt = TRUE))),2)



#############
### Total ###
#############


round(wtd.table(x = dfaddress$residence, weights = dfaddress$weightsdf, normwt = TRUE), 0)
round(wtd.table(x = dfaddress$residence, weights = dfaddress$weightsdf, normwt = TRUE)*100/
        (sum(wtd.table(x = dfaddress$residence, weights = dfaddress$weightsdf, normwt = TRUE))),2)

round(wtd.table(x = dfaddress$occupation, weights = dfaddress$weightsdf, normwt = TRUE),0)
round(wtd.table(x = dfaddress$occupation, weights = dfaddress$weightsdf, normwt = TRUE)*100/
        (sum(wtd.table(x = dfaddress$occupation, weights = dfaddress$weightsdf, normwt = TRUE))),2)

round(wtd.table(x = dfaddress$wealth, weights = dfaddress$weightsdf, normwt = TRUE),0)
round(wtd.table(x = dfaddress$wealth, weights = dfaddress$weightsdf, normwt = TRUE)*100/
        (sum(wtd.table(x = dfaddress$wealth, weights = dfaddress$weightsdf, normwt = TRUE))), 2)

round(wtd.table(x = dfaddress$maritalStatus, weights = dfaddress$weightsdf, normwt = TRUE),0)
round(wtd.table(x = dfaddress$maritalStatus, weights = dfaddress$weightsdf, normwt = TRUE)*100/
        (sum(wtd.table(x = dfaddress$maritalStatus, weights = dfaddress$weightsdf, normwt = TRUE))),2)

round(wtd.table(x = dfaddress$totalChildrenEverBorn, weights = dfaddress$weightsdf, normwt = TRUE),0)
round(wtd.table(x = dfaddress$totalChildrenEverBorn, weights = dfaddress$weightsdf, normwt = TRUE)*100/
        (sum(wtd.table(x = dfaddress$totalChildrenEverBorn, weights = dfaddress$weightsdf, normwt = TRUE))),2)

round(wtd.table(x = dfaddress$education, weights = dfaddress$weightsdf, normwt = TRUE), 0)
round(wtd.table(x = dfaddress$education, weights = dfaddress$weightsdf, normwt = TRUE)*100/
        (sum(wtd.table(x = dfaddress$education, weights = dfaddress$weightsdf, normwt = TRUE))),2)

round(wtd.table(x = dfaddress$religion, weights = dfaddress$weightsdf, normwt = TRUE), 0)
round(wtd.table(x = dfaddress$religion, weights = dfaddress$weightsdf, normwt = TRUE)*100/
        (sum(wtd.table(x = dfaddress$religion, weights = dfaddress$weightsdf, normwt = TRUE))),2)

round(wtd.table(x = dfaddress$education_partner, weights = dfaddress$weightsdf, normwt = TRUE), 0)
round(wtd.table(x = dfaddress$education_partner, weights = dfaddress$weightsdf, normwt = TRUE)*100/
        (sum(wtd.table(x = dfaddress$education_partner, weights = dfaddress$weightsdf, normwt = TRUE))),2)

round(wtd.table(x = dfaddress$occupation_partner, weights = dfaddress$weightsdf, normwt = TRUE),0)
round(wtd.table(x = dfaddress$occupation_partner, weights = dfaddress$weightsdf, normwt = TRUE)*100/
        (sum(wtd.table(x = dfaddress$occupation_partner, weights = dfaddress$weightsdf, normwt = TRUE))),2)

round(wtd.table(x = dfaddress$aliveChildren, weights = dfaddress$weightsdf, normwt = TRUE),0)
round(wtd.table(x = dfaddress$aliveChildren, weights = dfaddress$weightsdf, normwt = TRUE)*100/
        (sum(wtd.table(x = dfaddress$aliveChildren, weights = dfaddress$weightsdf, normwt = TRUE))),2)

#wtd.table(x = dfaddress$cohabAge, weights = dfaddress$weightsdf, normwt = TRUE)*100/(sum(wtd.table(x = dfaddress$cohabAge, weights = dfaddress$weightsdf, normwt = TRUE)))

weighted.median <- function(x, w, na.rm=TRUE) {
  unname(weighted.quantile(x, probs=0.5, w=w, na.rm=na.rm))
}

weighted.25 <- function(x, w, na.rm=TRUE) {
  unname(weighted.quantile(x, probs=0.25, w=w, na.rm=na.rm))
}

weighted.75 <- function(x, w, na.rm=TRUE) {
  unname(weighted.quantile(x, probs=0.75, w=w, na.rm=na.rm))
}

weighted.median(dfaddress$cohabAgeC, dfaddress$weightsdf, na.rm=TRUE)
weighted.75(dfaddress$cohabAgeC, dfaddress$weightsdf, na.rm=TRUE) - 
  weighted.25(dfaddress$cohabAgeC, dfaddress$weightsdf, na.rm=TRUE)


weighted.median(dfaddress$currentAge_exact, dfaddress$weightsdf, na.rm=TRUE)
weighted.75(dfaddress$currentAge_exact, dfaddress$weightsdf, na.rm=TRUE) - 
  weighted.25(dfaddress$currentAge_exact, dfaddress$weightsdf, na.rm=TRUE)


##########################################
##### Data: Terminated Pregnancies #######
##########################################
terminate <- subset(dfaddress, status == 1)


## Frequency and Percentage
round(wtd.table(x = terminate$residence, weights = terminate$weightsdf, normwt = TRUE), 0)
round(wtd.table(x = terminate$residence, weights = terminate$weightsdf, normwt = TRUE)*100/
  (sum(wtd.table(x = terminate$residence, weights = terminate$weightsdf, normwt = TRUE))),2)

round(wtd.table(x = terminate$occupation, weights = terminate$weightsdf, normwt = TRUE),0)
round(wtd.table(x = terminate$occupation, weights = terminate$weightsdf, normwt = TRUE)*100/
        (sum(wtd.table(x = terminate$occupation, weights = terminate$weightsdf, normwt = TRUE))),2)

round(wtd.table(x = terminate$wealth, weights = terminate$weightsdf, normwt = TRUE),0)
round(wtd.table(x = terminate$wealth, weights = terminate$weightsdf, normwt = TRUE)*100/
  (sum(wtd.table(x = terminate$wealth, weights = terminate$weightsdf, normwt = TRUE))), 2)

round(wtd.table(x = terminate$maritalStatus, weights = terminate$weightsdf, normwt = TRUE),0)
round(wtd.table(x = terminate$maritalStatus, weights = terminate$weightsdf, normwt = TRUE)*100/
  (sum(wtd.table(x = terminate$maritalStatus, weights = terminate$weightsdf, normwt = TRUE))),2)

round(wtd.table(x = terminate$totalChildrenEverBorn, weights = terminate$weightsdf, normwt = TRUE),0)
round(wtd.table(x = terminate$totalChildrenEverBorn, weights = terminate$weightsdf, normwt = TRUE)*100/
  (sum(wtd.table(x = terminate$totalChildrenEverBorn, weights = terminate$weightsdf, normwt = TRUE))),2)


round(wtd.table(x = terminate$education, weights = terminate$weightsdf, normwt = TRUE), 0)
round(wtd.table(x = terminate$education, weights = terminate$weightsdf, normwt = TRUE)*100/
        (sum(wtd.table(x = terminate$education, weights = terminate$weightsdf, normwt = TRUE))),2)

round(wtd.table(x = terminate$religion, weights = terminate$weightsdf, normwt = TRUE), 0)
round(wtd.table(x = terminate$religion, weights = terminate$weightsdf, normwt = TRUE)*100/
        (sum(wtd.table(x = terminate$religion, weights = terminate$weightsdf, normwt = TRUE))),2)

round(wtd.table(x = terminate$education_partner, weights = terminate$weightsdf, normwt = TRUE), 0)
round(wtd.table(x = terminate$education_partner, weights = terminate$weightsdf, normwt = TRUE)*100/
        (sum(wtd.table(x = terminate$education_partner, weights = terminate$weightsdf, normwt = TRUE))),2)


round(wtd.table(x = terminate$occupation_partner, weights = terminate$weightsdf, normwt = TRUE),0)
round(wtd.table(x = terminate$occupation_partner, weights = terminate$weightsdf, normwt = TRUE)*100/
        (sum(wtd.table(x = terminate$occupation_partner, weights = terminate$weightsdf, normwt = TRUE))),2)

round(wtd.table(x = terminate$aliveChildren, weights = terminate$weightsdf, normwt = TRUE),0)
round(wtd.table(x = terminate$aliveChildren, weights = terminate$weightsdf, normwt = TRUE)*100/
        (sum(wtd.table(x = terminate$aliveChildren, weights = terminate$weightsdf, normwt = TRUE))),2)


weighted.median(terminate$cohabAgeC, terminate$weightsdf, na.rm=TRUE)
weighted.75(terminate$cohabAgeC, terminate$weightsdf, na.rm=TRUE) - 
  weighted.25(terminate$cohabAgeC, terminate$weightsdf, na.rm=TRUE)


weighted.median(terminate$currentAge_exact, terminate$weightsdf, na.rm=TRUE)
weighted.75(terminate$currentAge_exact, terminate$weightsdf, na.rm=TRUE) - 
  weighted.25(terminate$currentAge_exact, terminate$weightsdf, na.rm=TRUE)

#wtd.table(x = terminate$cohabAge, weights = terminate$weightsdf, normwt = TRUE)


#################################################
##### Data: Did not Terminate Pregnancies #######
#################################################

didnotterminate <- subset(dfaddress, status == 0)

## Frequency and Percentage
round(wtd.table(x = didnotterminate$residence, weights = didnotterminate$weightsdf, normwt = TRUE), 0)
round(wtd.table(x = didnotterminate$residence, weights = didnotterminate$weightsdf, normwt = TRUE)*100/
        (sum(wtd.table(x = didnotterminate$residence, weights = didnotterminate$weightsdf, normwt = TRUE))),2)

round(wtd.table(x = didnotterminate$occupation, weights = didnotterminate$weightsdf, normwt = TRUE),0)
round(wtd.table(x = didnotterminate$occupation, weights = didnotterminate$weightsdf, normwt = TRUE)*100/
        (sum(wtd.table(x = didnotterminate$occupation, weights = didnotterminate$weightsdf, normwt = TRUE))),2)

round(wtd.table(x = didnotterminate$wealth, weights = didnotterminate$weightsdf, normwt = TRUE),0)
round(wtd.table(x = didnotterminate$wealth, weights = didnotterminate$weightsdf, normwt = TRUE)*100/
        (sum(wtd.table(x = didnotterminate$wealth, weights = didnotterminate$weightsdf, normwt = TRUE))), 2)

round(wtd.table(x = didnotterminate$maritalStatus, weights = didnotterminate$weightsdf, normwt = TRUE),0)
round(wtd.table(x = didnotterminate$maritalStatus, weights = didnotterminate$weightsdf, normwt = TRUE)*100/
        (sum(wtd.table(x = didnotterminate$maritalStatus, weights = didnotterminate$weightsdf, normwt = TRUE))),2)

round(wtd.table(x = didnotterminate$totalChildrenEverBorn, weights = didnotterminate$weightsdf, normwt = TRUE),0)
round(wtd.table(x = didnotterminate$totalChildrenEverBorn, weights = didnotterminate$weightsdf, normwt = TRUE)*100/
        (sum(wtd.table(x = didnotterminate$totalChildrenEverBorn, weights = didnotterminate$weightsdf, normwt = TRUE))),2)

round(wtd.table(x = didnotterminate$education, weights = didnotterminate$weightsdf, normwt = TRUE), 0)
round(wtd.table(x = didnotterminate$education, weights = didnotterminate$weightsdf, normwt = TRUE)*100/
        (sum(wtd.table(x = didnotterminate$education, weights = didnotterminate$weightsdf, normwt = TRUE))),2)

round(wtd.table(x = didnotterminate$religion, weights = didnotterminate$weightsdf, normwt = TRUE), 0)
round(wtd.table(x = didnotterminate$religion, weights = didnotterminate$weightsdf, normwt = TRUE)*100/
        (sum(wtd.table(x = didnotterminate$religion, weights = didnotterminate$weightsdf, normwt = TRUE))),2)

round(wtd.table(x = didnotterminate$education_partner, weights = didnotterminate$weightsdf, normwt = TRUE), 0)
round(wtd.table(x = didnotterminate$education_partner, weights = didnotterminate$weightsdf, normwt = TRUE)*100/
        (sum(wtd.table(x = didnotterminate$education_partner, weights = didnotterminate$weightsdf, normwt = TRUE))),2)

round(wtd.table(x = didnotterminate$occupation_partner, weights = didnotterminate$weightsdf, normwt = TRUE),0)
round(wtd.table(x = didnotterminate$occupation_partner, weights = didnotterminate$weightsdf, normwt = TRUE)*100/
        (sum(wtd.table(x = didnotterminate$occupation_partner, weights = didnotterminate$weightsdf, normwt = TRUE))),2)

round(wtd.table(x = didnotterminate$aliveChildren, weights = didnotterminate$weightsdf, normwt = TRUE),0)
round(wtd.table(x = didnotterminate$aliveChildren, weights = didnotterminate$weightsdf, normwt = TRUE)*100/
        (sum(wtd.table(x = didnotterminate$aliveChildren, weights = didnotterminate$weightsdf, normwt = TRUE))),2)

#wtd.table(x = terminate$cohabAge, weights = terminate$weightsdf, normwt = TRUE)
#wtd.table(x = dfaddress$cohabAge, weights = dfaddress$weightsdf, normwt = TRUE)*100/(sum(wtd.table(x = dfaddress$cohabAge, weights = dfaddress$weightsdf, normwt = TRUE)))

weighted.median(didnotterminate$cohabAgeC, didnotterminate$weightsdf, na.rm=TRUE)
weighted.75(didnotterminate$cohabAgeC, didnotterminate$weightsdf, na.rm=TRUE) - 
  weighted.25(didnotterminate$cohabAgeC, didnotterminate$weightsdf, na.rm=TRUE)


weighted.median(didnotterminate$currentAge_exact, didnotterminate$weightsdf, na.rm=TRUE)
weighted.75(didnotterminate$currentAge_exact, didnotterminate$weightsdf, na.rm=TRUE) - 
  weighted.25(didnotterminate$currentAge_exact, didnotterminate$weightsdf, na.rm=TRUE)
