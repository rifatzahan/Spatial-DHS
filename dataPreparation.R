women <- read.spss("BDIR72FL.SAV",to.data.frame = TRUE)

# head(women)

ID <- women$CASEID
clusterNum <- women$V001
surveyWeight <- women$V005
cmcInterview <- women$V008
DOBCMC <- women$V011
currentAge <- women$V012
currentAge_exact <- cmc_as_age(cmcInterview,DOBCMC,"cmc")$age
currentAgeGroup <- women$V013
division <- women$V024
residence <- women$V025
residenceDefacto <- women$V026

education <- women$V106
education <- relevel(education, ref = "No education")

education_partner <- as.character(women$V701)
education_partner[education_partner == "Don't know"] <- "No education"
education_partner <- as.factor(education_partner)
education_partner <- relevel(education_partner, ref = "No education")

religion <- women$V130
religion <- relevel(religion, ref = "Islam")

wealth <- women$V190
wealth <- relevel(wealth, ref = "Poorest")

totalChildrenEverBorn <- women$V201
totalChildrenEverBorn <- as.character(totalChildrenEverBorn)
totalChildrenEverBorn[totalChildrenEverBorn == "0"] <- "No child"
totalChildrenEverBorn[totalChildrenEverBorn == "1"] <- "One child"
totalChildrenEverBorn[totalChildrenEverBorn == "2"] <- "Two children"
totalChildrenEverBorn[totalChildrenEverBorn == "3"] <- "Three children"
totalChildrenEverBorn[totalChildrenEverBorn == "4"] <- "4 or more"
totalChildrenEverBorn[totalChildrenEverBorn == "5"] <- "4 or more"
totalChildrenEverBorn[totalChildrenEverBorn == "6"] <- "4 or more"
totalChildrenEverBorn[totalChildrenEverBorn == "7"] <- "4 or more"
totalChildrenEverBorn[totalChildrenEverBorn == "8"] <- "4 or more"
totalChildrenEverBorn[totalChildrenEverBorn == "9"] <- "4 or more"
totalChildrenEverBorn[totalChildrenEverBorn == "10"] <- "4 or more"
totalChildrenEverBorn[totalChildrenEverBorn == "11"] <- "4 or more"
totalChildrenEverBorn[totalChildrenEverBorn == "12"] <- "4 or more"
totalChildrenEverBorn[totalChildrenEverBorn == "13"] <- "4 or more"
totalChildrenEverBorn[totalChildrenEverBorn == "14"] <- "4 or more"
totalChildrenEverBorn[totalChildrenEverBorn == "15"] <- "4 or more"
totalChildrenEverBorn <- as.factor(totalChildrenEverBorn)
totalChildrenEverBorn <- relevel(totalChildrenEverBorn,ref = "No child")




ageAtFirstBirth <- women$V212

occupation <- women$V714

occupation_partner <- as.character(women$V705)
occupation_partner[occupation_partner == "Did not work"] <- "Unemployed"
occupation_partner[occupation_partner == "Professional/technical/managerial"] <- "Working"
occupation_partner[occupation_partner == "Clerical"] <- "Working"
occupation_partner[occupation_partner == "Sales"] <- "Working"
occupation_partner[occupation_partner == "Agricultural - self employed"] <- "Working"
occupation_partner[occupation_partner == "Agricultural - employee"] <- "Working"
occupation_partner[occupation_partner == "Household and domestic"] <- "Working"
occupation_partner[occupation_partner == "Services"] <- "Working"
occupation_partner[occupation_partner == "Skilled manual"] <- "Working"
occupation_partner[occupation_partner == "Unskilled manual"] <- "Working"
occupation_partner[occupation_partner == "Don't know"] <- NA
occupation_partner <- as.factor(occupation_partner)
summary(occupation_partner)


aliveChildren <- women$V218


aliveChildren <- as.character(aliveChildren)
aliveChildren[aliveChildren == "0"] <- "No child"
aliveChildren[aliveChildren == "1"] <- "One child"
aliveChildren[aliveChildren == "2"] <- "Two children"
aliveChildren[aliveChildren == "3"] <- "Three children"
aliveChildren[aliveChildren == "4"] <- "4 or more"
aliveChildren[aliveChildren == "5"] <- "4 or more"
aliveChildren[aliveChildren == "6"] <- "4 or more"
aliveChildren[aliveChildren == "7"] <- "4 or more"
aliveChildren[aliveChildren == "8"] <- "4 or more"
aliveChildren[aliveChildren == "9"] <- "4 or more"
aliveChildren[aliveChildren == "10"] <- "4 or more"
aliveChildren[aliveChildren == "11"] <- "4 or more"
aliveChildren[aliveChildren == "12"] <- "4 or more"
aliveChildren <- as.factor(aliveChildren)
aliveChildren <- relevel(aliveChildren, ref = "No child")

terminatedPreg <- women$V228
terminateCMC <- women$V231
terminateCMC <-  as.numeric(levels(terminateCMC)[terminateCMC])
yearEnded <- women$V230
YOB <- women$V010

maritalStatus <- women$V501
maritalStatus <- relevel(maritalStatus, ref = "Married")

weightsdf <- women$V005/1000000

# get the survival status variable
status <-  terminatedPreg # 1: Yes, 0: No
status <- as.numeric(status)
status[status == 1] <- "0"
status[status == 2] <- "1"

terminateAge <- cmc_as_age(terminateCMC,DOBCMC,"cmc")$age

cohabCMC <- women$V509
cohabAge <- cmc_as_age(cohabCMC,DOBCMC,"cmc")$age
# cohabAge[cohabAge <= 14] <- "10-14"
# cohabAge[14 < cohabAge & cohabAge <= 19] <- "15-19"
# cohabAge[19 < cohabAge & cohabAge <= 24] <- "20-24"
# cohabAge[24 < cohabAge & cohabAge <= 29] <- "25-29"
# cohabAge[29 < cohabAge & cohabAge <= 34] <- "30-34"
# cohabAge[34 < cohabAge & cohabAge <= 39] <- "35-39"
# cohabAge[39 < cohabAge & cohabAge <= 44] <- "40-44"
# cohabAge[44 < cohabAge] <- "45-49"
# cohabAge <- as.factor(cohabAge)
# cohabAge <- relevel(cohabAge, ref = "10-14")



df <- cbind.data.frame(ID,
                       clusterNum,
                       surveyWeight,
                       currentAge,
                       currentAgeGroup,
                       division,
                       residence,
                       residenceDefacto,
                       education,
                       religion,
                       wealth,
                       totalChildrenEverBorn,
                       ageAtFirstBirth,
                       cohabAge,
                       maritalStatus,
                       aliveChildren,
                       terminatedPreg,
                       status,
                       terminateAge,
                       currentAge_exact,
                       terminateCMC,
                       DOBCMC,
                       yearEnded,
                       YOB,
                       education_partner,
                       occupation_partner,
                       occupation,
                       weightsdf
)
# terminateAge2 <- as.numeric(levels(df$yearEnded)[df$yearEnded])- df$YOB


df$terminateAge[is.na(df$terminateAge)] <- df$currentAge_exact[is.na(df$terminateAge)]

gps_address <- read.csv("gps_address.csv")


dt1 <- data.table(df, key = "clusterNum") 
dt2 <- data.table(gps_address, key = "DHSCLUST")

dfaddress <- dt1[dt2]
dfaddress <- subset(dfaddress, terminateAge >= 11 ) # doesn't make sense if a woman had abortion 
dfaddress <- subset(dfaddress, lat>0) # 33 observations lost
dfaddress <- subset(dfaddress, select=-c(DHSID, DHSCC, DHSYEAR, CCFIPS, ADM1FIPS, ADM1FIPSNA,ADM1SALBNA, 
                                         ADM1SALBCO, ADM1DHS, DHSREGCO, DHSREGNA,SOURCE, URBAN_RURA, 
                                         ALT_GPS, ALT_DEM, DATUM))

# dfaddress$District = gsub("\\s*\\w*$", "", dfaddress$District)
# dfaddress$SubDistrict = gsub("\\s*\\w*$", "", dfaddress$SubDistrict)

dfaddress$District[dfaddress$SubDistrict == "Baliadangi"] <- "Thakurgaon"
dfaddress$District[dfaddress$SubDistrict == "Mehendiganj"] <- "Barisal"
dfaddress$District[dfaddress$SubDistrict == "Gazaria"] <- "Munshiganj"
dfaddress$District[dfaddress$SubDistrict == "Sirajdikhan"] <- "Munshiganj"
dfaddress$District[dfaddress$Route == "Sekherhat - Jhalokathi Road"] <- "Jhalokati"
dfaddress$ADM1NAME[dfaddress$Route == "Sekherhat - Jhalokathi Road"] <- "Barisal"

dfaddress$SubDistrict[dfaddress$cluster_ID == 541] <- "Nabiganj"
dfaddress$District[dfaddress$cluster_ID == 541] <- "Habiganj"
dfaddress$SubDistrict[dfaddress$cluster_ID == 565] <- "Balaganj"
dfaddress$District[dfaddress$cluster_ID == 565] <- "Sylhet"
dfaddress$SubDistrict[dfaddress$cluster_ID == 77] <- "Kasba"
dfaddress$District[dfaddress$cluster_ID == 77] <- "Brahmanbaria"
dfaddress$District[dfaddress$cluster_ID == 131] <- "Rangamati"
dfaddress$District[dfaddress$cluster_ID == 292] <- "Jessore"
dfaddress$SubDistrict[dfaddress$cluster_ID == 292] <- "Sharsha"
dfaddress$District[dfaddress$cluster_ID == 590] <- "Sylhet"
dfaddress$SubDistrict[dfaddress$cluster_ID == 590] <- "Zakiganj"
dfaddress$SubDistrict[dfaddress$cluster_ID == 360] <- "Dhunat"
dfaddress$District[dfaddress$cluster_ID == 360] <- "Bogra"
dfaddress$SubDistrict[dfaddress$cluster_ID == 370] <- "Joypurhat Sadar"
dfaddress$District[dfaddress$cluster_ID == 370] <- "Joypurhat"
dfaddress$SubDistrict[dfaddress$cluster_ID == 5] <- "Betagi"
dfaddress$District[dfaddress$cluster_ID == 5] <- "Barguna"
dfaddress$SubDistrict[dfaddress$cluster_ID == 79] <- "Nasirnagar"
dfaddress$District[dfaddress$cluster_ID == 79] <- "Brahmanbaria"
dfaddress$District[dfaddress$cluster_ID == 600] <- "Sylhet"
dfaddress$District[dfaddress$District == "Joypurhat"] <- "Jaipurhat"
dfaddress$District[dfaddress$District == "Rangamati Hill"] <- "Rangamati"
dfaddress$District[dfaddress$District == "North 24"] <- "Jessore"
dfaddress$District[dfaddress$clusterNum == "5"] <- "Barguna"
dfaddress$District[dfaddress$clusterNum == "77"] <- "Brahmanbaria"
dfaddress$District[dfaddress$clusterNum == "79"] <- "Brahmanbaria"
dfaddress$District[dfaddress$clusterNum == "360"] <- "Bogra"
dfaddress$District[dfaddress$clusterNum == "370"] <- "Jaipurhat"
dfaddress$District[dfaddress$clusterNum == "541"] <- "Habiganj"
dfaddress$District[dfaddress$clusterNum == "565"] <- "Sylhet"
dfaddress$District[dfaddress$clusterNum == "590"] <- "Sylhet"

dfaddress <- subset(dfaddress, District!="Cooch") # it's in India

#write.csv(dfaddress, "dflogisticwomen.csv")


#dfaddress <- read.csv("dflogisticwomen.csv", header = TRUE)
#dfaddress$status2 <- as.numeric(levels(dfaddress$status))[dfaddress$status]

bang_map <- readOGR(paste("BGD_adm2.shp", sep = ""))
bang_map <- fortify(bang_map, region="NAME_2")

bang_map$id <- as.character(bang_map$id)
bang_map$id[bang_map$id == "Borgona"] <- "Barguna"
bang_map$id[bang_map$id == "Brahmanbaria"] <- "Brahmanbaria"
bang_map$id[bang_map$id == "Choua Danga"] <- "Chuadanga"
bang_map$id[bang_map$id == "Gaibanda"] <- "Gaibandha"
bang_map$id[bang_map$id == "Gopalgonj"] <- "Gopalganj"
bang_map$id[bang_map$id == "Hobiganj"] <- "Habiganj"
bang_map$id[bang_map$id == "Moulvibazar"] <- "Moulvi Bazar"
bang_map$id[bang_map$id == "Manikgonj"] <- "Manikganj"
bang_map$id[bang_map$id == "Munshigonj"] <- "Munshiganj"
bang_map$id[bang_map$id == "Naray Angonj"] <- "Narayanganj"
bang_map$id[bang_map$id == "Narshingdi"] <- "Narsingdi"
bang_map$id[bang_map$id == "Netrakona"] <- "Netrokona"
bang_map$id[bang_map$id == "Rongpur"] <- "Rangpur"
bang_map$id[bang_map$id == "Shatkhira"] <- "Satkhira"
bang_map$id[bang_map$id == "Sirajgonj"] <- "Sirajganj"
bang_map$id[bang_map$id == "Sun Amgonj"] <- "Sunamganj"
bang_map$id[bang_map$id == "Jhalakati"] <- "Jhalokati"
bang_map$id[bang_map$id == "Jbang_ati"] <- "Jhalokati" 
bang_map$id[bang_map$id == "Jbang_mai"] <- "Jhalokati"
bang_map$id[bang_map$id == "Kustia"] <- "Kushtia"
bang_map$id[bang_map$id == "Nawabganj"] <- "Chapainawabganj"
bang_map$id[bang_map$id == "Parbattya Chattagram"] <- "Rangamati"
bang_map$id[bang_map$id == "Nasirabad"] <- "Mymensingh"
bang_map$id[bang_map$id == "Bandarbon"] <- "Bandarban"

o2=order(bang_map$id)
bang_map=bang_map[o2,]
district_centroid <- ddply(bang_map, .(id), summarize,  latc=mean(lat), longc=mean(long))

BD.gen <- readOGR(paste("BGD_ADM2.shp",sep=""))
BD.gen$NAME_2 <- as.character(BD.gen$NAME_2)
BD.gen$NAME_2[BD.gen$NAME_2 == "Borgona"] <- "Barguna"
BD.gen$NAME_2[BD.gen$NAME_2 == "Choua Danga"] <- "Chuadanga"
BD.gen$NAME_2[BD.gen$NAME_2 == "Gaibanda"] <- "Gaibandha"
BD.gen$NAME_2[BD.gen$NAME_2 == "Gopalgonj"] <- "Gopalganj"
BD.gen$NAME_2[BD.gen$NAME_2 == "Hobiganj"] <- "Habiganj"
BD.gen$NAME_2[BD.gen$NAME_2 == "Moulvibazar"] <- "Moulvi Bazar"
BD.gen$NAME_2[BD.gen$NAME_2 == "Manikgonj"] <- "Manikganj"
BD.gen$NAME_2[BD.gen$NAME_2 == "Munshigonj"] <- "Munshiganj"
BD.gen$NAME_2[BD.gen$NAME_2 == "Naray Angonj"] <- "Narayanganj"
BD.gen$NAME_2[BD.gen$NAME_2 == "Narshingdi"] <- "Narsingdi"
BD.gen$NAME_2[BD.gen$NAME_2 == "Netrakona"] <- "Netrokona"
BD.gen$NAME_2[BD.gen$NAME_2 == "Rongpur"] <- "Rangpur"
BD.gen$NAME_2[BD.gen$NAME_2 == "Shatkhira"] <- "Satkhira"
BD.gen$NAME_2[BD.gen$NAME_2 == "Sirajgonj"] <- "Sirajganj"
BD.gen$NAME_2[BD.gen$NAME_2 == "Sun Amgonj"] <- "Sunamganj"
BD.gen$NAME_2[BD.gen$NAME_2 == "Jhalakati"] <- "Jhalokati"
BD.gen$NAME_2[BD.gen$NAME_2 == "Jbang_ati"] <- "Jhalokati"
BD.gen$NAME_2[BD.gen$NAME_2 == "Kustia"] <- "Kushtia"
BD.gen$NAME_2[BD.gen$NAME_2 == "Nawabganj"] <- "Chapainawabganj"
BD.gen$NAME_2[BD.gen$NAME_2 == "Parbattya Chattagram"] <- "Rangamati"
BD.gen$NAME_2[BD.gen$NAME_2 == "Nasirabad"] <- "Mymensingh"
BD.gen$NAME_2[BD.gen$NAME_2 == "Bandarbon"] <- "Bandarban"

o1=order(BD.gen$NAME_2)
BD.gen=BD.gen[o1,]
