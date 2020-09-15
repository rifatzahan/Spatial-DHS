## Logistic regression with no spatial component


###################
### Model : No  ###
###################

## no random effect of age

formula_no <- as.numeric(status) ~ residence + occupation + wealth + maritalStatus + 
  totalChildrenEverBorn + currentAge_exact + cohabAgeC

model_no <- inla(formula_no,data=dfaddress,family="binomial",Ntrials=rep(1,dim(dfaddress)[1]),
                 control.inla = list(numint.maxfeval = 200000), verbose=T, weights = dfaddress$weightsdf,
                 control.predictor = list(compute=TRUE),
                 control.compute=list(waic=TRUE, dic = TRUE, return.marginals=TRUE))

summary(model_no)

# different prior

model_no_1 <- inla(formula_no,data=dfaddress,family="binomial",Ntrials=rep(1,dim(dfaddress)[1]),
                 control.inla = list(numint.maxfeval = 200000), verbose=T, weights = dfaddress$weightsdf,
                 control.predictor = list(compute=TRUE),
                 control.compute=list(waic=TRUE, dic = TRUE, return.marginals=TRUE),
                 control.fixed = list(
                   mean.intercept = 0,
                   prec.intercept=0.000001, 
                   mean = 0, prec = 0.000001 # for betas
                 ))

summary(model_no_1)



## with random effect of age
formula_no_re <- as.numeric(status) ~ residence + occupation + wealth + maritalStatus + totalChildrenEverBorn  +
  f(cohabAgeC, model = "rw1") + f(currentAge_exact, model = "rw1") 

model_no_re <- inla(formula_no_re,data=dfaddress,family="binomial",Ntrials=rep(1,dim(dfaddress)[1]),
                 control.inla = list(numint.maxfeval = 200000), verbose=T, weights = dfaddress$weightsdf,
                 control.predictor = list(compute=TRUE),
                 control.compute=list(waic=TRUE, dic = TRUE, return.marginals=TRUE), 
                 )

summary(model_no_re)


## different prior

model_no_re_1 <- inla(formula_no_re,data=dfaddress,family="binomial",
                    Ntrials=rep(1,dim(dfaddress)[1]),
                    control.inla = list(numint.maxfeval = 200000), verbose=T, 
                    weights = dfaddress$weightsdf,
                    control.predictor = list(compute=TRUE),
                    control.compute=list(waic=TRUE, dic = TRUE, return.marginals=TRUE),
                    control.fixed = list(
                      mean.intercept = 0,
                      prec.intercept=0.000001, 
                      mean = 0, prec = 0.000001 # for betas
                    )
)

summary(model_no_re_1)

## age with RW2
formula_no_re_rw2 <- as.numeric(status) ~ residence + occupation + wealth + maritalStatus + totalChildrenEverBorn  +
  f(cohabAgeC, model = "rw2") + f(currentAge_exact, model = "rw2") 

model_no_rer_w2 <- inla(formula_no_re_rw2,data=dfaddress,family="binomial",Ntrials=rep(1,dim(dfaddress)[1]),
                    control.inla = list(numint.maxfeval = 200000), verbose=T, weights = dfaddress$weightsdf,
                    control.predictor = list(compute=TRUE),
                    control.compute=list(waic=TRUE, dic = TRUE, return.marginals=TRUE))

summary(model_no_re_rw2)

## different prior
model_no_rer_w2_1 <- inla(formula_no_re_rw2,data=dfaddress,family="binomial",
                        Ntrials=rep(1,dim(dfaddress)[1]),
                        control.inla = list(numint.maxfeval = 200000), verbose=T, 
                        weights = dfaddress$weightsdf,
                        control.predictor = list(compute=TRUE),
                        control.compute=list(waic=TRUE, dic = TRUE, return.marginals=TRUE),
                        control.fixed = list(
                          mean.intercept = 0,
                          prec.intercept=0.000001, 
                          mean = 0, prec = 0.000001 # for betas
                        ))

summary(model_no_rer_w2_1)



###################
### Model : IID ###
###################

## no random effect of age

formula_iid <- as.numeric(status) ~ residence + occupation + wealth + maritalStatus + 
  totalChildrenEverBorn  + currentAge_exact + cohabAgeC +
  f(District,model="iid",graph="BD.graph", values = BD.gen$NAME_2,
    adjust.for.con.comp=TRUE)


model_iid <- inla(formula_iid,data=dfaddress,family="binomial",Ntrials=rep(1,dim(dfaddress)[1]),
                  control.inla = list(numint.maxfeval = 200000), verbose=T, weights = dfaddress$weightsdf,
                  control.predictor = list(compute=TRUE),
                  control.compute=list(waic=TRUE, dic = TRUE, return.marginals=TRUE))

summary(model_iid)
# plot(model_iid)

# different prior

formula_iid_1 <- as.numeric(status) ~ residence + occupation + wealth + maritalStatus + 
  totalChildrenEverBorn  + currentAge_exact + cohabAgeC +
  f(District,model="iid",graph="BD.graph", values = BD.gen$NAME_2,
    adjust.for.con.comp=TRUE, prior = 'loggama', param = c(0.0001, 0.0001))


model_iid_1 <- inla(formula_iid_1,data=dfaddress,family="binomial",Ntrials=rep(1,dim(dfaddress)[1]),
                  control.inla = list(numint.maxfeval = 200000), verbose=T, weights = dfaddress$weightsdf,
                  control.predictor = list(compute=TRUE),
                  control.compute=list(waic=TRUE, dic = TRUE, return.marginals=TRUE),
                  control.fixed = list(
                    mean.intercept = 0,
                    prec.intercept=0.000001, 
                    mean = 0, prec = 0.000001 # for betas
                  )
                  )

summary(model_iid_1)

## with random effect of age
formula_iid_re <- as.numeric(status) ~ residence + occupation + wealth + maritalStatus + totalChildrenEverBorn  +  
  f(cohabAgeC, model = "rw1") + f(currentAge_exact, model = "rw1") +
  f(District,model="iid",graph="BD.graph", values = BD.gen$NAME_2,
    adjust.for.con.comp=TRUE)


model_iid_re <- inla(formula_iid_re,data=dfaddress,family="binomial",Ntrials=rep(1,dim(dfaddress)[1]),
                  control.inla = list(numint.maxfeval = 200000), verbose=T, weights = dfaddress$weightsdf,
                  control.predictor = list(compute=TRUE),
                  control.compute=list(waic=TRUE, dic = TRUE, return.marginals=TRUE))

summary(model_iid_re)

# different prior

formula_iid_re_1 <- as.numeric(status) ~ residence + occupation + wealth + maritalStatus + 
  totalChildrenEverBorn  +  
  f(cohabAgeC, model = "rw1") + f(currentAge_exact, model = "rw1") +
  f(District,model="iid",graph="BD.graph", values = BD.gen$NAME_2,
    adjust.for.con.comp=TRUE, prior = 'loggama', param = c(0.0001, 0.0001))

model_iid_re_1 <- inla(formula_iid_re_1,data=dfaddress,family="binomial",
                       Ntrials=rep(1,dim(dfaddress)[1]),
                     control.inla = list(numint.maxfeval = 200000), 
                     verbose=T, weights = dfaddress$weightsdf,
                     control.predictor = list(compute=TRUE),
                     control.compute=list(waic=TRUE, dic = TRUE, return.marginals=TRUE), 
                     control.fixed = list(
                       mean.intercept = 0,
                       prec.intercept=0.000001, 
                       mean = 0, prec = 0.000001 # for betas
                     ))

summary(model_iid_re_1)

## age with RW2

formula_iid_re_rw2 <- as.numeric(status) ~ residence + occupation + wealth + maritalStatus + totalChildrenEverBorn  +  
  f(cohabAgeC, model = "rw2") + f(currentAge_exact, model = "rw2") +
  f(District,model="iid",graph="BD.graph", values = BD.gen$NAME_2,
    adjust.for.con.comp=TRUE)


model_iid_re_rw2 <- inla(formula_iid_re_rw2,data=dfaddress,family="binomial",Ntrials=rep(1,dim(dfaddress)[1]),
                     control.inla = list(numint.maxfeval = 200000), verbose=T, weights = dfaddress$weightsdf,
                     control.predictor = list(compute=TRUE),
                     control.compute=list(waic=TRUE, dic = TRUE, return.marginals=TRUE))

summary(model_iid_re_rw2)



## different prior
formula_iid_re_rw2_1 <- as.numeric(status) ~ residence + occupation + wealth + maritalStatus + totalChildrenEverBorn  +  
  f(cohabAgeC, model = "rw2") + f(currentAge_exact, model = "rw2") +
  f(District,model="iid",graph="BD.graph", values = BD.gen$NAME_2, adjust.for.con.comp=TRUE,
    prior = 'loggama', param = c(0.0001, 0.0001))


model_iid_re_rw2_1 <- inla(formula_iid_re_rw2_1,data=dfaddress,family="binomial",
                           Ntrials=rep(1,dim(dfaddress)[1]),
                         control.inla = list(numint.maxfeval = 200000), 
                         verbose=T, weights = dfaddress$weightsdf,
                         control.predictor = list(compute=TRUE),
                         control.compute=list(waic=TRUE, dic = TRUE, return.marginals=TRUE),
                         control.fixed = list(
                           mean.intercept = 0,
                           prec.intercept=0.000001, 
                           mean = 0, prec = 0.000001 # for betas
                         ))

summary(model_iid_re_rw2_1 )

# mapping

RE_iid <- model_iid$summary.random$District[2]
# plot(density(RE_linear$mean)) # density of district effect means: posterior means
district_means_iid <- as.data.frame(cbind(unique(bang_map$id), RE_iid))
colnames(district_means_iid) <- c("District", "District_Mean")

m1_iid <- ggplot() + 
  geom_map(data = district_means_iid, 
           aes(map_id = District, fill = District_Mean),map = bang_map) + 
  expand_limits(x = bang_map$long, y = bang_map$lat) + 
  scale_fill_gradient2(low = "white",mid = "pink",
                       midpoint = (max(district_means_iid$District_Mean) + 
                                     min(district_means_iid$District_Mean))/2,high = "red",
                       limits=c(min(district_means_iid$District_Mean),
                                max(district_means_iid$District_Mean)), na.value="grey50") +
  geom_text(data = district_centroid, aes(x = longc, y = latc, label = id), 
            check_overlap = TRUE, size = 3)

m1_iid




#####################
### Model : Besag ###
#####################


dfaddress$wealth <- relevel(dfaddress$wealth, ref = "Poorest")
dfaddress$totalChildrenEverBorn <- relevel(dfaddress$totalChildrenEverBorn, ref = "No child")
dfaddress$maritalStatus <- relevel(dfaddress$maritalStatus, ref = "Married")
#dfaddress$cohabAge <- relevel(dfaddress$cohabAge, ref = "10-14")
dfaddress$residence <- relevel(dfaddress$residence, ref = "Rural")

#cohabAge_df <- subset(dfaddress, select = c(cohabAge))
#write.csv(cohabAge_df, "cohabAgedf.csv")
cohabAge_df <- read.csv("cohabAgedf.csv")
cohabAgeC <- cohabAge_df$cohabAgeC
dfaddress$cohabAgeC <- cohabAgeC


# no random effect of age
formula_besag <- as.numeric(status) ~ residence + occupation + wealth + maritalStatus + totalChildrenEverBorn +
  + currentAge_exact + cohabAgeC +
  f(District,model="besag",graph="BD.graph", values = BD.gen$NAME_2,adjust.for.con.comp=TRUE,
    scale.model = FALSE)


model_besag <- inla(formula_besag,data=dfaddress,family="binomial",Ntrials=rep(1,dim(dfaddress)[1]),
  control.inla = list(numint.maxfeval = 100000), verbose=T, weights = dfaddress$weightsdf,
  control.predictor = list(compute=TRUE),
  control.compute=list(waic=TRUE, dic = TRUE, return.marginals=TRUE))

summary(model_besag)


# different priors

formula_besag_1 <- as.numeric(status) ~ residence + occupation + wealth + maritalStatus + totalChildrenEverBorn +
  + currentAge_exact + cohabAgeC +
  f(District,model="besag",graph="BD.graph", values = BD.gen$NAME_2,adjust.for.con.comp=TRUE,
    scale.model = FALSE, prior = 'loggama', param = c(0.0001, 0.0001))


model_besag_1 <- inla(formula_besag_1,data=dfaddress,family="binomial",Ntrials=rep(1,dim(dfaddress)[1]),
                    control.inla = list(numint.maxfeval = 100000), 
                    verbose=T, weights = dfaddress$weightsdf,
                    control.predictor = list(compute=TRUE),
                    control.compute=list(waic=TRUE, dic = TRUE, return.marginals=TRUE), 
                    control.fixed = list(
                      mean.intercept = 0,
                      prec.intercept=0.000001, 
                      mean = 0, prec = 0.000001 # for betas
                    ))

summary(model_besag_1)



# RW1 effect of age
formula_besag_re <- as.numeric(status) ~ residence + occupation + wealth + maritalStatus + totalChildrenEverBorn +
  f(cohabAgeC, model = "rw1") + f(currentAge_exact, model = "rw1") +
  f(District,model="besag",graph="BD.graph", values = BD.gen$NAME_2,adjust.for.con.comp=TRUE, 
    scale.model = FALSE)



model_besag_re <- inla(formula_besag_re,data=dfaddress,family="binomial",Ntrials=rep(1,dim(dfaddress)[1]),
                    control.inla = list(numint.maxfeval = 100000), verbose=T, weights = dfaddress$weightsdf,
                    control.predictor = list(compute=TRUE),
                    control.compute=list(waic=TRUE, dic = TRUE, return.marginals=TRUE))

summary(model_besag_re)

# different prior

formula_besag_re_1 <- as.numeric(status) ~ residence + occupation + wealth + maritalStatus + totalChildrenEverBorn +
  f(cohabAgeC, model = "rw1") + f(currentAge_exact, model = "rw1") +
  f(District,model="besag",graph="BD.graph", values = BD.gen$NAME_2,adjust.for.con.comp=TRUE, 
    scale.model = FALSE, prior = 'loggama', param = c(0.0001, 0.0001))



model_besag_re_1 <- inla(formula_besag_re_1,data=dfaddress,family="binomial",Ntrials=rep(1,dim(dfaddress)[1]),
                       control.inla = list(numint.maxfeval = 100000), 
                       verbose=T, weights = dfaddress$weightsdf,
                       control.predictor = list(compute=TRUE),
                       control.compute=list(waic=TRUE, dic = TRUE, return.marginals=TRUE), 
                       control.fixed = list(
                         mean.intercept = 0,
                         prec.intercept=0.000001, 
                         mean = 0, prec = 0.000001 # for betas
                       ))

summary(model_besag_re_1)

# RW2 effect of age
formula_besag_re_rw2 <- as.numeric(status) ~ residence + occupation + wealth + maritalStatus + totalChildrenEverBorn +
  f(cohabAgeC, model = "rw2") + + f(currentAge_exact, model = "rw2") + 
  f(District,model="besag",
    graph="BD.graph",values = BD.gen$NAME_2,adjust.for.con.comp=TRUE, 
    scale.model = FALSE)


model_besag_re_rw2 <- inla(formula_besag_re_rw2,data=dfaddress,family="binomial",Ntrials=rep(1,dim(dfaddress)[1]),
                       control.inla = list(numint.maxfeval = 100000), verbose=T, weights = dfaddress$weightsdf,
                       control.predictor = list(compute=TRUE),
                       control.compute=list(waic=TRUE, dic = TRUE, return.marginals=TRUE))

summary(model_besag_re_rw2)

# different prior

formula_besag_re_rw2_1 <- as.numeric(status) ~ residence + occupation + wealth + maritalStatus + totalChildrenEverBorn +
  f(cohabAgeC, model = "rw2") + + f(currentAge_exact, model = "rw2") + 
  f(District,model="besag",
    graph="BD.graph",values = BD.gen$NAME_2,adjust.for.con.comp=TRUE, 
    scale.model = FALSE, prior = 'loggama', param = c(0.0001, 0.0001))


model_besag_re_rw2_1 <- inla(formula_besag_re_rw2_1,data=dfaddress,family="binomial",Ntrials=rep(1,dim(dfaddress)[1]),
                           control.inla = list(numint.maxfeval = 100000), verbose=T, weights = dfaddress$weightsdf,
                           control.predictor = list(compute=TRUE),
                           control.compute=list(waic=TRUE, dic = TRUE, return.marginals=TRUE), 
                           control.fixed = list(
                             mean.intercept = 0,
                             prec.intercept= 0.000001, 
                             mean = 0, prec = 0.000001 # for betas
                           ))

summary(model_besag_re_rw2_1)

age <- unique(dfaddress$cohabAgeC)
agelabel <- c("10", "15", "20", "25", "30", "35", "40", "49")

post.mean.age <- model_besag$summary.random$cohabAgeC[, "mean"]
post.li.age <- model_besag$summary.random$cohabAgeC[, "0.025quant"]
post.ui.age <- model_besag$summary.random$cohabAgeC[, "0.975quant"]

par(las=1)  ## cosmetic: horizontal y-axis labels are nicer
plotCI(1:length(age),post.mean.age,ui=post.ui.age,li=post.li.age,col="blue",scol="black",
       axes=FALSE,   ## disable axes (including tick labels)
       xlab="",      ## suppress x-axis label
       ylim=c(-2,2)   ## specify y-axis limits
)
axis(side=2)         ## add default y-axis (ticks+labels)
axis(side=1,at=c(1, 61, 121, 181, 236, 263, 269, 272),  ## add custom x-axis
     label=agelabel)
box(bty="l")         ## add box
abline(h=0, lty=2, col="red")


memory.limit()


########################
### RW2 for CohabAge ###
########################

formula_besag2 <- as.numeric(status) ~ residence + occupation + wealth + maritalStatus + totalChildrenEverBorn +
  + currentAgeGroup + f(cohabAgeC, model = "rw2") + 
  f(District,model="besag",graph="BD.graph", values = BD.gen$NAME_2,adjust.for.con.comp=TRUE, scale.model = FALSE)


model_besag2 <- inla(formula_besag2,data=dfaddress,family="binomial",Ntrials=rep(1,dim(dfaddress)[1]),
                    control.inla = list(numint.maxfeval = 100000), verbose=T, weights = dfaddress$weightsdf,
                    control.predictor = list(compute=TRUE),
                    control.compute=list(waic=TRUE, dic = TRUE, return.marginals=TRUE))

summary(model_besag2)

# different prior

formula_besag2_1 <- as.numeric(status) ~ residence + occupation + wealth + maritalStatus + totalChildrenEverBorn +
  + currentAgeGroup + f(cohabAgeC, model = "rw2") + 
  f(District,model="besag",
    graph="BD.graph", values = BD.gen$NAME_2,adjust.for.con.comp=TRUE, 
    scale.model = FALSE, prior = 'loggama', param = c(0.0001, 0.0001))


model_besag2_1 <- inla(formula_besag2_1,data=dfaddress,family="binomial",Ntrials=rep(1,dim(dfaddress)[1]),
                     control.inla = list(numint.maxfeval = 100000), verbose=T, weights = dfaddress$weightsdf,
                     control.predictor = list(compute=TRUE),
                     control.compute=list(waic=TRUE, dic = TRUE, return.marginals=TRUE), 
                     control.fixed = list(
                       mean.intercept = 0,
                       prec.intercept= 0.000001, 
                       mean = 0, prec = 0.000001 # for betas
                     ))

summary(model_besag2_1)

# RW2 decreases WAIC by 0.87 from RW1 

RE_besag <- model_besag2$summary.random$District[2]
# plot(density(RE_linear$mean)) # density of district effect means: posterior means
district_means_besag <- as.data.frame(cbind(unique(bang_map$id), RE_besag))
colnames(district_means_besag) <- c("District", "District_Mean")

m1_besag <- ggplot() + 
  geom_map(data = district_means_besag, 
    aes(map_id = District, fill = District_Mean),map = bang_map) + 
  expand_limits(x = bang_map$long, y = bang_map$lat) + 
  scale_fill_gradient2(low = "white",mid = "pink",
                       midpoint = (max(district_means_besag$District_Mean) + 
                        min(district_means_besag$District_Mean))/2,high = "red",
                       limits=c(min(district_means_besag$District_Mean),
                                max(district_means_besag$District_Mean)), na.value="grey50") +
  geom_text(data = district_centroid, aes(x = longc, y = latc, label = id), check_overlap = TRUE, size = 3)

m1_besag


post.mean.age <- model_besag2$summary.random$cohabAgeC[, "mean"]
post.li.age <- model_besag2$summary.random$cohabAgeC[, "0.025quant"]
post.ui.age <- model_besag2$summary.random$cohabAgeC[, "0.975quant"]

par(las=1)  ## cosmetic: horizontal y-axis labels are nicer
plotCI(1:length(age),post.mean.age,ui=post.ui.age,li=post.li.age,col="blue",scol="black",
       axes=FALSE,   ## disable axes (including tick labels)
       xlab="",      ## suppress x-axis label
       ylim=c(-2,2)   ## specify y-axis limits
)
axis(side=2)         ## add default y-axis (ticks+labels)
axis(side=1,at=c(1, 61, 121, 181, 236, 263, 269, 272),  ## add custom x-axis
     label=agelabel)
box(bty="l")         ## add box
abline(h=0, lty=2, col="red")


###################
### Model : BYM ###
###################

## no random effect of age
formula_bym <- as.numeric(status) ~ residence + occupation + wealth + maritalStatus + 
  totalChildrenEverBorn  + currentAge_exact + cohabAgeC + 
  f(District,model="bym",graph="BD.graph", values = BD.gen$NAME_2,
    adjust.for.con.comp=TRUE, scale.model = TRUE)


model_bym <- inla(formula_bym,data=dfaddress,family="binomial",Ntrials=rep(1,dim(dfaddress)[1]),
                  control.inla = list(numint.maxfeval = 100000), verbose=T, weights = dfaddress$weightsdf,
                  control.predictor = list(compute=TRUE),
                  control.compute=list(waic=TRUE, dic = TRUE, return.marginals=TRUE))

summary(model_bym)

# different prior
formula_bym_1 <- as.numeric(status) ~ residence + occupation + wealth + maritalStatus + 
  totalChildrenEverBorn  + currentAge_exact + cohabAgeC + 
  f(District,model="bym",graph="BD.graph", values = BD.gen$NAME_2,
    adjust.for.con.comp=TRUE, scale.model = TRUE, 
    prior = 'loggama', param = c(0.0001, 0.0001))


model_bym_1 <- inla(formula_bym_1,data=dfaddress,family="binomial",Ntrials=rep(1,dim(dfaddress)[1]),
                  control.inla = list(numint.maxfeval = 100000), verbose=T, weights = dfaddress$weightsdf,
                  control.predictor = list(compute=TRUE),
                  control.compute=list(waic=TRUE, dic = TRUE, return.marginals=TRUE), 
                  control.fixed = list(
                    mean.intercept = 0,
                    prec.intercept= 0.000001, 
                    mean = 0, prec = 0.000001 # for betas
                  ))

summary(model_bym_1)

## with random effect of age
formula_bym_re <- as.numeric(status) ~ residence + occupation + wealth + maritalStatus + 
  totalChildrenEverBorn +
  f(cohabAgeC, model = "rw1") + f(currentAge_exact, model = "rw1")+
  f(District,model="bym",graph="BD.graph", values = BD.gen$NAME_2,
    adjust.for.con.comp=TRUE, scale.model = FALSE)


model_bym_re <- inla(formula_bym_re,data=dfaddress,family="binomial",Ntrials=rep(1,dim(dfaddress)[1]),
                    control.inla = list(numint.maxfeval = 100000), verbose=T, weights = dfaddress$weightsdf,
                    control.predictor = list(compute=TRUE),
                    control.compute=list(waic=TRUE, dic = TRUE, return.marginals=TRUE))

summary(model_bym_re)


# different prior

formula_bym_re_1 <- as.numeric(status) ~ residence + occupation + wealth + maritalStatus + 
  totalChildrenEverBorn +
  f(cohabAgeC, model = "rw1") + f(currentAge_exact, model = "rw1")+
  f(District,model="bym",graph="BD.graph", values = BD.gen$NAME_2,
    adjust.for.con.comp=TRUE, scale.model = FALSE,
    prior = 'loggama', param = c(0.0001, 0.0001))


model_bym_re_1 <- inla(formula_bym_re_1,data=dfaddress,family="binomial",Ntrials=rep(1,dim(dfaddress)[1]),
                     control.inla = list(numint.maxfeval = 100000), verbose=T, weights = dfaddress$weightsdf,
                     control.predictor = list(compute=TRUE),
                     control.compute=list(waic=TRUE, dic = TRUE, return.marginals=TRUE), 
                     control.fixed = list(
                       mean.intercept = 0,
                       prec.intercept= 0.000001, 
                       mean = 0, prec = 0.000001 # for betas
                     ))

summary(model_bym_re_1)


## age with RW2

formula_bym_re_rw2 <- as.numeric(status) ~ residence + occupation + wealth + maritalStatus + 
  totalChildrenEverBorn +
  f(cohabAgeC, model = "rw2") + f(currentAge_exact, model = "rw2")+
  f(District,model="bym",graph="BD.graph", values = BD.gen$NAME_2,
    adjust.for.con.comp=TRUE, scale.model = FALSE)


model_bym_re_rw2 <- inla(formula_bym_re_rw2,data=dfaddress,family="binomial",Ntrials=rep(1,dim(dfaddress)[1]),
                     control.inla = list(numint.maxfeval = 100000), verbose=T, weights = dfaddress$weightsdf,
                     control.predictor = list(compute=TRUE),
                     control.compute=list(waic=TRUE, dic = TRUE, return.marginals=TRUE))

summary(model_bym_re_rw2)

# different prior

formula_bym_re_rw2_1 <- as.numeric(status) ~ residence + occupation + wealth + maritalStatus + 
  totalChildrenEverBorn +
  f(cohabAgeC, model = "rw2") + f(currentAge_exact, model = "rw2")+
  f(District,model="bym",graph="BD.graph", values = BD.gen$NAME_2,
    adjust.for.con.comp=TRUE, scale.model = FALSE)


model_bym_re_rw2_1 <- inla(formula_bym_re_rw2_1,data=dfaddress,family="binomial",Ntrials=rep(1,dim(dfaddress)[1]),
                         control.inla = list(numint.maxfeval = 100000), verbose=T, weights = dfaddress$weightsdf,
                         control.predictor = list(compute=TRUE),
                         control.compute=list(waic=TRUE, dic = TRUE, return.marginals=TRUE))

summary(model_bym_re_rw2_1)


