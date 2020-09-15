
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



RE_bym <- model_bym$summary.random$District[2]
district_means_bym <- as.data.frame(cbind(unique(bang_map$id), RE_bym))
colnames(district_means_bym) <- c("District", "District_Mean")

m1_bym <- ggplot() + 
  geom_map(data = district_means_bym, 
           aes(map_id = District, fill = District_Mean),map = bang_map) + 
  expand_limits(x = bang_map$long, y = bang_map$lat) + 
  scale_fill_gradient2(low = "white",mid = "pink",
                       midpoint = (max(district_means_bym$District_Mean) + 
                                     min(district_means_bym$District_Mean))/2,high = "red",
                       limits=c(min(district_means_bym$District_Mean),
                                max(district_means_bym$District_Mean)), na.value="grey50") +
  geom_text(data = district_centroid, aes(x = longc, y = latc, label = id), check_overlap = TRUE, size = 3)

m1_bym




RE_bym_re <- model_bym_re$summary.random$District[2]
district_means_bym_re <- as.data.frame(cbind(unique(bang_map$id), RE_bym_re))
colnames(district_means_bym_re) <- c("District", "District_Mean")

m1_bym_re <- ggplot() + 
  geom_map(data = district_means_bym_re, 
           aes(map_id = District, fill = District_Mean),map = bang_map) + 
  expand_limits(x = bang_map$long, y = bang_map$lat) + 
  scale_fill_gradient2(low = "white",mid = "pink",
                       midpoint = (max(district_means_bym_re$District_Mean) + 
                                     min(district_means_bym_re$District_Mean))/2,high = "red",
                       limits=c(min(district_means_bym_re$District_Mean),
                                max(district_means_bym_re$District_Mean)), na.value="grey50") +
  geom_text(data = district_centroid, aes(x = longc, y = latc, label = id), check_overlap = TRUE, size = 3)

m1_bym_re




# For cohabitation age
age <- unique(dfaddress$cohabAgeC)
agelabel <- c("10", "15", "20", "25", "30", "35", "45", "40", "49")


post.mean.age <- model_besag_re$summary.random$cohabAgeC[, "mean"]
post.li.age <- model_besag_re$summary.random$cohabAgeC[, "0.025quant"]
post.ui.age <- model_besag_re$summary.random$cohabAgeC[, "0.975quant"]

par(las=1)  ## cosmetic: horizontal y-axis labels are nicer
plotCI(1:length(age),post.mean.age,ui=post.ui.age,li=post.li.age,col="blue",scol="black",
       axes=FALSE,   ## disable axes (including tick labels)
       xlab="First Cohabitation Age",      ## suppress x-axis label
       ylab = "Partial effect of cohabitation age",
       ylim=c(-1,1),   ## specify y-axis limits
       cex=1.5,
       cex.lab = 1.5,
       cex.axis = 1.5
)
axis(side=2)         ## add default y-axis (ticks+labels)
axis(side=1,at=c(1, 61, 121, 181, 241, 263, 269, 271, 272),  ## add custom x-axis
     label=agelabel)
box(bty="l")         ## add box
abline(h=0, lty=4, col="red")


# Current Age
age <- unique(dfaddress$currentAge_exact)
agelabel <- c("15", "20", "25", "30", "35", "40", "45", "49")


post.mean.age <- model_besag_re$summary.random$currentAge_exact[, "mean"]
post.li.age <- model_besag_re$summary.random$currentAge_exact[, "0.025quant"]
post.ui.age <- model_besag_re$summary.random$currentAge_exact[, "0.975quant"]

par(las=1)  ## cosmetic: horizontal y-axis labels are nicer
plotCI(1:length(age),post.mean.age,ui=post.ui.age,li=post.li.age,col="blue",scol="black",
       axes=FALSE,   ## disable axes (including tick labels)
       xlab="Age of the women at survey time",      ## suppress x-axis label
       ylab = "Partial effect of age at the survey time",
       ylim=c(-2,2),   ## specify y-axis limits
       cex=1.5,
       cex.lab = 1.5,
       cex.axis = 1.5
)
axis(side=2)         ## add default y-axis (ticks+labels)
axis(side=1,at=c(1, 61, 121, 181, 241, 301, 361, 409),  ## add custom x-axis
     label=agelabel)
box(bty="l")         ## add box
abline(h=0, lty=4, col="red")




RE_besag <- model_besag_re$summary.random$District[2]
# plot(density(RE_linear$mean)) # density of district effect means: posterior means
district_means_besag_re <- as.data.frame(cbind(unique(bang_map$id), RE_besag))
colnames(district_means_besag_re) <- c("District", "District_Mean")

m1_besag_re <- ggplot() + 
  geom_map(data = district_means_besag_re, 
           aes(map_id = District, fill = District_Mean),map = bang_map) + 
  expand_limits(x = bang_map$long, y = bang_map$lat) + 
  scale_fill_gradient2(low = "royalblue3",mid = "snow",
                       midpoint = 0,high = "violetred3",
                       limits=c(min(district_means_besag_re$District_Mean),
                                max(district_means_besag_re$District_Mean)), na.value="grey50") +
  geom_text(data = district_centroid, aes(x = longc, y = latc, label = id), 
            check_overlap = TRUE, size = 4)

m1_besag_re




iid_re <- model_iid_re$summary.random$District[2]
# plot(density(RE_linear$mean)) # density of district effect means: posterior means
district_means_iid_re <- as.data.frame(cbind(unique(bang_map$id), iid_re))
colnames(district_means_iid_re) <- c("District", "District_Mean")

m1_iid_re <- ggplot() + 
  geom_map(data = district_means_iid_re, 
           aes(map_id = District, fill = District_Mean),map = bang_map) + 
  expand_limits(x = bang_map$long, y = bang_map$lat) + 
  scale_fill_gradient2(low = "royalblue3",mid = "snow",
                       midpoint = 0, high = "violetred3",
                       limits=c(min(district_means_iid_re$District_Mean),
                                max(district_means_iid_re$District_Mean)), na.value="grey50") +
  geom_text(data = district_centroid, aes(x = longc, y = latc, label = id), 
            check_overlap = TRUE, size = 4)

m1_iid_re


linearmeanno <- model_no$summary.linear.predictor$mean
linearmeaniid <- model_iid$summary.linear.predictor$mean
linearmeanbesag2 <- model_besag2$summary.linear.predictor$mean
linearmeanbym <- model_bym$summary.linear.predictor$mean

ypostmeanno <-exp(linearmeanno)/(1+exp(linearmeanno))
ypostmeaniid <-exp(linearmeaniid)/(1+exp(linearmeaniid))
ypostmeanbesag2 <-exp(linearmeanbesag2)/(1+exp(linearmeanbesag2))
ypostmeanbym <-exp(linearmeanbym)/(1+exp(linearmeanbym))


MSEno <- mean((dfaddress$status - ypostmeanno)^2)
MSEiid <- mean((dfaddress$status - ypostmeaniid)^2)
MSEbesag2 <- mean((dfaddress$status - ypostmeanbesag2)^2)
MSEbym <- mean((dfaddress$status - ypostmeanbym)^2)

MSEno
MSEiid
MSEbesag2
MSEbym


WAIC <- c(
  model_no$waic$waic,
  model_iid$waic$waic,
  model_besag$waic$waic,
  model_bym$waic$waic,
  model_no_re$waic$waic, # lowest among no RE 
  model_iid_re$waic$waic, # low
  model_besag_re$waic$waic, # lowest
  model_bym_re$waic$waic, # low
  model_no_rer_w2$waic$waic,
  model_iid_re_rw2$waic$waic,
  model_besag_re_rw2$waic$waic, # low
  model_bym_re_rw2$waic$waic)

WAIC
min(WAIC)

DIC <- c(
  model_no$dic$dic,
  model_iid$dic$dic,
  model_besag$dic$dic,
  model_bym$dic$dic,
  model_no_re$dic$dic,
  model_iid_re$dic$dic,
  model_besag_re$dic$dic,
  model_bym_re$dic$dic,
  model_no_rer_w2$dic$dic,
  model_iid_re_rw2$dic$dic,
  model_besag_re_rw2$dic$dic,
  model_bym_re_rw2$dic$dic)

DIC

summary(model_no)
summary(model_iid)
summary(model_besag)
summary(model_bym)
summary(model_no_re)
summary(model_iid_re)
summary(model_besag_re)
summary(model_bym_re)
summary(model_no_rer_w2)
summary(model_iid_re_rw2)
summary(model_besag_re_rw2)
summary(model_bym_re_rw2)

pD <- c(
  model_no$dic$dic,
  model_iid$dic$dic,
  model_besag$dic$dic,
  model_bym$dic$dic,
  model_no_re$dic$dic,
  model_iid_re$dic$dic,
  model_besag_re$dic$dic,
  model_bym_re$dic$dic,
  model_no_rer_w2$dic$dic,
  model_iid_re_rw2$dic$dic,
  model_besag_re_rw2$dic$dic,
  model_bym_re_rw2$dic$dic)

DIC



m1_besag
m1_iid


pdf(file="m1_besag.pdf")
m1_besag
dev.off()



pdf(file="m1_iid.pdf")
m1_iid
dev.off()


summary(model_no_re)
summary(model_iid_re)
summary(model_bym_re)


odds_ratio_no_re <- as.data.frame(round(exp(cbind(OR = as.data.frame(model_no_re$summary.fixed)[1], 
                           LB = as.data.frame(model_no_re$summary.fixed)[3], 
                           UB = as.data.frame(model_no_re$summary.fixed)[5])),2))

odds_ratio_iid_re <- as.data.frame(round(exp(cbind(OR = as.data.frame(model_iid_re$summary.fixed)[1], 
                               LB = as.data.frame(model_iid_re$summary.fixed)[3], 
                               UB = as.data.frame(model_iid_re$summary.fixed)[5])), 2))

odds_ratio_bym_re <- as.data.frame(round(exp(cbind(OR = as.data.frame(model_bym_re$summary.fixed)[1], 
                                                  LB = as.data.frame(model_bym_re$summary.fixed)[3], 
                                                  UB = as.data.frame(model_bym_re$summary.fixed)[5])), 2))

odds_ratio_besag_no <- as.data.frame(round(exp(cbind(OR = as.data.frame(model_besag$summary.fixed)[1], 
                                                     LB = as.data.frame(model_besag$summary.fixed)[3], 
                                                     UB = as.data.frame(model_besag$summary.fixed)[5])), 2))
odds_ratio_besag_re <- as.data.frame(round(exp(cbind(OR = as.data.frame(model_besag_re$summary.fixed)[1], 
                                                     LB = as.data.frame(model_besag_re$summary.fixed)[3], 
                                                     UB = as.data.frame(model_besag_re$summary.fixed)[5])), 2))
odds_ratio_besag_re_rw2 <- as.data.frame(round(exp(cbind(
  OR = as.data.frame(model_besag_re_rw2$summary.fixed)[1],
  LB = as.data.frame(model_besag_re_rw2$summary.fixed)[3],
  UB = as.data.frame(model_besag_re_rw2$summary.fixed)[5])), 2))

summary(model_besag_re_rw2)

library(xtable)
xtable(odds_ratio_besag_re_rw2)

df_odds_no_re <- cbind(odds_ratio_no_re[1], no=do.call(paste, c(odds_ratio_no_re[-1], sep=",")))
df_odds_iid_re <- cbind(odds_ratio_iid_re[1], no=do.call(paste, c(odds_ratio_iid_re[-1], sep=",")))
df_odds_besag_re <- cbind(odds_ratio_besag_re[1], no=do.call(paste, c(odds_ratio_besag_re[-1], sep=",")))
df_odds_bym_re <- cbind(odds_ratio_bym_re[1], no=do.call(paste, c(odds_ratio_bym_re[-1], sep=",")))


## ROC, AUC ##

predictedprob <- model_besag_re$summary.fitted.values$mean

pred <-prediction(predictedprob, as.numeric(dfaddress$status))
perf <- performance(pred,"tpr","fpr")
plot(perf)


plot(perf, lwd=2, colorize=TRUE)
lines(x=c(0, 1), y=c(0, 1), col="black", lwd=1)

auc = performance(pred, "auc")
auc = unlist(auc@y.values)
auc

## District-level Average of Predicted Probability of mean

## CAR
predictedprob <- model_besag_re$summary.random$District$mean
dist_name <- model_besag_re$summary.random$District$ID
district_means_besag_re_pp <- data.frame(dist_name, predictedprob)
str(district_means_besag_re_pp)
colnames(district_means_besag_re_pp) <- c("District", "Mean")


m_post_mean_CAR <- ggplot() + 
  geom_map(data = district_means_besag_re_pp, 
           aes(map_id = District, fill = Mean),map = bang_map) + 
  expand_limits(x = bang_map$long, y = bang_map$lat) + 
  scale_fill_gradient2(low = "royalblue4",mid = "snow",
                       midpoint = (max(district_means_besag_re_pp$Mean) + 
                                     min(district_means_besag_re_pp$Mean))/2,high = "violetred3",
                       limits=c(min(district_means_besag_re_pp$Mean),
                                max(district_means_besag_re_pp$Mean)), na.value="grey50") +
  geom_text(data = district_centroid, aes(x = longc, y = latc, label = id), 
            check_overlap = TRUE, size = 3)

m_post_mean_CAR

## IID

predictedprob <- model_iid_re$summary.random$District$mean
dist_name <- model_iid_re$summary.random$District$ID
district_means_iid_re_pp <- data.frame(dist_name, predictedprob)
str(district_means_iid_re_pp)
colnames(district_means_iid_re_pp) <- c("District", "Mean")


m_post_mean_IID <- ggplot() + 
  geom_map(data = district_means_iid_re_pp, 
           aes(map_id = District, fill = Mean),map = bang_map) + 
  expand_limits(x = bang_map$long, y = bang_map$lat) + 
  scale_fill_gradient2(low = "royalblue4",mid = "snow",
                       midpoint = (max(district_means_iid_re_pp$Mean) + 
                                     min(district_means_iid_re_pp$Mean))/2,high = "violetred3",
                       limits=c(min(district_means_iid_re_pp$Mean),
                                max(district_means_iid_re_pp$Mean)), na.value="grey50") +
  geom_text(data = district_centroid, aes(x = longc, y = latc, label = id), 
            check_overlap = TRUE, size = 3)

m_post_mean_IID





## District-level Average of Predicted Probability of standard deviation

## CAR
predictedprob <- model_besag_re$summary.random$District$sd
dist_name <- model_besag_re$summary.random$District$ID
district_means_besag_re_pp <- data.frame(dist_name, predictedprob)
str(district_means_besag_re_pp)
colnames(district_means_besag_re_pp) <- c("District", "SD")


m_post_sd_CAR <- ggplot() + 
  geom_map(data = district_means_besag_re_pp, 
           aes(map_id = District, fill = SD),map = bang_map) + 
  expand_limits(x = bang_map$long, y = bang_map$lat) + 
  scale_fill_gradient2(low = "royalblue4",mid = "snow",
                       midpoint = (max(district_means_besag_re_pp$SD) + 
                                     min(district_means_besag_re_pp$SD))/2,high = "violetred3",
                       limits=c(min(district_means_besag_re_pp$SD),
                                max(district_means_besag_re_pp$SD)), na.value="grey50") +
  geom_text(data = district_centroid, aes(x = longc, y = latc, label = id), 
            check_overlap = TRUE, size = 3)

m_post_sd_CAR

## IID

predictedprob <- model_iid_re$summary.random$District$sd
dist_name <- model_iid_re$summary.random$District$ID
district_means_iid_re_pp <- data.frame(dist_name, predictedprob)
str(district_means_iid_re_pp)
colnames(district_means_iid_re_pp) <- c("District", "SD")


m_post_sd_IID <- ggplot() + 
  geom_map(data = district_means_iid_re_pp, 
           aes(map_id = District, fill = SD),map = bang_map) + 
  expand_limits(x = bang_map$long, y = bang_map$lat) + 
  scale_fill_gradient2(low = "royalblue4",mid = "snow",
                       midpoint = (max(district_means_iid_re_pp$SD) + 
                                     min(district_means_iid_re_pp$SD))/2,high = "violetred3",
                       limits=c(min(district_means_iid_re_pp$SD),
                                max(district_means_iid_re_pp$SD)), na.value="grey50") +
  geom_text(data = district_centroid, aes(x = longc, y = latc, label = id), 
            check_overlap = TRUE, size = 3)

m_post_sd_IID


## correlation coefficient between current_age, cohab_age

cor(dfaddress$currentAge_exact, dfaddress$cohabAgeC, method = c("spearman"))

# Pearson's cor cef: -0.0400129
# kendal: -0.05596674
# spearman: -0.08926271



#### OR Plot ####

library(tidyverse)
library(ggplot2)
library(broom)

tdf = as.data.frame(Titanic)
m1 = glm(Survived == "Yes" ~ Class + Sex, data = tdf, family = "binomial", weights = Freq)
m1_preds = tidy(m1, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(Model = "m1")
tdf$FreqScrambled = sample(tdf$Freq)
m2 = glm(Survived == "Yes" ~ Class + Sex, data = tdf, 
         family = "binomial", weights = FreqScrambled)
m2_preds = tidy(m2, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(Model = "m2")
ors = bind_rows(m1_preds, m2_preds)
ors

dodger = position_dodge(width = 0.3)


odds_ratio_besag$Model <- rep("Linear", dim(odds_ratio_besag)[1])
odds_ratio_besag_re$Model <- rep("RW1", dim(odds_ratio_besag_re)[1])

odds_ratio_besag$Effect <- row.names(odds_ratio_besag)
odds_ratio_besag_re$Effect <- row.names(odds_ratio_besag_re)

# remove last row
# odds_ratio_besag <- odds_ratio_besag[-nrow(odds_ratio_besag),] 


#oddsratio_df <- rbind(odds_ratio_besag, odds_ratio_besag_re)
# write.csv(oddsratio_df, "oddsratio_df.csv")
oddsratio_df <- read.csv("oddsratio_df.csv")
oddsratio_df$Effect <- factor(oddsratio_df$Effect,
                              levels = c("totalChildrenBorn#4 or more", "totalChildrenBorn#3",
                                         "totalChildrenBorn#2", "totalChildrenBorn#1",
                                         "maritalStatus_Widowed", "maritalStatus_Divorced", 
                                         "maritalStatus_Separated", 
                                         "wealth_Richest", "wealth_Richer", "wealth_Middle",
                                         "wealth_Poorer", "occupation_Yes", "residence_Urban"
                                          ))
str(oddsratio_df)

ggplot(oddsratio_df, aes(y = OR, x = Effect, color = Model)) +
  geom_pointrange(aes(ymin = oddsratio_df$LL, ymax = oddsratio_df$UL),
                  position = dodger,
                  size = 0.65) +
  geom_hline(yintercept = 1.0, linetype = "dotted", size = 1) +
  scale_y_log10(breaks = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10),
                minor_breaks = NULL) +
  labs(y = "Odds ratio", x = "") +
  coord_flip(ylim = c(0.2, 2.0)) +
  theme_bw() + theme(text = element_text(size=17)) +
  scale_color_brewer(palette="Set1")


plot_random_effects(model_besag_re)

inla.list.models("prior")
inla.set.control.fixed.default() 
