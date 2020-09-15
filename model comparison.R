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

