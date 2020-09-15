
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

