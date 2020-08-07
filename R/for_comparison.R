#######################
###load packages
#######################
#clean space, beware
rm(list=ls())

options(max.print=999999)
library(pacman)
p_load(here)
p_load(tidyverse)
p_load(lme4)
p_load(reshape2)
p_load(performance) #to get ICC
p_load(boot) # to get inv.logit
p_load(gridExtra)
p_load(lattice)

###lung cancer
### see https://stats.idre.ucla.edu/r/dae/mixed-effects-logistic-regression/
hdp <- read.csv("https://stats.idre.ucla.edu/stat/data/hdp.csv")
hdp <- within(hdp, {
  Married <- factor(Married, levels = 0:1, labels = c("no", "yes"))
  DID <- factor(DID)
  HID <- factor(HID)
  CancerStage <- factor(CancerStage)
})

###estiamtions
m0 <- glmer(remission ~ 1+(1 | DID), 
            data = hdp, family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 10)
mk_glmer_ci<-confint(m0)

m1 <- glm(remission ~ 1,
            data = hdp, family = binomial)
mk_glm_ci<-inv.logit(confint(m1))

###summarizing
res_df<-rbind(data.frame(method=class(m0),estimate=inv.logit(fixef(m0)),LCI=inv.logit(mk_glmer_ci[2,1]),UCI=inv.logit(mk_glmer_ci[2,2])),
      data.frame(method=class(m1)[1],estimate=inv.logit(m1$coefficients),LCI=mk_glm_ci[1],UCI=mk_glm_ci[2]))

pd<-position_dodge(0.5)
res_df%>%ggplot(aes(x=method, y=estimate,colour=method))+
  geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.5,position=pd)+
  geom_point(position=pd)+theme_bw()+ggtitle("dichotomous variables")+
  ylim(0, 0.5)

###ranef
dotplot(m0%>%ranef) 

###ranef sd estimate
m0%>%VarCorr()

###ICC
performance::icc(m0)$ICC_adjusted

###check number of measures by group
hdp$DID%>%table%>%unique
