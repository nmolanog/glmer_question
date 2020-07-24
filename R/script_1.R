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
path_RData<-"../data"

#######################
###load data
#######################
list.files(path = path_RData)%>%str_subset(".RData")
load(paste0(path_RData,"/", "problem_data",".RData"))

### inspect data
summary(ldf)

###estimate proportion for variable K using glm
mk_glm<-glm(K~1,data = ldf, family = binomial(link = "logit"))
mk_glm_ci<-inv.logit(confint(mk_glm))

##arrange result from gls model
(res_df<-data.frame(method="glm",estimate=inv.logit(mk_glm$coefficients),LCI=mk_glm_ci[1],UCI=mk_glm_ci[2]))

#compare to  raw estimate:
ldf$K%>%table()%>%{.[2]/sum(.)}

###estimate proportion for variable K using glmer model 1
mk_glmer<-glmer(K~1+(1|Id),data = ldf, family = binomial(link = "logit"),control=glmerControl(optimizer = "bobyqa"),nAGQ = 20)
mk_glmer_ci<-confint(mk_glmer)
#add result to res_df
(res_df<-rbind(res_df,data.frame(method="glmer",estimate=inv.logit(fixef(mk_glmer)),LCI=inv.logit(mk_glmer_ci[2,1]),UCI=inv.logit(mk_glmer_ci[2,2]))))

###estimate proportion for variable K using glmer model 2, fail
mk_glmer_2<-glmer(K~1+(1|Id/eye),data = ldf, family = binomial(link = "logit"),control=glmerControl(optimizer = "bobyqa"))
mk_glmer_2_ci<-confint(mk_glmer_2)
(res_df<-rbind(res_df,data.frame(method="glmer2",estimate=inv.logit(fixef(mk_glmer_2)),LCI=inv.logit(mk_glmer_2_ci[3,1]),UCI=inv.logit(mk_glmer_2_ci[3,2]))))
