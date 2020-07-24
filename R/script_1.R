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

#####################################################################
###part 2 of the experiment.
###we are going to do the same for all variables in ldf 
###using glmer or lmer accordingly
#####################################################################

vars_to_reg<-colnames(ldf)[-c(1:2,15)]
dic_vars<-c("C","D","K","L","M","N")
univar_mer<-list()
univar_glm<-list()

for(i in vars_to_reg){
  if(is.numeric(ldf[,i])){
    univar_glm[[i]]<-lm(formula(paste0(i,"~1")),data = ldf)
    univar_mer[[i]]<-lmer(formula(paste0(i,"~1+(1|Id)")),data = ldf)
  }else{
    univar_glm[[i]]<-glm(formula(paste0(i,"~1")),data = ldf, family = binomial(link = "logit"))
    univar_mer[[i]]<-glmer(formula(paste0(i,"~1+(1|Id)")),data = ldf, family = binomial(link = "logit"),control=glmerControl(optimizer = "bobyqa"),nAGQ = 20)
  }
}

glm_res_df<-univar_glm%>%names%>%map_df(function(x){
  if(x %in% dic_vars){
    mk_glm_ci<-inv.logit(confint(univar_glm[[x]]))
    data.frame(var=x,method=class(univar_glm[[x]])[1],estimate=inv.logit(univar_glm[[x]]$coefficients),LCI=mk_glm_ci[1],UCI=mk_glm_ci[2])
  }else{
    mk_glm_ci<-confint(univar_glm[[x]])
    data.frame(var=x,method=class(univar_glm[[x]])[1],estimate=univar_glm[[x]]$coefficients,LCI=mk_glm_ci[1],UCI=mk_glm_ci[2])
  }
})

glmer_res_df<-univar_mer%>%names%>%map_df(function(x){
  mk_glmer_ci<-confint(univar_mer[[x]])
  if(x %in% dic_vars){
    data.frame(var=x,method=class(univar_mer[[x]]),estimate=inv.logit(fixef(univar_mer[[x]])),LCI=inv.logit(mk_glmer_ci[2,1]),UCI=inv.logit(mk_glmer_ci[2,2]))
  }else{
    data.frame(var=x,method=class(univar_mer[[x]]),estimate=fixef(univar_mer[[x]]),LCI=mk_glmer_ci[3,1],UCI=mk_glmer_ci[3,2])
  }
})

raw_estimates<-vars_to_reg%>%map_df(function(x){
  if(x %in% dic_vars){
    data.frame(var=x,method="araw",estimate=mean(as.numeric(as.character(ldf[,x])),na.rm = T),LCI=NA,UCI=NA)
  }else{
    data.frame(var=x,method="araw",estimate=mean(ldf[,x],na.rm = T),LCI=NA,UCI=NA)
  }
})

res_df2<-rbind(glm_res_df,glmer_res_df,raw_estimates)%>%{.[order(.$var,.$method),]}
rownames(res_df2)<-NULL

for(i in c("estimate","LCI","UCI")){
  res_df2[,i]<-round(res_df2[,i],5)
}

res_df2
pd<-position_dodge(0.5)
p1<-res_df2%>%filter(!method %in% "araw" & var %in% dic_vars)%>%ggplot(aes(x=var, y=estimate,colour=method))+
  geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.5,position=pd)+geom_point(position=pd)+theme_bw()+ggtitle("dichotomous variables")

p2<-res_df2%>%filter(!method %in% "araw" & !(var %in% c(dic_vars,"I")))%>%ggplot(aes(x=var, y=estimate,colour=method))+
  geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.5,position=pd)+geom_point(position=pd)+theme_bw()+ggtitle("continuous variables")

p3<-res_df2%>%filter(!method %in% "araw" & var %in% "I")%>%ggplot(aes(x=var, y=estimate,colour=method))+
  geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.5,position=pd)+geom_point(position=pd)+theme_bw()+ggtitle("continuous variables")

grid.arrange(p1, p2,p3, nrow = 1)
