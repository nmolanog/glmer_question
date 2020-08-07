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
path_RData<-"../data"

#######################
###load data
#######################
list.files(path = path_RData)%>%str_subset(".RData")
load(paste0(path_RData,"/", "problem_data",".RData"))

###fitting models
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

###random effects
ranef_ls<-list()
for(i in vars_to_reg){
  ranef_ls[[i]]<-univar_mer[[i]]%>%ranef()%>%as.data.frame()%>%{cbind(.,var=i)}
}
ranef_df<-ranef_ls%>%reduce(rbind)

ranef_df[ranef_df$var %in% dic_vars,]%>%ggplot( aes(y=grp,x=condval)) +
  geom_point() + facet_wrap(~var,scales="free_x") +
  geom_errorbarh(aes(xmin=condval -2*condsd,
                     xmax=condval +2*condsd), height=0)

###get sd of random effects
dic_vars%>%map_df(~data.frame(var=.,sd=VarCorr(univar_mer[[.]])%>%unlist))
###get sd of random effects
dic_vars%>%map_df(~data.frame(var=.,icc=performance::icc(univar_mer[[.]])$ICC_adjusted))
