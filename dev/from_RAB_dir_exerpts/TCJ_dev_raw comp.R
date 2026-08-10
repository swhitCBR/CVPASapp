load("../CVPAS-steelhead/Sth route selection revision_1_v4.RData")


# list(tcj.df)

require("MuMIn")
require("parallel")
require("corrplot")
require("car")
options(na.action="na.fail")


head(tcj.stn.df)
head(tcj.df)

dim(tcj.stn.df)
dim(tcj.df)


unscl_ind_TCJ <- match(c("log.VNS.ent.tcj.cms","CVP.exit.tcj.cms","SWP.exit.tcj.cms","flength"),names(tcj.df))
scl_ind_TCJ <- match(c("log.VNS.ent.tcj.cms","CVP.exit.tcj.cms","SWP.exit.tcj.cms","flength"),names(tcj.stn.df))

data.frame(
  attributes(scale(tcj.df[,unscl_ind_TCJ]))["scaled:center"],
  attributes(scale(tcj.df[,unscl_ind_TCJ]))["scaled:scale"])


tcj.df[,c(which(names(hor.df)=="year"),unscl_ind_TCJ)]
tmp_data_TCJ <- tidyr::pivot_longer(tcj.df[,c(which(names(hor.df)=="year"),unscl_ind_TCJ)],cols=2:5)

library(ggplot2)
ggplot(data=tmp_data_TCJ) + geom_boxplot(aes(y=value,group=year)) + facet_wrap(~name,scale="free_y") 




m.tcj.mod.dd
m.tcj.mod.global

revis_m.tcj.mod.global<-glm(TCJ.route.fac ~ barrier.exit.tcj.fac*(log.VNS.ent.tcj.cms + CVP.exit.tcj.cms + SWP.exit.tcj.cms) + flength,
                      family="binomial", data=tcj.stn.df)
summary(revis_m.tcj.mod.global)

yr_revis_m.tcj.mod.global<-glm(TCJ.route.fac ~ barrier.exit.tcj.fac*(log.VNS.ent.tcj.cms + CVP.exit.tcj.cms + SWP.exit.tcj.cms) + flength + factor(year),
                            family="binomial", data=tcj.stn.df)
summary(revis_m.tcj.mod.global)
summary(yr_revis_m.tcj.mod.global)



QB_revis_m.tcj.mod.global<-glm(TCJ.route.fac ~ barrier.exit.tcj.fac*(log.VNS.ent.tcj.cms + CVP.exit.tcj.cms + SWP.exit.tcj.cms) + flength,
                            family="quasibinomial", data=tcj.stn.df)
summary(QB_revis_m.tcj.mod.global)


clusterType <- if(length(find.package("snow", quiet = TRUE))) "SOCK" else "PSOCK"
clust <- try(makeCluster(getOption("cl.cores", 8), type = clusterType))
clusterExport(clust, "tcj.stn.df")
clusterEvalQ(clust, library(glmmTMB))
revis_m.tcj.mod.dd<-dredge(revis_m.tcj.mod.global, beta="none", cluster=clust, trace=2) # M model with barrier interaction effects
stopCluster(clust)

dim(revis_m.tcj.mod.dd)
subset(revis_m.tcj.mod.dd,delta<2)
# subset(revis_m.tcj.mod.dd,delta<4)



library(glmmTMB)
# cluster doesn't seem to speed it up much work
# clusterType <- if(length(find.package("snow", quiet = TRUE))) "SOCK" else "PSOCK"
# clust <- try(makeCluster(getOption("cl.cores", 8), type = clusterType))
# clusterExport(clust, "hor.stn.df")
# clusterEvalQ(clust, library(glmmTMB))
table(tcj.stn.df$YrRel)

ME_revis_m.tcj.mod.global<-glmmTMB(TCJ.route.fac ~ barrier.exit.tcj.fac*(log.VNS.ent.tcj.cms + CVP.exit.tcj.cms + SWP.exit.tcj.cms)  + flength+ (1|YrRel),
                               family="binomial", data=tcj.stn.df)

# yr_ME_revis_m.tcj.mod.global<-glmmTMB(TCJ.route.fac ~ barrier.exit.tcj.fac*(log.VNS.ent.tcj.cms + CVP.exit.tcj.cms + SWP.exit.tcj.cms)  + flength+ (1|YrRel) ,
#                                    family="binomial", data=tcj.stn.df)
# 
# tcj.stn.df$fyear <- tcj.stn.df$year
# yrRE_ME_revis_m.tcj.mod.global<-glmmTMB(TCJ.route.fac ~ barrier.exit.tcj.fac*(log.VNS.ent.tcj.cms + CVP.exit.tcj.cms + SWP.exit.tcj.cms)  + flength+ (1|YrRel) ,
#                                       family="binomial", data=tcj.stn.df)
# 
ranef(ME_revis_m.tcj.mod.global)
ranef(ME_revis_m.tcj.mod.global,condVar = TRUE)

vcov(ME_revis_m.tcj.mod.global)$cond


summary(revis_m.tcj.mod.global)
summary(yr_ME_revis_m.tcj.mod.global)
summary(yrRE_ME_revis_m.tcj.mod.global)

# yr_ME_revis_m.tcj.mod.global
# 
clusterType <- if(length(find.package("snow", quiet = TRUE))) "SOCK" else "PSOCK"
clust <- try(makeCluster(getOption("cl.cores", 8), type = clusterType))
clusterExport(clust, "tcj.stn.df")
clusterEvalQ(clust, library(glmmTMB))

ME_revis_m.tcj.mod.dd<-dredge(ME_revis_m.tcj.mod.global, beta="none", cluster=clust, trace=2) # M model with barrier interaction effects
stopCluster(clust)

subset(ME_revis_m.tcj.mod.dd,delta<2)
summary(ME_revis_m.tcj.mod.global)
# plot(ME_revis_m.tcj.mod.global)
# summary(lm(CVP.exit.tcj.cms~SWP.exit.tcj.cms,data=tcj.stn.df))

models_below_threshold <- get.models(ME_revis_m.tcj.mod.dd, subset = delta < 2)
mods_d2_ls <- lapply(models_below_threshold, summary)
names(mods_d2_ls)
sapply(mods_d2_ls,function(x) sqrt(x$varcor[[1]][[1]]))

RE_ints_ls <- lapply(mods_d2_ls,function(x) ranef(models_below_threshold[[1]]) )
par(mfrow=c(3,3),mar=c(4,2,2,2))
lapply(RE_ints_ls,function(x) hist(unlist(x)))
sapply(models_below_threshold,function(x) sqrt(VarCorr(x)[["cond"]][["YrRel"]][1,1]))
sapply(models_below_threshold,function(x) formula(x))

tcj.stn.df$arr.est.obs
table(tcj.stn.df$arr.est.obs)
# table(hor.stn.df$day.exit.hor)
# head(hor.stn.df)
# hor.stn.df$HOR.det
# table(tcj.stn.df$TCJ.up.site=="TCJ",)
table(is.na(tcj.stn.df$TCJ.up.site=="TCJ"))

# tcj.stn.df$jx.arr
par(mfrow=c(1,1))
hist(tcj.stn.df$jx.exit.doy[tcj.stn.df$arr.est.obs=="observed"]-tcj.stn.df$jx.arr.doy[tcj.stn.df$arr.est.obs=="observed"])
v_obs_diff_arv_ext <- tcj.stn.df$jx.exit.doy[tcj.stn.df$arr.est.obs=="observed"]-tcj.stn.df$jx.arr.doy[tcj.stn.df$arr.est.obs=="observed"]
summary(v_obs_diff_arv_ext)
quantile(v_obs_diff_arv_ext)
table(v_obs_diff_arv_ext<0.5)/length(v_obs_diff_arv_ext)
# 99% of tags were observed exiting within 12 hours of arrival

sqrt(mods_d2_ls[[1]]$varcor[[1]]$YrRel[1,1])
# sqrt(VarCorr(model)$cond$group_name)
# ranef(models_below_threshold[[1]])

ME_revis_tcj_d2<-model.avg(models_below_threshold, subset=delta<2)#, rank="AICc")
ME_revis_tcj_d2_pred<-predict(ME_revis_tcj_d2, newdata=tcj.stn.df, type="response", backtransform = FALSE, se.fit=TRUE)


top_ME_revis_tcj_d2_pred<-predict(models_below_threshold[[1]], newdata=tcj.stn.df[1:20,], type="response",se.fit=TRUE)
data.frame(top_ME_revis_tcj_d2_pred)

# remote TCJ
# d.df$YrRel=tcj.stn.df$YrRel[1]
d.df$YrRel=NA
top_ME_revis_tcj_d2_pred<-predict(models_below_threshold[[1]], newdata=d.df, type="response",se.fit=TRUE)
data.frame(top_ME_revis_tcj_d2_pred)

d.df

newdata.11.tcj.stn<-newdata.11.tcj

# newdata.11.tcj.stn$datetime.doy.exit.tcj
newdata.11.tcj.stnSUB <- newdata.11.tcj.stn[!duplicated(newdata.11.tcj.stn$date.ent.tcj),]
# newdata.11.tcj.stnSUB <- newdata.11.tcj.stn#[!duplicated(newdata.11.tcj.stn$date.ent.tcj),]

newdata.11.tcj.stnSUB$YrRel=NA
top_ME_revis_tcj_d2_pred<-predict(models_below_threshold[[1]], newdata=newdata.11.tcj.stnSUB, type="response",se.fit=TRUE)
plot(newdata.11.tcj.stnSUB$date.ent.tcj,data.frame(top_ME_revis_tcj_d2_pred)[,1])
lines(newdata.11.tcj.stnSUB$date.ent.tcj,data.frame(top_ME_revis_tcj_d2_pred)[,1]-data.frame(top_ME_revis_tcj_d2_pred)[,2])
lines(newdata.11.tcj.stnSUB$date.ent.tcj,data.frame(top_ME_revis_tcj_d2_pred)[,1]+data.frame(top_ME_revis_tcj_d2_pred)[,2])

top_ME_revis_tcj_d2_pred<-predict(models_below_threshold[[1]], newdata=newdata.11.tcj.stnSUB, se.fit=TRUE)
subbEst_lp <- top_ME_revis_tcj_d2_pred[[1]]
subbLCL_lp <- subbEst_lp-top_ME_revis_tcj_d2_pred[[2]]
subbUCL_lp <- subbEst_lp+top_ME_revis_tcj_d2_pred[[2]]
subbEst <- plogis(subbEst_lp)
subbLCL <- plogis(subbLCL_lp)
subbUCL <- plogis(subbUCL_lp)

# plot(,NA,ylim=c())
# min(sapply(1:length(subbEst),function(ii) max(c(subbEst[ii],subbUCL[ii]))))
# min(sapply(1:length(subbEst),function(ii) min(c(subbEst[ii],subbLCL[ii]))))

plot(newdata.11.tcj.stnSUB$date.ent.tcj,subbEst,ylim=c(0.9,1))
lines(newdata.11.tcj.stnSUB$date.ent.tcj,subbLCL,lty=2)
lines(newdata.11.tcj.stnSUB$date.ent.tcj,subbUCL,lty=2)

head(data.frame(subbEst_lp,subbLCL_lp,subbUCL_lp))
plot(newdata.11.tcj.stnSUB$date.ent.tcj,subbEst_lp)
lines(newdata.11.tcj.stnSUB$date.ent.tcj,subbLCL_lp)
lines(newdata.11.tcj.stnSUB$date.ent.tcj,subbUCL_lp)



######################## 
# PREDICTION
######################## 

# newdata.11.hor<-cbind(x.ent,x.exit)

# remove duplicated variable:
# newdata.11.hor<-newdata.11.hor[,-which(names(newdata.11.hor)=="flength")[2]]

# rm(x.ent,x.exit)

# standardize using mean and SD from hor.df
newdata.11.hor.stn<-newdata.11.hor
tmp<-names(hor.df)[setdiff(grep("ent|exit|flength",names(hor.df)),grep("fac|barrier|Bar|Yr|Tod|tod|day|crep|night|jx|dawn|dusk",names(hor.df)))]
tmp<-intersect(tmp,names(newdata.11.hor))
for(i in 1:length(tmp)) newdata.11.hor.stn[,tmp[i]]<-(newdata.11.hor[,tmp[i]]-mean(hor.df[,tmp[i]],na.rm=T))/sd(hor.df[,tmp[i]],na.rm=T)
rm(i,tmp)





# plot trial
{
  ylabb<-"Probability of selecting SJR route"
  col.vec3<-c("red","black","blue")
  lty.vec<-c(4,5,1)
  lwd.vec<-rep(2,3)
  
  par(mar=c(1.5,3,1,1)+.3,lab=c(10,6,1),las=1,tck=-.01,mgp=c(2,.4,0))
  
  d.df<-newdata.11.hor.stn[newdata.11.hor.stn$datetime.ent.hor>=as.POSIXct("2011-05-01") & 
                             newdata.11.hor.stn$datetime.ent.hor<=as.POSIXct("2011-05-05"),]
  
  plot(d.df$datetime.ent.hor,d.df$pred.m,type="l",
       xlab="Date in 2011",ylab=ylabb,
       col=col.vec3[1], lty=lty.vec[1], lwd=lwd.vec[1],
       cex.lab=1.2,
       #axes=F,
       ylim=c(0.3,0.8))
  lines(d.df$datetime.ent.hor,d.df$pred.l,col=col.vec3[2], lty=lty.vec[2], lwd=lwd.vec[2])
  lines(d.df$datetime.ent.hor,d.df$pred.all,col=col.vec3[3], lty=lty.vec[3], lwd=lwd.vec[3])
  
  legend("topleft",legend=c("Management","Local","Combined"),title="Model",
         #fill=c(t_col(col.vec3[1]),t_col(col.vec3[2]),t_col(col.vec3[3])),
         col=col.vec3, lwd=lwd.vec*5) #, lty=lty.vec)
  
  polygon(x<-c(d.df$datetime.ent.hor,rev(d.df$datetime.ent.hor)),
          y=c(d.df$pred.m.ci.high,rev(d.df$pred.m.ci.low)),
          col=t_col(col.vec3[1],80), border=F)
  
  polygon(x<-c(d.df$datetime.ent.hor,rev(d.df$datetime.ent.hor)),
          y=c(d.df$pred.l.ci.high,rev(d.df$pred.l.ci.low)),
          col=t_col(col.vec3[2],80), border=F)
  
  polygon(x<-c(d.df$datetime.ent.hor,rev(d.df$datetime.ent.hor)),
          y=c(d.df$pred.all.ci.high,rev(d.df$pred.all.ci.low)),
          col=t_col(col.vec3[3],80), border=F)          
  
  
}


# <- list()

#[stddev]
# data.frame(revis_m.hor.mod.dd[1:5,c((ncol(revis_m.hor.mod.dd)-4):ncol(revis_m.hor.mod.dd))])
# data.frame(revis_m.hor.mod.dd[1:5,(ncol(revis_m.hor.mod.dd)-4):ncol(revis_m.hor.mod.dd)])
