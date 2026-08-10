
#exerpts from revision_1 HOR modeling v4.r
# load("../CVPAS-steelhead/Sth route selection revision_1_v4.RData")


rte_df_ls <- readRDS("../tmp/route_dfs_HOR_CHP.rds")
hor.df <- rte_df_ls$hor.df
hor.stn.df <- rte_df_ls$hor.df
d.df <- rte_df_ls$d.df

ann_HORbar_WYT_data_tmp <- ann_HORbar_WYT_data
ann_HORbar_WYT_data_tmp$fWYT <- ifelse(ann_HORbar_WYT_data_tmp$WYT=="Above Normal","Wet",ann_HORbar_WYT_data_tmp$WYT)
ann_HORbar_WYT_data_tmp$fWYT <- ifelse(ann_HORbar_WYT_data_tmp$WYT=="Below Normal","Dry",ann_HORbar_WYT_data_tmp$fWYT)

# resetting 

CVhelp_dat_w$fWYT <- factor(ann_HORbar_WYT_data_tmp[match(CVhelp_dat_w$Year,ann_HORbar_WYT_data_tmp$Year),"fWYT"],levels=c("Dry","Critical","Wet"))
head(CVhelp_dat_w)

load_all()


require("MuMIn")
require("parallel")
require("corrplot")
require("car")
options(na.action="na.fail")

ann_HORbar_WYT_data
subset(ann_HORbar_WYT_data,Year %in% c(2011:2016))

hor.df$WYT <- factor(ann_HORbar_WYT_data[match(hor.df$year,ann_HORbar_WYT_data$Year),"WYT"],levels=c("Dry","Critical","Wet"))
hor.stn.df$WYT <- factor(ann_HORbar_WYT_data[match(hor.stn.df$year,ann_HORbar_WYT_data$Year),"WYT"],levels=c("Dry","Critical","Wet"))

# GLM version of global model
# revis_m.hor.mod.global<-glm(HOR.route.fac ~ barrier.exit.hor.fac*(log.VNS.ent.hor.cms + CVP.exit.hor.cms + SWP.exit.hor.cms) + WYT + flength,
#                       family="binomial", data=hor.stn.df)
# revis_m.hor.mod.dd<-MuMIn::dredge(revis_m.hor.mod.global, fixed=c("barrier.exit.hor.fac"), beta="none")#, cluster=clust, trace=2) # M model with barrier interaction effects
# dim(revis_m.hor.mod.dd)
# # 243 -> 81
# subset(revis_m.hor.mod.dd,delta<2)

library(glmmTMB)
ME_revis_m.hor.mod.global<-glmmTMB(HOR.route.fac ~ WYT + barrier.ent.hor.fac*(log.VNS.ent.hor.cms + CVP.ent.hor.cms + SWP.ent.hor.cms) + flength + (1|YrRel),
                                 family="binomial", data=hor.stn.df)

# cluster doesn't seem to speed it up much work
clusterType <- if(length(find.package("snow", quiet = TRUE))) "SOCK" else "PSOCK"
clust <- try(makeCluster(getOption("cl.cores", 8), type = clusterType))
clusterExport(clust, "hor.stn.df")
clusterEvalQ(clust, library(glmmTMB))
ME_revis_m.hor.mod.dd<-MuMIn::dredge(ME_revis_m.hor.mod.global, fixed=c("barrier.exit.hor.fac"), beta="none")#, cluster=clust, trace=2) # M model with barrier interaction effects
stopCluster(clust)

ME_revis_m.hor.mod.dd

# global model is last place
subset(ME_revis_m.hor.mod.dd,delta<2)
summary(ME_revis_m.hor.mod.global)

models_below_threshold <- get.models(ME_revis_m.hor.mod.dd, subset = delta < 2)
mods_d2_ls <- lapply(models_below_threshold, summary)
names(mods_d2_ls)
# random intercept standard errors
sapply(mods_d2_ls,function(x) sqrt(x$varcor[[1]][[1]]))

RE_ints_ls <- lapply(mods_d2_ls,function(x) ranef(models_below_threshold[[1]]) )
par(mfrow=c(3,3),mar=c(4,2,2,2))
lapply(RE_ints_ls,function(x) hist(unlist(x),breaks=8))
sapply(models_below_threshold,function(x) sqrt(VarCorr(x)[["cond"]][["YrRel"]][1,1]))
sapply(models_below_threshold,function(x) formula(x))

hor.stn.df$arr.est.obs
table(hor.stn.df$arr.est.obs)
# table(hor.stn.df$day.exit.hor)
# head(hor.stn.df)
# hor.stn.df$HOR.det
# table(hor.stn.df$HOR.up.site=="HOR",)
# table(is.na(hor.stn.df$HOR.up.site=="HOR"))

hor.stn.df$jx.arr
par(mfrow=c(1,1))
hist(hor.stn.df$jx.exit.doy[hor.stn.df$arr.est.obs=="observed"]-hor.stn.df$jx.arr.doy[hor.stn.df$arr.est.obs=="observed"])
v_obs_diff_arv_ext <- hor.stn.df$jx.exit.doy[hor.stn.df$arr.est.obs=="observed"]-hor.stn.df$jx.arr.doy[hor.stn.df$arr.est.obs=="observed"]
summary(v_obs_diff_arv_ext)
quantile(v_obs_diff_arv_ext)
table(v_obs_diff_arv_ext<0.5)/length(v_obs_diff_arv_ext)
# 95% of tags were observed exiting within 12 hours of arrival
table(v_obs_diff_arv_ext<0.75)/length(v_obs_diff_arv_ext)
# 97% of tags were observed exiting within 12 hours of arrival
# hor.stn.df

# model averaging object
ME_revis_hor_d2<-model.avg(models_below_threshold, subset=delta<2)#, rank="AICc")
ME_revis_hor_d2_pred<-predict(ME_revis_hor_d2, newdata=hor.stn.df, type="response", backtransform = FALSE, se.fit=TRUE)

plot(hor.stn.df$HOR.det,ME_revis_hor_d2_pred$fit)

# models_below_threshold[[1]]
head(CVPAS_prev_yr_ref_tab)

tail(CVhelp_dat_w)


top_ME_revis_hor_d2_pred<-predict(models_below_threshold[[1]], newdata=hor.stn.df[1:20,], type="response",se.fit=TRUE)
data.frame(top_ME_revis_hor_d2_pred)


d.df$barrier.exit.hor.fac <- d.df$barrier.exit.tcj.fac 
d.df$YrRel=hor.stn.df$YrRel[1]
top_ME_revis_hor_d2_pred<-predict(models_below_threshold[[1]], newdata=d.df, type="response",se.fit=TRUE)
data.frame(top_ME_revis_hor_d2_pred)

d.df

######################## 
# PREDICTION
######################## 

# newdata.11.hor<-cbind(x.ent,x.exit)

# remove duplicated variable:
newdata.11.hor<-newdata.11.hor[,-which(names(newdata.11.hor)=="flength")[2]]

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
