
library(targets)
library(parallel)
library(doParallel)
library(foreach)
library(tidyverse)
library(ltmle)
# cl <- makeCluster(1)
# registerDoParallel(cl)
registerDoParallel(cores=parallel::detectCores()/2)

run_analysis_notargets <- function(data_list=data_list,
                                   SL.library,
                                   det.Q.function=NULL,
                                   varmethod="ic",
                                   iter,
                                   gcomp=F){
res_df <- foreach(j = 1:iter, .combine = 'bind_rows',
                  .errorhandling = 'remove',
                  .packages=c("ltmle")) %dopar% {
    data<- data_list[[j]]
  fit <- ltmle(data=data,
                  Anodes = c("glp1_1","glp1_2","glp1_3","glp1_4"),
               Cnodes = c("censor_1","censor_2","censor_3","censor_4"),
               Lnodes = c("insulin_1","insulin_2","insulin_3"),
               Ynodes = c("mace_hf_1","mace_hf_2","mace_hf_3","mace_hf_4"),
               survivalOutcome = T,
                  abar = list(c(1,1,1,1),c(0,0,0,0)),
                  deterministic.Q.function = det.Q.function,
                  SL.library = SL.library,
                  variance.method = varmethod ,
               gcomp=gcomp)


   if(!is.null(fit) & gcomp==F){
    res <- summary(fit)
    res.iptw <- summary(fit, estimator="iptw")
    res.RR <- as.data.frame(res$effect.measures$RR)
    res.ate <- as.data.frame(res$effect.measures$ATE) %>% rename(ate.long.name=long.name,
                                                                 ate=estimate,
                                                                 ate.sd=std.dev ,
                                                                 ate.pval=pvalue,
                                                                 ate.ci.lb=CI.2.5.,
                                                                 ate.ci.ub=  CI.97.5.,
                                                                 ate.log.std.err=log.std.err)

    res.RR.iptw <- as.data.frame(res.iptw$effect.measures$RR) %>% rename(iptw.long.name=long.name,
                                                                         iptw.estimate=estimate,
                                                                         iptw.sd=std.dev ,
                                                                         iptw.pval=pvalue,
                                                                         iptw.ci.lb=CI.2.5.,
                                                                         iptw.ci.ub=  CI.97.5.,
                                                                         iptw.log.std.err=log.std.err)
    res.ate.iptw <- as.data.frame(res.iptw$effect.measures$ATE) %>% rename(iptw.ate.long.name=long.name,
                                                                           iptw.ate=estimate,
                                                                           iptw.ate.sd=std.dev ,
                                                                           iptw.ate.pval=pvalue,
                                                                           iptw.ate.ci.lb=CI.2.5.,
                                                                           iptw.ate.ci.ub=  CI.97.5.,
                                                                           iptw.ate.log.std.err=log.std.err)

    res <- cbind(res.RR, res.ate, res.RR.iptw, res.ate.iptw)
    res$label <- j


   }else if(!is.null(fit) & gcomp==T){
     res  <- summary(fit, estimator="gcomp")

     res.RR  <- as.data.frame(res$effect.measures$RR) %>% rename(gcomp.long.name=long.name,
                                                                 gcomp.estimate=estimate,
                                                                 gcomp.sd=std.dev ,
                                                                 gcomp.pval=pvalue,
                                                                 gcomp.ci.lb=CI.2.5.,
                                                                 gcomp.ci.ub=  CI.97.5.,
                                                                 gcomp.log.std.err=log.std.err)
     res.ate <- as.data.frame(res$effect.measures$ATE) %>% rename(gcomp.ate.long.name=long.name,
                                                                  gcomp.ate=estimate,
                                                                  gcomp.ate.sd=std.dev ,
                                                                  gcomp.ate.pval=pvalue,
                                                                  gcomp.ate.ci.lb=CI.2.5.,
                                                                  gcomp.ate.ci.ub=  CI.97.5.,
                                                                  gcomp.ate.log.std.err=log.std.err)

     res <- cbind(res.RR, res.ate)
     res$label <- j


   }


  return(res)
   }



return(res_df)
}

# data_list <- tar_read(null_data)
# iter=tar_read(iter)
# system.time({
# run_tmle <- run_analysis_notargets(data_list=data_list,
#                                     SL.library=c("SL.glm","SL.mean"),
#                                     det.Q.function=NULL,
#                                     varmethod="ic",
#                                     iter=iter,
#                                     gcomp=F)
# })
# saveRDS(run_tmle, file="./data/tmle_run_SLglm.RDS")


# data_list <- tar_read(sig_data)
# iter=tar_read(iter)
# system.time({
#   run_tmle <- run_analysis_notargets(data_list=data_list,
#                                      SL.library=c("SL.glm","SL.mean"),
#                                      det.Q.function=NULL,
#                                      varmethod="ic",
#                                      iter=iter,
#                                      gcomp=F)
# })
# saveRDS(run_tmle, file="./data/tmle_run_SLglm_sigdata.RDS")


data_list <- tar_read(sig_data)
iter=tar_read(iter)
system.time({
  run_tmle <- run_analysis_notargets(data_list=data_list,
                                     SL.library=c("SL.glm","SL.mean"),
                                     det.Q.function=NULL,
                                     varmethod="ic",
                                     iter=iter,
                                     gcomp=T)
})
saveRDS(run_tmle, file="./data/tmle_run_SLglm_sigdata_gcomp.RDS")


data_list <- tar_read(null_data)
iter=tar_read(iter)
system.time({
  run_tmle <- run_analysis_notargets(data_list=data_list,
                                     SL.library=c("SL.glm","SL.mean"),
                                     det.Q.function=NULL,
                                     varmethod="ic",
                                     iter=iter,
                                     gcomp=T)
})
saveRDS(run_tmle, file="./data/tmle_run_SLglm_nulldata_gcomp.RDS")


# data_list <- tar_read(null_data)
# iter=tar_read(iter)
# system.time({
# run_tmle <- run_analysis_notargets(data_list=data_list,
#                                     SL.library=c("SL.glm","SL.mean"),
#                                     det.Q.function=NULL,
#                                     varmethod="ic",
#                                     iter=iter,
#                                     gcomp=F)
# })
# saveRDS(run_tmle, file="./data/tmle_run_SLglm.RDS")


# data_list <- tar_read(sig_data)
# iter=tar_read(iter)
# system.time({
#   run_tmle <- run_analysis_notargets(data_list=data_list,
#                                      SL.library=c("SL.glm","SL.mean"),
#                                      det.Q.function=NULL,
#                                      varmethod="ic",
#                                      iter=iter,
#                                      gcomp=F)
# })
# saveRDS(run_tmle, file="./data/tmle_run_SLglm_sigdata.RDS")
