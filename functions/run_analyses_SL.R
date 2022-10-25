
#set Nsub to NULL to run full analysis, or it will run a subset of the data
run_analyses_SL <- function(data_list,  SL.library,
                              det.Q.function=NULL, varmethod,iter=iter){

  #need to figure out parallel processing still!
   # res_df <- foreach(j = 1:iter, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  j=1
    data<- data_list[[j]]
  fit <- ltmle(data=data,
                  Anodes = c("glp1_1","glp1_2","glp1_3","glp1_4"),
                  Cnodes = NULL,
                  Lnodes = NULL,
                  Ynodes = c("heart.failure_1","heart.failure_2","heart.failure_3","heart.failure_4"),
                  survivalOutcome = F,
                  abar = list(c(1,1,1,1),c(0,0,0,0)),
                  deterministic.Q.function = det.Q.function,
                  SL.library = SL.library,
                  variance.method = varmethod )


   if(!is.null(fit)){
    res <- summary(fit)
    res.iptw <- summary(fit, estimator="iptw")
    res.RR <- as.data.frame(res$effect.measures$RR)
    res.ate <- as.data.frame(res$effect.measures$ATE) %>% rename(ate.long.name=long.name,ate=estimate, ate.sd=std.dev , ate.pval=pvalue, ate.ci.lb=CI.2.5., ate.ci.ub=  CI.97.5., ate.log.std.err=log.std.err)

    res.RR.iptw <- as.data.frame(res.iptw$effect.measures$RR) %>% rename(iptw.long.name=long.name, iptw.estimate=estimate, iptw.sd=std.dev , iptw.pval=pvalue, iptw.ci.lb=CI.2.5., iptw.ci.ub=  CI.97.5., iptw.log.std.err=log.std.err)
    res.ate.iptw <- as.data.frame(res$effect.measures$ATE) %>% rename(iptw.ate.long.name=long.name, iptw.ate=estimate, iptw.ate.sd=std.dev , iptw.ate.pval=pvalue, iptw.ate.ci.lb=CI.2.5., iptw.ate.ci.ub=  CI.97.5., iptw.ate.log.std.err=log.std.err)

    res <- cbind(res.RR, res.ate, res.RR.iptw, res.ate.iptw)
    res$label <- j
   }


  return(res)
  # }

  # return(res_df)
}
