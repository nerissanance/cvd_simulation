sim_null_data <- function(data_specs,n, N_time,reps){

  sim_list <- NULL

  for(i in 1:reps){
    cat("\ni: ",i,"\n")

    d <- sim(data_specs,n)

    # flag <- TRUE
    # for(j in 1:20){
    #   if(flag){
    #     set.seed(i*100 + j)
    #     d <- sim(u,115698)
    #     d$cum_glp1 <- d$glp1_0+d$glp1_1+d$glp1_2+d$glp1_3+d$glp1_4+d$glp1_5+d$glp1_6+d$glp1_7+d$glp1_8+d$glp1_9+d$glp1_10
    #     d$cum_dementia <- d$event_dementia_0+d$event_dementia_1+d$event_dementia_2+d$event_dementia_3+d$event_dementia_4+d$event_dementia_5+d$event_dementia_6+d$event_dementia_7+d$event_dementia_8+d$event_dementia_9+d$event_dementia_10
    #     cat(min(table(d$cum_glp1==11, d$cum_dementia>0)),", ")
    #     flag <- ifelse(min(table(d$cum_glp1==11, d$cum_dementia>0))<6, TRUE, FALSE)
    #   }
    # }

    #note: once events jump to 1, need to remain 1 for remainder of follow up
    for(k in 1:(N_time+1)){
      j=k+1
      d[get(paste0("event_dementia_",k))==1, (paste0("event_dementia_",j)):=1]
      d[get(paste0("event_death_",k))==1, (paste0("event_death_",j)):=1]
      d[get(paste0("event_death_",k))==1, (paste0("censor_",i)):=0]
      d[get(paste0("event_death_",k))==1, (paste0("censor_",j)):=1]
      d[get(paste0("censor_",k))==1, (paste0("censor_",j)):=1]
    }


    #Scramble outcomes (Keeping longitudinal pattern together)
    X <- d %>% select(!(starts_with("event_")|starts_with("censor_")))
    Y <- d %>% select((starts_with("event_")|starts_with("censor_")))
    Y <- Y[sample(nrow(Y)),]

    d<- cbind(X,Y)


    ## UNCOMMENT FOR RUNNING MANUAL COMPETING RISK FIX
    ## edit--when one occurs first, set other to zero so there's no competing event:
    dementia.nodes<- grep("event_dementia_",names(d))
    death.nodes<- grep("event_death_",names(d))
    d[, sum_death :=rowSums(.SD,na.rm=T), .SDcols = death.nodes]
    d[, sum_dementia :=rowSums(.SD,na.rm=T), .SDcols = dementia.nodes]

    d[sum_death > sum_dementia, (dementia.nodes) := replace(.SD, .SD == 1, 0), .SDcols = dementia.nodes]
    d[sum_death < sum_dementia, (death.nodes) := replace(.SD, .SD == 1, 0), .SDcols = death.nodes]
    # NOTE: decided to prioritize dementia in the event that both death and dementia occur in the same time bin
    d[sum_death== sum_dementia, (death.nodes) := replace(.SD, .SD == 1, 0), .SDcols = death.nodes]


    d <- d %>% select("glp1_1","heart.failure_1","glp1_2","heart.failure_2","glp1_3","heart.failure_3","glp1_4","heart.failure_4")

    sim_list[[i]] <- d
    gc()
  }
  return(sim_list)
}
