packs <- c("targets",
           # "heaven",
           "foreach",
           "parallel",
           "data.table",
           "tidyverse",
           "lava",
           "ltmle",
           "future",
           "future.callr")
library(targets)
targets::tar_option_set(packages = packs)

#source functions from functions folder
for(f in list.files("functions",".R$",full.names=TRUE)){source(f)}
# for(f in list.files("../heaven/R",".R$",full.names=TRUE)){source(f)}


# plan(callr)
# future::plan(future::multisession, workers = 2)



list(
  tar_target(N_time,4)
  ,tar_target(iter,250)
  ,tar_target(cc,{
    cc <- data.table::fread("./data/coefficients.txt")
    cc <- cc[cc$time%in%0:N_time,]
    return(cc)
  }
)
  ,tar_target(data_specs, {
    data_specs <- synthesizeDD(cc)
    return(data_specs)
  } )
  ,tar_target(null_data,sim_null_data(data_specs=data_specs,
                                       n=115698,
                                       N_time=N_time,
                                       iter = iter))
 ,tar_target(sig_data,sim_sig_data(data_specs=data_specs,
                                    n=115698,
                                    N_time=N_time,
                                    iter = iter))
  # ,tar_target(analysis,run_analyses_SL(data_list=null_data,
  #                                      SL.library="glm",
  #                                      det.Q.function=NULL,
  #                                      varmethod="ic",
  #                                      iter=iter))
  )
