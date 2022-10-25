packs <- c("targets",
           "heaven",
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

# plan(callr)
# future::plan(future::multisession, workers = 2)



list(
  tar_target(iter,5)
  ,tar_target(data_specs, {
    cc <- data.table::fread("./data/coefficients.txt")
    data_specs <- synthesizeDD(cc)
    return(data_specs)
  } )
  ,tar_target(null_data, sim_null_data(data_specs=data_specs,
                                       n=115698,
                                       N_time=4,
                                       reps = iter))
  ,tar_target(analysis,run_analyses_SL(data_list=null_data,
                                       SL.library="glm",
                                       det.Q.function=NULL,
                                       varmethod="ic",
                                       iter=iter))
  )
