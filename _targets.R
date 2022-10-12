packs <- c("targets",
           "heaven",
           "foreach",
           "parallel",
           "data.table",
           "tidyverse",
           "lava")
library(targets)
targets::tar_option_set(packages = packs)
for(f in list.files("functions",".R$",full.names=TRUE)){source(f)}

#source functions from functions folder
for(f in list.files("functions",".R$",full.names=TRUE)){source(f)}

list(
  tar_target(data_specs, {
    cc <- data.table::fread("./data/coefficients.txt")
    data_specs <- synthesizeDD(cc)
    return(data_specs)
  } )
  ,tar_target(null_data, sim_null_data(data_specs=data_specs,
                                       n=115698,
                                       N_time=11,
                                       reps = 10))
)
