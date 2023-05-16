library(tidyverse)
library(yaml)
library(tictoc)
library(igraph)

for (f in list.files("src", full.names = T)){
  print(f)
  source(f)
}

configs <- read_yaml("configs.yml")
args = commandArgs(trailingOnly = T)
if (length(args)==0){
  args = configs$default_args
} else {
  args <- as.list(args)
  names(args) <- names(configs$default_args)
  for (x in names(args)){
    if (grepl("^[0-9]+$", args[[x]])){
      args[[x]] <- as.numeric(args[[x]])
    }    
  }
}
print(paste(names(args), args, collapse = ", "))
dir.create(file.path("results",args$fldr), showWarnings = F)

set.seed(2945)

# load data
inputs <- readRDS(file.path("inputData","model_inputs_NC.RDS"))
plan_init <- inputs$seed_plans[[args$seed_num]]

# run sampler function
res <- run_sampler(plan_init, args$iter, args$progress)

out_name <- paste0("seed", args$seed_num, 
                   "_", args$iter, ".RDS")

saveRDS(res, file.path("results",args$fldr, out_name))
