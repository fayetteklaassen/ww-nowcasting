#!/usr/bin/env Rscript

library(rstan)
library(tidyverse)
library(glue)

args = commandArgs(trailingOnly=TRUE)

### This is obtained from the slurm scheduler, schedule an array of #configs * #stan files to run this script in separate nodes
runID <- as.numeric(args)

configs <- list.files("config")
stanfil <- list.files("stan")
tt <- expand.grid(configs,stanfil)
print(tt)
runID <- as.numeric(args)

print(runID)

# Script to run config and stan script ------------------------------------

cfg <- readRDS(paste0("config/", tt[runID,1]))
cfgrun <- cfg$config
cfgname <- cfg$shed

if(! grepl("ww", tt[runID,2]) & grepl("ts", tt[runID,1])) {

}else{
model_code <- read_file(paste0("stan/",tt[runID,2]))
model <- stan_model(model_code = model_code)

rstan::sampling(
  object  = model,
  data    = cfgrun,
  cores   = 3,
  control = list(adapt_delta = .98, max_treedepth = 15),
  seed    = 12345678,
  chains  = 3,
  iter    = 4000,
  verbose = TRUE,
  thin    = 1,
  warmup  = round((2/3)*4000)) -> result

result <- list(id = cfgname,
               config = as.character(tt[runID,1]),
               stanfile = as.character(tt[runID,2]),
fullres = result,               
result = rstan::summary(result)$summary)

saveRDS(result, paste0("result/", tt[runID,1], "-",tt[runID,2],".RDS"))
}


