
# Results -----------------------------------------------------------------

### First, after importing, all files need to be renamed to remove the .RDS and .stan suffixes
### Then, read in all the results

library(tidyverse)
fnames <- list.files("result-deaths", full.names = TRUE, pattern = ".RDS")
source("R/covidestim-summary-timed.R")


out <- data.frame()
outss <- data.frame()
for(i in fnames){
  tmp <- readRDS(i)
  res2 <- summary_ww(tmp)
  id  <- tmp$id
  model <- tmp$stanfile
  config <- tmp$config
  out <- rbind(out, res2 %>%
                 mutate(id = id, model = model, config = config))
  if("fullres" %in% names(tmp)){
    tmpss <- rstan::extract(tmp$fullres)
    outss <- rbind(outss,
                   as_tibble(tmpss$fitted_wastewater_prvl, rownames = "iter") %>%
                     mutate(par = "ww") %>%
                     rbind(as.tibble(tmpss$r_t, rownames = "iter") %>%
                             mutate(par = "rt")) %>%
                     rbind(as.tibble(tmpss$infections, rownames = "iter") %>%
                             mutate(par = "infections")) %>%
                     rbind(as.tibble(tmpss$deaths, rownames = "iter") %>%
                             mutate(par = "deaths")) %>%
                     pivot_longer(-c(iter, par), names_to = "week",
                                  values_to = "value",
                                  names_prefix = "V") %>%
                     pivot_wider(id_cols = c("week", "par"),
                                 names_from = "iter",
                                 values_from = "value") %>%
                     mutate(id = id,
                            model = model,
                            config = config))
  }
}
write_csv(out, "result-deaths/result.csv")
write_csv(outss, "result-deaths/result-ss.csv")



