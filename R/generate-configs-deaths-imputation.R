library(tidyverse)
devtools::install("../covidestim")
devtools::load_all("../covidestim") ### make sure branch wastewater and latest installation!

# Wastewater data ---------------------------------------------------------

ww <- read_csv("data-products/covidestim-dataframe.csv") %>%
  mutate(observed = if_else(is.na(logratio), 0, 1)) %>%
  rename(ratiologs = logratio) %>%
  filter(date > as.Date("2020-08-16")) %>%
  transmute(date, cases, deaths= deaths2, ## #use the alternate imputation method for deaths! 
            N1, N1_ts,
            logN1, logN1_ts,
            ratio, ratio_ts, observed, RR, shedID) %>%
  replace(is.na(.), 0) 

shedPop <- read_csv("data-products/shedPop.csv") %>%
  summarize(shedID = "total", pop = sum(pop)) %>%
  rbind(read_csv("data-products/shedPop.csv"))

shedIDs <- c("1","2", "3","4","5","total")
weeks <- c(4)
prep0 <- c(0)
wwnames <- c("N1")
minWeeks <- ww %>% group_by(shedID) %>%
  summarize(fitweeks = c(seq.Date(min(date) + 28,
                                  max(date) - 7, 
                                  by = '4 weeks'),
                         max(date)))

for(i in shedIDs){
  fitweeks <- minWeeks %>% ungroup() %>%
    filter(shedID == i) %>%
    pull(fitweeks)
  for(j in length(fitweeks)){
    for(n in wwnames){
      d_cases <- dplyr::select(ww %>% filter(shedID == i) %>% filter(date <= fitweeks[j]), date, observation = cases)
      d_deaths <- dplyr::select(ww %>% filter(shedID == i) %>% filter(date <= fitweeks[j]), date, observation = deaths)
      d_rr <- dplyr::select(ww %>% filter(shedID == i) %>% filter(date <= fitweeks[j]), date, observation = RR)
      d_ww <- dplyr::select(ww %>% filter(shedID == i) %>% filter(date <= fitweeks[j]), date, observation = N1)
      d_obs <- dplyr::select(ww %>% filter(shedID == i) %>% filter(date <= fitweeks[j]), date, observation = observed)
      cfg_sewer <- covidestim(nweeks    = nrow(ww %>% filter(shedID == i) %>% filter(date <= fitweeks[j]) ),
                              seed     = 45,#sample.int(.Machine$integer.max, 1),
                              region   = "21111",
                              pop_size = shedPop %>% filter(shedID == i)%>% pull(pop),
                              nweeks_before = 4) +
        input_cases(d_cases) +
        input_deaths(d_deaths) +
        input_rr(d_rr) +
        input_ww(d_ww)
      tmp <- ww %>% filter(shedID == i) %>% filter(date <= fitweeks[j]) %>% pull(all_of(n))
      tmp[which(tmp > 0)] <- 1
      cfg_sewer$config$observed_ww <- tmp
      
      cfg_sewer$config$obs_ww <- ww %>% filter(shedID == i) %>% filter(date <= fitweeks[j]) %>% pull(all_of(n))
      cfg_sewer$config$pre_period_zero <- prep0
      cfg_sewer$shed <- paste0("shed",i)
      
      saveRDS(cfg_sewer, paste0("config-deaths/ww-",n,"-months",j,"-shed",i,".RDS"))
    }
  }
}

