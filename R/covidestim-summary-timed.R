
# Covidestim summary functionality ----------------------------------------



summary_ww <- function(res) {
  
  # Used for dealing with indices and dates
  configname <- res$config
  print(configname)
  ccr <- readRDS(paste0("config-final/", configname))
  config <- ccr$config
  nweeks        <- config$N_weeks
  nweeks_before <- config$N_weeks_before
  nweeks_total  <- nweeks_before + nweeks
  start_date    <- as.Date("2020-08-16") +14
  
  
  toDate <- function(idx) {
    start_date + lubridate::weeks(idx - 1 - nweeks_before)
  }   
  # Dates an index and converts it to a date
  
  # Mappings between names in Stan and variable names in the output `df`
  c(
    "deaths"                                 = "deaths",
    "deaths_of_diagnosed"                    = "deaths_of_diagnosed",
    "diagnoses"                              = "diagnoses",
    "diagnoses_of_symptomatic"               = "diagnoses_of_symptomatic",
    "effective_protection_inf_prvl"          = "effective_protection_inf_prvl",
    "effective_protection_inf_vax_boost_prvl"= "effective_protection_inf_vax_boost_prvl",
    "effective_protection_inf_vax_prvl"      = "effective_protection_inf_vax_prvl",
    "effective_protection_vax_boost_prvl"    = "effective_protection_vax_boost_prvl",
    "effective_protection_vax_prvl"          = "effective_protection_vax_prvl",
    "fitted_cases"                           = "fitted_cases",
    "fitted_deaths"                          = "fitted_deaths",
    "fitted_hospitalizations"                = "fitted_hospitalizations",
    "fitted_wastewater_prvl"                 = "fitted_wastewater_prvl",
    "immunoexposed_cumulative"               = "immunoexposed_cumulative",
    "infections_cumulative"                  = "infections_cumulative",
    "infections"                             = "infections",
    "infections_premiere"                    = "infections_premiere",
    "pop_infectiousness_prvl"                = "pop_infectiousness_prvl",
    "r_t"                                    = "r_t",
    "seropositive_prvl"                      = "seropositive_prvl",
    "severe"                                 = "severe",
    "susceptible_prvl"                       = "susceptible_prvl",
    "susceptible_severe_prvl"                = "susceptible_severe_prvl",
    "symptomatic"                            = "symptomatic"
  ) -> params
  
  # Used for renaming quantiles output by Stan
  quantile_names <- c(
    "mean" = "_mn",
    "2.5%"  = "_p2_5",
    # "25%"   = "_p25",
    "50%"   = "",
    # "75%"   = "_p75",
    "97.5%" = "_p97_5"
  )
  
  # This creates the list of `pars` that gets passed to `rstan::summary`
  # by enumerating all combs of varnames and indices
  purrr::cross2(names(params), as.character(1:nweeks_total)) %>% 
    purrr::map_chr(
      function(item)
        glue::glue("{par}[{idx}]", par = item[[1]], idx = item[[2]])
    ) -> pars
  
  res$result %>% 
    as.data.frame %>%
    dplyr::select("mean", "2.5%", "50%", "97.5%") %>%
    drop_na() %>%
    tibble::as_tibble(rownames = "parname" ) -> melted
  
  # These are the variables that are going to be selected from the melted
  # representation created above
  vars_of_interest <- c("variable", "date", names(quantile_names))
  
  # Join the melted representation to the array indices that have been split
  # into their name:idx components
  stan_extracts <- dplyr::left_join(
    melted,
    split_array_indexing(melted$parname),
    by = "parname"
  ) %>%
    drop_na(index) %>%
    filter(index <= nweeks_total) %>%
    # Reformat the dates, and rename some of the variable names
    dplyr::mutate(date = toDate(index)) %>%
    # Eliminate things like R-hat that we don't care about right now
    dplyr::select_at(vars_of_interest) %>%
    # Melt things more to get down to three columns
    tidyr::gather(names(quantile_names), key = "quantile", value = "value") %>%
    # Create the finalized names for the quantiles, and delete the now-unneeded
    # quantile variable
    dplyr::mutate(
      variable = paste0(variable, quantile_names[I(quantile)]),
      quantile = NULL
    ) %>%
    # Cast everything back out
    tidyr::spread(key = "variable", value = "value") 
  
  stan_extracts
  
}

split_array_indexing <- function(elnames) {
  
  # Matches a valid indexed variable in Stan, capturing it into two groups
  regex <- '^([A-Za-z_][A-Za-z_0-9]+)\\[([0-9]+)\\]$'
  
  # Match everything into a data.frame
  captured <- stringr::str_match(elnames, regex)
  captured <- as.data.frame(captured, stringsAsFactors = FALSE)
  
  # Rename cols, coerce the index to a number
  colnames(captured) <- c("parname", "variable", "index")
  captured$index <- as.numeric(captured$index)
  
  captured
}

