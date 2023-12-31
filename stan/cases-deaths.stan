functions {
  vector conv1d(vector x, vector kernel) {
    int nk = rows(kernel);
    int nx = rows(x);
    matrix[nx, nk] X;

    if (nx < nk)
      reject("nrow(x) must be >= nrow(kernel). x had nrow =", nx);
    for (i in 1:nk) {
      if (i < nk)
        X[1:(nk - i), i] = rep_vector(0, nk - i);
      X[(nk - i + 1):nx, i] = x[1:(nx - nk + i)];
    }

    return X * kernel;
  }

}

data {
  // INPUT DATA
  int<lower=0>           N_weeks; // weeks of data
  int<lower=0>           N_weeks_before; // weeks before data to init epi model
  int<lower=0>           Max_delay; // maximum days delay 
  int<lower=0>           obs_cas[N_weeks]; // vector of cases
  int<lower=0>           obs_die[N_weeks]; // vector of deaths
  vector[N_weeks]    obs_ww; // vector of wastewater data
  real<lower=0>          pop_size; // population size
  
  int<lower=0>           N_ifr_adj; // length of ifr_adjustment
  vector<lower=0>[N_ifr_adj] ifr_adj; // ifr_adjustment
  vector<lower=0>[N_weeks+N_weeks_before] ifr_vac_adj; // ifr_vaccine_adjustment
  real<lower=0>          pri_ifr_decl_OR_a; 
  real<lower=0>          pri_ifr_decl_OR_b;
  real<lower=0>          pri_rr_decl_sev_a;
  real<lower=0>          pri_rr_decl_sev_b;
  real<lower=0>          pri_rr_decl_die_a;
  real<lower=0>          pri_rr_decl_die_b;
  real<lower=0>          ifr_adj_fixed;
  
  real<lower=0>          infect_dist_rate;
  real<lower=0>          infect_dist_shap;
  real<lower=0>          seropos_dist_rate;
  real<lower=0>          seropos_dist_shap;

  // terms for splines
  // spline parameters and bases
  int<lower=0>                              N_spl_par_rt;
  matrix[N_weeks+N_weeks_before,N_spl_par_rt] spl_basis_rt;
  int<lower=0>                              N_spl_par_dx;
  matrix[N_weeks+N_weeks_before,N_spl_par_dx] spl_basis_dx;

  // fixed delay distributions
  // fixed delay distribtions. time from inf -> sym, sym -> sev, sev -> die
  real<lower=0>          inf_prg_delay_shap; 
  real<lower=0>          inf_prg_delay_rate;
  real<lower=0>          asy_rec_delay_shap;
  real<lower=0>          asy_rec_delay_rate;
  real<lower=0>          sym_prg_delay_shap;
  real<lower=0>          sym_prg_delay_rate;
  real<lower=0>          sev_prg_delay_shap;
  real<lower=0>          sev_prg_delay_rate;

  // fixed delay from diagnosis to report
  real<lower=0>          cas_rep_delay_shap;
  real<lower=0>          cas_rep_delay_rate;
  real<lower=0>          die_rep_delay_shap;
  real<lower=0>          die_rep_delay_rate;
  
  //// control nobs
  // whether to assume zero cases and deaths during warm-up
  int<lower = 0, upper = 1> pre_period_zero; 

  //  what data are included --  cases, deaths: 
  int<lower = 0, upper = 1> cas_yes; 
  int<lower = 0, upper = 1> die_yes; 

  //  how are the data dated -- report, occurrence: 
  int<lower = 0, upper = 1> obs_cas_rep;
  int<lower = 0, upper = 1> obs_die_rep;
  int<lower = 0, upper = 1> observed_ww[N_weeks];

  //  how many days should be used for the moving average in the likelihood 
  //  function? 
  // is there a last obeserved deaths data day?
  int<lower=0> lastDeathWeek;
    // is there a last obeserved hospitalizations data day?
  // is there a last obeserved case data day?
  int<lower=0> lastCaseWeek;

  /////////
  // TERMS FOR PRIOR DISTRIBTUIONS
  // for new infections
  real          pri_log_infections_0_mu;
  real<lower=0> pri_log_infections_0_sd;
  real          pri_logRt_mu;   
  real<lower=0> pri_logRt_sd;   
  real<lower=0> pri_serial_i_shap; 
  real<lower=0> pri_serial_i_rate; 
  real<lower=0> pri_deriv1_spl_par_sd;
  real<lower=0> pri_deriv2_spl_par_sd;
  
  // probabilities of progression inf -> sym -> sev -> die
  real<lower=0> pri_p_sym_if_inf_a; 
  real<lower=0> pri_p_sym_if_inf_b;
  real<lower=0> pri_new_p_sym_if_inf_a; 
  real<lower=0> pri_new_p_sym_if_inf_b;
  real<lower=0> pri_p_sev_if_sym_a;
  real<lower=0> pri_p_sev_if_sym_b;
  real<lower=0> pri_p_die_if_sev_a;
  real<lower=0> pri_p_die_if_sev_b;

  // overall case fatality rate
  real<lower=0> pri_p_die_if_inf_a;
  real<lower=0> pri_p_die_if_inf_b;

  // probabilities of diagnosis 
     // rate ratio, pr(dx) asymptomatic to symptomatic
  real<lower=0> pri_rr_diag_asy_vs_sym_a; 
  real<lower=0> pri_rr_diag_asy_vs_sym_b;

      // rate ratio, pr(dx) symptomatic to severe
  real<lower=0> pri_rr_diag_sym_vs_sev_a; 
  real<lower=0> pri_rr_diag_sym_vs_sev_b;

     // probability of diagnosis at severe 
  real<lower=0> pri_p_diag_if_sev_a;
  real<lower=0> pri_p_diag_if_sev_b;

  // delay to diagnosis assumed to be some fraction of progression delay
  // Beta prior distribtuion for that fraction 
  real<lower=0> scale_dx_delay_sym_a; 
  real<lower=0> scale_dx_delay_sym_b; 
  real<lower=0> scale_dx_delay_sev_a; 
  real<lower=0> scale_dx_delay_sev_b;

}
///////////////////////////////////////////////////////////
transformed data {
  // int  N_days_tot;
  int  N_weeks_tot;

  // Moving sums
  int<lower=0>           obs_cas_mvs[N_weeks]; // vector of cases
  int<lower=0>           obs_die_mvs[N_weeks]; // vector of deaths
  vector[N_weeks]           obs_ww_mvs; // vector of wastewater
  // Progression delays
  vector[Max_delay]  inf_prg_delay_rv;
  vector[Max_delay]  asy_rec_delay_rv; 
  vector[Max_delay]  sym_prg_delay_rv;
  vector[Max_delay]  sev_prg_delay_rv;
 
  // Reporting delays
  vector[Max_delay]  cas_rep_delay_rv;
  vector[Max_delay]  die_rep_delay_rv;
    vector[Max_delay]   infect_dist_rv;
 
  // Cumulative reporting delays
  vector[N_weeks + N_weeks_before]  cas_cum_report_delay_rv; 
  vector[N_weeks + N_weeks_before]  die_cum_report_delay_rv; 

  
   int  idx1[N_weeks + N_weeks_before];
 int  idx2[N_weeks + N_weeks_before];
 vector[N_weeks + N_weeks_before] idx3;
  N_weeks_tot = N_weeks + N_weeks_before; 
  // Indexes for convolutions
for(i in 1:N_weeks_tot) {
  if(i-Max_delay>0){
    idx1[i] = i-Max_delay+1;
    idx2[i] = 1;
  } else {
    idx1[i] = 1;
    idx2[i] = Max_delay-i+1;
  }
  if(i < (N_weeks_tot - 1)){ 
  idx3[i] = N_weeks_tot-1-i;
  } else {
    idx3[i] = 1;
  }
}

  // compute the moving sums
  for(i in 1:N_weeks) {
    obs_cas_mvs[i] = obs_cas[i];
    obs_die_mvs[i] = obs_die[i];
    obs_ww_mvs[i] = obs_ww[i];
  }
 
  // calculate the daily probability of transitioning to a new disease state
  // for days 1 to 60 after entering that state
  for(i in 1:Max_delay) {
    inf_prg_delay_rv[1+Max_delay-i] =
      gamma_cdf(i   , inf_prg_delay_shap, inf_prg_delay_rate) -
      gamma_cdf(i-1 , inf_prg_delay_shap, inf_prg_delay_rate);

    asy_rec_delay_rv[1+Max_delay-i] =
      gamma_cdf(i   ,  asy_rec_delay_shap, asy_rec_delay_rate*2) -
      gamma_cdf(i-1 , asy_rec_delay_shap, asy_rec_delay_rate*2); 

    // diagnosis happens, on average, midway through the infectious period
    // therefore, we multiple the rate parameter by 2
    sym_prg_delay_rv[1+Max_delay-i] =
      gamma_cdf(i   , sym_prg_delay_shap, sym_prg_delay_rate) -
      gamma_cdf(i-1 , sym_prg_delay_shap, sym_prg_delay_rate);

    sev_prg_delay_rv[1+Max_delay-i] =
      gamma_cdf(i   , sev_prg_delay_shap, sev_prg_delay_rate) -
      gamma_cdf(i-1 , sev_prg_delay_shap, sev_prg_delay_rate);
  }
  
   // vector to distribute infectiousness
  for(i in 1:Max_delay)
    infect_dist_rv[1+Max_delay-i] =
      gamma_cdf(i   , infect_dist_shap, infect_dist_rate) -
      gamma_cdf(i-1 , infect_dist_shap, infect_dist_rate);
      
  // Calcluate the probability of reporting for each day after diagnosis
  // for 1 to 60 post diagnosis. 
  for(i in 1:Max_delay) {
    cas_rep_delay_rv[1+Max_delay-i] = 
      gamma_cdf(i   , cas_rep_delay_shap, cas_rep_delay_rate) -
      gamma_cdf(i-1 , cas_rep_delay_shap, cas_rep_delay_rate);

    die_rep_delay_rv[1+Max_delay-i] =
      gamma_cdf(i   , die_rep_delay_shap, die_rep_delay_rate) -
      gamma_cdf(i-1 , die_rep_delay_shap, die_rep_delay_rate);
  }
  
   // Make sure sum to 1
  inf_prg_delay_rv = inf_prg_delay_rv/sum(inf_prg_delay_rv);
  asy_rec_delay_rv = asy_rec_delay_rv/sum(asy_rec_delay_rv);
  sym_prg_delay_rv = sym_prg_delay_rv/sum(sym_prg_delay_rv);
  sev_prg_delay_rv = sev_prg_delay_rv/sum(sev_prg_delay_rv);
  cas_rep_delay_rv = cas_rep_delay_rv/sum(cas_rep_delay_rv);
  die_rep_delay_rv = die_rep_delay_rv/sum(die_rep_delay_rv);

  // Cumulative reporting probability
   for(i in 1:N_weeks_tot) {
    if(i < Max_delay){
      cas_cum_report_delay_rv[1+N_weeks_tot-i] = gamma_cdf(i , cas_rep_delay_shap, cas_rep_delay_rate);
      die_cum_report_delay_rv[1+N_weeks_tot-i] = gamma_cdf(i , die_rep_delay_shap, die_rep_delay_rate);
    } else {
      cas_cum_report_delay_rv[1+N_weeks_tot-i] = 1.0;
      die_cum_report_delay_rv[1+N_weeks_tot-i] = 1.0;
    }
  }
}
///////////////////////////////////////////////////////////
parameters {
  
// INCIDENCE 
  real                    log_infections_0; // starting intercept
  real serial_i; // serial interval
  vector[N_spl_par_rt]    spl_par_rt;

// DISEASE PROGRESSION
// probability of transitioning between disease states
  real<lower=0, upper=1>    p_sym_if_inf;
  real<lower=0, upper=1>    new_p_sym_if_inf;
  real<lower=0, upper=1>    p_sev_if_sym;
  real<lower=0, upper=1>    p_die_if_sev;
  real<lower=0>             ifr_decl_OR;
  
// DIANGOSIS
// scaling factor for time to diagnosis
  real<lower=0, upper=1>    scale_dx_delay_sym; 
  real<lower=0, upper=1>    scale_dx_delay_sev; 
// probability of diagnosis at each illness state
  real<lower=0, upper=1>    rr_diag_asy_vs_sym; 
  real<lower=0, upper=1>    p_diag_if_sev;
  vector<lower=0, upper=1>[N_spl_par_dx]  spl_par_sym_dx;

// LIKELIHOOD 
// phi terms for negative b ino imal likelihood function 
  real<lower=0>             inv_sqrt_phi_c;
  real<lower=0>             inv_sqrt_phi_d;
  // VACCINE ADJUSTMENT
  simplex[3]                prob_vac;
  // regression for wastewater
  real                      a;
  real                      b;
  real<lower=0>             sigma;
}
///////////////////////////////////////////
transformed parameters {
  ///~~~~~~~ Define ~~~~~~~
  // INCIDENCE
  vector[N_weeks_tot]      log_infections;
  vector[N_weeks_tot]      deriv1_log_infections;
  vector[N_weeks_tot]      infections;
  real                    ever_inf;
  vector[N_weeks_tot]     susceptible_prvl;
  vector[N_weeks_tot]     population_protection_init;
  vector[N_weeks_tot]      population_protection_inf;
  vector[N_weeks_tot]      population_protection_boost;
  vector[N_weeks_tot]      effective_protection_prvl;

  // Rt spline
  vector[N_weeks_tot]      logRt0;
  vector[N_weeks_tot]      logRt;
  vector[N_weeks_tot]      r_t;
  vector[N_spl_par_rt-1]  deriv1_spl_par_rt;
  vector[N_spl_par_rt-2]  deriv2_spl_par_rt;
  
  // transitions
  vector[N_ifr_adj]   p_die_if_sevt;
  vector[N_weeks_tot]  p_sev_if_symt;
  vector[N_weeks_tot]  p_sym_if_inft;  
  
  // DIAGNOSIS AND REPORTING  
 // probability of diagnosis
  vector[N_weeks_tot]  rr_diag_sym_vs_sev;
  vector[N_weeks_tot]  p_diag_if_asy; 
  vector[N_weeks_tot]  p_diag_if_sym;

 // daily probabilities of diagnosis and report
 // for days 1 to 60 after entering that state
  vector[Max_delay]  sym_diag_delay_rv;
  vector[Max_delay]  sev_diag_delay_rv;

  // DISEASE OUTCOMES
  // overall case fatality rate
  real                p_die_if_inf;

  // "true" number entering disease state each day
  vector[N_weeks_tot]  symptomatic; 
  vector[N_weeks_tot]  severe;
  vector[N_weeks_tot]  deaths;

  // newly diagnosed
  vector[N_weeks_tot]  new_asy_dx; 
  vector[N_weeks_tot]  diagnoses_of_symptomatic; 
  vector[N_weeks_tot]  diagnoses_severe;

  // follow diagnosed cases forward to calculate deaths among diagnosed
  vector[N_weeks_tot]  dx_sym_sev; 
  vector[N_weeks_tot]  dx_sym_die; 
  vector[N_weeks_tot]  dx_sev_die; 

  // sum to diagnosed cases and deaths
  vector[N_weeks_tot]  diagnoses;
  vector[N_weeks_tot]  deaths_of_diagnosed;

  // number of cases and deaths in official record on each day
  // (all diagnosed cases with an additional delay to report) 
  vector[N_weeks_tot]  fitted_cases;
  vector[N_weeks_tot]  fitted_deaths; 
  // moving sum cases and deaths
  vector[N_weeks_tot]  fitted_cases_mvs;
  vector[N_weeks_tot]  fitted_deaths_mvs; 
  vector[N_weeks_tot]  fitted_wastewater_prvl;



  // LIKELIHOOD
  // phi terms for negative binomial likelihood function 
  real phi_cas;
  real phi_die;
 
  // NATURAL HISTORY CASCADE
  p_die_if_sevt = p_die_if_sev * ifr_adj_fixed * (1 + ifr_adj * ifr_decl_OR);

  // for( i in 1:N_days_tot){
  for( i in 1:N_weeks_tot){
  p_die_if_sevt[i]     = p_die_if_sevt[i]   .* pow(ifr_vac_adj[i], prob_vac[1]);
  p_sev_if_symt[i]     = p_sev_if_sym        * pow(ifr_vac_adj[i], prob_vac[2]);
  p_sym_if_inft[i]     = p_sym_if_inf        * pow(ifr_vac_adj[i], prob_vac[3]);
  }
  
  // DIAGNOSIS // 
  // rate ratio of diagnosis at asymptomatic vs symptomatic, symptomatic vs severe
  rr_diag_sym_vs_sev = inv_logit(spl_basis_dx * logit(spl_par_sym_dx));
  
  // probability of diagnosis 
  p_diag_if_sym = p_diag_if_sev * rr_diag_sym_vs_sev;
  p_diag_if_asy = p_diag_if_sym * rr_diag_asy_vs_sym; 
  
  // DELAYS //
  // Diagnosis Delays
  // Calculate the probability of diagnosis for each day in state
  // for 1 to 60 days in state. We do this by scaling the rate term
  // of the gamma distribution of progressiong delays by a modeled fraction 
  // (scale_dx_delay_xxx)
  {
    // Use two vectors to store the results of the `gamma_cdf()` calls in order
    // to avoid double-computing them
    vector[Max_delay+1] sym_delay_gammas;
    vector[Max_delay+1] sev_delay_gammas;
    for (i in 1:Max_delay+1) {
      sym_delay_gammas[i] = gamma_cdf(i-1 , sym_prg_delay_shap, sym_prg_delay_rate/scale_dx_delay_sym);
      sev_delay_gammas[i] = gamma_cdf(i-1 , sev_prg_delay_shap, sev_prg_delay_rate/scale_dx_delay_sev);
    }

    // Diff and reverse, vectorized
    for(i in 1:Max_delay){
    sym_diag_delay_rv[1+Max_delay-i] = sym_delay_gammas[i+1] - sym_delay_gammas[i];
    sev_diag_delay_rv[1+Max_delay-i] = sev_delay_gammas[i+1] - sev_delay_gammas[i];
    }

  }

  // DEATHS // 
  // infection fatality rate is the product of the probability of death among
  // severely ill individuals, the probability of being severely ill if 
  // symptomatic, and the probability of becoming symptomatic if infected. 
  p_die_if_inf = p_sym_if_inf * p_sev_if_sym * p_die_if_sev;

  // CASCADE OF INCIDENT OUTCOMES ("TRUE") //

  // NEW INCIDENT CASES
  
  // modeled with a spline
  logRt0 = spl_basis_rt * spl_par_rt;
  
    effective_protection_prvl[1] = 0.0;
  susceptible_prvl[1] = pop_size - effective_protection_prvl[1];

  for(i in 1:N_weeks_tot) {
   if(i > 1){
       susceptible_prvl[i] = pop_size - effective_protection_prvl[i-1];
   }

    if (susceptible_prvl[i] < 1) {
      // print("WARNING susceptible reliminary value was ", susceptible_prvl[i]);
      susceptible_prvl[i] = 1;
    }
    logRt[i] = logRt0[i] + log(susceptible_prvl[i]/pop_size);

    deriv1_log_infections[i] = logRt[i]/serial_i;

    log_infections[i] = sum(deriv1_log_infections[1:i]) + log_infections_0;

    infections[i] = exp(log_infections[i]);

    if(i > 1) {
    effective_protection_prvl[i] = effective_protection_prvl[i-1] + infections[i];
    } else {
    effective_protection_prvl[i] = infections[i];
    }
  }
  
  r_t = exp(logRt); 
  
  // second derivative
  deriv2_spl_par_rt[1:(N_spl_par_rt-2)] =
    2 * spl_par_rt[2:(N_spl_par_rt-1)] - 
        spl_par_rt[1:(N_spl_par_rt-2)] -
        spl_par_rt[3:N_spl_par_rt]; 
  
  // first derivative
  deriv1_spl_par_rt[1:(N_spl_par_rt-1)] =
    spl_par_rt[2:N_spl_par_rt] - 
    spl_par_rt[1:(N_spl_par_rt-1)];

  // SYMPTOMATIC CASES
  // cases entering a state on day i + j - 1: 
  // cases entering previous state on day i * the probability of progression *
  
  symptomatic =
    p_sym_if_inft     .* conv1d(infections , inf_prg_delay_rv);

  severe = p_sev_if_symt               .* conv1d(symptomatic, sym_prg_delay_rv);
  deaths = p_die_if_sevt[1:N_weeks_tot] .* conv1d(severe, sev_prg_delay_rv);

  // CASCADE OF INCIDENT OUTCOMES (DIAGNOSED) //

  // diagnosed at asymptomatic
  // a diagnosed asymptomatic infection on day i + j - 1
  // is an asymptomatic case on day i with some probability of diagnosis, 
  // and some probability that the diagnosis occurred on day j.
  // we assume asymptomatic diagnosis only occurs among individuals who will be
  // asymptomatic for the entire course of their infection. 
  new_asy_dx = (1 - p_sym_if_inft) .* conv1d(
    infections .* p_diag_if_asy,
    asy_rec_delay_rv
  );
  
  // diagnosed at symptomatic
  // a diagnosed symptomatic (not severe) case on day i + j - 1
  // is a symptomatic case on day i with some probability of diagnosis
  // and some probability that the diagnosis occurred on day j.  
  diagnoses_of_symptomatic = conv1d(symptomatic .* p_diag_if_sym, sym_diag_delay_rv);
  
  // cascade from diagnosis 
  // follow diagnosed cases forward to determine how many cases diagnosed
  // at symptomatic eventually die 
  dx_sym_sev = p_sev_if_symt .* conv1d(
    symptomatic .* p_diag_if_sym,
    sym_prg_delay_rv
  );
        
  dx_sym_die = p_die_if_sevt[1:N_weeks_tot] .* conv1d(dx_sym_sev, sev_prg_delay_rv);
        
  // diagnosed at severe 
  // as above for symptomatic 
  diagnoses_severe = p_diag_if_sev * conv1d(severe - dx_sym_sev, sev_diag_delay_rv);
  
  // cascade from diagnosis
  // as above for symptomatic 
  dx_sev_die = p_diag_if_sev * p_die_if_sevt[1:N_weeks_tot] .* conv1d(
    severe - dx_sym_sev,
    sev_prg_delay_rv
  );

  // TOTAL DIAGNOSED CASES AND DEATHS //
  diagnoses   = new_asy_dx + diagnoses_of_symptomatic + diagnoses_severe;
  deaths_of_diagnosed = dx_sym_die + dx_sev_die;

  // REPORTING //
  // Calcluate "fitted_cases" and "fitted_deaths", which are vectors of diagnosed cases 
  // and deaths by the date we expect them to appear in the reported data. 

  // How reporting delays are reflected in the data depend on how the data are 
  // dated.
  //
  // For cases by date of occurrence, we assume all cases diagnosed more than
  // 60 days from the final day of data.
  if(obs_cas_rep == 1)
    fitted_cases = conv1d(diagnoses, cas_rep_delay_rv);
  else
    fitted_cases = diagnoses .* cas_cum_report_delay_rv;

// the hospitalizations data is reported by date of occurance;
// and does not have a reporting delay (the data is reported delayed and 
// is therefore always up to date). 
  // reporting delays modeled as described above for cases
  if(obs_die_rep == 1)
    fitted_deaths = conv1d(deaths_of_diagnosed, die_rep_delay_rv);
  else
    fitted_deaths = deaths_of_diagnosed .* die_cum_report_delay_rv;
    
  // compute moving sums
  for(i in 1:N_weeks_tot) {
        fitted_cases_mvs[i] = fitted_cases[i];
        fitted_deaths_mvs[i] = fitted_deaths[i];
  }
  
    // infectiousness
fitted_wastewater_prvl = a+b*(infections/pop_size);
  // phi
  phi_cas = pow(inv_sqrt_phi_c, -2);
  phi_die = pow(inv_sqrt_phi_d, -2);
}
///////////////////////////////////////////////////////////  
model {
  
  // PRIORS
  log_infections_0         ~ normal(pri_log_infections_0_mu, pri_log_infections_0_sd);
  spl_par_rt            ~ normal(pri_logRt_mu, pri_logRt_sd);
  serial_i              ~ gamma(pri_serial_i_shap, pri_serial_i_rate);
  deriv1_spl_par_rt     ~ normal(0, pri_deriv1_spl_par_sd);
  deriv2_spl_par_rt     ~ normal(0, pri_deriv2_spl_par_sd);

  // PRIORS: DISEASE PROGRESSION
  // probability of transitioning from inf -> sym -> sev -> die
  p_sym_if_inf         ~ beta(pri_p_sym_if_inf_a, pri_p_sym_if_inf_b);
  new_p_sym_if_inf     ~ beta(pri_new_p_sym_if_inf_a, pri_new_p_sym_if_inf_b);
  p_sev_if_sym         ~ beta(pri_p_sev_if_sym_a, pri_p_sev_if_sym_b);
  p_die_if_sev         ~ beta(pri_p_die_if_sev_a, pri_p_die_if_sev_b);
  ifr_decl_OR          ~ gamma(pri_ifr_decl_OR_a, pri_ifr_decl_OR_b);
  
  // PRIORS: overall infection fatality rate
  p_die_if_inf         ~ beta(pri_p_die_if_inf_a, pri_p_die_if_inf_b);

  // PRIORS: diagnosis    
  // probabilities of diagnosis
  rr_diag_asy_vs_sym   ~ beta(pri_rr_diag_asy_vs_sym_a, pri_rr_diag_asy_vs_sym_b);
  spl_par_sym_dx       ~ beta(pri_rr_diag_sym_vs_sev_a,pri_rr_diag_sym_vs_sev_b);
  p_diag_if_sev        ~ beta(pri_p_diag_if_sev_a, pri_p_diag_if_sev_b);

  // delay distribution scaling factors
  scale_dx_delay_sym   ~ beta(scale_dx_delay_sym_a, scale_dx_delay_sym_b); 
  scale_dx_delay_sev   ~ beta(scale_dx_delay_sev_a, scale_dx_delay_sev_b);
  
  // phi  
  inv_sqrt_phi_c       ~ normal(0, 1);
  inv_sqrt_phi_d       ~ normal(0, 1);
  a ~ normal(0,10);
  b ~ normal(1000,1000);
  sigma ~ cauchy(10,20);

  // prop for vaccine
  prob_vac             ~ dirichlet(rep_vector(5, 3));

   
  // LIKELIHOOD
  // Before data
  if(pre_period_zero==1){
    if(N_weeks_before>0){
  
      if (sum(fitted_cases[1:N_weeks_before]) < 0)
        reject("`sum(fitted_cases[1:N_weeks_before])` had a negative value");
      if (sum(fitted_deaths[1:N_weeks_before]) < 0)
        reject("`sum(fitted_deaths[1:N_weeks_before])` had a negative value");

      target += neg_binomial_2_lpmf( 0 | sum(fitted_cases[1:N_weeks_before]), phi_cas);
      target += neg_binomial_2_lpmf( 0 | sum(fitted_deaths[1:N_weeks_before]), phi_die);
    }
  } else { // if there is no pre-period zero
        if(N_weeks_before>0){
 
      if (fitted_cases[1] < 0)
        reject("`fitted_cases[1]` had a negative value");

      if (fitted_deaths[1] < 0)
        reject("`fitted_deaths[1]` had a negative value");
        
    }
  }

  if (min(fitted_cases) < 0)
    reject("`fitted_cases` had a negative value");

  if (min(fitted_deaths) < 0)
    reject("`fitted_deaths` had a negative value");
  // LIKELIHOOD
  // During data
 
   for(i in 1:N_weeks){
    if(i < lastDeathWeek) {
    target += neg_binomial_2_lpmf(obs_cas_mvs[i]|fitted_cases_mvs[N_weeks_before+i], phi_cas);
 if(i > 4) 
    target += neg_binomial_2_lpmf(obs_die_mvs[i]|fitted_deaths_mvs[N_weeks_before+i], phi_die);
  }
  }
}
///////////////////////////////////////////////////////////
generated quantities {
  // calculate cumulative incidence + seropositive + pop_infectiousness
  real                p_die_if_sym;

  vector[N_weeks_tot]  diag_cases;
  vector[N_weeks_tot]  infections_cumulative;  
  vector[N_weeks_tot]  seropositive_prvl;
  vector[N_weeks_tot]  pop_infectiousness_prvl;


  // cumulative incidence
  // cumulative incidence is only calculated for the data weeks! any prior infections are added through cum_p_inf_init.
  infections_cumulative[N_weeks_before+1:] = cumulative_sum(infections[N_weeks_before+1:]);
  
  p_die_if_sym = p_die_if_sev * p_sev_if_sym; 

  diag_cases = diagnoses_of_symptomatic + diagnoses_severe;
  
  // infectiousness
   pop_infectiousness_prvl = conv1d(infections, infect_dist_rv);
  
}

