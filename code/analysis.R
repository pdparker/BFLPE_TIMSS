# BFLPE Analysis ####
bflpe_meta <- function(data, est = 'index'){
  v = glue("bf_est_{est}")
  s = glue("bf_se_{est}")
  tmp = data %>%
    dplyr::select(var_est = all_of(v), var_se = all_of(s),
                  year, grade, iso) %>%
    rowid_to_column(var = "id") %>%
    mutate(year = case_when(
      year == '03' ~ 1,
      year == '07' ~ 2,
      year == '11' ~ 3,
      year == '15' ~ 4,
    ) %>% as.numeric)
  
  m_between_fixed <- brm(var_est|se(var_se)~1,data = tmp, cores=4,chains=4, iter = 6000)
  m_between_random <- brm(var_est|se(var_se)~1+(1|id),data = tmp, cores=4,chains=4, iter = 6000)
  m_between_grade_random <- brm(var_est|se(var_se)~grade+(1|id),data = tmp, cores=4,chains=4, iter = 6000)
  m_between_year_random <-  brm(var_est|se(var_se)~year+(1|id)+iso+year,data = tmp, cores=4,chains=4, iter = 6000)
  m_between_year_grade_random <- brm(var_est|se(var_se)~grade*year+(1|id),data = tmp, cores=4,chains=4, iter = 6000)
  
  return(list(between_fixed = m_between_fixed,
              between_random =  m_between_random,
              between_grade_random = m_within_grade_random,
              between_year_random = m_within_year_random,
              between_year_grade_random = m_within_year_grade_random)
  )
}

# ICC Analysis ####
icc_meta <- function(data){
  tmp = data %>%
    dplyr::select(var_est = icc_est, var_se = icc_se,
                  year, grade, iso) %>%
    rowid_to_column(var = "id") %>%
    mutate(year = case_when(
      year == '03' ~ 1,
      year == '07' ~ 2,
      year == '11' ~ 3,
      year == '15' ~ 4,
    ) %>% as.numeric)
  
  m_between_fixed <- brm(var_est|se(var_se)~1,data = tmp, cores=4,chains=4, iter = 6000)
  m_between_random <- brm(var_est|se(var_se)~1+(1|id),data = tmp, cores=4,chains=4, iter = 6000)
  m_between_grade_random <- brm(var_est|se(var_se)~grade+(1|id),data = tmp, cores=4,chains=4, iter = 6000)
  m_between_year_random <-  brm(var_est|se(var_se)~year+(1|id)+iso+year,data = tmp, cores=4,chains=4, iter = 6000)
  m_between_year_grade_random <- brm(var_est|se(var_se)~grade*year+(1|id),data = tmp, cores=4,chains=4, iter = 6000)
  
  return(list(between_fixed = m_between_fixed,
              between_random =  m_between_random,
              between_grade_random = m_within_grade_random,
              between_year_random = m_within_year_random,
              between_year_grade_random = m_within_year_grade_random)
  )
}


# Main Analysis ####
main_meta <- function(data, est = 'index', scale=TRUE){
  v = glue("bf_est_{est}")
  s = glue("bf_se_{est}")
  tmp = data %>%
    dplyr::select(var_est = all_of(v), var_se = all_of(s),
                  year, grade, iso, icc_est, icc_se) %>%
    rowid_to_column(var = "id") %>%
    mutate(year = case_when(
      year == '03' ~ 1,
      year == '07' ~ 2,
      year == '11' ~ 3,
      year == '15' ~ 4,
    ) %>% as.numeric)
  
  if(scale) {
    #sd_x = Hmisc::wtd.var(tmp$icc_est, weights = 1/tmp$icc_se,na.rm=TRUE) %>% sqrt
    #sd_y = Hmisc::wtd.var(tmp$var_est, weights = 1/tmp$var_se,na.rm=TRUE) %>% sqrt
    sd_x = sd(tmp$icc_est, na.rm=TRUE) 
    sd_y = sd(tmp$var_est, na.rm=TRUE) 
  }
  
  m_between_fixed <- brm(var_est|se(var_se)~icc_est,data = tmp, cores=4,chains=4, iter = 6000)
  m_between_random <- brm(var_est|se(var_se)~icc_est+(1|id),data = tmp, cores=4,chains=4, iter = 6000)
  m_within_random <- brm(var_est|se(var_se)~icc_est+(1|id)+iso,data = tmp, cores=4,chains=4, iter = 6000)
  m_within_grade_random <- brm(var_est|se(var_se)~icc_est+(1|id)+iso+grade,data = tmp, cores=4,chains=4, iter = 6000)
  m_within_year_random <-  brm(var_est|se(var_se)~icc_est+(1|id)+iso+year,data = tmp, cores=4,chains=4, iter = 6000)
  m_within_year_grade_random <- brm(var_est|se(var_se)~icc_est+(1|id)+iso+grade*year,data = tmp, cores=4,chains=4, iter = 6000)
  
  return(list(between_fixed = m_between_fixed,
              between_random =  m_between_random,
              within_random = m_within_random,
              within_grade_random = m_within_grade_random,
              within_year_random = m_within_year_random,
              within_year_grade_random = m_within_year_grade_random)
  )
}

summary(m_within_year_grade_random)
pp_check(m_within_year_grade_random)
loo(m_within_year_grade_random)

posterior_samples(m_within_year_grade_random) %>%
  select(b_icc_est,b_gradeG8) %>%
  mutate(b_icc_est = (b_icc_est+b_gradeG8) * sd_x/sd_y) %>%
  dplyr::summarise(
    SMD = median(b_icc_est),
    SMD_lb = quantile(b_icc_est, probs = .025),
    SMD_ub = quantile(b_icc_est, probs = .975)
  )
  
  
  
