# BFLPE Analysis ####
bflpe_meta <- function(data, est = 'index', cores = 1){
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
  
  m_between_fixed <- brm(var_est|se(var_se)~1,data = tmp, cores=cores,chains=4, iter = 6000)
  m_between_random <- brm(var_est|se(var_se)~1+(1|id),data = tmp, cores=cores,chains=4, iter = 6000)
  m_between_grade_random <- brm(var_est|se(var_se)~grade+(1|id),data = tmp, cores=cores,chains=4, iter = 6000)
  m_between_year_random <-  brm(var_est|se(var_se)~year+(1|id)+year,data = tmp, cores=cores,chains=4, iter = 6000)
  m_between_year_grade_random <- brm(var_est|se(var_se)~grade*year+(1|id),data = tmp, cores=cores,chains=4, iter = 6000)
  
  return(list(between_fixed = m_between_fixed,
              between_random =  m_between_random,
              between_grade_random = m_between_grade_random,
              between_year_random = m_between_year_random,
              between_year_grade_random = m_between_year_grade_random)
  )
}

# ICC Analysis ####
icc_meta <- function(data, cores = 1){
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
  
  m_between_fixed <- brm(var_est|se(var_se)~1,data = tmp, cores=cores,chains=4, iter = 6000)
  m_between_random <- brm(var_est|se(var_se)~1+(1|id),data = tmp, cores=cores,chains=4, iter = 6000)
  m_between_grade_random <- brm(var_est|se(var_se)~grade+(1|id),data = tmp, cores=cores,chains=4, iter = 6000)
  m_between_year_random <-  brm(var_est|se(var_se)~year+(1|id)+year,data = tmp, cores=cores,chains=4, iter = 6000)
  m_between_year_grade_random <- brm(var_est|se(var_se)~grade*year+(1|id),data = tmp, cores=cores,chains=4, iter = 6000)
  
  return(list(between_fixed = m_between_fixed,
              between_random =  m_between_random,
              between_grade_random = m_between_grade_random,
              between_year_random = m_between_year_random,
              between_year_grade_random = m_between_year_grade_random)
  )
}


# Correlations ####
cor <- function(data, est = 'index'){
  v = glue("bf_est_{est}")
  cor_overall = data %>%
    select(v = all_of(v), icc_est) %>%
    summarize(tidy(cor.test(.$v, .$icc_est))) %>%
    mutate(year = "Overall", grade = NA_character_,.before = estimate)
  
  cor_group = data %>%
    select(v = all_of(v), icc_est, year, grade) %>%
    nest_by(year, grade) %>%
    mutate(mod = list(cor.test(data$v, data$icc_est))) %>%
    dplyr::summarise(tidy(mod))
  
  cor = bind_rows(cor_overall, cor_group) %>%
    select(year:estimate, conf.low,conf.high)
  
  return(cor)
  
}

# Main Analysis ####
main_meta <- function(data, est = 'index', cores = 1){
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
  
  m_between_fixed <- brm(var_est|se(var_se)~icc_est,data = tmp, cores=cores,chains=4, iter = 6000)
  m_between_random <- brm(var_est|se(var_se)~icc_est+(1|id),data = tmp, cores=cores,chains=4, iter = 6000)
  m_within_random <- brm(var_est|se(var_se)~icc_est+(1|id)+iso,data = tmp, cores=cores,chains=4, iter = 6000)
  m_within_grade_random <- brm(var_est|se(var_se)~icc_est*grade+(1|id)+iso,data = tmp, cores=cores,chains=4, iter = 6000)
  m_within_year_random <-  brm(var_est|se(var_se)~icc_est*year+(1|id)+iso,data = tmp, cores=cores,chains=4, iter = 6000)
  m_within_year_grade_random <- brm(var_est|se(var_se)~icc_est*grade*year+(1|id)+iso,data = tmp, cores=cores,chains=4, iter = 6000)
  
  return(list(between_fixed = m_between_fixed,
              between_random =  m_between_random,
              within_random = m_within_random,
              within_grade_random = m_within_grade_random,
              within_year_random = m_within_year_random,
              within_year_grade_random = m_within_year_grade_random)
  )
}
  
  
