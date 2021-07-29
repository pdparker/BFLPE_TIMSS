# Estimate Fixed Effects ####
#loadd(timss_raw)
fixed_effects <- function(data = timss_raw, sc = c("raw","idx"), poly = 3) {
  # Specify which self-concept measure to use
  sc = glue("msc{sc[1]}")
  # fit the model by grade, country and TIMSS cycle
  fit_fixed = data  %>%
    select(sc = all_of(sc), everything()) %>%
    group_by(grade, iso, year) %>%
    do(fit = tidy(plm(sc ~ poly(mach,poly) + percach, data = ., model = 'within',index = 'idschool', weights = houwgt)) )
  #extract the coefficents
  fit_coef <- fit_fixed %>% unnest()
  return(fit_coef)
}

random_bflpe <- function(data = timss_raw, sc = c("raw","idx"), poly = 1) {
  # Specify which self-concept measure to use
  sc = glue("msc{sc[1]}")
  # fit the model by grade, country and TIMSS cycle
  fit_fixed = data  %>%
    mutate(schach = z(schach)) %>%
    select(sc = all_of(sc), everything()) %>%
    group_by(grade, iso, year) %>%
    do(fit = tidy(plm(sc ~ poly(mach,poly) + schach, data = ., model = 'random',index = 'idschool', weights = houwgt)) )
  #extract the coefficents
  fit_coef <- fit_fixed %>% unnest()
  return(fit_coef)
}


#Convert the data to a useable form
#loadd(index_bflpe)
data_index_long <- function(index_bflpe){
  data = index_bflpe %>%
    pivot_longer(cols = ICC_03G4:bf_15G8_se,
                 names_to = "variable",
                 values_to = "value"
                 ) %>%
    tidyr::separate(variable,
                    into = c("index_ml","year_grade","index_type")) %>%
    mutate(index_type = replace_na(index_type, "est")) %>%
    tidyr::separate(year_grade, into = c("year","grade"), sep = 2) %>%
    pivot_wider(id_cols = c(CNT,ISO,year,grade),
                names_from = c(index_ml, index_type),
                values_from = value) %>%
    set_names(c('cnt', 'iso','year', 'grade','icc_est','icc_se','bf_est_index','bf_se_index')) %>%
    select(-cnt)
  return(data)
}

#Convert the data to a useable form
#loadd(raw_bflpe)
data_raw_long <- function(raw_bflpe){
  data = raw_bflpe %>%
    pivot_longer(cols = bf_03G4:bf_15G8_se,
                 names_to = "variable",
                 values_to = "value"
    ) %>%
    tidyr::separate(variable,
                    into = c("raw_ml","year_grade","raw_type")) %>%
    mutate(raw_type = replace_na(raw_type, "est")) %>%
    tidyr::separate(year_grade, into = c("year","grade"), sep = 2) %>%
    pivot_wider(id_cols = c(CNT,ISO,year,grade),
                names_from = c(raw_ml, raw_type),
                values_from = value) %>%
    set_names(c('cnt', 'iso','year', 'grade','bf_est_raw','bf_se_raw')) %>%
    select(-cnt)
  return(data)
}


#loadd(fixed_est)
#loadd(long_index)
#loadd(long_raw)
combine_data <- function(fixed_est,long_index,long_raw){
  data = fixed_est %>%
    filter(term == 'percach') %>%
    select(iso,year,grade,bf_est_fixed = estimate, bf_se_fixed = std.error,
           fixed_p = p.value) %>%
    mutate(year = str_sub(as.character(year),-2L, -1L) ) %>%
    right_join(long_index) %>%
    left_join(long_raw)
  
  return(data)
}


# loadd(data)
# names(data)
# 
# data %>%
#   mutate(fixed_p = round(fixed_p, 4)) %>%
#   View
# 
# table(data$fixed_p < .05)
# data %>%
#   ungroup() %>%
#   select(bf_est_fixed, bf_est_index, bf_est_raw, icc_est) %>%
#   cor(.[,1:4], use = 'pairwise.complete.obs')
# 
# library(ggrepel)
# data %>%
#   ggplot(aes(x = bf_est_fixed, y = icc_est, label = iso)) +
#   geom_point() +
#   geom_text_repel() +
#   facet_grid(year ~ grade, scales = 'free')

