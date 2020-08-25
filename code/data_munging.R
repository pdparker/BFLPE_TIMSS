# Estimate Fixed Effects ####

fixed_effects <- function(data = timss_raw, sc = c("raw","idx")) {
  # Specify which self-concept measure to use
  sc = glue("msc{sc[1]}")
  # fit the model by grade, country and TIMSS cycle
  fit_fixed = data  %>%
    select(sc = all_of(sc), everything()) %>%
    group_by(grade,iso,year) %>%
    do(fit = plm(sc ~ mach + roach , data = .,model = 'within',index = 'idschool')) 
  #extract the coefficents
  fit_coef <- tidy(fit_fixed,fit,conf.int = TRUE)
  }

#loadd(index_bflpe)
data_index_long <- function(index_bflpe){
  data = index_bflpe %>%
    pivot_longer(cols = ICC_03G4:bf_15G8_se,
                 names_to = "variable",
                 values_to = "value"
                 ) %>%
    tidyr::separate(variable,
                    into = c("estimate","year_grade","type")) %>%
    mutate(type = replace_na(type, "est")) %>%
    tidyr::separate(year_grade, into = c("year","grade"), sep = 2) %>%
    pivot_wider(id_cols = c(CNT,ISO,year,grade),
                names_from = c(estimate, type),
                values_from = value)
  return(data)
}

loadd(raw_bflpe)
data_raw_long <- function(raw_bflpe){
  data = raw_bflpe %>%
    pivot_longer(cols = bf_03G4:bf_15G8_se,
                 names_to = "variable",
                 values_to = "value"
    ) %>%
    tidyr::separate(variable,
                    into = c("estimate","year_grade","type")) %>%
    mutate(type = replace_na(type, "est")) %>%
    tidyr::separate(year_grade, into = c("year","grade"), sep = 2) %>%
    pivot_wider(id_cols = c(CNT,ISO,year,grade),
                names_from = c(estimate, type),
                values_from = value)
  return(data)
}



