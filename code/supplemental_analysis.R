# Revision letter 1 R2.6
source(here::here("code","packages.R"))
source(here::here("code","functions.R"))
# Requirements
library(drake)
library(countrycode)
library(widyr)
loadd(data)
bol <- tribble(
  ~Country, ~`Length of tracked curriculum (2002)`, ~`Age of first selection(2003)`,~`Number of tracks at 15 years old (2003)`,~`Index of tracking`,
  "Australia", 0.15, 16, 1, -1.08,
  "Austria", 0.67, 10, 4, 1.75,
  "Belgium", 0.50, 12, 4, 1.04,
  "Bulgaria", 0.36, 14, 2, -0.08,
  "Canada", 0.00, 16, 1, -1.31,
  "Chile", 0.42, 13, 2, 0.23,
  "Czech Republic", 0.62, 11, 5, 1.67,
  "Denmark", 0.25, 16, 1, -0.93,
  "Finland", 0.25, 16, 1, -0.93,
  "France", 0.25, 15, 2, -0.48,
  "Germany", 0.69, 10, 4, 1.79,
  "Greece", 0.25, 15, 2, -0.48,
  "Hong Kong (China)", 0.28, 15, 3, -0.20,
  "Hungary", 0.67, 11, 3, 1.30,
  "Iceland", 0.29, 16, 1, -0.88,
  "Ireland", 0.18, 15, 4, -0.13,
  "Israel", 0.48, 15, 2, -0.13,
  "Italy", 0.38, 14, 3, 0.18,
  "Japan", 0.25, 15, 2, -0.48,
  "Korea", 0.33, 14, 3, 0.10,
  "Latvia", 0.25, 16, 3, -0.48,
  "Liechtenstein", 0.18, 11, 3, 0.55,
  "Luxembourg", 0.46, 13, 4, 0.76,
  "Mexico", 0.45, 12, 3, 0.74,
  "Netherlands", 0.45, 12, 4, 0.97,
  "New Zealand", 0.50, 16, 1, -0.55,
  "Norway", 0.15, 16, 1, -1.08,
  "Poland", 0.38, 15, 3, -0.04,
  "Portugal", 0.38, 15, 3, -0.04,
  "Russia", 0.25, 15, 3, -0.25,
  "Slovakia", 0.22, 11, 5, 1.06,
  "Slovenia", 0.62, 15, 5, 0.76,
  "Spain", 0.33, 16, 1, -0.80,
  "Sweden", 0.17, 16, 1, -1.06,
  "Switzerland", 0.25, 15, 4, -0.02,
  "Taiwan", 0.27, 15, 3, -0.22,
  "Turkey", 0.55, 11, 3, 1.11,
  "United Kingdom", 0.15, 16, 1, -1.08,
  "United States", 0.00, 16, 1, -1.31
)
# Convert country name to iso
bol <- bol %>%
  mutate(iso = countrycode::countrycode(Country,origin = 'country.name', destination = 'iso3c'))
# Join with TIMSS Data
data_bol <- left_join(data,bol)

data_bol %>%
  # Limit TIMSS data to 2003 given Bol Index is based on 2002 data
  filter(year == "03") %>%
  # get correlation by school grade
  group_by(grade) %>%
  # Get correlation, Number of matched countries, Number of countries total, Proportion of matched countries
  summarise(cor = cor(icc_est, `Index of tracking`, use = "pairwise.complete.obs"),
            n_index = sum(!is.na( `Index of tracking`)),
            n = n(),
            prop = n_index/n)


# Sensitivity: Non-linear polynomial ####
# Fixed effects model function
fixed_bflpe_poly <- function(data = timss_raw, sc = c("raw","idx"), poly = 1) {
  # Specify which self-concept measure to use
  sc = glue("msc{sc[1]}")
  # fit the model by grade, country and TIMSS cycle
  fit_fixed = data  %>%
    mutate(schach = z(schach)) %>%
    select(sc = all_of(sc), everything()) %>%
    group_by(grade, iso, year) %>%
    do(fit = tidy(plm(sc ~ poly(mach,poly) + percach, data = ., model = 'within',index = 'idschool', weights = houwgt), conf.int = TRUE) )
  #extract the coefficents
  fit_coef <- fit_fixed %>% unnest()
  return(fit_coef)
}
# Random effects model function
random_bflpe_poly <- function(data = timss_raw, sc = c("raw","idx"), poly = 1) {
  # Specify which self-concept measure to use
  sc = glue("msc{sc[1]}")
  # fit the model by grade, country and TIMSS cycle
  fit_fixed = data  %>%
    mutate(schach = z(schach)) %>%
    select(sc = all_of(sc), everything()) %>%
    group_by(grade, iso, year) %>%
    do(fit = tidy(plm(sc ~ poly(mach,poly) + schach, data = ., model = 'random',index = 'idschool', weights = houwgt), conf.int = TRUE) )
  #extract the coefficents
  fit_coef <- fit_fixed %>% unnest()
  return(fit_coef)
}

#load Raw data 
loadd(timss_raw)
# Fixed Correlations
fixed_poly <- map_dfr(1:5, ~fixed_bflpe_poly(poly= ., sc = "idx"), .id = 'polynomial')
fixed_poly %>%
  filter(term == 'percach') %>%
  select(year, estimate, iso, polynomial, grade) %>%
  pivot_wider(id_cols = c(iso, year, grade), names_from = polynomial, values_from = estimate) %>%
  select(-iso:-grade) %>%
  cor
# Summary Statistics
fixed_poly %>%
  filter(term == 'percach') %>%
  select(year, estimate, iso, polynomial, grade) %>%
  pivot_wider(id_cols = c(iso, year, grade), names_from = polynomial, values_from = estimate) %>%
  dplyr::summarise(across(where(is.numeric), list(m = mean, s = sd))) 
# Random Correlations 
random_poly <- map_dfr(1:5, ~random_bflpe_poly(poly= ., sc = "idx"), .id = 'polynomial')
# Random Correlations
random_poly %>%
  filter(term == 'schach') %>%
  select(year, estimate, iso, polynomial, grade) %>%
  pivot_wider(id_cols = c(iso, year, grade), names_from = polynomial, values_from = estimate) %>%
  select(-iso:-grade) %>%
  cor
# Summary Statistics
random_poly %>%
  filter(term == 'schach') %>%
  select(year, estimate, iso, polynomial, grade) %>%
  pivot_wider(id_cols = c(iso, year, grade), names_from = polynomial, values_from = estimate) %>%
  dplyr::summarise(across(where(is.numeric), list(m = mean, s = sd))) 
