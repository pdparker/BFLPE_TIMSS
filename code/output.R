# Outputs ####
# Requirements ####
source(here::here("code","packages.R"))
source(here::here("code","functions.R"))
source(here::here("code","analysis.R"))
# Requirements
library(drake) 
# Standard Deviations for later standardization
sd_x = sd(data$icc_est, na.rm=TRUE) 
sd_y = sd(data$bf_est_index, na.rm=TRUE) 

# Load data and run models
loadd(data)
bflp_out <- bflpe_meta(cores = 4, data = data)
icc_out <- icc_meta(cores = 4, data = data)
main_meta <- main_meta(cores = 4, data = data)
# bflpe Results ####
# Assumption Checking
bflp_out %>% map(pp_check)
bflp_out %>% map(loo)
# Extract Summary Stats
bflp_out %>%
  map(summary) %>%
  print(., digits = 3)
# Extract Marginal Effects for Significant Moderator
tmp <- marginal_effects(icc_out$between_grade_random, "grade")
tmp$grade
#ICC Results ####
# Assumption Checking
icc_out %>% map(pp_check)
icc_out %>% map(loo)
# Summary Statistics
icc_out %>%
  map(summary)  %>%
  print(., digits = 3)
# Extract Marginal Effects for Significant Moderator
tmp <- marginal_effects(icc_out$between_grade_random, "grade")
tmp$grade
# Main model output ####
# Assumption Checking
main_meta %>% map(pp_check)
main_meta %>% map(loo)
# R-squared Values
main_meta %>%
  map(loo_R2)%>%
  map_df(~round(.,2),.id = "Model")

main_meta %>%
  map(~bayes_R2(., robust=TRUE)) %>%
  map(~round(.,2))
# Summary Results: Tau-squared
main_meta %>%
  map_dfr(tidy, .id = "Model") %>%
  filter(str_detect(term, "sd_"))  %>%
  mutate(across(where(is.numeric),
                ~.*1/1       )
  ) %>%
  mutate(across(where(is.numeric), ~round(., 3)))
# Extract POsterior Samples
posterior = main_meta %>%
  map(posterior_samples, .id = "Model") 
  
# Results Table
# Row 1
row1 <- posterior$between_fixed %>%
  select(-starts_with("lp"), -contains("Interce")) %>%
  mutate(across(everything(),
         ~.*sd_x/sd_y       )
         ) %>%
  dplyr::summarise(across(everything(),
                   list(m = median,
                        lb = ~quantile(., probs = .025),
                        ub = ~quantile(., probs = .975),
                        prob = ~ecdf(.)(-.50))       )
                   ) %>%
  mutate(Model = "Fixed Effect",.before = b_icc_est_m) %>%
  set_names(c("Model", "m","lb","ub","prob>.50"))
# Row 2
row2 <- posterior$between_random %>%
  select(-starts_with("lp"), -contains("Interce")) %>%
  mutate(across(everything(),
                ~.*sd_x/sd_y       )
  ) %>%
  dplyr::summarise(across(everything(),
                          list(m = median,
                               lb = ~quantile(., probs = .025),
                               ub = ~quantile(., probs = .975),
                               prob = ~ecdf(.)(-.50))       )
  ) %>%
  mutate(Model = "Random Effect",.before = b_icc_est_m) %>%
  set_names(c("Model", "m","lb","ub","prob>.50"))
# Row 3
row3 <- posterior$within_random %>%
  select(-starts_with("lp"), -contains("Interce"), -contains("iso")) %>%
  mutate(across(everything(),
                ~.*sd_x/sd_y       )
  ) %>%
  dplyr::summarise(across(everything(),
                          list(m = median,
                               lb = ~quantile(., probs = .025),
                               ub = ~quantile(., probs = .975),
                               prob = ~ecdf(.)(-.50))       )
  ) %>%
  mutate(Model = "Within Country Effect",.before = b_icc_est_m) %>%
  set_names(c("Model", "m","lb","ub","prob>.50"))
# Row 5
row4 <- posterior$within_year_random %>% 
  select(-starts_with("lp"), -contains("Interce"), -contains("iso")) %>%
  mutate(`TIMSS 2003` = b_icc_est + `b_icc_est:year`*1,
         `TIMSS 2007` = b_icc_est + `b_icc_est:year`*2,
         `TIMSS 2011` = b_icc_est + `b_icc_est:year`*3,
         `TIMSS 2015` = b_icc_est + `b_icc_est:year`*4) %>%
  select(starts_with("TIMSS")) %>%
  mutate(across(everything(),
                ~.*sd_x/sd_y       )
  ) %>%
  dplyr::summarise(across(everything(),
                          list(m = median,
                               lb = ~quantile(., probs = .025),
                               ub = ~quantile(., probs = .975),
                               prob = ~ecdf(.)(-.50))       )
  ) %>%
  pivot_longer(cols = everything(),names_sep = "_", names_to = c("grade", "statistic")) %>%
  pivot_wider(id_cols = grade, names_from = statistic, values_from = value) %>%
  set_names(c("Model", "m","lb","ub","prob>.50"))

# Produce Table
bind_rows(row1,row2,row3,row4) %>%
  mutate(across(where(is.numeric),~round(., 3)))

loo(main_meta$within_random, main_meta$within_year_random)

# Plots ####
# # Figure 1 ####
p_tmp <- tidy(bflp_out$between_random) %>% filter(term == "b_Intercept") %>%
  rename(Estimate.Intercept = "estimate") %>%
  mutate(label = "", grade = "")

data %>%
  drop_na() %>%
  bind_cols(.,bflp_out$between_random %>% ranef %>% purrr::pluck("id") %>% as_tibble()) %>%
  mutate(across(where(is.numeric),~.+p_tmp$Estimate.Intercept)) %>%
  mutate(label = glue("{iso} {year} {grade} {round(Estimate.Intercept,2)} [{round(Q2.5.Intercept,2)}, {round(Q97.5.Intercept,2)}]") %>% factor, 
         label = forcats::fct_reorder(.f = label, .x = Estimate.Intercept) ) %>%
  ggplot(aes(y = label, x = Estimate.Intercept, color = grade)) +
  geom_rect(xmin = p_tmp$lower, xmax = p_tmp$upper, ymin = -Inf, ymax = Inf, fill = "grey90", color = "grey90", alpha = .80) +
  geom_vline(xintercept = p_tmp$Estimate.Intercept) +
  geom_point() +
  geom_linerange(aes(xmin = Q2.5.Intercept, xmax = Q97.5.Intercept)) +
  hrbrthemes::theme_ipsum() +
  theme(
    text = element_text(family = "Arial Narrow"),
    axis.text.y = element_text(size = 6),
    legend.position = "none"
  ) +
  scale_y_discrete(position = "right") +
  labs (y = element_blank(),
        x = "BFLPE (Unit = Standardized β)") +
  scale_color_manual(values = c("grey", "black"))
ggsave(here::here("fig","figure1.png"), dpi = 300, width = 8, height = 11)  
# Figure 2 ####
p_tmp <- tidy(icc_out$between_random) %>% filter(term == "b_Intercept") %>%
  rename(Estimate.Intercept = "estimate") %>%
  mutate(label = "", grade = "")

data %>%
  drop_na() %>%
  bind_cols(.,icc_out$between_random %>% ranef %>% purrr::pluck("id") %>% as_tibble()) %>%
  mutate(across(where(is.numeric),~.+p_tmp$Estimate.Intercept)) %>%
  mutate(label = glue("{iso} {year} {grade} {round(Estimate.Intercept,2)} [{round(Q2.5.Intercept,2)}, {round(Q97.5.Intercept,2)}]") %>% factor, 
         label = forcats::fct_reorder(.f = label, .x = Estimate.Intercept) ) %>%
  ggplot(aes(y = label, x = Estimate.Intercept, color = grade)) +
  geom_rect(xmin = p_tmp$lower, xmax = p_tmp$upper, ymin = -Inf, ymax = Inf, fill = "grey90", color = "grey90", alpha = .80) +
  geom_vline(xintercept = p_tmp$Estimate.Intercept) +
  geom_point() +
  geom_linerange(aes(xmin = Q2.5.Intercept, xmax = Q97.5.Intercept)) +
  hrbrthemes::theme_ipsum() +
  theme(
    text = element_text(family = "Arial Narrow"),
    axis.text.y = element_text(size = 6),
    legend.position = "none"
  ) +
  scale_y_discrete(position = "right") +
  labs (y = element_blank(),
        x = "Intraclass correlation coefficent (Unit = r)") +
  scale_color_manual(values = c("grey", "black"))

ggsave(here::here("fig","figure2.png"), dpi = 300, width = 8, height = 11)  

# Figure 3 ####
data %>%
  tidyr::unite("label", iso:grade, sep = " ") %>%
  ggplot(aes(x = icc_est, y = bf_est_index, label = label)) +
  geom_point(color = "grey30") +
  geom_smooth(method = "lm", color = "black", alpha = .50) +
  ggrepel::geom_text_repel(size = 2, color = "grey30") +
  labs(
    x = "Intraclass Correlation Coefficent (Units = r)",
    y = "BFLPE Estimate (Unit = Standardized β)"
  ) +
  hrbrthemes::theme_ipsum() +
  theme(
    text = element_text(family = "Arial Narrow")
  )
ggsave(here::here("fig","figure3.png"), dpi = 300, width = 10, height = 8)  




