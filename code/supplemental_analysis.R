

bflpe1 <- random_bflpe()
bflpe2 <- random_bflpe(poly = 2)
bflpe3 <- random_bflpe(poly = 3)
bflpe4 <- random_bflpe(poly = 4)
bflpe5 <- random_bflpe(poly = 5)


list(bflpe1,bflpe2,bflpe3,bflpe4,bflpe5) %>%
  map(~filter(., term == 'schach')) %>%
  map_dfc(~pull(., estimate)) %>%
  cor

list(bflpe1,bflpe2,bflpe3,bflpe4,bflpe5) %>%
  map(~filter(., term == 'schach')) %>%
  map_dfc(~pull(., estimate)) %>%
  set_names(glue::glue("poly{1:5}")) %>%
  summarise(across(everything(), 
                   list(m = mean,s = sd, min = min, max = max))) %>%
  pivot_longer(cols = everything()) %>%
  tidyr::separate(name, into = c("poly", "stat")) %>%
  pivot_wider(id_cols = poly, names_from = stat, values_from = value, names_sep = "_")

  

fixed1 <- fixed_effects(poly = 1)
fixed2 <- fixed_effects(poly = 2)
fixed3 <- fixed_effects(poly = 3)
fixed4 <- fixed_effects(poly = 4)
fixed5 <- fixed_effects(poly = 5)


list(fixed1,fixed2,fixed3,fixed4,fixed5) %>%
  map(~filter(., term == 'percach')) %>%
  map_dfc(~pull(., estimate)) %>%
  cor

list(fixed1,fixed2,fixed3,fixed4,fixed5) %>%
  map(~filter(., term == 'percach')) %>%
  map_dfc(~pull(., estimate)) %>%
  set_names(glue::glue("poly{1:5}")) %>%
  summarise(across(everything(), 
                   list(m = mean,s = sd, min = min, max = max))) %>%
  pivot_longer(cols = everything()) %>%
  tidyr::separate(name, into = c("poly", "stat")) %>%
  pivot_wider(id_cols = poly, names_from = stat, values_from = value, names_sep = "_")
