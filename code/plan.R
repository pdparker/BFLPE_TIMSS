library(drake)
source(here("code", "packages.R"))
source(here("code", "functions.R"))
source(here("code", "data_munging.R"))

plan <- drake_plan(
  timss_raw = read_csv(here("data","timss_raw.csv")) %>%
    set_names(tolower(names(.))) %>%
    mutate(across(starts_with("m"), z)),
  raw_bflpe = read_csv(here("data","raw_sc_bflpe.csv")),
  index_bflpe = read_csv(here("data","index_sc_bflpe.csv")),
  fixed_est = fixed_effects(data = timss_raw, sc = 'raw'),
  long_index = data_index_long(index_bflpe)
)

vis_drake_graph(plan)
make(plan)
