library(drake)
source(here("code", "packages.R"))
source(here("code", "functions.R"))
source(here("code", "data_munging.R"))
source(here("code", "analysis.R"))

plan <- drake_plan(
  timss_raw = read_csv(here("data","timss_raw.csv")) %>%
    set_names(tolower(names(.))) %>%
    mutate(across(starts_with("m"), z)),
  raw_bflpe = read_csv(here("data","raw_sc_bflpe.csv")),
  index_bflpe = read_csv(here("data","index_sc_bflpe.csv")),
  fixed_est = fixed_effects(data = timss_raw, sc = 'idx'),
  long_index = data_index_long(index_bflpe),
  long_raw = data_raw_long(raw_bflpe),
  data = combine_data(fixed_est,long_index,long_raw),
  bflpe_meta_out = target(
    bflpe_meta(data, est = type),
    transform = cross(type = c("index", "raw"))
  ),
  icc_meta_out = icc_meta(data),
  main_meta_out = target(
    main_meta(data, est = type, scale = s),
    transform = cross(type = c("index", "raw"),
                      s = c(FALSE,TRUE))
  )
)

vis_drake_graph(plan)
make(plan)
#clean(garbage_collection = TRUE)

loadd(main_meta_out_index_FALSE)
loadd(data)

sd_x <- sd(data$icc_est, na.rm=TRUE)
sd_y <- sd(data$bf_est_index, na.rm=TRUE)

main_meta_out_index_FALSE$within_random$beta * sd_x/sd_y
main_meta_out_index_FALSE$within_random$ci.ub * sd_x/sd_y
