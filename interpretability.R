library(DALEXtra)

# build explainer ------

explain_xgb <- explain_tidymodels( 
  model = xgb_fit,
  data = train %>% select(-`35`),
  y = train %>% select(`35`),
  label = "xgboost"
)

# create data model for plotting ------
vars <- c("lifetime", "26", "44", "31", "13",
          "33", "5", "2", "49", "14")

create_ice <- function(var) {
  pdp_xgb <- model_profile(explain_xgb, 
                           N = 50, 
                           center = T,
                           type = "accumulated", 
                           # k = 10, # controls for number of clusters
                           variables = as.character(vars[var])) 
  
  pdp_xgb$agr_profiles %>% 
    as_tibble() %>% 
    mutate(var = as.character(vars[var])) %>% 
    rename_all(
      funs(
        stringr::str_replace_all(., "_", ""))) 
}
    

to_ice_plot <- map(1:length(vars), ~create_ice(.x)) %>% 
  reduce(bind_rows)

# plotting ------
to_ice_plot %>% 
    ggplot(aes(x, yhat)) +
    geom_line(size = 0.8) +
    geom_rug() +
    facet_wrap(~var, nrow = 2,
               scales = "free_x") +
    labs(title = "Relationship between Target (Sensor 35) and Used Feautures",
         x = "Feature Values",
         y = "Sensor 35 Values") +
    theme_bw() +
    theme(legend.position = "none")


