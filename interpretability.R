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


ggsave("08_ICE XGBoost Model.png", path = "Plots", 
      width = 10, height = 6, dpi = 300)

# build ALE plots 

# create training data in format
to_ale <- train %>% 
  mutate_if(is.character, as.factor)

feature_classes <- to_ale %>% 
  summarise_all(class) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  rename(feature = 1, class = 2) %>% 
  mutate(row_number = row_number())

numerical_features <- feature_classes %>% 
  filter(class == "numeric") %>% 
  pull(row_number)

# normally filtering out, but all classes are numeric 
xgb_model <- finalize_workflow(workflow_xgb,
                               tuned_xgb %>% 
                                 select_best()) %>% 
  fit(train)

# create ALE
tic_ale <- Sys.time()
ale <- map(
  numerical_features,
  ~safely(custom_ale)(X = as.data.frame(to_ale),
                      X.model = xgb_model,
                      pred.fun = predict_tidymodels,
                      J = .x,
                      K = 100)$result %>% 
    as_tibble() %>% 
    mutate(feature = colnames(to_ale)[.x]) %>% 
    select(feature, everything())) %>% 
  bind_rows()

# TODO: try out with previous finals, especially final_workflow_xgb  
# does not need the actual final model (model_final_xgb), which contains the final parameters
# does not need the final model and its fit (so model_final_xgb %>% fit + formlula etc.)
# does not need final_workflow_xgb because we need a pure workflow, not a a full definition of recipe etc.

(toc_ale <- Sys.time() - tic_ale)

# plot ALE

ale %>% 
  filter(feature %in% c("lifetime", "26", "44", "31", "13",
                      "33", "5", "2", "49", "14")) %>% 
  ggplot(aes(x.values, f.values, color = feature)) +
  geom_hline(yintercept = 0) +
  geom_line(size = .8) +
  geom_rug(color = "black", alpha = 0.5, sides = "b") +
  facet_wrap(~feature, scales = "free_x") +
  labs(title = "Accumulated Local Effects for 10 strongest features",
       subtitle = "Only sensor 44 and lifetime show noteworthy effect on the predictor, \nwith sensor 26 and 31 showing contextual influence",
       x = "Sensor Values",
       y = "Relative Importance") +
  theme_bw() +
  theme(legend.position = "none")

ggsave("09_ALE XGBoost Model.png", path = "Plots", 
       width = 10, height = 6, dpi = 300)
