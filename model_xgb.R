library(recipes)
library(tidymodels)


# forecasting model (prediction for sensor 35) -----

set.seed(112)

to_model <- lifetime_clean %>% 
  select(installation_dttm, sensor_name, value, lifetime) %>% 
  pivot_wider(id_cols = installation_dttm, 
              names_from = sensor_name,
              values_from = value)  %>% 
  left_join(lifetime_clean %>% 
              select(installation_dttm, lifetime) %>% 
              group_by(installation_dttm) %>% 
              summarise(lifetime = mean(lifetime))) %>% 
  drop_na(`35`)  %>%  # TODO: treating NA
  select(-installation_dttm)

# data splits
splits <-  initial_split(to_model, strata = `35`, prop = 0.7)
train <- training(splits)
test <- testing(splits)

# preprocessing
recipe_xgb <- train %>% 
  recipe(`35` ~ .) %>% 
  step_normalize(all_predictors(), -all_outcomes()) %>% # change all factor/nominals to dummies
  # step_nzv(all_nominal()) %>% # near-zero variance 
  prep() # FIXME above


# cv splits
cv_folds <- bake(recipe_xgb,
                 new_data = training(splits)) %>% 
  rsample::vfold_cv(v = 10)

# model specification
model_xgb <- boost_tree(mode = "regression",
                        trees = 500, # FIXME
                        min_n = tune(),
                        tree_depth = tune(),
                        learn_rate = tune(),
                        loss_reduction = tune()) %>% 
  set_engine("xgboost", objective = "reg:squarederror")

# specify hyperparam tuning grid
params_xgb <- dials::parameters(
  min_n(),
  tree_depth(),
  learn_rate(),
  loss_reduction())

grid_xgb <- dials::grid_max_entropy(
  params_xgb, 
  size = 60)

# workflow
workflow_xgb <- workflow() %>% 
  add_model(model_xgb) %>% 
  add_recipe(recipe_xgb)

# tuning
tuned_xgb <- tune_grid(
  object = workflow_xgb,
  resamples = cv_folds,
  grid = grid_xgb,
  metrics = metric_set(rmse, mae, mape, rsq),
  control = control_grid(verbose = T)
)

# analysis
tuned_xgb %>%
  tune::show_best(metric = "rmse")

final_xgb <- tuned_xgb %>% 
  select_best("rmse")

model_final_xgb <- model_xgb %>% 
  finalize_model(final_xgb)

# train set fit -----
train_processed <- bake(recipe_xgb,  new_data = training(splits))
train_prediction <- model_final_xgb %>%
  # fit the model on all the training data
  fit(
    formula = `35` ~ ., 
    data    = train_processed
  )


# in-sample feature importance for most important features -----
xgboost::xgb.importance(model = train_prediction$fit) %>% head(n = 10)




# test set fit -----
test_processed  <- bake(recipe_xgb, new_data = testing(splits))
test_prediction <- model_final_xgb %>%
  fit(
    # select most important features
    formula = `35` ~ lifetime + `26` + `44`, 
    # + `31` + `13`,
    # `33` + `5` + `2` + `49` + `14`, 
    data    = train_processed
  ) %>%
  predict(new_data = test_processed) %>%
  bind_cols(testing(splits))

# out-of-sample accuracy  -----
metrics_xgb <- 
  test_prediction %>%
  yardstick::metrics(`35`, .pred) %>%
  mutate(.estimate = format(round(.estimate, 3), big.mark = ","))



# GRAPHS -----

# predicted vs. actual sensor data for sensor 35
test_prediction %>% 
  select(.pred, `35`, lifetime) %>%
  rename(Prediction = .pred,
         Actual = `35`) %>% 
  pivot_longer(cols = c(Prediction, Actual),
               names_to = "type",
               values_to ="value") %>% 
  ggplot(aes(lifetime, value, colour = type)) + 
  geom_line(alpha = .5) + 
  geom_smooth(se = F) +
  scale_x_continuous(breaks = seq(0,900,50)) +
  scale_y_continuous(limits = c(0.02,0.125)) +
  scale_colour_manual(values = c("Actual" = "gray30",
                                 "Prediction" = "#0533ff")) +
  labs(title = "Model fit using 3 most important features",
       x = "Lifetime",
       y = "Sensor 35 Values") +
  theme_bw() +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  guides(colour = guide_legend(nrow = 1))
  
ggsave("06_XGB_Fit Results.png", path = "Plots", 
         width = 7, height = 6, dpi = 300)

# feature importance for XGB
xgboost::xgb.importance(model = train_prediction$fit) %>% head(n = 10) %>% 
  ggplot(aes(Gain, reorder(Feature, Gain))) +
  geom_col() +
  labs(title = "Top 10 features",
       x = "Explanatory Gain",
       y = "Feature") +
  theme_bw() 

ggsave("07_XGB_FI Results.png", path = "Plots", 
       width = 3, height = 6, dpi = 300)
  

