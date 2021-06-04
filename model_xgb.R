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
splits <-  initial_split(to_model, strata = `35`, prop = 0.3)
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
  rsample::vfold_cv(v = 10) # TODO: or 10?

# model specification
model_xgb <- boost_tree(mode = "regression",
                        trees = 500,
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

# train set fit
train_processed <- bake(recipe_xgb,  new_data = training(splits))
train_prediction <- model_final_xgb %>%
  # fit the model on all the training data
  fit(
    formula = `35` ~ ., 
    data    = train_processed
  )
# # predict the sale prices for the training data
# predict(new_data = train_processed) %>%
# bind_cols(training(splits))

# feature importance for most important features
xgboost::xgb.importance(model = train_prediction$fit) %>% head(n = 10)




# test set fit
test_processed  <- bake(recipe_xgb, new_data = testing(splits))
test_prediction <- model_final_xgb %>%
  # fit the model on all the training data
  fit(
    # select most important features
    formula = `35` ~ lifetime + `26` + `13` + `39` + `12` + `51` + `53` + `22` + `33` + `45`, 
    data    = train_processed
  ) %>%
  # use the training model fit to predict the test data
  predict(new_data = test_processed) %>%
  bind_cols(testing(splits))
# measure the accuracy of our model using `yardstick`

xgboost_score <- 
  test_prediction %>%
  yardstick::metrics(`35`, .pred) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ","))

test_prediction %>% 
  select(.pred, `35`, lifetime) %>% 
  ggplot(aes(`35`, .pred)) + 
  geom_point() # TODO: plot together with lifetime

test_prediction %>% 
  select(.pred, `35`, lifetime) %>%
  rename(Prediction = .pred,
         Actual = `35`) %>% 
  pivot_longer(cols = c(Prediction, Actual),
               names_to = "type",
               values_to ="value") %>% 
  ggplot(aes(lifetime, value, colour = type)) + 
  geom_line(alpha = .3) + # TODO: find a way to smoothe it
  geom_smooth(se = F) +
  theme_bw() +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  scale_color_discrete(NULL) + 
  guides(colour = guide_legend(nrow = 1))
