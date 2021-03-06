Predictive Maintenance for Heavy Machinery
-------------------

Core Goals
------------------
1. Find best feature that predicts lifetime
2. Create a model to predict the respective feature from 1
3. Design model results so that final sensor effects are interpretable (ICE & ALE)

Code Architecture 
-------------------
* explanatory_data_analysis.R - data preprocessing & EDA
* model_xgb.R - XGB model training, selection, and testing
* interpretability - interpretability graphs based on ICE and ALE
* functions.R - functions fpr prediction extraction and custom ALE function

Samples from Results 
-------------------

![](https://github.com/aleksejhoffaerber/sensordata/blob/18ac97a909fc84ff81fb1070a5737adb13c9300e/Plots/04_EDA_Sensors.png)

![](https://github.com/aleksejhoffaerber/sensordata/blob/18ac97a909fc84ff81fb1070a5737adb13c9300e/Plots/06_XGB_Fit%20Results.png)

![](https://github.com/aleksejhoffaerber/sensordata/blob/18ac97a909fc84ff81fb1070a5737adb13c9300e/Plots/08_ICE%20XGBoost%20Model.png)

![](https://github.com/aleksejhoffaerber/sensordata/blob/2450744e420d7f2244053aece5e10f3ce34d053c/Plots/09_ALE%20XGBoost%20Model.png)


