# masterProject
Replication material for my master's project, "Forecasting Japanese Macroeconomy Using High-Dimensional Data"  


* ar.r: Autoregressive model with flexible lag length selected by AIC (each model is allowed to have different lag length). Estimation using expanding window  

* ar2.r: Autoregressive model with fixed lag length selected by cross validation. Expanding window  

* ar3.r: Autoregressive model with flexible lag length selected by AIC. Rolling window  

* var.r: Vector autoregression with flexible lag length selected by AIC. Expanding window  

* var2.r: Vector autoregression with fixed lag length selected by cross-validation. Expanding window  

* lasso.r: lasso with lag length and penalty parameter selected by cv. Expanding window  

* lasso2.r: `lasso.r` with rolling window
