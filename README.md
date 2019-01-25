# Master Thesis
Replication material for my master thesis, "Forecasting Japanese Macroeconomy Using High-Dimensional Data"  

Folders are organised as follows.  
* `data` contains dataset, trandformation codes, and their descriptions. (`dat.rds` is what you get in the end)  
    * `dat`: Folders containing data before transformation. Filenames correspind to the sources or variable names (see `datDetail.xlsx`) . Only publicly available sources are uploaded (Datastream and NEEDS upon request). Abbreviations are: BOJ = Bank of Japan, METI = Ministery of Economy, Trade and Industry, MHLW = Ministry of Health, Labour and Welfare, MIC = Ministry of Internal Affairs and Communication, MLIT = Ministry of Land, Infrastructure, Transport and Tourism. 
    * `prep`: Folders containing data preprocessing codes. Filenames correspond to the sources.  
    * `rds`: Folders containing preprocessed data from `prep`. Stored in RDS format.  
    * `dat.r`: puts the preprocessed data together for the analysis. Seasonal adjustments, unit root tests and data transformations are also done here.
    * `dat.rds`: Dataset to be used for the analysis.  
    * `preprocessing.r`: executes all the codes at once  
    * `txt`: Folders of some text files containing variables names to avoid writing long names in .r files
    * `datDetail.xlsx`: Data descriptions.  
  
* `models`: Contain implementation codes of the corresponding model at a specific horizon and for a specific variable, defined in `execution.r`. Results can be found in `results` folder in rds format.  
    * `ar.r`: Benchmark AR model. Lag orders are selected by BIC and kept track on `ARlags`. See `result` folder.  
    * `di.r`: Diffusion index model with the number of factors selected using the information criterion by Bai and Ng (2002). `DIfactor` and `DIlags` record the number of factors and lag orders within a window, respectively. Note the number of factors are fixed irrespective of the target variables in this approach.  
    * `dicv.r`: Diffusion indel model with the number of factors selected by h-step-ahead MSFE. `DICVfactor` and `DICVlags` record the number of factors and lag orders as in `di.r`. `DICVr2` reports the R^2 of a regression of individual series onto estimated factor(s), which is to be used for the interpretation.  
    * `dilasso.r`: Diffution index model with factors and lags selected by the lasso. `DILASSOcoef` is a list of 20 target variables within which contains sublist of three forecasting horizons. Each sublist contains 60 x 32 matrix (Number of window x number of factors & lags) with binary input; one means that the factor/lag is selected and zero not selected. `DILASSOlambda` reports the tuning parameter, lambda, selected by h-step-ahead MSE from the second subperiod. `DILASSOnonzero` reports the average number of selected parameters in a window and `DILASSOr2` is R^2 of individual series onto selected factor(s) as in `DICVr2`.  
    * `lasso.r`: Lasso implemented using glmnet package. `LASSOcoefs` reports if the variable is selected. Structured as `DILASSOcoef` with each sublist being 60 x 508 matrix, where 508 reflects 127 variables and 4 lags. `LASSOlambda` and `LASSOnonzero` is same as DILASSO.  
    * `enet.r`: Elastic net with glmnet. `ENETalpha` contain the tuning parameter alpha to balance lasso-ridge weight. `ENETlambda`, `ENETnonzero`, and `ENETcoefs` are same as the lasso.  
    * `glasso.r`: Group lasso with grplasso. `gLASSOcoefs`, `gLASSOlambda`, and `gLASSOnonzero` are kept track of.  

* `placeholders.r`: Placeholders filled later in the implementation. Put in a different file so that `execution.r` remains neat.  
* `results`: Contains results. `MSFE` contains MSFE in the absolute term (`MSFEs.rds`) and in the relative term (`MSFE2.rds`). See also the explanations for `models` above.   
* `txt`: Text files containing the names of target variables to facilitate codes to be tidy.      
