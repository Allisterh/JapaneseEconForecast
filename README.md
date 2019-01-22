# masterProject
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
    
