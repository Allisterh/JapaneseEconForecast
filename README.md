# masterProject
Replication material for my master thesis, "Forecasting Japanese Macroeconomy Using High-Dimensional Data"  

Folders are organised as follows.  
* `data` contains dataset, trandformation codes, and their descriptions. (`dat.rds` is what you get in the end)  
    * `dat`: Data before transformation. Filenames correspind to the sources or variable name (see `datDetail.xlsx`) . Only publicly available sources are uploaded (Datastream and NEEDS upon request). Abbreviations are: BOJ = Bank of Japan, METI = Ministery of Economy, Trade and Industry, MHLW = Ministry of Health, Labour and Welfare, MIC = Ministry of Internal Affairs and Communication, MLIT = Ministry of Land, Infrastructure, Transport and Tourism. 
    * `prep`: Data preprocessing codes. Filenames correspond to the sources.  
    * `rds`: Preprocessed data from `prep`. Stored in RDS format.  
    * `dat.r`: Puts data together for the analysis. Seasonal adjustment, unit root tests and data transformation.
    * `dat.rds`: Dataset to be used for the analysis.  
    * `preprocessing.r`: executes all the codes at once  
    * `txt`: Some text files containing variables names to avoid writing long names in .r files
    * `datDetail.xlsx`: Data descriptions. 
    
