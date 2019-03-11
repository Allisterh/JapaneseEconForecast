In recent years, large-scale macroeconomic data has become increasingly available. 
When dealing with such data, traditional forecasting methodologies, such as a forecast based on vector autoregression (VAR), suffers from curse of dimensionality.
In addition, macroeconomic data tends to have a unique structure in that it has a highly-correlated wide variety of variables observed less frequently, resulting in the estimation to be unstable or even infeasible.




Supported by the rise of high-performance computer systems, a number of methodologies are proposed to circumvent the problems in conventional econometrics since the end of the twentieth century. 
The idea is to reduce the dimensionality of the original high-dimensional data with the least loss of information possible. 
Those methodologies are broadly classified into two broad categories of approaches. 



The first approach is to reduce the number of predictors in VAR models by penalized estimation. 
\cite{tib1996} introduced an $L1$-penalized least squares method called lasso, which makes some coefficients exactly zero, and some variants of the lasso were proposed \cite{zou_has2005} and \cite{yua_lin2006}. 
Although these methods were introduced to analyze cross-section data, they can also be applied for time-series data.
Applications in macroeconomic forecasting are studied, for example, by \cite{son_bic2011}, \cite{li_che2014}, and \cite{cal_koc2014}.  



The second approach is to employ dynamic factor models to exploit a small number of latent variables that may affect a large number of macroeconomic variables.
As \cite{bur_mit1946} defined the business cycle as a type of fluctuation found in the aggregate economic activity, the idea that a few unobservable common factors have an impact on a large set of observable variables is well rooted in economics.
Since the seminal work by \cite{sto_wat2002jasa}, dynamic factor models have been widely used in macroeconomics, not only for forecasting but also for other purposes such as the evaluation of monetary policy (\citealt{ber_et_al2005} for the U.S.; \citealt{shi2007} for Japan), the estimation of DSGE models (\citealt{boi_gia2006}), and the development of real-time economic indicators (\citealt{alt_et_al2010} for the Euro Area; \citealt{hay_kob2011} for Japan). 
Several empirical works in macroeconomic forecastings include \cite{sto_wat2002jasa, sto_wat2002jbes} for the macroeconomic indicators in the U. S., \cite{art_et_al2005} for the U.K., \cite{ban_et_al2008} for the Euro Area and \cite{shi2005} for Japan. 

 
This paper makes three contributions in the literature of macroeconomic forecasting. 
First, we apply various high-dimensional models to the Japanese economy in order to forecast important macroeconomic indicators. 
Making an accurate prediction of the macroeconomy is crucial for the better understanding of countryâs economic structure on which policymakers and businesses make a decision (\citealt{li_che2014}), yet few studies have been done to explore the benefit of high-dimensional data in Japan. 
Although \cite{shi2005} is a showed the benefit of exploiting the high-dimensional data, it is limited to the diffusion index model.
Moreover the focus is purely on forecasting and not on the interpretability. 
Our attempt is the first to study the forecasting performances of various high-dimensional models to explore the possibility of those models in Japan as well as to provide the interpretation of the results. 


Second, we propose two forecast-oriented methods for selecting the number of factors in diffusion index (DI) models.
Our DI model consists of two equations.
The first equation is a factor model that is used to extract common factors from high-dimensional data.
The second equation is a factor-augmented AR model that is used  to predict a variable of interest.
Because the number of factors is unknown to researchers, it has to be determined in a data-dependent way.
A possible approach  is  to use a $C_p$-type criterion of \cite{bai_ng2002}.
However, their criterion tends to select an excessive number of factors because it is designed to find the true number of factors in the factor model and does not care a forecasting performance of the factor-augmented AR model.
We propose cross validation-based methods for selecting the number of factors that are designed to minimize the mean squared forecasting error (MSFE).


Third, we provide a $R^2$-based approach for interpreting the relationship between the latent common factors and the original dataset in terms of the predictive power of the dynamic factor model. 
One of the problems of the dynamic factor models is the interpretability of the latent factors because they are affected by a large number of variables in the original dataset. 
\cite{sto_wat2002jbes} and \cite{lud_ng2009} have studied the economic interpretations of the factors by looking into $R^2$ but is limited to the general interpretation of individual factors and does not account for the predictive power of those factors. 
We attempt to extend their approach and provide the interpretability to the common factors.


We have conducted an empirical analysis to investigate the forecasting performances of several high-dimensional models using 127 monthly Japanese macroeconomic data spanning from April 2003 to June 2018. 
Our main findings are; 1) Incorporating the abundant information contributes to the improvements in the accuracy of forecasting, except for the dynamic factor models with the number of factors selected using the criterion of \cite{bai_ng2002}. 
We have also found that the extent of improvement is relatively small when forecasting the leading indicators and the benefit of using high-dimensional data is large for longer horizons. 
2) Selecting the number of factors in terms of predictive power improves forecasting performances. 
The number of selected factors is much smaller in this approach than the one selected by Bai and Ng (2002) criteria, which performed poorly in forecasting. 
Our proposed approach performs as good as the other high-dimensional models employed and have outperformed the benchmark model. 
3) When factors are estimated in terms of the predictive power, the estimated factors load heavily on the variables that are selected in the lasso and other variable selection methods, indicating that both variable selection and common factor approaches exploit the information from the same groups of variables. 
The groups shown to be important are different depending on the variables to forecast, yet are mostly consistent across models.


The remainder of this paper is organized as follows. 
Section 2 introduces the high-dimensional models along with their backgrounds. 
Dataset and implementation notes are given in Section 3. 
Section 4 reports and interprets the results and Section 5 concludes.
