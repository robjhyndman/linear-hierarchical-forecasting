Wikipedia pageviews dataset (folder: Wikipedia_data) 

Note: all the output *.csv files are inputs for `lhf.Rmd' file to display figures and 
tables. (Tables: 19 and 20 - Figures: 23, 24, 25, 26, 27 and 28)  

- ETS and ARIMA:
Run `ETS-ARIMA.R' file (both rolling and fixed origin methods).
input: `wikipedia_data.csv'
output: `fc.fix.wiki.ets.arima.csv' and `fc.rolling.wiki.ets.arima.csv'
#####
- OLS:
Run `OLS.R' file. For rolling origin approach, the function is `OLSmodel' in `OLSmodel_se.R'
file. For fixed origin approach,  the function is `olsfc' in `olsfc_se.R' file. For computing 
reconciliation matrix we also need `smatrix.R' file.

input: `wikipedia_data.csv'
output: `fc.fix.wiki.OLS.csv' and `fc.rolling.wiki.OLS.csv' 