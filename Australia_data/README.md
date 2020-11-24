### Australian tourism dataset (folder: Australian_data) 

**Note**: all the output *.csv files are inputs for `lhf.Rmd` file to display figures and tables. (Tables: 3, 4, 15 and 16 - Figures: 5, 6, 7, 8, 9, 10, 19, 20 and 21)  

#### ETS and ARIMA:
Run `ETS-ARIMA.R` file (both rolling and fixed origin methods).

- *input*: `TourismData_v3.csv`
- *output*: `fc.fix.tourism.ets.arima.csv` and `fc.rolling.mint.shrink.ets.arima.csv`

#### OLS:
Run `OLS.R` file. For rolling origin approach, the function is `OLSmodel` in `OLSmodel_se.R` file. For fixed origin approach,  the function is `olsfc` in `olsfc_se.R` file. For computing  reconciliation matrix we also need `smatrix.R` file. 

- *input*: `TourismData_v3.csv`
- *output*: `fc.fix.tourism.OLS.csv` and `fc.rolling.tourism.OLS.csv`

#### Different reconciliation approaches:
For ETS and ARIMA, run `dif-rec-ETS-ARIMA.R` file and for OLS run `dif-rec-OLS.R`.

- *input*: `TourismData_v3.csv`
- *output*: `fc.fix.OLS.mint.shrink.wls.var.csv`, `fc.rolling.OLS.mint.shrink.wls.var.csv`, `fc.fix.ets.arima.mint.shrink.csv`, `fc.fix.ets.arima.wls.var.csv`, `fc.rolling.mint.shrink.ets.arima.csv` and `fc.rolling.wls.var.ets.arima.csv`


#### Computing OLS results with matrix:

Run `matrix-rolling.R` file for rolling origin approach and `matrix-fixed.R` for fixed origin approach.

- *input*: `TourismData_v3.csv`
