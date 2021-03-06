### Simulated Australian tourism dataset (folder: Australia_data/sim) 

**Note**: all the output *.csv files are inputs for `lhf.Rmd` file to display figures and tables. (Tables: 7 and 8 - Figures: 11, 12, 15 and 16)  

#### Simulating the series:
Run `seriessimulation.R` file. It simulates all the simulated series we used with different forecast horizon, noise, level and number of series. 

- *input*: `TourismData_v3.csv`
- *output*: `actual.sim.noise.FH.csv`, `sim.304.csv`, `sim.608.csv`, `sim.1520.csv`, `sim.3040.csv`


#### ETS and ARIMA:
Run `ETS-ARIMA-noiseFH.R` - changing the added noise (0.01, 0.1, 0.5, 1) and forecast horizon (12, 24, 36, 48) and `ETS-ARIMA-levelsNS.R`files - changing number of levels (8, 10, 12, 18) and number of series (304, 608, 1520, 3040) (both rolling and fixed origin methods).

- *input*: `TourismData_v3.csv`
- *output*: `fc.fix.sim.ets.arima.levelNS8.csv`, `fc.rolling.sim.ets.arima.levelNS8.csv`, `fc.fix.sim.ets.arima.levelNS10.csv`, `fc.rolling.sim.ets.arima.levelNS10.csv`, `fc.fix.sim.ets.arima.levelNS12.csv`, `fc.rolling.sim.ets.arima.levelNS12.csv`, `fc.fix.sim.ets.arima.levelNS18.csv`, `fc.rolling.sim.ets.arima.levelNS18.csv`, `fc.fix.noise0.01.FH.csv`, `fc.rolling.noise0.01.FH.csv`, `fc.fix.noise0.1.FH.csv`, `fc.rolling.noise0.1.FH.csv`, `fc.fix.noise0.5.FH.csv`, `fc.rolling.noise0.5.FH.csv`,`fc.fix.noise1.FH.csv` and `fc.rolling.noise1.FH.csv`

**Note**: After running `ETS-ARIMA-levelsNS.R` file we need to run `cleaning-results.R` to clean the results anf get above outputs.

##### OLS:
Run `OLS-fix-noiseFH.R`, `OLS-rolling-noiseFH.R`, `OLS-fix-levelsNS.R`, `OLS-rolling-levelsNS.R` files. For rolling origin approach, the function is `OLSmodel` in `OLSmodel_se.R` file. For fixed origin approach,  the function is `olsfc` in `olsfc_se.R` file. For computing reconciliation matrix we also need to run `smatrix.R` file. Then, to compute the OLS results, first we neet to run `OLSmodel_se.R`, `olsfc_se.R` and `smatrix.R`.

- *input*: `TourismData_v3.csv`
- *output*: `OLS.fix.levelNS.csv`, `OLS.rolling.levelNS.csv`, `fc.OLS.fix.noise.FH.csv` and `fc.OLS.rolling.noise.FH.csv`


