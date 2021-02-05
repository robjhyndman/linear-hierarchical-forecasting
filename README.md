
### Linear hierarchical forecasting

The repository for manuscript entitled **Fast forecast reconciliation using linear models**. This is a new linear approach for forecasting hierarchical and grouped time series to address a key computation challenge with other current methods such as Exponential Smoothing (ETS) and Autoregressive Integrated Moving Average (ARIMA) models. Our proposed approach is not only scalable to large forecasting problems, but also flexible in several ways: it allows easily incorporating external variables, handling missing values, and performing model selection. We illustrate the usefulness of our approach by applying it to two real and one simulated datasets. 

#### Datasets

- Australian domestic tourism dataset:  contains 304 series with both hierarchical and grouped structure with strong seasonality.
- Australian domestic tourism (simulated): simulated series based on the monthly Australian domestic tourism data and systematically modify the forecasting horizon, noise level, hierarchy levels, and number of series.
- Wikipedia pageviews: includes 12 months of daily data (2016-06-01 to 2017-06-29) on Wikipedia pageviews for the most popular social networks articles.

#### Reproducing results

The root directory contains the source file for the paper including the figures and `.rmd` file. For particular code and dataset of each specific section one may refer to corresponding directory linked below:

  + [**Australian domestic toursim**](Australia_data/): dataset and source code (*Section 3.1* \& *Appendix A*) 
  + [**Australian domestic tourism simulation**](Australia_data/sim/):  study simulation code (*Section 3.2*)
  + [**Wikipedia pageviews**](Wikipedia_data/):  dataset and source code (*Appendix B*)



To reproduce tables and figures for Australian domestic tourism and Wikipedia pageviews exampels, one might find the `demo` repository provided [here](https://github.com/mahsaashouri/AUS-Wiki-Binder) useful. A binder link is provided to run the code online. Note the online RStudio has limited resources.
