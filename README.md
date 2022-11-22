# Replicate "Common Shocks..."

Final Project for Empirical Macro. Replicate some results from "Common Shocks in Stocks and Bonds" by Anna Cieslak and Hao Pang.

# Data

## Raw Data Downloads

### Yield Curve Data

Downloaded from the Federal Reserve Board website, [Nominal Yield Curve](https://www.federalreserve.gov/data/nominal-yield-curve.htm). This data is based off the paper [The U.S. Treasury Yield Curve: 1961 to the Present](https://www.federalreserve.gov/pubs/feds/2006/200628/200628abs.html) (2006) by Refet S. Gurkaynak, Brian Sack, and Jonathan H. Wright.

To download the dataset, click on "CSV". It's about 16 MB as of October 29, 2022.

Data coverage is from 1961-06-14 to 2022-10-21 as of October 29, 2022.

### Stock Data

Downloaded from WRDS, for which you will need a subscription/access.

CRSP, Index File on the S&P 500. Data coverage 1962-07-02 to 2021-12-31.

S&P 500 daily data for 2022 is pulled from FRED, using their API.

### FOMC Dates

Script pulls these by scraping the Fed Board website.

### Economic Policy Uncertainty

Downloaded from [Policy Uncertainty](https://www.policyuncertainty.com/index.html) website. Click on "USA" country, then scroll down to "US Daily News Index", and click "Download Data".

This data is not a part of the original "Common Shocks..." paper and is used for an extension.

### Election Dates

....
