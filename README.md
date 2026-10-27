# BDA400 - Assignment 5: Technical Analysis using R

Author: Alba Avila

Technical indicators implemented in base R (no libraries):
`sma`, `ema`, `macd`, `stdev`, `linreg`, `rsi`, `stoch_rsi`, `crossover`, `crossunder`.

## Run
```
Rscript test_indicators.R
```
Or in R: `source("sma.R"); sma(c(10,12,15,20,18,22,25,24,21), period = 3)`

`macd.R` loads `ema.R`; `stoch_rsi.R` loads `rsi.R` and `sma.R`. Keep all files in the same folder.

Documentation: `AlbaAvila_BDA400_A05.docx`
