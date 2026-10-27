# Test script - run from the repository folder:  Rscript test_indicators.R
for (f in c("sma.R","ema.R","macd.R","stdev.R","linreg.R","rsi.R","stoch_rsi.R","crossover.R","crossunder.R")) source(f)

ok <- function(name, cond) cat(sprintf("%-45s %s\n", name, if (isTRUE(cond)) "PASS" else "FAIL"))
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)

# SMA: hand-checked, (10+12+15)/3 = 12.3333...
s <- sma(data, 3)
ok("sma length = n - period + 1", length(s) == 7)
ok("sma first value = 37/3", abs(s[1] - 37/3) < 1e-12)
ok("sma last value = (25+24+21)/3", abs(s[7] - 70/3) < 1e-12)
ok("sma error when data shorter than period", inherits(try(sma(1:2, 3), silent = TRUE), "try-error"))

# EMA: multiplier 0.5 -> 10, 11, 13, 16.5, 17.25, ...
e <- ema(data, 3)
ok("ema first = data[1]", e[1] == 10)
ok("ema[2] = 11", abs(e[2] - 11) < 1e-12)
ok("ema[4] = 16.5", abs(e[4] - 16.5) < 1e-12)

# MACD
m <- macd(c(100,105,110,115,120,125,130), 3, 5, 2)
ok("macd returns 3 components", all(c("macd_line","signal_line","histogram") %in% names(m)))
ok("macd histogram = macd - signal", all(abs(m$histogram - (m$macd_line - m$signal_line)) < 1e-12))
ok("macd_line = ema3 - ema5", all(abs(m$macd_line - (ema(c(100,105,110,115,120,125,130),3) - ema(c(100,105,110,115,120,125,130),5))) < 1e-12))

# stdev (population): compare with sqrt(mean((x-mean(x))^2))
ok("stdev matches population formula", abs(stdev(data) - sqrt(mean((data - mean(data))^2))) < 1e-12)
ok("stdev of constant vector = 0", stdev(c(5,5,5)) == 0)

# linreg: perfect line y = 2x + 1 over the full series
y <- 2 * (1:6) + 1
l <- linreg(y, 6, 0)
ok("linreg slope = 2", abs(l$slope - 2) < 1e-12)
ok("linreg intercept = 1", abs(l$intercept - 1) < 1e-12)
ok("linreg predicted = y", all(abs(l$predicted_values - y) < 1e-12))
ok("linreg error: length > n", inherits(try(linreg(y, 7, 0), silent = TRUE), "try-error"))
ok("linreg error: offset >= length", inherits(try(linreg(y, 3, 3), silent = TRUE), "try-error"))

# RSI
d <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)
r <- rsi(d, 5)
ok("rsi length = length(data)", length(r) == length(d))
ok("rsi first 'period' values are NA", all(is.na(r[1:5])) && !any(is.na(r[6:10])))
ok("rsi within 0..100", all(r[6:10] >= 0 & r[6:10] <= 100))
ok("rsi = 100 when only gains", all(rsi(1:10, 3)[4:10] == 100))

# StochRSI
set.seed(1); long <- cumsum(rnorm(60)) + 100
st <- stoch_rsi(long, 14, 3, 3)
ok("stoch_rsi returns k_line and d_line", all(c("k_line","d_line") %in% names(st)))
ok("stoch_rsi %K within 0..1", all(st$k_line[!is.na(st$k_line)] >= 0 & st$k_line[!is.na(st$k_line)] <= 1))
ok("stoch_rsi does not fail on short data (PDF example)", is.list(stoch_rsi(d, 14, 3, 3)))

# Crossover / Crossunder with the PDF example
a1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21); a2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
co <- crossover(a1, a2); cu <- crossunder(a1, a2)
ok("crossover length", length(co) == 9)
ok("crossover 'Up' at i=4 (20>18 and 15<=22)", co[4] == "Up")
ok("crossover has a single 'Up'", sum(co == "Up") == 1)
ok("crossunder: arr1 never crosses under here", all(cu[-1] == "False"))
ok("crossunder detects a downward cross", crossunder(c(5,4), c(3,6))[2] == "True")
ok("crossover error on different lengths", inherits(try(crossover(1:3, 1:2), silent = TRUE), "try-error"))

cat("\n--- Outputs for the examples in the assignment ---\n")
cat("sma:\n");        print(s)
cat("ema:\n");        print(e)
cat("macd:\n");       print(m)
cat("stdev:", stdev(data), "\n")
cat("rsi (period 5):\n"); print(r)
cat("crossover:\n");  print(co)
cat("crossunder:\n"); print(cu)
