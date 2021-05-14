## Load the libraries
library(dplyr) # For processing dataframe
library(xts)
library(ggplot2) # For plots
library(forecast) # For ggtsdisplay; to plot ts, acf and pacf
library(stats)
library(astsa)
library(portes)
library(xtable) # For latex tables
library(fUnitRoots) # Pour test racine unitaire
library(tseries)


## Load the data
# Chosen time series : Indice CVS-CJO de la production industrielle (base 100 en 2015) - Fabrication de gaz industriels (NAF rév. 2, niveau classe, poste 20.11)
# Link : https://www.insee.fr/fr/statistiques/serie/010537426
# Import the df(using rio::import)
file <- "data/valeurs_mensuelles.csv"
df <- rio::import(file = file, setclass = "tbl_df", skip = 2, header = TRUE)
# Create the clean time series X
# Remove V3 (because only value is 'A'), rename columns and convert dates to yearmon format (using dplyr)
X <- df %>%
  select(-V3) %>%
  rename(date = `PÃ©riode`, value = V2) %>%
  mutate(date = as.yearmon(date)) %>%
  arrange(date)


## Question 1
# Transform to xts object
ts <- xts(X$value, order.by = X$date)
tformat(ts) <- "%Y-%m"
# Dataframe slices
n <- 3
slice <- arrange(df, `PÃ©riode`)[1:n,]
png("output/images/df.png", height = n*30, width = 170)
gridExtra::grid.table(slice)
dev.off()
sliceX <- X[1:n,]
png("output/images/X.png", height = n*30, width = 145)
gridExtra::grid.table(sliceX)
dev.off()
# Plot the time serie, acf and pacf (using ggtsdisplay)
png("./output/images/ts_acf_pacf.png")
p2 <- ggtsdisplay(
  X$value,
  plot.type = "partial",
  points = FALSE,
  main = "Indice CVS-CJO de la fabrication de gaz industriels entre 1990 et 2021",
  xlab = "",
  ylab = "Valeur de l'indice",
  theme = theme_minimal()
)
dev.off()
# Plot with ggplot2
p1 <- ggplot(data = X, aes(x = date, y = value))+
  geom_line(color = "#00AFBB", size = 2)+
  labs(title = "Indice CVS-CJO de la fabrication de gaz industriels entre 1990 et 2021", x = "", y = "Valeur de l'indice")+
  theme_minimal()
ggsave(filename = "ts_plot.png", plot = p1, path = "./output/images/", width = 8, height = 2.5)



## Question 2
# Differenciate the data to make it stationary
Y <- ts %>% diff(differences = 1, lag = 1)
value <- coredata(Y)[2:length(Y)]
date <- index(Y)[2:length(Y)]
# Check for constant and trend
fit_diff <- lm(value ~ date)
# Plot diff and regression
p_diff <- ggplot(data.frame(value=value, date=date), aes(x = date, y = value)) +
  geom_line(color = "#00AFBB", size = 1) +
  stat_smooth(method = "lm", col = "red") +
  labs(title = "Série différenciée", x = "", y = "") +
  theme_minimal()
ggsave(filename = "diff_ts.png", plot = p_diff, path = "./output/images/", width = 8, height = 2.5)
# Test stationarity with adf test
f <- file("output/tables/adf_test.txt", encoding = "iso-8859-1", open = "wt")
capture.output(adfTest(Y, lags = 0, type = "nc"), file = f)
close(f)
f <- file("output/tables/lm_diff.txt", encoding = "iso-8859-1", open = "wt")
capture.output(summary(fit_diff), file = f)
close(f)


## Question 3
# Plot both time series
tmp <- X
tmp$Y <- coredata(Y)
ba <- tmp %>% rename(X=value) %>% tidyr::gather(Série, value, -date)
p_ba <- ggplot(ba, aes(x = date, y = value)) +
  geom_line(aes(color = Série)) +
  facet_grid(Série ~ ., scales = "free_y") +
  labs(title = "Série avant et après transformation", x="", y="")
ggsave(filename = "before_after.png", plot = p_ba, path = "./output/images/", width = 8, height = 6)


## Question 4
# Plot the time serie, acf and pacf (using ggtsdisplay)
png("./output/images/acf_pacf.png")
p2 <- ggtsdisplay(
  Y,
  plot.type = "partial",
  points = FALSE,
  main = "Série différenciée",
  xlab = "",
  ylab = "",
  theme = theme_minimal(),
  lag.max = 40
)
dev.off()


## Question 5
# ARMA(3,2) ?
x <- Y[2:length(Y)]
pmax = 3
qmax = 2
mat <- matrix(NA,nrow=pmax+1,ncol=qmax+1) 
rownames(mat) <- paste0("p=",0:pmax)
colnames(mat) <- paste0("q=",0:qmax)
AICs <- mat
BICs <- mat
pqs <- expand.grid(0:pmax,0:qmax)
for (row in 1:dim(pqs)[1]) {
  p <- pqs[row,1]
  q <- pqs[row,2]
  estim <- try(arima(ts, c(p,1,q), include.mean = F))
  AICs[p+1,q+1] <- if (class(estim)=="try-error") NA else estim$aic
  BICs[p+1,q+1] <- if (class(estim)=="try-error") NA else BIC(estim)
}
AICs
AICs==min(AICs, na.rm = TRUE)
BICs
BICs==min(BICs, na.rm = TRUE)

arima111 <- arima(ts, c(1,1,1), include.mean=F)
arima212 <- arima(ts, c(2,1,2), include.mean=F)


# Test residual autocorrelation
Qtests <- function(series, k, fitdf=0){
  pvals <- apply(matrix(1:k), 1, FUN=function(l){
    pval <- if (l<=fitdf) NA else Box.test(series, lag=l, type="Ljung-Box", fitdf=fitdf)$p.value
    return(c("lag"=l,"pval"=pval))
  })    
  return(t(pvals))
}
# Check coefficients significance
signif <- function(estim){
  coef <- estim$coef
  se <- sqrt(diag(estim$var.coef))
  t <- abs(coef/se)
  pval <- (1-pnorm(t))*2
  return(rbind(coef,se,t,pval))
}

arimafit <- function(estim){
  adjust <- round(signif(estim),3)
  pvals <- Qtests(estim$residuals, 24, length(estim$coef)-1)
  pvals %<>% as_tibble %>% drop_na %>% mutate(">0.05" = (pval > 0.05)) %>% as.matrix
  cat("Tests de nullité des coefficients :\n")
  print(adjust)
  cat("\nTests d'absence d'autocorrélation :\n")
  print(pvals)
}


res212<-sarima(ts,2,1,2, no.constant = T)
res212$ttable
LjungBox(res212$fit)
checkresiduals(res212$fit)

res0113<-sarima(ts,0,1,13, no.constant = T)
res0113$ttable
LjungBox(res0113$fit)
checkresiduals(res0113$fit)

res2113<-sarima(ts,2,1,13, no.constant = T)
res2113$ttable
LjungBox(res2113$fit)
checkresiduals(res2113$fit)



f <- file("output/tables/AIC.txt", encoding = "iso-8859-1", open = "wt")
sink(file = f)
"Matrice des AIC"
AICs
print("")
"Minimum de la matrice"
AICs==min(AICs, na.rm = TRUE)
sink()
close(f)

f <- file("output/tables/BIC.txt", encoding = "iso-8859-1", open = "wt")
sink(file = f)
"Matrice des BIC"
BICs
print("")
"Minimum de la matrice"
BICs==min(BICs, na.rm = TRUE)
sink()
close(f)

png(filename = "output/images/arima111.png")
sarima(ts, 1, 1, 1, no.constant = T)
dev.off()

png(filename = "output/images/arima212.png")
sarima(ts, 2, 1, 2, no.constant = T)
dev.off()

png(filename = "output/images/arima0113.png")
sarima(ts, 0, 1, 13, no.constant = T)
dev.off()

png(filename = "output/images/arima2113.png")
sarima(ts, 2, 1, 13, no.constant = T)
dev.off()

f <- file("output/tables/test111.txt", encoding = "iso-8859-1", open = "wt")
capture.output(arimafit(arima111), file = f)
close(f)

f <- file("output/tables/test212.txt", encoding = "iso-8859-1", open = "wt")
capture.output(arimafit(arima212), file = f)
close(f)

f <- file("output/tables/test2113.txt", encoding = "iso-8859-1", open = "wt")
capture.output(arimafit(res2113$fit), file = f)
close(f)

f <- file("output/tables/test0113.txt", encoding = "iso-8859-1", open = "wt")
capture.output(arimafit(res0113$fit), file = f)
close(f)
