## Load the libraries
library(dplyr) # For processing dataframe
# library(lubridate)
library(xts)
library(ggplot2) # For plots
library(forecast) # For ggtsdisplay; to plot ts, acf and pacf
library(stats)
library(astsa)
library(portes)


## Load the data
# Chosen time series : Indice CVS-CJO de la production industrielle (base 100 en 2015) - Fabrication de gaz industriels (NAF rév. 2, niveau classe, poste 20.11)
# Link : https://www.insee.fr/fr/information/3128533?CORRECTION=2238605&INDICATEUR=2765760

# Import the df (using rio::import)
df <- rio::import(file = "data/valeurs_mensuelles.csv", skip = 2, setclass = "tbl_df", header = TRUE)

# Remove V3 (because only value is 'A'), rename columns and convert dates to yearmon format (using dplyr)
df %<>%
  select(-V3) %>%
  rename(date = `PÃ©riode`, value = V2) %>%
  mutate(date = as.yearmon(date)) %>%
  arrange(date)

# Transform to xts object
ts <- xts(df$value, order.by = df$date)
tformat(ts) <- "%Y-%m"



## Plots
# Plot with ggplot2
p1 <- ggplot(data = df, aes(x = date, y = value))+
  geom_line(color = "#00AFBB", size = 2)+
  labs(title = "Indice CVS-CJO de la fabrication de gaz industriels entre 1990 et 2021", x = "", y = "Valeur de l'indice")+
  theme_minimal()
ggsave(filename = "ts_plot.png", plot = p1, path = "./output/images/", width = 8, height = 2.5)

# Plot the time serie, acf and pacf (using ggtsdisplay)
png("./output/images/ts_acf_pacf_plot.png")
p2 <- ggtsdisplay(
  df$value,
  plot.type = "partial",
  points = FALSE,
  main = "Indice CVS-CJO de la fabrication de gaz industriels entre 1990 et 2021",
  xlab = "",
  ylab = "Valeur de l'indice",
  theme = theme_minimal()
)
dev.off()
# ggsave(filename = "ts_acf_pacf_plot.png", plot = p2, path = "./output/images/", width = 8, height = 7)

# Plot yearly boxplot
png("./output/images/yearly_boxplot.png")
year <- as.factor(format(filter(df, date != 2021)$date, "%Y"))
boxplot(filter(df, date != 2021)$value~year, col = "lightblue", pch = 20, cex = 0.5, main = "Boxplot par année entre 1990 et 2020", xlab = "", ylab = "Valeur de l'indice")
dev.off()

# Statistics of the time serie
summary(df$value)
mean(df$value) # = 99.13244
sd(df$value) # = 12.85422
acf(df$value, lag.max = 300, main = "Fonction d'auto-corrélation")
pacf(df$value, main = "Fonction d'auto-corrélation partielle")



# Differenciate the data to make it stationary
ts2 <- ts %>% diff(differences = 1, lag = 1) %>% diff(differences = 1, lag = 12) # %>% stl(s.window='periodic') %>% seasadj()
ggtsdisplay(ts2, main = "Titre", xlab = "", ylab = "Value", theme = theme_bw())

lambda <- BoxCox.lambda(x = ts2, lower = -5, upper = 5)
lambda
ts2 <- BoxCox(ts2, lambda = 1)
ggtsdisplay(ts2, main = "Titre", xlab = "", ylab = "Value", theme = theme_bw())


# Plot both time series
ggplot()+ 
  geom_line(data = ts, aes(x = index(ts), y = coredata(ts)), color = "red")+
  geom_line(data = ts2, aes(x = index(ts), y = coredata(ts2)), color = "blue")+
  xlab('')+
  ylab('Value')+
  theme_minimal()

# Fit ARIMA(2,1,0), (0,1,4)
(res20 <- sarima(ts2,2,1,0))
res20$ttable
LjungBox(res20$fit)
checkresiduals(res20$fit)

(res04 <- sarima(ts2,0,1,4))
res04$ttable
LjungBox(res04$fit)
checkresiduals(res04$fit)

(res11 <- sarima(ts2,1,1,1))
res11$ttable
LjungBox(res11$fit)
checkresiduals(res11$fit)

# Fit SARIMA
params <- list(
  list(xdata = ts, p = 1, d = 1, q = 1, P = 1, D = 1, Q = 1, S = 12, details = FALSE), 
  list(xdata = ts, p = 1, d = 1, q = 2, P = 0, D = 1, Q = 1, S = 12, details = FALSE), 
  list(xdata = ts, p = 2, d = 1, q = 2, P = 1, D = 1, Q = 1, S = 12, details = FALSE), 
  list(xdata = ts, p = 1, d = 1, q = 2, P = 1, D = 1, Q = 1, S = 12, details = FALSE), 
  list(xdata = ts, p = 2, d = 1, q = 2, P = 1, D = 1, Q = 2, S = 12, details = FALSE), 
  list(xdata = ts, p = 2, d = 1, q = 4, P = 3, D = 1, Q = 0, S = 12, details = FALSE)
)
models <- list()
for (p in params) {
  res <- do.call(sarima, p)
  models <- list(models, res)
}

res111111<-do.call(sarima(), c(ts2,1,1,1,1,1,1,12))
res111111$ttable
LjungBox(res111111$fit)
checkresiduals(res111111$fit)

res112011<-sarima(ts,1,1,2,0,1,1,12)
res112011$ttable
LjungBox(res112011$fit)
checkresiduals(res112011$fit)

res212111<-sarima(ts,2,1,2,1,1,1,12)
res212111$ttable
LjungBox(res212111$fit)
checkresiduals(res212111$fit)

res112111<-sarima(ts,1,1,2,1,1,1,12)
res112111$ttable
LjungBox(res112111$fit)
checkresiduals(res112111$fit)

res212112<-sarima(ts,2,1,2,1,1,2,12)
res212112$ttable
LjungBox(res212112$fit)
checkresiduals(res212112$fit)

res214310<-sarima(ts,2,1,4,3,1,0,12)
res214310$ttable
LjungBox(res214310$fit)
checkresiduals(res214310$fit)

# AIC / BIC scores
aic <- AIC(res111111$fit, res112011$fit, res212111$fit, res112111$fit, res212112$fit, res214310$fit)
which.min(aic$AIC)
bic <- BIC(res111111$fit, res112011$fit, res212111$fit, res112111$fit, res212112$fit, res214310$fit)
which.min(bic$BIC)

# Autofit ARIMA model
(autofit <- auto.arima(ts)) # -> ARIMA(1,1,1)(2,0,0)[12]
checkresiduals(autofit)
