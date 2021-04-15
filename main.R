## Load the libraries
library(dplyr) # For processing dataframe
# library(lubridate)
library(xts)
library(ggplot2) # For plots
library(forecast) # For ggtsdisplay; to plot ts, acf and pacf
library(stats)



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
ts2 <- diff(ts2)
ggtsdisplay(ts2, main = "Titre", xlab = "", ylab = "Value", theme = theme_bw())

diff(log(df$value))

lambda <- BoxCox.lambda(x = df$value, lower = -5, upper = 5)
BoxCox(df$value, lambda = lambda)

lambdadiff <- BoxCox.lambda(x = diff(df$value), lower = -5, upper = 5)
xdiff <- BoxCox(diff(df$value), lambda = lambdadiff)

s1 <- 12
D <- 2
d <- 2
diff(diff(df$value, lag = s1, differences = D), lag = 1, differences = d)


# Plot both time series
ggplot() + 
  geom_line(data = ts, aes(x = index(ts), y = coredata(ts)), color = "red") +
  geom_line(data = ts2, aes(x = index(ts), y = coredata(ts2)), color = "blue") +
  xlab('') +
  ylab('Value')

# Fit ARIMA model
(fit <- Arima(ts2, order = c(3,1,1)))
checkresiduals(fit)
autoplot(forecast(fit))

(fit2 <- auto.arima(ts2)) # -> ARIMA(2,0,2)
checkresiduals(fit2)
autoplot(forecast(fit2))

