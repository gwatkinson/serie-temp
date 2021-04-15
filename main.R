# Load the libraries
library(dplyr)
# library(lubridate)
library(xts)
library(dygraphs)
library(ggplot2)
library(forecast)
library(stats)
library(ggfortify)
library(magrittr)



# Dataset choisi : Indice CVS-CJO de la production industrielle (base 100 en 2015) - Fabrication de gaz industriels (NAF rév. 2, niveau classe, poste 20.11)
# Lien : https://www.insee.fr/fr/information/3128533?CORRECTION=2238605&INDICATEUR=2765760
# Import the df (using rio::import)
df <- rio::import(file="data/valeurs_mensuelles.csv", skip=2, setclass="tbl_df", header=TRUE)

# Remove the categorie, rename columns and convert dates to yearmon format (using dplyr)
df %<>%
  select(-V3) %>%
  rename(date=`PÃ©riode`, value=V2) %>%
  mutate(date=as.yearmon(date))

# Transform to xts object
ts <- xts(df$value, order.by=df$date)
tformat(ts) <- "%Y-%m"



# Plot with ggplot2
ggplot(data = df, aes(x = date, y = value))+
  geom_line(color = "#00AFBB", size = 2)+
  labs(title = "Indice CVS-CJO de la fabrication de gaz industriels entre 1990 et 2021", x= "", y = "Valeur de l'indice")+
  theme_minimal()

# Plot with ggfortify
autoplot(ts)

# Plot the time serie (using ggtsdisplay)
ggtsdisplay(ts, main="Titre", xlab="", ylab="Value", theme=theme_bw())

# Plot the time serie (using plot.xts)
plot(ts, type = "l", xaxt="n", xlab="Date", ylab="Indice CVS-CJO de la fabrication de gaz industriels", col="Blue", lwd=2, las=2) #, major.format = "%Y-%m", )
# axis.POSIXct(1, at=seq(df$date[1], tail(df$date, 1), "monts"), format=yearmon, las=2)

# Plot interactive graph (using dygraphs)
dygraph(ts, main="Titre", xlab="", ylab="Value") %>% dyRangeSelector()



# Plot histogramm of the values of the ts
hist(df$value, breaks=20)

# Plot yearly boxplot
year <-as.factor(format(df$date, "%Y"))
boxplot(df$value~year, col="lightblue", pch=20, cex=0.5)



# Statistics of the time serie
summary(df$value)
mean(ts) # = 99.13244
sd(ts) # = 12.85422
acf(ts, 400)



# Remove the seasonal component
ts2 <- ts %>% stl(s.window='periodic') %>% seasadj()

# Differenciate the data to make it stationary
ts2 <- diff(ts2)
ggtsdisplay(ts2, main="Titre", xlab="", ylab="Value", theme=theme_bw())

# Plot both time series
ggplot() + 
  geom_line(data = ts, aes(x=index(ts), y=coredata(ts)), color = "red") +
  geom_line(data = ts2, aes(x=index(ts), y=coredata(ts2)), color = "blue") +
  xlab('') +
  ylab('Value')

# Fit ARIMA model
(fit <- Arima(ts2, order=c(3,1,1)))
checkresiduals(fit)
autoplot(forecast(fit))

(fit2 <- auto.arima(ts2)) # -> ARIMA(2,0,2)
checkresiduals(fit2)
autoplot(forecast(fit2))

