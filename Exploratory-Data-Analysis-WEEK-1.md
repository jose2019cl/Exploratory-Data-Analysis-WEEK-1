

# Exploratory-Data-Analysis-WEEK-1
This assignment uses data from the UC Irvine Machine Learning Repository


## CARGA DE DATOS

data <- read.csv2("../Especialización Data Science Coursera/exdata_data_household_power_consumption/household_power_consumption.csv")

data



## Aquí estoy pasando los datos de Character a Numeric

data$Global_active_power <- as.numeric(as.character(data$Global_active_power))

data$Global_reactive_power <- as.numeric(as.character(data$Global_reactive_power))
data$Voltage <- as.numeric(as.character(data$Voltage))

data$Global_intensity <- as.numeric(as.character(data$Global_intensity))

data$Sub_metering_1 <- as.numeric(as.character(data$Sub_metering_1))
data$Sub_metering_2 <- as.numeric(as.character(data$Sub_metering_2))
data$Sub_metering_3 <- as.numeric(as.character(data$Sub_metering_3))

data


## Aquí se muestra un RESUMEN de las características para cada variable numérica

summary(data$Global_active_power)
summary(data$Global_reactive_power)
summary(data$Voltage)
summary(data$Global_intensity)
summary(data$Sub_metering_1)
summary(data$Sub_metering_2)
summary(data$Sub_metering_3)



## Box plot para cada variable numérica

### Se visualizan distintos tipos de distribuciones, con outlayers variados dependiendo del grafico
### la variable data$Voltage es la que presenta una distribución más cercano a lo normal

boxplot(data$Global_active_power)
boxplot(data$Global_reactive_power)
boxplot(data$Voltage)
boxplot(data$Global_intensity)
boxplot(data$Sub_metering_1)
boxplot(data$Sub_metering_2)
boxplot(data$Sub_metering_3)


library(ggplot2)
hist(data$Voltage, col="green")
abline(v = 12, lwd =2)
abline(v = median(data$Voltage), col = "magenta", lwd = 2)
abline(v = mean(data$Voltage), col = "blue", lwd = 2)

hist(data$Global_reactive_power, col="green")
abline(v = 12, lwd =2)
abline(v = median(data$Global_reactive_power), col = "magenta", lwd = 2)
abline(v = mean(data$Global_reactive_power), col = "blue", lwd = 2)

hist(data$Voltage, col="green")
abline(v = 12, lwd =2)
abline(v = median(data$Voltage), col = "magenta", lwd = 2)
abline(v = mean(data$Voltage), col = "blue", lwd = 2)

hist(data$Global_intensity, col="green")
abline(v = 12, lwd =2)
abline(v = median(data$Global_intensity), col = "magenta", lwd = 2)
abline(v = mean(data$Global_intensity), col = "blue", lwd = 2)

hist(data$Sub_metering_1, col="green")
abline(v = 12, lwd =2)
abline(v = median(data$Sub_metering_1), col = "magenta", lwd = 2)
abline(v = mean(data$Sub_metering_1), col = "blue", lwd = 2)

hist(data$Sub_metering_3, col="green")
abline(v = 12, lwd =2)
abline(v = median(data$Sub_metering_3), col = "magenta", lwd = 2)
abline(v = mean(data$Sub_metering_3), col = "blue", lwd = 2)

## REGRESIÓN LINEAL MÚLTIPLE

### EN LA SIGUIENTE REGRESIÓN LINEAL MÚLTIPLE SE COMPARA EL VOLTAJE COMO VARIABLE DEPENDIENTE, SEGÚN LAS VARIABLES "Global_active_power + Global_reactive_power + Global_intensity + Sub_metering_1 + Sub_metering_2 + Sub_metering_3", MUESTRA UN R^2 DE 02481 Y UN p-value ENTRE 0 Y 0.001 EXACTAMENTE DE 2.2*10e-16


model_rline1 <- lm(Voltage  ~ Global_active_power + Global_reactive_power + Global_intensity + Sub_metering_1 + Sub_metering_2 + Sub_metering_3, data = data)
summary(model_rline1)

plot(model_rline1)


model_rline2 <- lm(Voltage  ~ Global_active_power + Global_reactive_power, data = data)
summary(model_rline2)

plot(model_rline2)


