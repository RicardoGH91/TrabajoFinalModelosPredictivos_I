# Librerias
library(forecast)
library(openxlsx)
library(dplyr)
library(lubridate)

#---- Modelo de regresion lineal Oferta Monetaria #----
base <- read.xlsx("D:\\Personal Data/My Folders/Ricardo/Cursos/Cursos/Ciencia de datos/Modulos/Modulo 3/21 Introduccion a modelos estadisticos para prediccion/Ejercicios FTVM/Indicadores Externos/Oferta monetaria BANXICO.xlsx",
                  detectDates = TRUE)

base_OM <- base %>% 
 filter(Fecha >= as.Date("1989/01/01") & Fecha < as.Date("2023/07/01")) 

base_OM <- base_OM %>% 
  mutate( dia = day(Fecha),
          mes = month(Fecha),
          anio = year(Fecha),
         OM = as.numeric(replace(base_OM$OM, base_OM$OM == "N/E", 0)),
         OM_BMC = as.numeric(replace(base_OM$OM_BMC, base_OM$OM_BMC == "N/E", 0)),
         OM_DEP = as.numeric(replace(base_OM$OM_DEP, base_OM$OM_DEP == "N/E", 0)),
         OM_TOTAL = OM + OM_BMC + OM_DEP) %>% 
  group_by(anio,mes) %>% 
  summarise(OM_TOTAL = sum(OM_TOTAL))
  
attach(base_OM)
tsbase <- ts(base_OM, start = c(1988,1), frequency = 12)  
plot(tsbase[,"OM_TOTAL"],
     xlab = "Periodo",
     ylab = "Oferta monetaria")

tasa <- tslm(log(OM_TOTAL) ~ trend, data = tsbase) # se aplica logaritmo natural a la variable dependiente
summary(tasa)

# nos interesa conocer cual es la tasa de crecimiento relativa compuesta, para el periodo 1988 - 2023 es de :
(exp(0.01397)-1)*100

# entonces en promedio mes a mes la oferta monetaria se ha incrementado a razon de 1.4 %

#---- Modelo de regresion lineal Exportaciones #----
base_Exp <- read.xlsx("D:\\Personal Data/My Folders/Ricardo/Cursos/Cursos/Ciencia de datos/Modulos/Modulo 3/21 Introduccion a modelos estadisticos para prediccion/Ejercicios FTVM/Indicadores Externos/Exportaciones BANXICO.xlsx",
                  detectDates = TRUE)


base_Exp <- base_Exp %>% 
  mutate( dia = day(Fecha),
          mes = month(Fecha),
          anio = year(Fecha)) %>% 
  group_by(anio,mes) %>% 
  summarise(E_Totales = sum(E_Totales))


attach(base_Exp)
tsbase_Exp <- ts(base_Exp, start = c(1993,1), frequency = 12)  
plot(tsbase_Exp[,"E_Totales"])

tasa <- tslm(log(E_Totales) ~ trend , data = tsbase_Exp) # se aplica logaritmo natural a la variable dependiente
summary(tasa)

# nos interesa conocer cual es la tasa de crecimiento relativa compuesta, para el periodo 1993 - 2023 es de:
(exp(.005922)-1)*100

# entonces en promedio mes a mes la oferta monetaria se ha incrementado a razon de 0.5939 %
