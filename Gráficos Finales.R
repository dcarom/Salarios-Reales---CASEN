########. Gráficos Finales
setwd("/Users/ryandmackenzie/Desktop/Work/Espacio Público/Salarios/Clean Data/")

library(readxl)
library(zoo)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(modi)

################
# Gráfico 2    #
################
datos_jovenes_SC <- read.csv("adultos_jovenes_SC_prom.csv")
datos_jovenes_SC <- datos_jovenes_SC[,-1]

datos_jovenes_SC_25 <- datos_jovenes_SC[datos_jovenes_SC$edad_gama=="25-29",c("Fecha","trend","edad_gama")]
colnames(datos_jovenes_SC_25) <- c("Fecha","salario","Edad")
datos_jovenes_SC_25$fuente <- "Seguro Cesantía"
datos_jovenes_SC_35 <- datos_jovenes_SC[datos_jovenes_SC$edad_gama=="35-50",c("Fecha","trend","edad_gama")]
colnames(datos_jovenes_SC_35) <- c("Fecha","salario","Edad")
datos_jovenes_SC_35$fuente <- "Seguro Cesantía"

datos_jovenes_SC <- rbind(datos_jovenes_SC_25, datos_jovenes_SC_35)
datos_jovenes_SC$Fecha <- as.yearmon(datos_jovenes_SC$Fecha)
datos_jovenes_SC$salario <- datos_jovenes_SC$salario*.9593801 #adjusting so salaries are in Jan 2023 pesos

datos_merge <- merge(datos_jovenes_SC$Fecha, 1)[c(1:246),]

datos_jovenes_CE <- read_excel("GráficoJóvenesvs3550.xlsx")
datos_jovenes_CE$Fecha <- paste(substr(as.character(datos_jovenes_CE$Año),1,4),"-12", sep = "")
datos_jovenes_CE$Fecha <- as.yearmon(datos_jovenes_CE$Fecha, "%Y-%m")
datos_jovenes_CE <- datos_jovenes_CE[,-1]


datos_ESI_25 <- datos_jovenes_CE[,c("Fecha","Jóvenes ESI")]
datos_ESI_25$Edad <- "25-29"
datos_ESI_25$fuente <- "ESI"
colnames(datos_ESI_25)[2] <- "salario"
datos_ESI_25 <- merge(datos_ESI_25, datos_merge, by.x="Fecha", by.y = "x", all.y = TRUE)
datos_ESI_25$Edad <- "25-29"
datos_ESI_25$fuente <- "ESI"

datos_ESI_35 <- datos_jovenes_CE[,c("Fecha","35-50 ESI")]
datos_ESI_35$Edad <- "35-50"
datos_ESI_35$fuente <- "ESI"
colnames(datos_ESI_35)[2] <- "salario"
datos_ESI_35 <- merge(datos_ESI_35, datos_merge, by.x="Fecha", by.y = "x", all.y = TRUE)
datos_ESI_35$Edad <- "35-50"
datos_ESI_35$fuente <- "ESI"

datos_C_25 <- datos_jovenes_CE[,c("Fecha","Jóvenes CASEN")]
datos_C_25$Edad <- "25-29"
datos_C_25$fuente <- "CASEN"
colnames(datos_C_25)[2] <- "salario"
datos_C_25 <- merge(datos_C_25, datos_merge, by.x="Fecha", by.y = "x", all.y = TRUE)
datos_C_25$Edad <- "25-29"
datos_C_25$fuente <- "CASEN"

datos_C_35 <- datos_jovenes_CE[,c("Fecha","35-50 CASEN")]
datos_C_35$Edad <- "35-50"
datos_C_35$fuente <- "CASEN"
colnames(datos_C_35)[2] <- "salario"
datos_C_35 <- merge(datos_C_35, datos_merge, by.x="Fecha", by.y = "x", all.y = TRUE)
datos_C_35$Edad <- "35-50"
datos_C_35$fuente <- "CASEN"

datos_jovenes_CE <- rbind(datos_ESI_25, datos_ESI_35, datos_C_25, datos_C_35)
datos_jovenes_CE$salario <- datos_jovenes_CE$salario/1000
datos_jovenes_CE$Fecha <- as.yearmon(datos_jovenes_CE$Fecha)
datos_jovenes_CE <- datos_jovenes_CE[,-5]


datos_jovenes_todos <- rbind(datos_jovenes_CE, datos_jovenes_SC)
datos_jovenes_todos <- datos_jovenes_todos[datos_jovenes_todos$Fecha>=2010,]
datos_jovenes_todos <- datos_jovenes_todos[!(is.na(datos_jovenes_todos$salario)),]

#eso es para el gráfico, es la unica manera de que se puede usar 2 axis
datos_jovenes_todos$salario[datos_jovenes_todos$fuente=="Seguro Cesantía"] <- 
  datos_jovenes_todos$salario[datos_jovenes_todos$fuente=="Seguro Cesantía"]-250

ggplot(data = datos_jovenes_todos, aes(x=Fecha, y=salario, color=fuente)) +
  geom_line(aes(linetype = Edad)) +
  geom_point(data = subset(datos_jovenes_todos, fuente %in% "ESI")) +
  geom_point(data = subset(datos_jovenes_todos, fuente %in% "CASEN")) +
  scale_y_continuous(name="salarios (mil)", sec.axis = sec_axis(~.+250, name="salarios (Seguro Cesantía)"))+
  ggtitle("Salarios por edad y fuente de datos")

ggplot(data = datos_test, aes(x=Fecha, y=salario, color=fuente)) +
  geom_line(aes(linetype = Edad)) +
  geom_point(data = subset(datos_test, fuente %in% "ESI")) +
  geom_point(data = subset(datos_test, fuente %in% "CASEN"))

#eso es para gráfico de varianza cumulativa
datos_jovenes_todos$base_year <- NA
datos_jovenes_todos$base_year[datos_jovenes_todos$fuente=="CASEN"] <- as.yearmon("Dec 2011")
datos_jovenes_todos$base_year[datos_jovenes_todos$fuente=="ESI"
                              | datos_jovenes_todos$fuente=="Seguro Cesantía"] <- as.yearmon("Dec 2012")

datos_jovenes_todos <- datos_jovenes_todos %>%
  mutate(base = salario[Fecha==base_year], .by=c(fuente,Edad)) %>%
  mutate(perc_change = 100*(salario-base)/base)

ggplot(data = datos_jovenes_todos[datos_jovenes_todos$Fecha<2023,], aes(x=Fecha, y=perc_change, color=fuente)) +
  geom_line(aes(linetype = Edad)) +
  geom_point(data = subset(datos_jovenes_todos[datos_jovenes_todos$Fecha<2023,], fuente %in% "ESI")) +
  geom_point(data = subset(datos_jovenes_todos[datos_jovenes_todos$Fecha<2023,], fuente %in% "CASEN")) +
  ggtitle("Varianza cumulativa por edad según bases de datos") + ylab("cambio cumulativo (%)")



#haciendo un gráfico de varianza cumulativa




################
# Gráfico 3.   #
################
SC_cambios <- read.csv("adultos_jovenes_datos SC.csv")
SC_cambios <- SC_cambios[,-1] 
SC_cambios$fuente <- "Seguro Cesantía"
colnames(SC_cambios)[c(3,8)] <- c("sexo","perc_change")
SC_cambios <- SC_cambios[,c("edad_gama","quintil","sexo","Fecha",
                            "perc_change","fuente")]
SC_cambios$Fecha <- as.yearmon(SC_cambios$Fecha)

casen_cambios <- read.csv("cambios_cumulativos casen.csv")
casen_cambios <- casen_cambios[,-c(1,2)]
casen_cambios$Fecha <- paste(substr(as.character(casen_cambios$año),1,4),"-12", sep = "")
casen_cambios$Fecha <- as.yearmon(casen_cambios$Fecha, "%Y-%m")
casen_cambios$fuente <- "CASEN"
casen_cambios <- casen_cambios[,c("edad_gama","quintil","sexo","Fecha",
                                  "perc_change","fuente")]


cumulativo <- rbind(SC_cambios,casen_cambios)
cumulativo$Fecha <- as.yearmon(cumulativo$Fecha)

ggplot(data = cumulativo[cumulativo$edad_gama=="25-29" & cumulativo$Fecha>=2010,],
       aes(x=Fecha, y=perc_change, color=quintil)) + geom_line() +
  facet_grid(rows = vars(sexo), cols = vars(fuente)) +
  geom_point(data = subset(cumulativo, fuente %in% "CASEN" & Fecha>=2010 & edad_gama=="25-29")) +
  ggtitle("Cambio cumulativo de salarios (%) desde 2010") + ylab("% cumulativo")
    

write.csv(cumulativo, file="cumulativo_SC_Casen.csv")







#############################
# Gráfico 3 (no por sexo)   #
#############################

#los datos
casen_cumulativo <- read.csv("datos_jovenes_CASEN.csv")
esi_cumulativo <- read.csv("datos_jovenes_ESI.csv")
sc_cumulativo <- read.csv("datos_jovenes_SeguroCesantía.csv")

#arreglar Fecha y agregar fuente
casen_cumulativo$Fecha <- as.yearmon(casen_cumulativo$Fecha)
casen_cumulativo$fuente <- "CASEN"
casen_cumulativo <- casen_cumulativo[casen_cumulativo$Fecha>=2011,]

esi_cumulativo$Fecha <- as.yearmon(esi_cumulativo$Fecha)
esi_cumulativo$fuente <- "ESI"
esi_cumulativo <- esi_cumulativo[esi_cumulativo$Fecha>=2012.917,]

sc_cumulativo$Fecha <- as.yearmon(sc_cumulativo$Fecha)
sc_cumulativo$fuente <- "Seguro Cesantía"
sc_cumulativo <- sc_cumulativo[sc_cumulativo$Fecha>=2012.917,]
sc_cumulativo$quintil <- factor(sc_cumulativo$quintil, levels = c(1:5),
                                labels = c("i","ii","iii","iv","v"))

var_cumulativo <- rbind(casen_cumulativo[,c("Fecha","quintil","perc_change","fuente")],
                        esi_cumulativo[,c("Fecha","quintil","perc_change","fuente")],
                        sc_cumulativo[,c("Fecha","quintil","perc_change","fuente")])

ggplot(data = var_cumulativo[var_cumulativo$Fecha<2023,],aes(x=Fecha, y=perc_change, color=quintil)) + geom_line() +
  facet_grid(cols = vars(fuente)) +
  geom_point(data = subset(var_cumulativo[var_cumulativo$Fecha<2023,], fuente %in% "CASEN")) +
  geom_point(data = subset(var_cumulativo[var_cumulativo$Fecha<2023,], fuente %in% "ESI")) +
  scale_x_continuous(breaks = c(2012, 2015, 2018, 2022)) +
  ggtitle("Varianza cumulativa de salarios (%) desde 2012") + ylab("% cumulativo")


##################
# Gráfico 3 con quintil 5 dividido
#################
casen_cumulativo <- read.csv("datos_jovenes_CASEN.csv")
esi_cumulativo <- read.csv("datos_jovenes_ESI.csv")
sc_cumulativo <- read.csv("datos_jovenes_SC.csv")

casen_cumulativo$Fecha <- as.yearmon(casen_cumulativo$Fecha)
casen_cumulativo$fuente <- "CASEN"
casen_cumulativo <- casen_cumulativo[casen_cumulativo$Fecha>=2011,]

esi_cumulativo$Fecha <- as.yearmon(esi_cumulativo$Fecha)
esi_cumulativo$fuente <- "ESI"
esi_cumulativo <- esi_cumulativo[esi_cumulativo$Fecha>=2012.917,]

sc_cumulativo$Fecha <- as.yearmon(sc_cumulativo$Fecha)
sc_cumulativo$fuente <- "Seguro Cesantía"
sc_cumulativo <- sc_cumulativo[sc_cumulativo$Fecha>=2012.917,]

var_cumulativo <- rbind(casen_cumulativo[,c("Fecha","percentil","perc_change","fuente")],
                        esi_cumulativo[,c("Fecha","percentil","perc_change","fuente")],
                        sc_cumulativo[,c("Fecha","percentil","perc_change","fuente")])
var_cumulativo$percentil <- factor(var_cumulativo$percentil, levels = c("0-20%","20-40%","40-60%","60-80%",
                                                                        "80-90%","90-95%","95-99%",">99%"),
                                   labels = c("0-20%","20-40%","40-60%","60-80%",
                                              "80-90%","90-95%","95-99%",">99%"))

ggplot(data = var_cumulativo[var_cumulativo$Fecha<2023,],aes(x=Fecha, y=perc_change, color=percentil)) +
  geom_line() +
  facet_grid(cols = vars(fuente)) +
  geom_point(data = subset(var_cumulativo[var_cumulativo$Fecha<2023,], fuente %in% "CASEN")) +
  geom_point(data = subset(var_cumulativo[var_cumulativo$Fecha<2023,], fuente %in% "ESI")) +
  scale_x_continuous(breaks = c(2012, 2015, 2018, 2022)) +
  ggtitle("Varianza cumulativa de salarios (%) desde 2012") + ylab("% cumulativo")




                                                   







######################
# Grafico 2 otra vez
######################
casen_datos <- read.csv("casen_ingresos_horas.csv")
casen_datos$horas[casen_datos$horas==0] <- NA

casen_salarios <- casen_datos[casen_datos$salario>0,-1]
casen_salarios <- casen_salarios[casen_salarios$edad_gama=="25-29" | casen_salarios$edad_gama=="35-50",]

casen_salarios$wage <- casen_salarios$salario/casen_salarios$horas
casen_salarios <- casen_salarios[!is.na(casen_salarios$wage),]

########### por hora
casen_salarios <- casen_salarios %>%
  group_by(año, edad_gama) %>%
  summarise(wage = sum(wage*expr)/sum(expr))
colnames(casen_salarios)[2] <- "Edad"

ggplot(data = casen_salarios, aes(x=año, y=wage, color=Edad))+geom_line()+geom_point()+
  ggtitle("Salarios por Hora (mil pesos), adultos y jovenes")

esi_jovenes_promedio <- matrix(c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,
                                    3.519,3.553,4.645,4.085,4.185,3.872,4.094,4.125,4.047,4.317,4.472,4.069,4.012),
                                  nrow = 13, ncol = 2)
esi_jovenes_promedio <- as.data.frame(esi_jovenes_promedio)
colnames(esi_jovenes_promedio) <- c("año","wage")

ggplot()+
  geom_line(data = casen_salarios, aes(x=año, y=wage, color=Edad))+
  geom_point(data = casen_salarios, aes(x=año, y=wage, color=Edad))+
  geom_line(data = esi_jovenes_promedio, aes(x=año,y=wage), color="red")+
  ggtitle("Salarios por Hora (mil pesos), adultos y jovenes")

#seguro cesantía datos
datos_jovenes_SC <- read.csv("adultos_jovenes_SC_prom.csv")
datos_jovenes_SC <- datos_jovenes_SC[,-1]

datos_jovenes_SC_25 <- datos_jovenes_SC[datos_jovenes_SC$edad_gama=="25-29",c("Fecha","trend","edad_gama")]
colnames(datos_jovenes_SC_25) <- c("Fecha","salario","Edad")
datos_jovenes_SC_25$fuente <- "Seguro Cesantía"
datos_jovenes_SC_35 <- datos_jovenes_SC[datos_jovenes_SC$edad_gama=="35-50",c("Fecha","trend","edad_gama")]
colnames(datos_jovenes_SC_35) <- c("Fecha","salario","Edad")
datos_jovenes_SC_35$fuente <- "Seguro Cesantía"

datos_jovenes_SC <- rbind(datos_jovenes_SC_25, datos_jovenes_SC_35)
datos_jovenes_SC$Fecha <- as.yearmon(datos_jovenes_SC$Fecha)
datos_jovenes_SC$salario <- datos_jovenes_SC$salario*.9593801 #adjusting so salaries are in Jan 2023 pesos
colnames(datos_jovenes_SC)[4] <- "Fuente"

datos_merge <- merge(datos_jovenes_SC$Fecha, 1)[c(1:246),]

###########
# mismo pero full-time
###########
casen_comparacion <- casen_salarios[casen_salarios$horas>=172,] %>%
  group_by(año, edad_gama) %>%
  summarise(salario = sum(salario*expr)/sum(expr),
            wage = sum(wage*expr)/sum(expr))
colnames(casen_comparacion)[2] <- "Edad"

ggplot()+
  geom_line(data = casen_comparacion, aes(x=año, y=wage, color=Edad))+
  geom_point(data = casen_comparacion, aes(x=año, y=wage, color=Edad))+
  ggtitle("Salarios por Hora (mil pesos), full time adultos y jovenes")

ggplot()+
  geom_line(data = casen_comparacion, aes(x=año, y=salario, color=Edad))+
  geom_point(data = casen_comparacion, aes(x=año, y=salario, color=Edad))+
  ggtitle("Salarios mensuales (mil pesos), full time adultos y jovenes")

                                                                                                                       write.csv(casen_comparacion, "casen_fulltime_ingresos.csv")

casen_comparacion$Fecha <- paste("Dec", casen_comparacion$año)
casen_comparacion$Fecha <- as.yearmon(casen_comparacion$Fecha)
casen_comparacion$Fuente <- "CASEN"

esi_fulltime <- read_excel("Promedio_Salario_Real.xlsx")
colnames(esi_fulltime) <- c("año","Edad","salario")
esi_fulltime$Fecha <- paste("Dec", esi_fulltime$año)
esi_fulltime$Fecha <- as.yearmon(esi_fulltime$Fecha)
esi_fulltime$Edad <- factor(esi_fulltime$Edad, levels = c("Joven","No Joven"),
                            labels = c("25-29","35-50"))
esi_fulltime$Fuente <- "ESI"
esi_fulltime$salario <- esi_fulltime$salario/1000

salarios_fulltime_fuentes <- rbind(datos_jovenes_SC, esi_fulltime[,c(4,3,2,5)],
                                   casen_comparacion[c(5,3,2,6)])

salarios_fulltime_fuentes$salario[salarios_fulltime_fuentes$Fuente=="Seguro Cesantía"] <-
  salarios_fulltime_fuentes$salario[salarios_fulltime_fuentes$Fuente=="Seguro Cesantía"]-100

ggplot(data = subset(salarios_fulltime_fuentes, (Fecha>=2010.9 & Fecha<2023)),
       aes(x=Fecha, y=salario, color=Fuente))+
  geom_line(aes(linetype = Edad))+
  geom_point(data = subset(salarios_fulltime_fuentes, Fuente %in% c("ESI","CASEN") & Fecha>=2010.9))+
  scale_y_continuous(name="salarios (mil)", sec.axis = sec_axis(~.+150, name="salarios (Seguro Cesantía)"))+
  ggtitle("Salarios Mensuales Full Time Jovenes vs No Jovenes")+
  labs(caption = "salarios de seguro cesantía no tiene datos de full time (horas mensuales ≥ 172)")+
  theme(plot.caption = element_text(hjust = 0))

######################
# Grafico 3 otra vez
######################
casen_datos <- read.csv("casen_ingresos_horas.csv")
casen_datos$horas[casen_datos$horas==0] <- NA

casen_jovenes <- casen_datos[casen_datos$edad_gama=="25-29" & casen_datos$salario>0,-1]

casen_jovenes$wage <- casen_jovenes$salario/casen_jovenes$horas
casen_jovenes <- casen_jovenes[!is.na(casen_jovenes$wage),]

casen_jovenes <- casen_jovenes %>%
  mutate(quintil = 1+1*(wage >= weighted.quantile(wage,expr,.2,FALSE))+
           1*(wage >= weighted.quantile(wage,expr,.4,FALSE))+
           1*(wage >= weighted.quantile(wage,expr,.6,FALSE))+
           1*(wage >= weighted.quantile(wage,expr,.8,FALSE)), .by = c(año))
casen_jovenes$quintil <- factor(casen_jovenes$quintil, levels = c(1:5),
                                labels = c("i","ii","iii","iv","v"))

casen_jovenes <- casen_jovenes %>%
  group_by(año, quintil) %>%
  summarise(avg_wage = 1000*sum(wage*expr)/sum(expr))

casen_jovenes$base <- rep(casen_jovenes$avg_wage[casen_jovenes$año==2011],14)
casen_jovenes$perc_change <- 100*(casen_jovenes$avg_wage - casen_jovenes$base)/casen_jovenes$base

ggplot(data = casen_jovenes, aes(x=año, y=perc_change, color=quintil))+
  geom_line()+geom_point()+
  ggtitle("Salario por hora Jovenes por quintil")

#########
# doing the same but w/ full time
#########
casen_datos <- read.csv("casen_ingresos_horas.csv")
casen_datos$horas[casen_datos$horas==0] <- NA

casen_jovenes <- casen_datos[casen_datos$edad_gama=="25-29" & casen_datos$salario>0,-1]

casen_jovenes$wage <- casen_jovenes$salario/casen_jovenes$horas
casen_jovenes <- casen_jovenes[!is.na(casen_jovenes$wage) & casen_jovenes$horas>=160,]

casen_jovenes <- casen_jovenes %>%
  mutate(quintil = 1+1*(wage >= weighted.quantile(wage,expr,.2,FALSE))+
           1*(wage >= weighted.quantile(wage,expr,.4,FALSE))+
           1*(wage >= weighted.quantile(wage,expr,.6,FALSE))+
           1*(wage >= weighted.quantile(wage,expr,.8,FALSE)), .by = c(año))
casen_jovenes$quintil <- factor(casen_jovenes$quintil, levels = c(1:5),
                                labels = c("i","ii","iii","iv","v"))

casen_jovenes <- casen_jovenes %>%
  group_by(año, quintil) %>%
  summarise(avg_wage = 1000*sum(wage*expr)/sum(expr))

casen_jovenes$base <- rep(casen_jovenes$avg_wage[casen_jovenes$año==2011],14)
casen_jovenes$perc_change <- 100*(casen_jovenes$avg_wage - casen_jovenes$base)/casen_jovenes$base

ggplot(data = casen_jovenes, aes(x=año, y=perc_change, color=quintil))+
  geom_line()+geom_point()+
  ggtitle("Salario por hora Jovenes full time por quintil")

casen_jovenes_salarios <- casen_jovenes %>%
  mutate(quintil = 1+1*(salario >= weighted.quantile(salario,expr,.2,FALSE))+
           1*(salario >= weighted.quantile(salario,expr,.4,FALSE))+
           1*(salario >= weighted.quantile(salario,expr,.6,FALSE))+
           1*(salario >= weighted.quantile(salario,expr,.8,FALSE)), .by = c(año))
casen_jovenes_salarios$quintil <- factor(casen_jovenes_salarios$quintil, levels = c(1:5),
                                         labels = c("i","ii","iii","iv","v"))
casen_jovenes_salarios <- casen_jovenes_salarios %>%
  group_by(año, quintil) %>%
  summarise(salario = 1000*sum(salario*expr)/sum(expr))


casen_jovenes_salarios$base <- rep(casen_jovenes_salarios$salario[casen_jovenes_salarios$año==2011],14)
casen_jovenes_salarios$perc_change <- 100*(casen_jovenes_salarios$salario - casen_jovenes_salarios$base)/casen_jovenes_salarios$base

ggplot(data = casen_jovenes_salarios, aes(x=año, y=perc_change, color=quintil))+
  geom_line()+geom_point()+
  ggtitle("Salario mensuales Jovenes full time por quintil")

sc_cumulativo <- read.csv("datos_jovenes_SeguroCesantía.csv")
esi_cumulativo <- read.csv("datos_jovenes_ESI.csv")
casen_cumulativo <- casen_jovenes

casen_cumulativo$Fecha <- as.yearmon(paste("Dec", casen_cumulativo$año))
casen_cumulativo$Fuente <- "CASEN"
casen_cumulativo <- casen_cumulativo[casen_cumulativo$Fecha>=2011,]

esi_cumulativo$Fecha <- as.yearmon(esi_cumulativo$Fecha)
esi_cumulativo$Fuente <- "ESI"
esi_cumulativo <- esi_cumulativo[esi_cumulativo$Fecha>=2012.917,]
colnames(esi_cumulativo)[3] <- "quintil"

sc_cumulativo$Fecha <- as.yearmon(sc_cumulativo$Fecha)
sc_cumulativo$Fuente <- "Seguro Cesantía"
sc_cumulativo <- sc_cumulativo[sc_cumulativo$Fecha>=2012.917,]
sc_cumulativo$quintil <- factor(sc_cumulativo$quintil, levels = c(1:5),
                                labels = c("i","ii","iii","iv","v"))

var_cumulativo <- rbind(casen_cumulativo[,c("Fecha","quintil","perc_change","Fuente")],
                        esi_cumulativo[,c("Fecha","quintil","perc_change","Fuente")],
                        sc_cumulativo[,c("Fecha","quintil","perc_change","Fuente")])

ggplot(data = var_cumulativo[var_cumulativo$Fecha<2023,],aes(x=Fecha, y=perc_change, color=quintil)) + geom_line() +
  facet_grid(cols = vars(Fuente)) +
  geom_point(data = subset(var_cumulativo[var_cumulativo$Fecha<2023,], Fuente %in% "CASEN")) +
  geom_point(data = subset(var_cumulativo[var_cumulativo$Fecha<2023,], Fuente %in% "ESI")) +
  scale_x_continuous(breaks = c(2012, 2015, 2018, 2022)) +
  ggtitle("Varianza cumulativa de salarios full time (%) desde 2012") + ylab("% cumulativo")





########################
# Crecimiento PIB y salarios
########################
pibpc <- read_excel("PIBpc.xlsx", skip = 2)
pibpc <- pibpc[-c(29:40),]
pibpc$año <- c(1996:2023)
colnames(pibpc)[c(2,3,4)] <- c("PIB","PIBds","poblacion")
pibpc$base <- pibpc$PIBpc[pibpc$año==1996]
pibpc$base2000 <- pibpc$PIBpc[pibpc$año==2000]
pibpc$base2013 <- pibpc$PIBpc[pibpc$año==2013]
pibpc$per_change <-100*(pibpc$PIBpc-pibpc$base)/pibpc$base
pibpc$per_change00 <-100*(pibpc$PIBpc-pibpc$base2000)/pibpc$base2000
pibpc$per_change13 <-100*(pibpc$PIBpc-pibpc$base2013)/pibpc$base2013
pibpc$basepib <- pibpc$PIB[pibpc$año==1996]
pibpc$basepib2000 <- pibpc$PIB[pibpc$año==2000]
pibpc$basepib2013 <- pibpc$PIB[pibpc$año==2013]
pibpc$per_changepib <-100*(pibpc$PIB-pibpc$basepib)/pibpc$basepib
pibpc$per_changepib00 <-100*(pibpc$PIB-pibpc$basepib2000)/pibpc$basepib2000
pibpc$per_changepib13 <-100*(pibpc$PIB-pibpc$basepib2013)/pibpc$basepib2013


casen_datos <- read.csv("casen_ingresos_horas.csv")
casen_datos$horas[casen_datos$horas==0] <- NA

casen_horas <- casen_datos[casen_datos$salario>0,-1]

casen_horas$wage <- casen_horas$salario/casen_horas$horas
casen_horas <- casen_horas[!is.na(casen_horas$wage),]

casen_horas <- casen_horas %>%
  mutate(quintil = 1+1*(wage >= weighted.quantile(wage,expr,.2,FALSE))+
           1*(wage >= weighted.quantile(wage,expr,.4,FALSE))+
           1*(wage >= weighted.quantile(wage,expr,.6,FALSE))+
           1*(wage >= weighted.quantile(wage,expr,.8,FALSE)), .by = c(año))
casen_horas$quintil <- factor(casen_horas$quintil, levels = c(1:5),
                                labels = c("i","ii","iii","iv","v"))
casen_horas <- casen_horas %>%
  group_by(año, quintil) %>%
  summarise(avg_wage = 1000*sum(wage*expr)/sum(expr))

casen_horas$base <- rep(casen_horas$avg_wage[casen_horas$año==2013],14)
casen_horas$perc_change <- 100*(casen_horas$avg_wage - casen_horas$base)/casen_horas$base


ggplot(data = casen_horas, aes(x=año, y=perc_change, color=quintil)) + geom_line() +
  geom_line(data = subset(pibpc, año %in% c(1996,1998,2000,2003,2006,2009,2011,2013,2015,2017,2022)),
            aes(x=año, y=per_change13), lwd=2, col="black")+
  geom_point()+ggtitle("Wages (quintiles) vs PIBpc")


ggplot(data = casen_horas, aes(x=año, y=perc_change, color=quintil)) + geom_line() +
  geom_line(data = subset(pibpc, año %in% c(1996,1998,2000,2003,2006,2009,2011,2013,2015,2017,2022)),
            aes(x=año, y=per_changepib13), lwd=2, col="black")+
  geom_point()+ggtitle("Wages (quintiles) vs PIB")
#################
#con jovenes
casen_jovenes <- casen_datos[casen_datos$edad_gama=="25-29" & casen_datos$salario>0,-1]

casen_jovenes$wage <- casen_jovenes$salario/casen_jovenes$horas
casen_jovenes <- casen_jovenes[!is.na(casen_jovenes$wage),]

casen_jovenes <- casen_jovenes %>%
  mutate(quintil = 1+1*(wage >= weighted.quantile(wage,expr,.2,FALSE))+
           1*(wage >= weighted.quantile(wage,expr,.4,FALSE))+
           1*(wage >= weighted.quantile(wage,expr,.6,FALSE))+
           1*(wage >= weighted.quantile(wage,expr,.8,FALSE)), .by = c(año))
casen_jovenes$quintil <- factor(casen_jovenes$quintil, levels = c(1:5),
                                labels = c("i","ii","iii","iv","v"))

casen_jovenes <- casen_jovenes %>%
  group_by(año, quintil) %>%
  summarise(avg_wage = 1000*sum(wage*expr)/sum(expr))

casen_jovenes$base <- rep(casen_jovenes$avg_wage[casen_jovenes$año==1996],14)
casen_jovenes$perc_change <- 100*(casen_jovenes$avg_wage - casen_jovenes$base)/casen_jovenes$base

ggplot(data = casen_jovenes, aes(x=año, y=perc_change, color=quintil)) + geom_line() +
  geom_line(data = subset(pibpc, año %in% c(1996,1998,2000,2003,2006,2009,2011,2013,2015,2017,2022)),
            aes(x=año, y=per_change, col="PIB"), lwd=2)+
  geom_point()




###################










