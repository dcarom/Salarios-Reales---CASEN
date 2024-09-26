####### Ingresos Reales Mensuales: CASEN

library(dplyr)
library(tidyverse)
library(ggplot2)
library(zoo)
library(haven)

#################
# cleaning data #
#################
ipc_datos <- readxl::read_excel("Cuadro_03092024092934.xlsx", skip = 2)
ipc_datos$date <- as.numeric(paste(substr(ipc_datos$Periodo,1,4),
                                   substr(ipc_datos$Periodo,6,7),sep = ""))
indice <- ipc_datos$`1.IPC general (empalme BCCh)`[ipc_datos$date==202212]
ipc_datos$`1.IPC general (empalme BCCh)` <- ipc_datos$`1.IPC general (empalme BCCh)`/indice

#method for adjusting for IPC:
#go to INE Calculadora IPC and go from month of 2023 report to month of
#actual report and use that index to create an annual IPC to get real incomes

###1990
setwd("/Users/ryandmackenzie/Desktop/Work/Espacio Público/Salarios/Raw Data/")
casen_datos_1990 <- read_dta("casen1990.dta")
#casen_datos_1990$ind <- as.numeric(paste(casen_datos_1990$f, casen_datos_1990$o, sep = ""))
casen_datos_1990 <- casen_datos_1990[,c("edad","sexo","dau","qaut","educ","expr",
                                        "o1","o2","o3","o4","o7","r",
                                        "yautaj","yauthaj","yjubaj","yjubhaj","ymoneaj")]
casen_datos_1990$año <- 1990
colnames(casen_datos_1990) <- c("edad","sexo","decil","quintil","educ","expr",
                                "trab","tiene_trab","desemp","raz_desemp","posición","region",
                                "ing_aut","ing_aut_hogar","ing_jub","ing_jub_hogar","ing_tot","año")

###1992
casen_datos_1992 <- read_dta("casen1992.dta")
casen_datos_1992 <- casen_datos_1992[,c("edad","sexo","dau","qaut","educ","expr",
                                        "o1","o2","o3","o4","o7","r",
                                        "yautaj","yauthaj","yjubaj","yjubhaj","ymoneaj")]
casen_datos_1992$año <- 1992
colnames(casen_datos_1992) <- c("edad","sexo","decil","quintil","educ","expr",
                                "trab","tiene_trab","desemp","raz_desemp","posición","region",
                                "ing_aut","ing_aut_hogar","ing_jub","ing_jub_hogar","ing_tot","año")

#1994
casen_datos_1994 <- read_dta("casen1994.dta")
casen_datos_1994 <- casen_datos_1994[,c("edad","sexo","dau","qaut","expr",
                                        "o1","o2","o3","o4","o7","r",
                                        "yautaj","yauthaj","yjubaj","yjubhaj","ymoneaj")]
casen_datos_1994$educ <- NA
casen_datos_1994$año <- 1994
colnames(casen_datos_1994) <- c("edad","sexo","decil","quintil","expr",
                                "trab","tiene_trab","desemp","raz_desemp","posición","region",
                                "ing_aut","ing_aut_hogar","ing_jub","ing_jub_hogar","ing_tot","educ","año")


#1996
casen_datos_1996 <- read_dta("casen1996.dta")
casen_datos_1996 <- casen_datos_1996[,c("edad","sexo","dau","qaut","expr",
                                        "o1","o2","o3","o5","o8","r",
                                        "yautaj","yauthaj","yjubaj","yjubhaj","ymoneaj")]
casen_datos_1996$educ <- NA
casen_datos_1996$año <- 1996
colnames(casen_datos_1996) <- c("edad","sexo","decil","quintil","expr",
                                "trab","tiene_trab","desemp","raz_desemp","posición","region",
                                "ing_aut","ing_aut_hogar","ing_jub","ing_jub_hogar","ing_tot","educ","año")

#1998
casen_datos_1998 <- read_dta("casen1998.dta")
casen_datos_1998 <- casen_datos_1998[,c("edad","sexo","dau","qaut","educ","expr",
                                        "o1","o2","o3","o4","o7","r",
                                        "yautaj","yauthaj","yjubaj","yjubhaj","ymoneaj")]
casen_datos_1998$año <- 1998
colnames(casen_datos_1998) <- c("edad","sexo","decil","quintil","educ","expr",
                                "trab","tiene_trab","desemp","raz_desemp","posición","region",
                                "ing_aut","ing_aut_hogar","ing_jub","ing_jub_hogar","ing_tot","año")


#2000
casen_datos_2000 <- read_dta("casen2000_Stata.dta")
casen_datos_2000$ymoneaj <- casen_datos_2000$yautaj + casen_datos_2000$ysubaj
casen_datos_2000 <- casen_datos_2000[,c("edad","sexo","dau","qaut","educ","expr",
                                        "o1","o2","o3","o7","o10","r",
                                        "yautaj","yauthaj","yjubaj","yjubhaj","ymoneaj")]
casen_datos_2000$año <- 2000
colnames(casen_datos_2000) <- c("edad","sexo","decil","quintil","educ","expr",
                                "trab","tiene_trab","desemp","raz_desemp","posición","region",
                                "ing_aut","ing_aut_hogar","ing_jub","ing_jub_hogar","ing_tot","año")

#2003
casen_datos_2003 <- read_dta("casen2003.dta")
casen_datos_2003$ymoneaj <- casen_datos_2003$yautaj + casen_datos_2003$ysubaj
casen_datos_2003 <- casen_datos_2003[,c("edad","sexo","dau","qaut","educ","expr",
                                        "o1","o2","o3","o6","o9","r",
                                        "yautaj","yauthaj","yjubaj","yjubhaj","ymoneaj")]
casen_datos_2003$año <- 2003
colnames(casen_datos_2003) <- c("edad","sexo","decil","quintil","educ","expr",
                                "trab","tiene_trab","desemp","raz_desemp","posición","region",
                                "ing_aut","ing_aut_hogar","ing_jub","ing_jub_hogar","ing_tot","año")


#2006
casen_datos_2006 <- read_dta("casen2006.dta")
casen_datos_2006$ymoneaj <- casen_datos_2006$yautaj + casen_datos_2006$ysubaj
casen_datos_2006 <- casen_datos_2006[,c("edad","sexo","dau","qaut","educ","expr",
                                        "o1","o2","o3","o6","o19","r",
                                        "yautaj","yauthaj","yjubaj","yjubhaj","ymoneaj")]
casen_datos_2006$año <- 2006
colnames(casen_datos_2006) <- c("edad","sexo","decil","quintil","educ","expr",
                                "trab","tiene_trab","desemp","raz_desemp","posición","region",
                                "ing_aut","ing_aut_hogar","ing_jub","ing_jub_hogar","ing_tot","año")


#2009
casen_datos_2009 <- read_dta("casen2009stata.dta")
casen_datos_2009$ymoneaj <- casen_datos_2009$yautaj + casen_datos_2009$ysubaj
casen_datos_2009 <- casen_datos_2009[,c("edad","sexo","dau","qaut","educ","expr",
                                        "o1","o2","o3","o6","o23","region",
                                        "yautaj","yauthaj","yjubaj","yjubhaj","ymoneaj")]
casen_datos_2009$año <- 2009
colnames(casen_datos_2009) <- c("edad","sexo","decil","quintil","educ","expr",
                                "trab","tiene_trab","desemp","raz_desemp","posición","region",
                                "ing_aut","ing_aut_hogar","ing_jub","ing_jub_hogar","ing_tot","año")

#2011
casen_datos_2011 <- read_dta("casen2011stata.dta", encoding = "latin1")
casen_datos_2011 <- casen_datos_2011[,c("edad","sexo","daut","qaut","educ","expr_r2",
                                        "o1","o2","o3","o7r1","o15","region",
                                        "yautaj","yauthaj","yjubaj","yjubhaj","ymoneaj")]
casen_datos_2011$año <- 2011
colnames(casen_datos_2011) <- c("edad","sexo","decil","quintil","educ","expr",
                                "trab","tiene_trab","desemp","raz_desemp","posición","region",
                                "ing_aut","ing_aut_hogar","ing_jub","ing_jub_hogar","ing_tot","año")


#2013
casen_datos_2013 <- read_dta("casen_2013_mn_b_principal.dta")
casen_datos_2013 <- casen_datos_2013[,c("edad","sexo","dau_mn","qaut_mn","educ","expr",
                                        "o1","o2","o3","o7r1","o15","region",
                                        "yautcor","yautcorh","ypbs","ypbsh","ytotcor")]
casen_datos_2013$año <- 2013
colnames(casen_datos_2013) <- c("edad","sexo","decil","quintil","educ","expr",
                                "trab","tiene_trab","desemp","raz_desemp","posición","region",
                                "ing_aut","ing_aut_hogar","ing_jub","ing_jub_hogar","ing_tot","año")

#2015
casen_datos_2015 <- read_dta("Casen 2015.dta")
casen_datos_2015 <- casen_datos_2015[,c("edad","sexo","dau","qaut","educ","expr",
                                        "o1","o2","o3","o7r1","o15","region",
                                        "yaut","yauth","y2603","ytot")]
casen_datos_2015$año <- 2015
casen_datos_2015$yjubhaj <- NA
colnames(casen_datos_2015) <- c("edad","sexo","decil","quintil","educ","expr",
                                "trab","tiene_trab","desemp","raz_desemp","posición","region",
                                "ing_aut","ing_aut_hogar","ing_jub","ing_tot","año","ing_jub_hogar")

#2017
casen_datos_2017 <- read_dta("Casen 2017.dta")
casen_datos_2017 <- casen_datos_2017[,c("edad","sexo","dau","qaut","educ","expr",
                                        "o1","o2","o3","o7r1","o15","region",
                                        "yaut","yauth","y2603","ytot")]
casen_datos_2017$año <- 2017
casen_datos_2017$yjubhaj <- NA
colnames(casen_datos_2017) <- c("edad","sexo","decil","quintil","educ","expr",
                                "trab","tiene_trab","desemp","raz_desemp","posición","region",
                                "ing_aut","ing_aut_hogar","ing_jub","ing_tot","año","ing_jub_hogar")

#2022
casen_datos_2022 <- read_dta("Base de datos Casen 2022 STATA_18 marzo 2024.dta")
casen_datos_2022 <- casen_datos_2022[,c("edad","sexo","dau","qaut","educ","expr",
                                        "o1","o2","o3","o7","o15","region",
                                        "yaut","yauth","y28_2c","ytot")]
casen_datos_2022$año <- 2022
casen_datos_2022$yjubhaj <- NA
colnames(casen_datos_2022) <- c("edad","sexo","decil","quintil","educ","expr",
                                "trab","tiene_trab","desemp","raz_desemp","posición","region",
                                "ing_aut","ing_aut_hogar","ing_jub","ing_tot","año","ing_jub_hogar")


casen_ingresos <- rbind(casen_datos_1990[,c("edad","sexo","decil","quintil","expr","ing_aut","ing_aut_hogar","ing_tot","año")],
                        casen_datos_1992[,c("edad","sexo","decil","quintil","expr","ing_aut","ing_aut_hogar","ing_tot","año")],
                        casen_datos_1994[,c("edad","sexo","decil","quintil","expr","ing_aut","ing_aut_hogar","ing_tot","año")],
                        casen_datos_1996[,c("edad","sexo","decil","quintil","expr","ing_aut","ing_aut_hogar","ing_tot","año")],
                        casen_datos_1998[,c("edad","sexo","decil","quintil","expr","ing_aut","ing_aut_hogar","ing_tot","año")],
                        casen_datos_2000[,c("edad","sexo","decil","quintil","expr","ing_aut","ing_aut_hogar","ing_tot","año")],
                        casen_datos_2003[,c("edad","sexo","decil","quintil","expr","ing_aut","ing_aut_hogar","ing_tot","año")],
                        casen_datos_2006[,c("edad","sexo","decil","quintil","expr","ing_aut","ing_aut_hogar","ing_tot","año")],
                        casen_datos_2009[-1,c("edad","sexo","decil","quintil","expr","ing_aut","ing_aut_hogar","ing_tot","año")],
                        casen_datos_2011[,c("edad","sexo","decil","quintil","expr","ing_aut","ing_aut_hogar","ing_tot","año")],
                        casen_datos_2013[,c("edad","sexo","decil","quintil","expr","ing_aut","ing_aut_hogar","ing_tot","año")],
                        casen_datos_2015[,c("edad","sexo","decil","quintil","expr","ing_aut","ing_aut_hogar","ing_tot","año")],
                        casen_datos_2017[,c("edad","sexo","decil","quintil","expr","ing_aut","ing_aut_hogar","ing_tot","año")],
                        casen_datos_2022[,c("edad","sexo","decil","quintil","expr","ing_aut","ing_aut_hogar","ing_tot","año")])
ipc_factores <- c(5.065, 3.787, 3.097, 2.685, 2.419, 2.262, 2.121, 1.948, 1.711, 1.588, 1.518, 1.386, 1.319, 1)
for (i in 1:14) {
  casen_ingresos$ing_aut[casen_ingresos$año == as.numeric(levels(as.factor(casen_ingresos$año)))[i]] <-
    casen_ingresos$ing_aut[casen_ingresos$año == as.numeric(levels(as.factor(casen_ingresos$año)))[i]]*ipc_factores[i]/1000
  casen_ingresos$ing_aut_hogar[casen_ingresos$año == as.numeric(levels(as.factor(casen_ingresos$año)))[i]] <-
    casen_ingresos$ing_aut_hogar[casen_ingresos$año == as.numeric(levels(as.factor(casen_ingresos$año)))[i]]*ipc_factores[i]/1000
  casen_ingresos$ing_tot[casen_ingresos$año == as.numeric(levels(as.factor(casen_ingresos$año)))[i]] <-
    casen_ingresos$ing_tot[casen_ingresos$año == as.numeric(levels(as.factor(casen_ingresos$año)))[i]]*ipc_factores[i]/1000
}
casen_ingresos$ing_aut[is.na(casen_ingresos$ing_aut)] <- 0
casen_ingresos$ing_aut_hogar[is.na(casen_ingresos$ing_aut_hogar)] <- 0
casen_ingresos$ing_tot[is.na(casen_ingresos$ing_tot)] <- 0

casen_ingresos$sexo <- as_factor(casen_ingresos$sexo)
casen_ingresos$decil <- as_factor(casen_ingresos$decil)
casen_ingresos$quintil <- as_factor(casen_ingresos$quintil)

casen_ingresos$edad_gama <- 1*(casen_ingresos$edad>=25) + 1*(casen_ingresos$edad>29) +
  1*(casen_ingresos$edad>34) + 1*(casen_ingresos$edad>40) + 1*(casen_ingresos$edad>50)
casen_ingresos$edad_gama <- factor(casen_ingresos$edad_gama, levels = c(0:5),
                                   labels = c("<25","25-29","30-34","35-40","40-50",">50"))

write.csv(casen_ingresos, file = "casen_ingresos.csv")
##################
# Gráficos       #
##################
#promedio
avg_ingresos <- casen_ingresos[casen_ingresos$ing_aut>0,] %>%
  group_by(año) %>%
  summarise(total = sum(expr),
            ingresos_aut = sum(ing_aut*expr)/(total),
            ingresos_aut_hogar = sum(ing_aut_hogar*expr)/(total),
            ingresos_tot = sum(ing_tot*expr)/(total))

plot(avg_ingresos$ingresos_aut ~ avg_ingresos$año, type="l")
plot(avg_ingresos$ingresos_aut_hogar ~ avg_ingresos$año, type="l")

ggplot(data = avg_ingresos, aes(x=año, y=ingresos_aut)) + geom_line() +
  ggtitle("Promedio Ingreso") + ylab("Ingresos Mensuales (mil)")


#por sexo
sexo_ingresos <- casen_ingresos[casen_ingresos$ing_aut>0,] %>%
  group_by(año, sexo) %>%
  summarise(total = sum(expr),
            ingresos_aut = sum(ing_aut*expr)/(total),
            ingresos_aut_hogar = sum(ing_aut_hogar*expr)/(total),
            ingresos_tot = sum(ing_tot*expr)/(total))
ggplot(data = sexo_ingresos, aes(x=año, y=ingresos_aut, color = sexo)) + geom_line() +
  ggtitle("Promedio Ingreso") + ylab("Ingresos Mensuales (mil)")

#por edad
edad_ingresos <- casen_ingresos[casen_ingresos$ing_aut>0,] %>%
  group_by(año, edad_gama) %>%
  summarise(total = sum(expr),
            ingresos_aut = sum(ing_aut*expr)/(total),
            ingresos_aut_hogar = sum(ing_aut_hogar*expr)/(total),
            ingresos_tot = sum(ing_tot*expr)/(total))
ggplot(data = edad_ingresos[edad_ingresos$edad_gama!="<25" & edad_ingresos$edad_gama!=">50",], aes(x=año, y=ingresos_aut, color = edad_gama)) + geom_line() +
  ggtitle("Promedio Ingreso Annual") + ylab("Ingresos Mensuales (mil)") + labs(color = "edad")

#por edad, por sexo
sexo_edad_ingresos <- casen_ingresos[casen_ingresos$ing_aut>0,] %>%
  group_by(año, sexo, edad_gama) %>%
  summarise(total = sum(expr),
            ingresos_aut = sum(ing_aut*expr)/(total),
            ingresos_aut_hogar = sum(ing_aut_hogar*expr)/(total),
            ingresos_tot = sum(ing_tot*expr)/(total))
ggplot(data = sexo_edad_ingresos[sexo_edad_ingresos$edad_gama!="<25" & sexo_edad_ingresos$edad_gama!=">50",], aes(x=año, y=ingresos_aut, color = edad_gama)) + geom_line() +
  facet_grid(cols = vars(sexo)) + 
  ggtitle("Promedio Ingreso Annual") + ylab("Ingresos Mensuales (mil)") + labs(color = "edad")

#por decil
decil_ingresos <- casen_ingresos[casen_ingresos$ing_aut>0 & !is.na(casen_ingresos$decil)
                                   & !(casen_ingresos$decil==0),] %>%
  group_by(año, decil) %>%
  summarise(total = sum(expr),
            ingresos_aut = sum(ing_aut*expr)/(total),
            ingresos_aut_hogar = sum(ing_aut_hogar*expr)/(total),
            ingresos_tot = sum(ing_tot*expr)/(total))
ggplot(data = decil_ingresos, aes(x=año, y=ingresos_aut, color = decil)) + geom_line() +
  ggtitle("Promedio Ingreso Annual") + ylab("Ingresos Mensuales (mil)")

#por quintil
quintil_ingresos <- casen_ingresos[casen_ingresos$ing_aut>0 & !is.na(casen_ingresos$quintil)
                                   & !(casen_ingresos$quintil==0),] %>%
  group_by(año, quintil) %>%
  summarise(total = sum(expr),
            ingresos_aut = sum(ing_aut*expr)/(total),
            ingresos_aut_hogar = sum(ing_aut_hogar*expr)/(total),
            ingresos_tot = sum(ing_tot*expr)/(total))
ggplot(data = quintil_ingresos, aes(x=año, y=ingresos_aut, color = quintil)) + geom_line() +
  ggtitle("Promedio Ingreso Annual") + ylab("Ingresos Mensuales (mil)")

#jovenes, por quintil
quintil_ingresos_joven <- casen_ingresos[casen_ingresos$ing_aut>0 & !is.na(casen_ingresos$quintil)
                                   & !(casen_ingresos$quintil==0) & casen_ingresos$edad_gama=="25-29",] %>%
  group_by(año, quintil) %>%
  summarise(total = sum(expr),
            ingresos_aut = sum(ing_aut*expr)/(total),
            ingresos_aut_hogar = sum(ing_aut_hogar*expr)/(total),
            ingresos_tot = sum(ing_tot*expr)/(total))
ggplot(data = quintil_ingresos_joven, aes(x=año, y=ingresos_aut, color = quintil)) + geom_line() +
  ggtitle("Promedio Ingreso Annual, jovenes") + ylab("Ingresos Mensuales (mil)")

#jovenes, por sexo, por quintil
quintil_ingresos_joven <- casen_ingresos[casen_ingresos$ing_aut>0 & !is.na(casen_ingresos$quintil)
                                         & !(casen_ingresos$quintil==0) & casen_ingresos$edad_gama=="25-29",] %>%
  group_by(año, sexo, quintil) %>%
  summarise(total = sum(expr),
            ingresos_aut = sum(ing_aut*expr)/(total),
            ingresos_aut_hogar = sum(ing_aut_hogar*expr)/(total),
            ingresos_tot = sum(ing_tot*expr)/(total))
ggplot(data = quintil_ingresos_joven, aes(x=año, y=ingresos_aut, color = quintil)) + geom_line() +
  facet_grid(cols = vars(sexo)) +
  ggtitle("Promedio Ingreso Annual, jovenes") + ylab("Ingresos Mensuales (mil)")

#cambios cumulativos
cambios_cumulativos <- casen_ingresos[casen_ingresos$ing_aut>0 & !is.na(casen_ingresos$quintil)
                                      & casen_ingresos$quintil!="0",] %>%
  group_by(año, edad_gama, quintil) %>%
  summarise(total = sum(expr),
            ingresos_aut = sum(ing_aut*expr)/(total),
            ingresos_aut_hogar = sum(ing_aut_hogar*expr)/(total),
            ingresos_tot = sum(ing_tot*expr)/(total))
cambios_cumulativos$base <- NA
for(i in 1:5){
  for(j in 2:6){
    cambios_cumulativos$base[cambios_cumulativos$edad_gama == levels(cambios_cumulativos$edad_gama)[i]
                             & cambios_cumulativos$quintil == levels(cambios_cumulativos$quintil)[j]] <-
      cambios_cumulativos$ingresos_aut[cambios_cumulativos$edad_gama == levels(cambios_cumulativos$edad_gama)[i]
                                       & cambios_cumulativos$quintil == levels(cambios_cumulativos$quintil)[j]
                                       & cambios_cumulativos$año==1990]
  }
}
cambios_cumulativos$perc_change <- 100*(cambios_cumulativos$ingresos_aut - cambios_cumulativos$base)/cambios_cumulativos$base

ggplot(data = cambios_cumulativos[!(cambios_cumulativos$edad_gama=="<25" | cambios_cumulativos$edad_gama==">50"),],
       aes(x = año, y = perc_change, color = quintil)) + geom_line() +
  facet_grid(cols = vars(edad_gama)) +
  ggtitle("% cambio cumulativo desde 1990") + ylab("% cambio")



