#Libreria ----
rm(list=ls())
library(readr)
library(tidyverse)
library(reshape2)
library(readxl)
library(plyr)
library(stringi)
library(modeest) #MODA
library(Microsoft365R)
library(lubridate)

#Crear carpeta donde se alamcena la informacion
# Fecha en específico
fecha_especifica <- ymd(Sys.Date())

# Calcular el sábado anterior
sabado_anterior <- fecha_especifica - days((wday(fecha_especifica)) %% 7) 

# Calcular el número de semana (domingo a sábado)
numero_semana <- (yday(sabado_anterior) - 1) %/% 7 + 1

# Imprimir resultados
print(paste0("Fecha del sábado anterior:", sabado_anterior))
cat("Número de semana:", numero_semana, "\n")

# Vector con los nombres de los meses en español
meses_en_espanol <- c(
  "Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
  "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"
)

# Obtener el nombre del mes en español
numero_del_mes <- month(sabado_anterior)
nombre_del_mes <- meses_en_espanol[numero_del_mes]

print(nombre_del_mes)  # Imprimirá "agosto"
#Parametros del archivo ----
dia <- as.character(day(sabado_anterior))
mes <- numero_del_mes
month <- nombre_del_mes
year <- year(sabado_anterior)

#Base Originacion digital----  
# Obtener la lista de archivos en la carpeta
archivos <- list.files("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Escritorio/Cosechas/Historico_Diario_OD", pattern = "^Originacion_Digital_")
archivos
# Ordenar los archivos por fecha de modificación (el más reciente primero)
archivos_ordenados <- sort(archivos, decreasing = TRUE)
archivos_ordenados
# Obtener el nombre del archivo más reciente
archivo_mas_reciente <- archivos_ordenados[1]
archivo_mas_reciente
# Leer el archivo más reciente
OD <- read_csv(paste0("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Escritorio/Cosechas/Historico_Diario_OD/", archivo_mas_reciente),
               locale = locale(encoding = "WINDOWS-1252"))

OD <- OD %>% distinct(No_Credito,.keep_all = T)
length(OD$No_Credito)
nombres <- names(OD)
nombres <- gsub(" ",".",nombres)
nombres <- gsub("Â£",".",nombres)
nombres <- gsub("£",".",nombres)
colnames(OD) <- nombres
colnames(OD)[1] <- "X.Nombre.del.cliente"
OD <- OD %>% dplyr::select(No_Cliente,No_Credito,X.Nombre.del.cliente,Telefono.celular,Telefono.del.Negocio,Calle,
                           No.Interior,No.Exterior,Manzana,Lote,Colonia,Municipio,Entidad.Federativa,CP,Correo,
                    Producto,Fecha.Desembolso,Monto.Desembolsado,Total.Cuotas,Cuotas.Pagadas,Cuotas.Devengadas,
                    Frecuencia,Tasa_Interes.Anual, Dias.de.atraso, Fecha.Vencimiento.Credito,Fecha.de.liquidacion.del.credito,
                    Estatus,Tipo.de.Credito)
OD <- OD %>% 
  mutate(X.Nombre.del.cliente = toupper(stri_trans_general(sub("([[:punct:]])","",X.Nombre.del.cliente),"Latin-ASCII")))
OD <- mutate_all(OD, funs(replace(., .=='', NA)))#Quitar espeacios en blanco y reemplazarlos con NA
OD <- mutate_all(OD, funs(replace(., .==' ', NA)))#Quitar espeacios en blanco y reemplazarlos con NA
OD$Fecha.Desembolso <- as.Date(OD$Fecha.Desembolso,format = "%d/%m/%Y") 
OD$Fecha.Vencimiento.Credito <- as.Date(OD$Fecha.Vencimiento.Credito,format = "%d/%m/%Y") 
OD$Fecha.de.liquidacion.del.credito <- as.Date(OD$Fecha.de.liquidacion.del.credito,format = "%d/%m/%Y") 
sapply(OD, function(x) sum(is.na(x)))
length(OD$No_Cliente)

#Reporte CLientes OD====
# Obtener la lista de archivos en la carpeta
archivos <- list.files("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Escritorio/Cosechas/Historico_Clientes_OD", pattern = "^Reporte_Clientes OD_")
archivos
# Ordenar los archivos por fecha de modificación (el más reciente primero)
archivos_ordenados <- sort(archivos, decreasing = TRUE)
archivos_ordenados
# Obtener el nombre del archivo más reciente
archivo_mas_reciente <- archivos_ordenados[1]
archivo_mas_reciente
# Leer el archivo más reciente
clientes_OD <- read_csv(paste0("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Escritorio/Cosechas/Historico_Clientes_OD/", archivo_mas_reciente),
               locale = locale(encoding = "WINDOWS-1252"))

# <- read_csv(paste0("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Escritorio/Cosechas/Historico_Clientes_OD/","CalificacionOD_20240913.csv"))

clientes_OD <- mutate_all(clientes_OD, funs(replace(., .=='', NA)))#Quitar espeacios en blanco y reemplazarlos con NA
clientes_OD <- mutate_all(clientes_OD, funs(replace(., .==' ', NA)))#Quitar espeacios en blanco y reemplazarlos con NA
nombres <- names(clientes_OD)
nombres <- gsub(" ",".",nombres)
nombres <- gsub("Â£",".",nombres)
nombres <- gsub("£",".",nombres)
colnames(clientes_OD) <- nombres

clientes_OD <- clientes_OD %>% dplyr::select(Num_Cliente,Ultimo.credito.desembolsado,Numero.Creditos.otorgados,
                                      Plazo.en.dias,Maximo.de.atraso.a.180.dias,Moda.de.Producto,
                                      Moda.Tasa.de.Interes,Monto.Total.Otorgado.de.los.Creditos.Activos, Creditos.Castigados,
                                      Creditos.Activos.Restructurados,Plazos.de.espera,Fecha.Ultimo.Credito.Liquidado,Cuota.Demostrada)

clientes_OD <- clientes_OD %>% distinct(Num_Cliente,.keep_all = T)
sapply(clientes_OD, function(x) sum(is.na(x)))
names(clientes_OD)
head(clientes_OD)

#df <- df %>% mutate(comparacion75 = ifelse())


#Diferidos ----
Diferidos <- clientes_OD %>% dplyr::select(Num_Cliente,Plazos.de.espera)#read_excel("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Escritorio/Bimbo RS/Documentación RS/Recompras RS/Diferidos/Diferidos.xlsx",sheet = "ORIGINACION DIGITAL")
sum(is.na(Diferidos$Plazos.de.espera))
Diferidos <- Diferidos %>% mutate(Dif = ifelse(Plazos.de.espera > 0,"Diferido","No diferido"))#mutate(Dif = ifelse(Estatus == "APLICADO" | Estatus == "Aplicado","Diferido","No diferido"))
sapply(Diferidos, function(x) sum(is.na(x)))
table(Diferidos$Dif)

#union ----
df <- OD %>% left_join(clientes_OD,by =c("No_Cliente" = "Num_Cliente"))
df <- df %>% left_join(Diferidos[,c(1,3)],by = c("No_Cliente"="Num_Cliente"))
df <- df %>% mutate(Dif = ifelse(is.na(Dif),"No diferido",Dif))
sapply(df, function(x) sum(is.na(x)))
length(df$No_Credito)
table(df$Dif)

table(df$Dias.de.atraso)
summary(df$Fecha.de.liquidacion.del.credito)
df <- df %>% mutate(Dias.de.atraso = ifelse(!is.na(Fecha.de.liquidacion.del.credito),0,Dias.de.atraso ))
  #mutate(Cuotas.Pagadas = if_else(Dias.de.atraso == 0 & Cuotas.Devengadas > 0,))

#%Avance ----
df <- df %>% mutate(Avance = Cuotas.Pagadas/Total.Cuotas)
df <- df %>% mutate(Avance = ifelse(!is.na(Fecha.de.liquidacion.del.credito),1,Avance))
df <- df %>% mutate(Avance = ifelse(is.infinite(Avance),0,Avance))
#df <- df %>% mutate(lequidado = if_else(!is.na(Fecha.de.liquidacion.del.credito),1,0))
table(df$Avance)
#table(df$lequidado)
sum(is.na(df$Avance))

#Calificacion ----
df <- df %>% mutate(Calificacion = case_when(Maximo.de.atraso.a.180.dias >= 0 & Maximo.de.atraso.a.180.dias <= 13 ~ "EXCELENTE",
                                             Maximo.de.atraso.a.180.dias > 13 & Maximo.de.atraso.a.180.dias <= 29 ~ "BUENO",
                                             Maximo.de.atraso.a.180.dias > 29 & Maximo.de.atraso.a.180.dias <= 59 ~ "LEVE",
                                             Maximo.de.atraso.a.180.dias > 59 & Maximo.de.atraso.a.180.dias <= 89 ~ "GRAVE",
                                             Maximo.de.atraso.a.180.dias > 89  ~ "MUY GRAVE",
                                             TRUE ~ "SIN INFORMACION"))
sapply(df, function(x) sum(is.na(x)))
table(df$Calificacion)
#Filtros ----
df <- df %>% filter(Producto %in% c(15109,15116,15130)) #%>%# Producto T-Conecta
            # filter(Avance >= 0.5) #Porcentaje de avance o cuotas pagadas mayor o igual al 50%min(which(x >= 6))
length(df$No_Credito)#5089

df <- df %>% mutate(Rango_Avance = case_when(Avance == 0 ~ "a) Sin avance",
                                             Avance > 0 & Avance <= 0.09999999 ~ "b) 0% - 9%",
                                             Avance >= 0.1 & Avance <= 0.1999999 ~ "c) 10% - 19%",
                                             Avance >= 0.2 & Avance <= 0.2999999 ~ "d) 20% - 29%",
                                             Avance >= 0.3 & Avance <= 0.3999999 ~ "e) 30% - 39%",
                                             Avance >= 0.4 & Avance <= 0.49999999 ~ "f) 40% - 49%",
                                             Avance >= 0.5 & Avance <= 0.5999999 ~ "g) 50% - 59%",
                                             Avance >= 0.6 & Avance <= 0.6999999 ~ "h) 60% - 69%",
                                             Avance >= 0.7 & Avance <= 0.7999999 ~ "i) 70% - 79%",
                                             Avance >= 0.8 & Avance <= 0.8999999 ~ "j) 80% - 89%",
                                             Avance >= 0.9 & Avance <= 1 ~ "k) 90% - 100%",
                                             TRUE ~ "SIN INFORMACION"))
table(df$Rango_Avance)
table(df$Rango_Avance[df$Avance >= 0.5])

df <- df %>% mutate(Liquidado = ifelse(!is.na(Fecha.de.liquidacion.del.credito),"si","no"),
                    Atraso_status = case_when(Dias.de.atraso == 0 ~ "a) Al corriente",
                                              Dias.de.atraso > 0 & Dias.de.atraso <= 3~ "b) 1-3",
                                              Dias.de.atraso > 3 & Dias.de.atraso <= 14~ "c) 3-14",
                                              Dias.de.atraso > 14 & Dias.de.atraso <= 30~ "d) 15-30",
                                              Dias.de.atraso > 30 & Dias.de.atraso <= 60~ "e) 31-60",
                                              Dias.de.atraso > 60 & Dias.de.atraso <= 90~ "f) 61-90",
                                              Dias.de.atraso > 90 ~ "g) mas de 90",
                                              TRUE ~ "SIN INFORMACION"))
table(df$Liquidado)
table(df$Atraso_status)
df$Fecha.Desembolso <- as.Date(df$Fecha.Desembolso)
#Obtener el credito mas reciente con base en la fecha de desembolso
Reciente <- df %>% group_by(No_Cliente) %>% dplyr::summarise(recompras = n(),#Numero de crediros recomprados
                                                  Fecha_reciente = max(Fecha.Desembolso),#Fechas de desembolso mas reciente
                                                  posicion = which(Fecha.Desembolso == max(Fecha.Desembolso))[1],#posicion de la fecha mas reciente en el arreglo
                                                  Num_credito = ifelse(Fecha_reciente == max(Fecha.Desembolso),No_Credito[posicion],"error"),
                                                  producto = ifelse(Fecha_reciente == max(Fecha.Desembolso),Producto[posicion],"error"),#Credito mas reciente
                                                  monto = ifelse(Fecha_reciente == max(Fecha.Desembolso),Monto.Desembolsado[posicion],"error"),
                                                  monto_maximo = max(Monto.Desembolsado), #Fecha_reciente = max(Fecha.Desembolso),#Fechas de desembolso mas reciente
                                                  posicion_m = which(Monto.Desembolsado == max(Monto.Desembolsado))[1],#posicion de la fecha mas reciente en el arreglo
                                                  Num_credito_m = ifelse(monto_maximo == max(Monto.Desembolsado),No_Credito[posicion_m],"error"),
                                                  producto_m = ifelse(monto_maximo == max(Monto.Desembolsado),Producto[posicion_m],"error"),
                                                  Fecha_reciente_m = as.Date(Fecha.Desembolso[posicion_m]),
                                                  Num_credito_f = if_else(monto < monto_maximo,Num_credito_m,Num_credito))

names(df)
#Filtrar por numero de cliente y credito mas reciente 
df_1 <- df %>% inner_join(Reciente[,c(1,5,8)],by = c("No_Cliente"="No_Cliente","No_Credito"="Num_credito"))#"Fecha.Desembolso"="Fecha_reciente"
length(df_1$No_Credito)#4922
df_1 <- df_1 %>% mutate(Monto.Desembolsado = if_else(Monto.Desembolsado < monto_maximo,monto_maximo,Monto.Desembolsado))
df_1 <- df_1 %>% dplyr::select(-monto_maximo)


#Resumen
#*Se tienen 4004 casos
#*1. quedan 2035 | 2035
#*2. quedan 1677 | 1814
#*3. quedan 282 | 334
#*4. 5. y 6. 282 | 328
length(df_1$No_Cliente)
`%notin%` <- Negate(`%in%`)
df_1 <- df_1 %>% filter(X.Nombre.del.cliente %notin% c("JUAN CARLOS MORENO ORTIZ","ENRIQUE SOSA VALDES",
                                                       "FRANCISCO ADRIAN RAYA PEREZ","AMBROSIO NAVA GUADARRAMA",
                                                       "EMMANUELIVAN RAYA PEREZ","JOSUE DAVID CORONA MENDEZ",
                                                       "ARTURO VEGA SALCEDO"))


#Incrementos segun el numero de recompras ----
names(df_1)
table(df_1$Monto.Desembolsado)
table(df_1$Rango_Avance)
table(df_1$Numero.Creditos.otorgados)
df_1$Numero.Creditos.otorgados[is.na(df_1$Numero.Creditos.otorgados)] <- 1
monto_recompra <- c()
for (i in 1:length(df_1$No_Credito)) {
  if(df_1$Numero.Creditos.otorgados[i] == 1){
    monto_recompra[i] <- df_1$Monto.Desembolsado[i]*(1 + 0.30)
  }
  if(df_1$No_Credito[i] == 2){
    monto_recompra[i] <- df_1$Monto.Desembolsado[i]*(1 + 0.35)
  }
  if(df_1$No_Credito[i] == 3){
    monto_recompra[i] <- df_1$Monto.Desembolsado[i]*(1 + 0.40)
  }
  if(df_1$No_Credito[i] > 3){#Capacidad de pago
    monto_recompra[i] <- df_1$Monto.Desembolsado[i]*(1 + 0.45)
  }else{
    monto_recompra[i] <- 0
  }
}
sum(is.na(monto_recompra))
table(monto_recompra)

monto_recompra <- round_any(monto_recompra,accuracy = 100,f = ceiling)#redondear al superior 7576 -> 7600
#monto_recompra <- if_else(monto_recompra > 24000,24000,monto_recompra)#Topar a 3000 UDIS
#monto_recompra <- if_else(monto_recompra < df_1$Monto.Desembolsado,0,monto_recompra)

df_1$monto_recompra <- monto_recompra
df_1 <- df_1 %>% filter(monto_recompra >=4000)
table(df_1$monto_recompra)
names(df_1)


#Calificaciones ----
cali <- df_1 %>% dplyr::select(No_Cliente,No_Credito,X.Nombre.del.cliente,Telefono.celular,Telefono.del.Negocio,Calle,No.Interior,
                               No.Exterior,Manzana,Lote,Colonia,Municipio,Entidad.Federativa,CP,Correo,
                               monto_recompra,Tasa_Interes.Anual,Avance,
                               Calificacion,Liquidado,Creditos.Castigados,
                               Creditos.Activos.Restructurados,Dif,
                               Monto.Desembolsado,Atraso_status,Total.Cuotas,Cuotas.Pagadas,Cuotas.Devengadas,
                               Frecuencia,Producto)
length(df$No_Cliente)#4942

#Clientes con recompra que cumplen condiciones
df_2 <- df_1 %>% filter(Atraso_status %in% c("a) Al corriente","b) 1-3")) %>%# 1. Clientes al corriente
                 filter(Calificacion %in% c("BUENO","EXCELENTE")) %>% #2. Calificaciones EXCELENTES O BUENOS
                 filter(Avance >= 0.5) %>% #3. Con 50% de avance o mas
                 filter(Dif %in% c("No diferido")) %>% #4. Sin diferidos
                 filter(Creditos.Activos.Restructurados == 0) %>% #5. Sin reestructuras
                 filter(Creditos.Castigados == 0)#6. Sin castigos
                 
length(df_2$No_Credito)# 639

head(df_2)
names(df_2)
length(df_2$No_Cliente)

#Clientes con recompra que cumplen condiciones
df_4 <- df_1 %>% filter(Atraso_status %in% c("a) Al corriente","b) 1-3")) %>%# 1. Clientes al corriente
  filter(Calificacion %in% c("BUENO","EXCELENTE")) %>% #2. Calificaciones EXCELENTES O BUENOS
  filter(Avance >= 0.5) %>% #3. Con 50% de avance o mas
  filter(Dif %in% c("Diferido")) %>% #4. Sin diferidos
  filter(Creditos.Activos.Restructurados == 0) %>% #5. Sin reestructuras
  filter(Creditos.Castigados == 0)%>%#6. Sin castigos
  filter(Liquidado == "si")
length(df_4$No_Cliente)

df_2 <- rbind(df_2,df_4)
length(unique(df_2$No_Cliente))
length(df_2$No_Cliente)


#Clientes con recompra que mo cumplen condiciones
df_3 <- df_1 %>% filter(Atraso_status %in% c("a) Al corriente")) %>%# 1. Clientes al corriente
  filter(Calificacion %in% c("MUY GRAVE","GRAVE","LEVE")) %>% #2. Calificaciones EXCELENTES O BUENOS
  filter(Avance >= 0.5) %>% #3. Con 50% de avance o mas
  #filter(Dif %in% c("No diferido")) %>% #4. Sin diferidos
  #filter(Creditos.Activos.Restructurados == 0) %>% #5. Sin reestructuras
  filter(Creditos.Castigados == 0)%>%#6. Sin castigos
  filter(Liquidado == "si")
length(df_3$No_Cliente)


#SI TIENE CALIFICACION MUY GRAVE GRAVE Y LEVE SIN INCREMENTO
df_3$monto_recompra <- df_3$Monto.Desembolsado#if_else(df_3 %in% c("MUY GRAVE","GRAVE"),df_3$Monto.Desembolsado*(1-0.2),df_3$Monto.Desembolsado)
df_3$monto_recompra <- round_any(df_3$monto_recompra,accuracy = 100,f = ceiling)
mean(df_3$monto_recompra)
mean(df_3$Monto.Desembolsado)

df_0 <- rbind(df_2,df_3)
length(df_0$No_Cliente)
length(unique(df_0$No_Cliente))
names(df_0)

recompra <- df_0[,c(1:15,47,23,42,43,45,16)]
names(recompra)


credval <- read.csv(paste0("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Escritorio/Cosechas/Acumulado_BimboId/","/CREDVAL_PLUS_20220618",".csv"))
credval <- credval %>% 
  mutate(Nombre.del.Cliente = toupper(stri_trans_general(sub("([[:punct:]])","",Nombre.del.Cliente),"Latin-ASCII")))

credval <- credval %>% 
  group_by(Nombre.del.Cliente) %>%
  dplyr::summarise(recompras = n(),#Numero de crediros recomprados
                   Fecha_reciente = max(Fecha.de.Autorizacion),#Fechas de desembolso mas reciente
                   posicion = which(Fecha.de.Autorizacion == max(Fecha.de.Autorizacion))[1],#posicion de la fecha mas reciente en el arreglo
                   bimboid = ifelse(Fecha_reciente == max(Fecha.de.Autorizacion),Cliente.Bimbo[posicion],"error"))#Credito mas reciente

length(credval$Nombre.del.Cliente)
length(unique(credval$Nombre.del.Cliente))
length(credval$bimboid)
length(unique(credval$bimboid))
sapply(credval, function(x)sum(is.na(x)))



#View(credval %>% filter(duplicated(Nombre.del.Cliente)))
names(credval)
#credval <- read.csv(paste0("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Escritorio/Credval_originacion/",year,"/",mes," - ", month,"/",dia,"/CREDVAL_PLUS_",year,mes,dia,".csv"))
recompra <- recompra %>% left_join(credval[,c(1,5)],by = c("X.Nombre.del.cliente"="Nombre.del.Cliente"))
names(recompra)
recompra <- recompra[,c(22,1:20,21)]
recompra <- recompra %>% mutate(Tasa_Interes.Anual = Tasa_Interes.Anual/100)
names(recompra)
length(recompra$No_Cliente)
sapply(recompra, function(x)sum(is.na(x)))

length(cali$No_Cliente)
cali <- cali %>% left_join(credval[,c(1,5)],by = c("X.Nombre.del.cliente"="Nombre.del.Cliente"))
sapply(cali, function(x)sum(is.na(x)))
#View(cali %>% filter(is.na(Cliente.Bimbo)))
names(cali)
#tasa oferta ====
#Calificaciones EXCLENTES y BUENOS se quedan con la tasa que tenian 
# otra calificacion se les asigna la nueva tasa segun el monto que tengan
Tasa.Asignada <- function(z){
  #y pre.oferta 1 
  #x Urbano  o Rural  
  y <- rep(0,length(z))
  #Urbano****    
  for (i in 1:length(z)) {
    if(z[i] >= 0 & z[i] <= 6000){
      y[i] <- 0.94
      
    }
    if(z[i] > 6000 & z[i] <= 8000){
      y[i] <- 0.90
      
    }
    if(z[i] > 8000 & z[i] <= 10000){
      y[i] <- 0.88
      
    }
    if(z[i] > 10000 & z[i] <= 15000){
      y[i] <- 0.87
      
    }
    if(z[i] > 15000){
      y[i] <- 0.85
      
    }
  }

  return(y)
}
#Recompras
recompra <- recompra %>% mutate(Tasa_2 = Tasa.Asignada(monto_recompra))
table(recompra$Tasa_2)
names(recompra)
recompra <- recompra %>% mutate(Tasa_Interes.Anual = if_else(Calificacion %in% c("EXCELENTE","BUENO"),Tasa_Interes.Anual,Tasa_2))
table(recompra$Tasa_Interes.Anual)
names(recompra)
recompra <- recompra[-23]  

#Listado completo
cali <- cali %>% mutate(Tasa_2 = Tasa.Asignada(monto_recompra))
table(cali$Tasa_2)

cali <- cali %>% mutate(Tasa_Interes.Anual = if_else(Calificacion %in% c("EXCELENTE","BUENO"),Tasa_Interes.Anual/100,Tasa_2))
table(cali$Tasa_Interes.Anual)

cali <- cali %>% mutate(monto_recompra = if_else(Calificacion %in% c("LEVE","GRAVE","MUY GRAVE"),Monto.Desembolsado,monto_recompra))
names(cali)

cali <- cali %>% dplyr::select(bimboid,1:30)#,15,17
names(cali)

#Prioridades ====
# Obtener la lista de archivos en la carpeta
archivos <- list.files("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Escritorio/Cosechas/CredVal", pattern = "^CREDVAL_PLUS_")
archivos
# Ordenar los archivos por fecha de modificación (el más reciente primero)
archivos_ordenados <- sort(archivos, decreasing = TRUE)
archivos_ordenados
# Obtener el nombre del archivo más reciente
archivo_mas_reciente <- archivos_ordenados[1]
archivo_mas_reciente
# Leer el archivo más reciente
credval <- read_csv(paste0("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Escritorio/Cosechas/CredVal/", archivo_mas_reciente),
                        locale = locale(encoding = "WINDOWS-1252"))



credval <- credval %>% dplyr::select(`Numero de Cliente`,`Numero de Credito`,`Fecha de Autorizacion`,
                              `Fecha de Proximo Pago`,`Fecha Ult Pago`)

fecha <- as.Date(paste0(year,"-",mes,"-",dia),format = "%Y-%m-%d") + 7#fecha de cierre de semana
fecha_inicial <- fecha - 6#fecha de inicio de semana
cali <- cali %>% left_join(credval,by = c("No_Cliente"="Numero de Cliente","No_Credito"="Numero de Credito"))
cali$`Fecha de Proximo Pago` <- as.Date(cali$`Fecha de Proximo Pago`,format = "%d/%m/%Y")
cali$`Fecha Ult Pago` <- as.Date(cali$`Fecha Ult Pago`,format = "%d/%m/%Y")
cali$`Fecha de Proximo Pago`[is.na(cali$`Fecha de Proximo Pago`)] <- as.Date("1900-01-01")
cali$`Fecha Ult Pago`[is.na(cali$`Fecha Ult Pago`)] <- as.Date("1900-01-01")
cali <- cali %>% mutate(pago_sem = if_else(`Fecha de Proximo Pago`>= fecha_inicial & `Fecha de Proximo Pago` <= fecha,1,0))
table(cali$pago_sem)

cali <- cali %>% mutate(avance2 = (Cuotas.Pagadas + 1)/Total.Cuotas)
cali <- cali %>% mutate(avance2 = ifelse(Liquidado == "si",1,avance2))
cali <- cali %>% mutate(avance2 = ifelse(is.infinite(avance2),0,avance2))

cali <- cali %>% mutate(prioridad = if_else(Avance >= 0.85 & Avance <= 0.999999, "Prioridad 1",
                                            if_else(Avance >= 0.70 & Avance <= 0.8499999, "Prioridad 2",
                                                    if_else(Avance >= 0.5 & Avance <= 0.69999, "Prioridad 3","Sin prioridad"))))#& Avance <= 0.5 & pago_sem == 1 & Atraso_status == "a) Al corriente" & Calificacion %in% c("EXCELENTE","BUENO")
table(cali$prioridad)
names(cali)
names(recompra)
recompra <- recompra %>% left_join(cali[c(2,37)], by = "No_Cliente")
table(recompra$prioridad[recompra$Calificacion %in% c("EXCELENTE","BUENO")])
table(recompra$prioridad)
cali2 <- cali %>% 
  filter(prioridad =="Prioridad 2") %>%
  filter(Creditos.Castigados == 0) %>%
  filter(Creditos.Activos.Restructurados == 0) %>%
  filter(Dif == "No diferido") %>%
  filter(Atraso_status %in% c("a) Al corriente","b) 1-3")) %>%
  filter(Calificacion %in% c("EXCELENTE","BUENO")) %>%
  dplyr::select(bimboid, No_Cliente,No_Credito,X.Nombre.del.cliente,Telefono.celular,Telefono.del.Negocio,Calle,No.Interior,
                No.Exterior,Manzana,Lote,Colonia,Municipio,Entidad.Federativa,CP,Correo,
                monto_recompra,Tasa_Interes.Anual,Avance,
                Calificacion,Liquidado,Producto,prioridad)
length(cali2$No_Cliente)

recompra <- rbind(recompra,cali2)
length(unique(recompra$No_Cliente))
length(recompra$bimboid)

recompra <- recompra %>% distinct(No_Cliente,.keep_all = T)
cali <- cali %>% distinct(No_Cliente,.keep_all = T)
names(OD)
names(recompra)

recompra <- recompra %>% left_join(cali[,c(2,25)], by = "No_Cliente")
recompra <- recompra %>% mutate( monto_recompra = if_else(Calificacion %in% c("LEVE","GRAVE","MUY GRAVE"),0,monto_recompra))

sum(recompra$Monto.Desembolsado > recompra$monto_recompra)
#recompra['val'] <- recompra$Monto.Desembolsado >= recompra$monto_recompra
#monto_recompra <- if_else(monto_recompra > 24000,24000,monto_recompra)#Topar a 3000 UDIS
#monto_recompra <- if_else(monto_recompra < df_1$Monto.Desembolsado,0,monto_recompra)

#PEOR EXPERIENCIA ====
Peor_Experiencia <- read_csv("C:/Users/mfsierra/fincomun.com.mx/Riesgos - 03_Peor Experiencia/Peor_Experiencia_Ene2024.csv",
                             locale = locale(encoding = "WINDOWS-1252"))
names(Peor_Experiencia)

Peor_Experiencia <- Peor_Experiencia%>%
  dplyr::select(RFC_Cliente,Calif_Cliente)

Peor_Experiencia$RFC_Cliente <- substr(Peor_Experiencia$RFC_Cliente, 1, 10)

credval <- read_csv(paste0("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Escritorio/Cosechas/CredVal/", archivo_mas_reciente),
                    locale = locale(encoding = "WINDOWS-1252"))
credval$`RFC del Cliente` <- substr(credval$`RFC del Cliente`, 1, 10)

credval$existe <- 1
length(recompra$bimboid)
length(unique(recompra$No_Cliente))

recompra <- recompra %>% 
  left_join(credval[,c(4,10)], by = c("No_Credito"="Numero de Credito"))
sum(is.na(recompra$`RFC del Cliente`))

recompra <- recompra %>%
  left_join(Peor_Experiencia,by = c("RFC del Cliente"="RFC_Cliente"))



recompra$Calif_Cliente[is.na(recompra$Calif_Cliente)] <- "SIN CALIFICACION"
table(recompra$Calif_Cliente)

recompra <- recompra %>%
  #filter(Calif_Cliente != "MUY GRAVE")%>%
  filter(Calif_Cliente == "SIN CALIFICACION")

#Desembolsos ====
library(haven)
desembolso <- read_sas("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Escritorio/Cosechas/Desembolsos/desembolsos_01ene2016_29feb2023.sas7bdat", 
                       NULL)
table(desembolso$Cre_Produc)
names(desembolso)
desembolso <- desembolso %>% 
  filter(Cre_Produc != 15109)%>%
  filter(Cre_Produc != 15116) %>%
  distinct(RFC,.keep_all = T)
desembolso$RFC <- substr(desembolso$RFC, 1, 10)

recompra <- recompra %>%
  left_join(desembolso[,c(23,5)],by = c("RFC del Cliente"="RFC"))#por RFC 
length(recompra$bimboid)

table(recompra$Cre_Produc)
recompra <- recompra%>%
  filter(is.na(Cre_Produc))
length(recompra$bimboid)
length(unique(recompra$No_Cliente))

recompra <- recompra %>%
  dplyr::select(-c(`RFC del Cliente`,Calif_Cliente,Cre_Produc))
names(recompra)

recompra <- recompra %>%
  distinct(No_Cliente,.keep_all = T)


length(recompra$bimboid)
table(recompra$Producto)

#Etiquetas de gestion ====
CC_sol <- read_excel("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Escritorio/Bimbo RS/Documentación RS/Recompras RS/Recompras_TCONECTA/CC_sol.xlsx")
CC_sol <- CC_sol %>% dplyr::select(`Nombre de Cliente/Prospecto`,`Tipo de Lead`,`Estatus CC`,`Causa de Rechazo de la Llamada`,`Causa de Rechazo del Credito`)
table(CC_sol$`Tipo de Lead`)
table(CC_sol$`Estatus CC`)
CC_sol <- CC_sol %>% 
  filter(`Tipo de Lead` == "Recompras") %>%
  filter(`Estatus CC` %in% c("Rechazo de Credito"))

#table(CC_sol$`Estatus CC`[CC_sol$`Estatus CC`=="Rechazo por Llamada"],CC_sol$`Causa de Rechazo de la Llamada`[CC_sol$`Estatus CC`=="Rechazo por Llamada"])
table(CC_sol$`Estatus CC`[CC_sol$`Estatus CC`=="Rechazo de Credito"],CC_sol$`Causa de Rechazo del Credito`[CC_sol$`Estatus CC`=="Rechazo de Credito"])

#ETIQUETAS DE GESTIÓN
#Necesita una cantidad mayor
#Si le interesó, pero lo va a pensar
#No le interesa
#Tiene crédito con otra institución
#No le gustan los créditos
names(CC_sol)

CC_sol <- CC_sol %>% 
  filter(`Causa de Rechazo del Credito` %in% c("Necesita una cantidad mayor","Si le interesó, pero lo va a pensar",
                                               "No le interesa","No le gustan los créditos"))
 
recompra <- recompra %>% left_join(CC_sol[,c(1,5)],by = c("X.Nombre.del.cliente"="Nombre de Cliente/Prospecto"))

recompra$`Causa de Rechazo del Credito`[is.na(recompra$`Causa de Rechazo del Credito`)] <- "Sin etiqueta"

if(numero_del_mes < 10)
{
  Ofertas_Tconecta <- read_csv(paste0("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Escritorio/Bimbo RS/Ofertas/","0",numero_del_mes," - ",nombre_del_mes,"/",numero_semana,"_completa-Tconecta.csv"))
}else{
  Ofertas_Tconecta <- read_csv(paste0("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Escritorio/Bimbo RS/Ofertas/",numero_del_mes," - ",nombre_del_mes,"/",numero_semana,"_completa-Tconecta.csv"))
}

Ofertas_Tconecta <- Ofertas_Tconecta %>%dplyr::select(bimboid,Oferta.diferenciada)
Ofertas_Tconecta$bimboid <- as.character(Ofertas_Tconecta$bimboid)
table(Ofertas_Tconecta$Oferta.diferenciada)

recompra <- recompra %>% left_join(Ofertas_Tconecta,by = c("bimboid"="bimboid"))
recompra$Oferta.diferenciada[is.na(recompra$Oferta.diferenciada)] <- 0

recompra <- recompra %>% mutate(Oferta.diferenciada = if_else(Oferta.diferenciada <= monto_recompra,monto_recompra,Oferta.diferenciada))
recompra <- recompra %>% mutate(Oferta.diferenciada = if_else(Oferta.diferenciada < monto_recompra,0,Oferta.diferenciada))
recompra <- recompra %>% mutate(Oferta.diferenciada = if_else(Calificacion %in% c("LEVE","GRAVE","MUY GRAVE"),0,Oferta.diferenciada))
recompra$Oferta.diferenciada[recompra$`Causa de Rechazo del Credito` == "Sin etiqueta"] <- 0
#recompra <- recompra[-25]


#exportar data's

# CAMPAÑA VIP CRECIMIENTO DE CUENTA 
terri_fc <- read_excel("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Escritorio/Bimbo RS/Documentación RS/Recompras RS/Recompras_TCONECTA/Recompras_TCONECTA_FC.xlsx")
names(terri_fc)

recompra <- recompra %>% left_join(terri_fc[,c(2,24,26)],by = "No_Cliente")
recompra$territorio[is.na(recompra$territorio)] <- "Sin territorio"
recompra$no_suc[is.na(recompra$no_suc)] <- "Sin sucursal"
casos <- OD %>% 
  group_by(No_Cliente) %>%
  dplyr::summarise(recompras = n(),
                   productos = length(unique(Producto)))
head(casos)
recompra <- recompra %>% left_join(casos,by = "No_Cliente")
table(recompra$recompras[recompra$Calificacion == "EXCELENTE" & recompra$territorio != "Sin territorio"])
table(recompra$recompras[recompra$Calificacion == "BUENO"& recompra$territorio != "Sin territorio"])
table(recompra$recompras[recompra$Calificacion == "EXCELENTE" & recompra$Avance >= 0.5])
table(recompra$recompras[recompra$Calificacion == "BUENO" & recompra$Avance >= 0.5])

recompra <- recompra %>% distinct(No_Cliente,.keep_all = T)
recompra <- recompra %>% mutate(campana_vip = if_else(recompras >=3 & Calificacion %in% c("EXCELENTE","BUENO"),"VIP","Sin oferta"))
table(recompra$campana_vip)

length(recompra$bimboid)
length(unique(recompra$No_Cliente))

if(numero_del_mes < 10)
{
  Ofertas_Tconecta <- read_csv(paste0("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Escritorio/Bimbo RS/Ofertas/","0",numero_del_mes," - ",nombre_del_mes,"/",numero_semana,"_completa-Tconecta.csv"))
  
}else{
  Ofertas_Tconecta <- read_csv(paste0("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Escritorio/Bimbo RS/Ofertas/",numero_del_mes," - ",nombre_del_mes,"/",numero_semana,"_completa-Tconecta.csv"))
}

Ofertas_Tconecta <- Ofertas_Tconecta %>%dplyr::select(bimboid,pre_oferta)
Ofertas_Tconecta$bimboid <- as.character(Ofertas_Tconecta$bimboid)
Ofertas_Tconecta$pre_oferta <- round_any(Ofertas_Tconecta$pre_oferta,accuracy = 100,f = ceiling)
recompra <- recompra %>% left_join(Ofertas_Tconecta,by = c("bimboid"="bimboid"))
recompra <- recompra %>% mutate(oferta_vip = if_else(campana_vip == "VIP",pre_oferta,0))
recompra <- recompra %>% mutate(oferta_vip = if_else(campana_vip == "VIP" & oferta_vip < 24000,25000,oferta_vip))
recompra <- recompra %>% mutate(oferta_vip = if_else(campana_vip == "VIP" & oferta_vip >25000 & oferta_vip <= 30000,30000,oferta_vip))
recompra <- recompra %>% mutate(oferta_vip = if_else(campana_vip == "VIP" & is.na(oferta_vip),25000,oferta_vip))
table(recompra$oferta_vip[recompra$campana_vip == "VIP" & recompra$pre_oferta <= 18000])
names(recompra)

colnames(recompra)[c(17,24,26)] <- c("oferta_plus","oferta_diaria","oferta_oportunidad")
length(recompra$bimboid)
recompra <- recompra %>%
  left_join(cali[c(2,26)],by = "No_Cliente" )
length(recompra$bimboid)
colnames(recompra)[34] <- "Estatus"

#recompra$Estatus <- "a) Al corriente"
recompra$Tasa_recuperado <- recompra$Tasa_Interes.Anual - 0.02
recompra <- recompra %>% mutate(Etiqueta_de_riesgo = if_else(oferta_vip != 0, "Verificar capacidad de pago","No aplica"))
recompra <- recompra %>% 
  mutate(prioridad = if_else(Calificacion %in% c("EXCELENTE","BUENO") & Liquidado == "si","Recuperado",prioridad))
recompra_1 <- recompra %>%dplyr::select(1:16,territorio,no_suc,19:21,Estatus,prioridad,Tasa_Interes.Anual,Tasa_recuperado,
                                  oferta_diaria,oferta_plus,oferta_oportunidad,campana_vip,oferta_vip,Etiqueta_de_riesgo,
                                  Producto,recompras)

#recompra_1 <- recompra_1 %>%
#  mutate(oferta_vip = if_else(max(oferta_plus,oferta_oportunidad) > oferta_vip & max(oferta_plus,oferta_oportunidad) > 24000),)

names(recompra_1)
names(cali)

table(recompra_1$Avance[recompra_1$prioridad == "Sin prioridad"])
table(recompra_1$Calificacion[recompra_1$prioridad == "Sin prioridad"])
table(recompra_1$prioridad)
table(recompra_1$Producto)

#Obtener el credito mas reciente con base en la fecha de desembolso
Reciente <- OD %>% 
  group_by(No_Cliente) %>% 
  dplyr::summarise(recompras = n(),#Numero de crediros recomprados
                   Fecha_reciente = max(Fecha.Desembolso),#Fechas de desembolso mas reciente
                   posicion = which(Fecha.Desembolso == max(Fecha.Desembolso))[1],#posicion de la fecha mas reciente en el arreglo
                   Num_credito = ifelse(Fecha_reciente == max(Fecha.Desembolso),No_Credito[posicion],"error"),
                   producto = ifelse(Fecha_reciente == max(Fecha.Desembolso),Producto[posicion],"error"))#Credito mas reciente
names(Reciente)
recompra_1 <- recompra_1 %>%
  left_join(Reciente[,c(1,6)],by = "No_Cliente")
recompra_1 <- recompra_1 %>%
  filter(producto %in% c(15109,15116))
table(recompra_1$producto)                                                             
max(recompra_1$oferta_diaria)
max(recompra_1$oferta_plus)
max(recompra_1$oferta_oportunidad)
#recompra_1$oferta_diaria[recompra_1$oferta_diaria > 18000] <- 18000
#recompra_1$oferta_plus[recompra_1$oferta_plus > 18000] <- 18000
#recompra_1$oferta_oportunidad[recompra_1$oferta_oportunidad > 18000] <- 18000

table(recompra_1$producto)
table(recompra_1$Producto)

recompra_1 <- recompra_1 %>%
  mutate(Tasa_recuperado = if_else(prioridad == "Recuperado" & Tasa_Interes.Anual >= 0.72,Tasa_recuperado,Tasa_Interes.Anual))

recompra_1 <- recompra_1 %>%
  mutate(Tasa_recuperado = if_else(prioridad == "Recuperado",Tasa_recuperado,0))

recompra_1 <- recompra_1 %>%
  mutate(oferta_plus = if_else(oferta_plus == oferta_diaria,0,oferta_plus))

names(recompra_1)
recompra_1 <- recompra_1[-32]
names(cali)
cali1 <- cali[,c(1:16,18:26,17,37,31)]
table(cali1$Producto)
table(recompra_1$producto)
recompra_1 <- recompra_1 %>% filter(Calificacion != "MUY GRAVE")
cali1 <- cali1 %>% filter(Producto %in% c(15109,15116))#15116

#Red flags !!!
#RedFlags <- read_excel("C:/Users/mfsierra/fincomun.com.mx/Beatriz Samano Figueroa - Afiliados TConecta acumulado/Red Flags/RedFlags.xlsx")
#RedFlags$Flag <- 1

#recompra_1 <- recompra_1 %>% left_join(RedFlags[c(3,8)],by = "No_Cliente")
#recompra_1 <- recompra_1 %>% filter(is.na(Flag))
#recompra_1$Flag <- NULL

# CAMPANA INCREMENTO ====

fecha_actual <- Sys.Date()# Obtener la fecha actual

# Obtener la fecha de cierre del mes anterior
fecha_cierre_mes_anterior <- floor_date(fecha_actual, unit = "month") - days(1)

# Imprimir la fecha de cierre del mes anterior
print(fecha_cierre_mes_anterior)

OD  <- OD %>% 
  mutate(liquidacion_mes_anterior = if_else(Fecha.de.liquidacion.del.credito <= fecha_cierre_mes_anterior,1,0)) 

OD$liquidacion_mes_anterior[is.na(OD$liquidacion_mes_anterior)] <- 0

recompra_1 <- recompra_1 %>%
  left_join(OD[c(2,29)],by = "No_Credito")

c_incremento <- recompra_1 %>% 
  filter(liquidacion_mes_anterior == 1) %>%
  filter(Calificacion %in% c("EXCELENTE","BUENO"))%>%
  #filter(Liquidado == "si") %>%
  filter(campana_vip != "VIP")%>%
  filter(producto == 15109)%>%
  mutate(of_max = pmax(oferta_diaria,oferta_plus,oferta_oportunidad))%>%
  filter(oferta_diaria >= 10000) %>% 
  mutate(#campana_vip = "Incremento",
         oferta_oportunidad = 20000,
         #Etiqueta_de_riesgo = "39 semanas",
         incremento = 1)

names(c_incremento)
c_incremento$liquidacion_mes_anterior <- NULL
c_incremento$of_max <- NULL
recompra_1$liquidacion_mes_anterior <- NULL
names(c_incremento)
recompra_1 <- recompra_1 %>%
  left_join(c_incremento[c(2,34)], by= "No_Cliente" )

recompra_1 <- recompra_1 %>%
  filter(is.na(incremento))

recompra_1 <- rbind(recompra_1,c_incremento)
recompra_1$incremento <- NULL

############################
###### TIPO DE CUENTA    ====
############################

# Obtener la lista de archivos en la carpeta
archivos <- list.files("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Escritorio/Cosechas/Transacciones cuentas", pattern = "^repsal-")
archivos

# Extraer la fecha del nombre del archivo (formato: repsal-YYYYMonDD.csv)
# Usando una expresión regular para capturar la parte YYYYMonDD
fechas <- sub(".*-(\\d{4}[A-Za-z]{3}\\d{2})\\.csv", "\\1", archivos)
fechas

# Convertir las fechas al formato Date en R
Sys.setlocale(category = "LC_TIME", locale = "English") # cambiar a formato ingles 
fechas_convertidas <- as.Date(fechas, format="%Y%b%d")
Sys.setlocale(category = "LC_TIME", locale = "Spanish")
fechas_convertidas

# Ordenar los archivos por fecha de modificación (el más reciente primero)
archivos_ordenados <- archivos[order(fechas_convertidas, decreasing = TRUE)]
archivos_ordenados

# Obtener el nombre del archivo más reciente
archivo_mas_reciente <- archivos_ordenados[1]
archivo_mas_reciente

repsal <-  read_delim(paste0("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Escritorio/Cosechas/Transacciones cuentas/",archivo_mas_reciente), 
                      delim = "|", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
                      trim_ws = TRUE)

repsal <- repsal %>%
  dplyr::select(`Sub-aplic`,`No de Clte`)%>%
  distinct(`No de Clte`,.keep_all = T)

colnames(repsal)[1] <- "codigo_cuenta"
colnames(repsal)[2] <- "Num_Cliente_SBMX"

repsal <- repsal %>%
  mutate(Cuenta = if_else(codigo_cuenta == "80","Simplificada",
                          if_else(codigo_cuenta == "1","Comun",
                                  if_else(codigo_cuenta == "93", "Avanzada",
                                          "Otro tipo de cuenta"))))
credval <- credval %>%
  dplyr::select(`Numero de Cliente`,Num_Cliente_SBMX) %>%
  distinct(`Numero de Cliente`,.keep_all = T)

recompra_1 <- recompra_1 %>%
  left_join(credval,by = c("No_Cliente" = "Numero de Cliente"))

recompra_1 <- recompra_1 %>%
  left_join(repsal,by = "Num_Cliente_SBMX")

recompra_1$codigo_cuenta[is.na(recompra_1$codigo_cuenta)] <- "80"
recompra_1$Cuenta[is.na(recompra_1$Cuenta)] <- "Simplificada"

cali1 <- cali1 %>%
  left_join(credval,by = c("No_Cliente" = "Numero de Cliente"))

cali1 <- cali1 %>%
  left_join(repsal,by = "Num_Cliente_SBMX")

cali1$codigo_cuenta[is.na(cali1$codigo_cuenta)] <- "80"
cali1$Cuenta[is.na(cali1$Cuenta)] <- "Simplificada"

sapply(cali1, function(x) sum(is.na(x)))

### BLMID ====
tconecta <- read_excel("C:/Users/mfsierra/OneDrive - fincomun.com.mx/TConecta/2024_08Ago/ClientesBimbonetConTConecta_28.xlsx")

#Ver cuales afiliados cumplen condiciones
ordenar_data_frame <- function(data_frame, columna_fecha) {
  # Asegurarse de que la columna de fecha sea de tipo Date
  data_frame[[columna_fecha]] <- as.Date(data_frame[[columna_fecha]])
  
  # Ordenar el data frame de forma descendente
  data_frame <- data_frame %>%
    arrange(desc(data_frame[[columna_fecha]]))
  
  return(data_frame)
}
head(tconecta$installDate)
class(tconecta$installDate)
tconecta$installDate <- as.Date(tconecta$installDate,"%Y-%m-%d") 
tconecta <- ordenar_data_frame(tconecta, "installDate")

tconecta <- tconecta %>% 
  mutate(antiguedad = round((Sys.Date()-installDate)/30,3))%>%#Sys.Date()
  drop_na(bimboId)%>%
  drop_na(blmId)%>%
  drop_na(nombre)%>%
  distinct(bimboId,.keep_all = T)%>%
  distinct(blmId,.keep_all = T)%>%
  distinct(nombre,.keep_all = T)

tconecta <- tconecta %>%
  dplyr::select(blmId,bimboId)

recompra_1 <- recompra_1 %>%
  left_join(tconecta,by = c("bimboid"="bimboId"))

cali1 <- cali1 %>%
  left_join(tconecta,by = c("bimboid"="bimboId"))

recompra_1$blmId[is.na(recompra_1$blmId)] <- -1
cali1$blmId[is.na(cali1$blmId)] <- -1

sapply(cali1, function(x) sum(is.na(x)))
sapply(recompra_1, function(x) sum(is.na(x)))

recompra_1 <- recompra_1 %>% mutate(oferta_plus = if_else(oferta_diaria>= oferta_plus,0,oferta_plus))
#recompra_1 <- recompra_1 %>% mutate(oferta_plus = if_else(oferta_diaria>= oferta_plus,0,oferta_plus))
recompra_1 <- recompra_1 %>% mutate(oferta_oportunidad = if_else(oferta_diaria>= oferta_oportunidad,0,oferta_oportunidad))

table(recompra_1$producto)
recompra_1 <- recompra_1 %>% 
  distinct(bimboid,.keep_all = TRUE)
names(recompra_1)



##################################
##############Semaforo###########
################################

# Cargar base de semforo
Prediciones <- read_excel("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Escritorio/Bimbo RS/Modelos ML/XGB Recompras T-Conecta/Prediciones_20241005.xlsx")

# Seleccionar columnas
Prediciones <- Prediciones %>%
  dplyr::select(No_Cliente,Semaforo,Incremento_recom)

# Cruce con base de recompras y Calificaciones
Prediciones$No_Cliente <- as.numeric(Prediciones$No_Cliente)
sum(is.na(Prediciones$No_Cliente))
recompra_1 <- recompra_1 %>% 
  left_join(Prediciones,by = "No_Cliente")

cali1 <- cali1 %>% 
  left_join(Prediciones,by = "No_Cliente")

# Eliminar casos sin semaforo
sapply(recompra_1, function(x)sum(is.na(x)))
sapply(cali1, function(x)sum(is.na(x)))

recompra_1 <- recompra_1%>%
  distinct(No_Cliente,.keep_all = TRUE)%>%
  drop_na(Semaforo)

cali1 <- cali1%>%
  distinct(No_Cliente,.keep_all = TRUE)%>%
  drop_na(Semaforo)
table(cali1$Semaforo)



# Linea maxima
cali1 <- cali1 %>%
  mutate(Linea_maxima = if_else(Semaforo == "Verde", pmax(Monto.Desembolsado,monto_recompra) + Incremento_recom,
                                if_else(Semaforo == "Amarillo",pmax(Monto.Desembolsado,monto_recompra) + Incremento_recom*0.8,
                                        if_else(Semaforo %in% c("Naranja", "Rojo"),0,-1))))
recompra_1 <- recompra_1 %>%
  mutate(Linea_maxima = if_else(Semaforo == "Verde", pmax(oferta_diaria,oferta_plus,oferta_oportunidad,oferta_vip) + Incremento_recom,
                                if_else(Semaforo == "Amarillo",pmax(oferta_diaria,oferta_plus,oferta_oportunidad,oferta_vip) + Incremento_recom*0.8,
                                        if_else(Semaforo %in% c("Naranja", "Rojo"),0,-1))))

cali1 <- cali1 %>% mutate(Linea_maxima = if_else(Linea_maxima > 100000,100000,Linea_maxima))
recompra_1 <- recompra_1 %>% mutate(Linea_maxima = if_else(Linea_maxima > 100000,100000,Linea_maxima))

names(cali1)

###############################
####### Llamada #############
###########################
# Crear la columna 'llamada' para 'Verde', 'Rojo', y 'Naranja'
cali1 <- cali1 %>%
  mutate(llamada = case_when(
    Semaforo == "Verde" ~ "Sin llamada",
    Semaforo == "Rojo" | Semaforo == "Naranja" ~ "Con llamada",
    TRUE ~ NA_character_  # Temporalmente NA para los Amarillos
  ))

# Filtrar los "Amarillos" y asignar las etiquetas manualmente
amarillos <- cali1 %>%
  filter(Semaforo == "Amarillo")

# Determinar el número de casos "Amarillo"
n_amarillos <- nrow(amarillos)

# Asignar 50% "Con llamada" y 50% "Sin llamada"
amarillos <- amarillos %>%
  mutate(llamada = c(rep("Con llamada", n_amarillos / 2), 
                     rep("Sin llamada", n_amarillos / 2)))

# Reunir todos los datos con la nueva columna 'llamada' asignada correctamente
cali1 <- cali1 %>%
  filter(Semaforo != "Amarillo") %>%
  bind_rows(amarillos)

# Verificar la distribución de las nuevas etiquetas
table(cali1$Semaforo, cali1$llamada)

recompra_1 <- recompra_1 %>%
  left_join(cali1[c(2,36)],by = "No_Cliente")
names(recompra_1)
table(recompra_1$Semaforo, recompra_1$llamada)
length(recompra_1$bimboid)

#### Indicador de si era VIP ----
cali1 <- cali1 %>%
  mutate(val_vip = if_else(Monto.Desembolsado > 24000, "VIP","Normal"))

recompra_1 <- recompra_1 %>%
  left_join(cali1[c(2,37)], by = "No_Cliente")


##################
### ADV #######
################
adv <- read_csv("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Escritorio/Bimbo RS/Ofertas/09 - Septiembre/ADV_ofertas_20240913.csv")
adv <- adv %>%
  dplyr::select(BLMID,Oferta_final_Bimbonet)
names(adv)

# limitar montos
cali1 <- cali1 %>% 
  mutate(monto_recompra = if_else(Producto == 15116,0,monto_recompra))


recompra_1 <- recompra_1 %>%
  mutate(oferta_plus = if_else(producto == 15116,0,oferta_plus),
         oferta_oportunidad = if_else(producto == 15116,0,oferta_oportunidad),
         oferta_diaria = if_else(producto == 15116 & oferta_diaria >24000,24000,oferta_diaria),
         oferta_vip = if_else(producto == 15116,0,oferta_vip),
         campana_vip = if_else(producto == 15116,"Sin oferta",campana_vip),
         Etiqueta_de_riesgo = if_else(producto == 15116,"No aplica",Etiqueta_de_riesgo))

recompra_1 <- recompra_1 %>%
  left_join(adv,by = c("blmId"="BLMID"))

sum(is.na(recompra_1$Oferta_final_Bimbonet))
recompra_1 <- recompra_1 %>%
  mutate(Producto_anterior = producto)%>%
  mutate(producto = if_else(is.na(Oferta_final_Bimbonet), 15109, producto))

names(recompra_1)
table(recompra_1$producto,recompra_1$Liquidado)
table(recompra_1$Producto_anterior)

adv_reno <- recompra_1 %>% 
  filter(producto == 15116) %>%
  filter(campana_vip != "VIP")%>%
  filter(Liquidado == "si")%>%
  mutate(oferta_plus = if_else(oferta_diaria >= Oferta_final_Bimbonet & Oferta_final_Bimbonet <= 24000, 0,
                               if_else(oferta_diaria < Oferta_final_Bimbonet & Oferta_final_Bimbonet <= 24000, Oferta_final_Bimbonet,24000)),
         oferta_vip = if_else(oferta_diaria < Oferta_final_Bimbonet & Oferta_final_Bimbonet > 24000, Oferta_final_Bimbonet,0),
         campana_vip = if_else(oferta_vip != 0,"VIP",campana_vip))
length(adv_reno$bimboid)
adv_reno <- adv_reno %>%
  mutate(oferta_vip = case_when(
    oferta_vip >= 40000 & recompras > 2 ~ 40000,
    oferta_vip >= 30000 & recompras == 2 ~ 30000,
    oferta_vip >= 26000 & recompras < 2 ~ 26000,
    TRUE ~ oferta_vip  # Esto asegura que se mantengan los valores originales si no se cumple ninguna condición
  ))
table(adv_reno$producto)
table(recompra_1$producto)

# limitar incrementos
adv_reno <- adv_reno %>%
  mutate(Incremento_recom = if_else(producto == 15116, 0,Incremento_recom),
         Linea_maxima = if_else(producto == 15116,0,Linea_maxima))

# Llamada
adv_reno <- adv_reno %>%
  mutate(llamada = if_else(producto == 15116, "Con llamada",llamada))

recompra_1 <- recompra_1 %>%
  filter(producto!=15116)

table(recompra_1$producto)

recompra_1 <- rbind(recompra_1,adv_reno)
table(recompra_1$producto)

names(recompra_1)
recompra_1$Oferta_final_Bimbonet <- NULL





#RENOVACIONES ====

renovaciones <- recompra_1 %>% 
  filter(producto %in% c(15109,15116)) %>%
  filter(Liquidado == "si") %>% 
  filter(Estatus == "a) Al corriente") %>%
  filter(Calificacion %in% c("EXCELENTE","BUENO","LEVE","GRAVE"))
length(renovaciones$bimboid)
OD <- OD %>% 
  mutate(tipo_liquidacion = case_when(Fecha.de.liquidacion.del.credito < Fecha.Vencimiento.Credito ~ "a) Liquido anticipado",
                                      Fecha.de.liquidacion.del.credito == Fecha.Vencimiento.Credito ~ "b) Liquido en tiempo",
                                      Fecha.de.liquidacion.del.credito > Fecha.Vencimiento.Credito ~ "c) Liquido con atraso",
                                      is.na(Fecha.de.liquidacion.del.credito) ~ "d) Vigente",
                                      TRUE ~ "e) sin dato"),
         meses_liquidacion = round((Sys.Date()-Fecha.de.liquidacion.del.credito)/30.3334,0),
         Rango_liquidacion = case_when(meses_liquidacion == 0 ~ "a) Recientemente",
                                       meses_liquidacion > 0 & meses_liquidacion <= 1 ~ "b) < 1 mes",
                                       meses_liquidacion > 1 & meses_liquidacion <= 3 ~ "c) < 3 meses",
                                       meses_liquidacion > 3 & meses_liquidacion <= 6 ~ "d) < 6 meses",
                                       meses_liquidacion > 6 & meses_liquidacion <= 12 ~ "e) < 12 meses",
                                       meses_liquidacion > 12 ~ "f) + 12 meses",
                                       is.na(meses_liquidacion)~ "g) Vigente",
                                       TRUE ~ "h) Sin dato"))
names(OD)
renovaciones <- renovaciones %>% 
  inner_join(OD[,c(2,30,32)],by = "No_Credito" )%>%
  distinct(No_Cliente,.keep_all = T)
length(renovaciones$bimboid)
length(unique(renovaciones$bimboid))
length(renovaciones$No_Cliente)
length(unique(renovaciones$No_Cliente))
names(renovaciones)
table(renovaciones$tipo_liquidacion)
#table(renovaciones$meses_liquidacion)
table(renovaciones$Rango_liquidacion)
table(cali1$Calificacion)

#View(cali %>% select(No_Cliente,Total.Cuotas,Cuotas.Pagadas,Frecuencia,Calificacion,Atraso_status,Avance,avance2,prioridad,pago_sem,`Fecha de Proximo Pago`) )
#View(recompra %>% select(No_Cliente,Calificacion,prioridad,Avance))
sapply(recompra_1, function(x)sum(is.na(x)))
length(unique(recompra_1$No_Cliente))
length(recompra_1$No_Cliente)

#VIP
vip <- recompra_1 %>% 
  filter(campana_vip %in% c("VIP"))%>%
  filter(producto == 15109)


vip <- vip %>% mutate(oferta_vip_2 = if_else(oferta_vip > 100000,100000,oferta_vip))
recompra_1 <- recompra_1 %>% 
  mutate(oferta_vip= if_else(oferta_vip > 100000,100000,oferta_vip))

#Recompras aumentar monto
#recompra_1 <- recompra_1 %>%
#  mutate(oferta_oportunidad = if_else((Calificacion == "EXCELENTE") & (oferta_diaria >= 5000 & oferta_diaria <= 17999) & (oferta_plus <= 18000),18000,0))

recompra_1 <- recompra_1 %>%
  mutate(oferta_oportunidad = if_else( oferta_plus >= oferta_oportunidad,0,oferta_oportunidad))

recompra_1 <- recompra_1 %>%
  mutate(oferta_oportunidad = if_else( Calificacion %in% c("LEVE","GRAVE","MUY GRAVE"),0,oferta_oportunidad),
         oferta_plus = if_else( Calificacion %in% c("LEVE","GRAVE","MUY GRAVE"),0,oferta_plus))

adv <- recompra_1 %>% 
  filter(producto == 15116)%>%
  filter(Liquidado == "si")

recompra_1 <- recompra_1%>%
  filter(producto != 15116)

recompra_1 <- rbind(recompra_1,adv)


#Renovaciones aumentar monto
#renovaciones <- renovaciones %>%
#  mutate(oferta_oportunidad = if_else(Calificacion == "EXCELENTE" & oferta_diaria >= 5000 & oferta_diaria <= 17999,18000,0))

#renovaciones <- renovaciones %>%
#  mutate(oferta_vip = if_else(oferta_vip > 50000,50000,oferta_vip))
renovaciones <- renovaciones %>%
  mutate(oferta_oportunidad = if_else( oferta_plus >= oferta_oportunidad,0,oferta_oportunidad))

renovaciones <- renovaciones %>%
  mutate(oferta_oportunidad = if_else( Calificacion %in% c("LEVE","GRAVE","MUY GRAVE"),0,oferta_oportunidad),
         oferta_plus = if_else( Calificacion %in% c("LEVE","GRAVE","MUY GRAVE"),0,oferta_plus))

length(renovaciones$bimboid)
length(recompra_1$bimboid)
length(recompra_1$bimboid)
length(unique(recompra_1$No_Cliente))

#cali1 <- cali1 %>% 
#  mutate(Dif = if_else(Calificacion %in% c("EXCELENTE","BUENO"),"No diferido",Dif))
#write.csv(cali1,paste0("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Recompras_bimbonet/Calificaciones_TCONECTA_",year,"0",mes,"0",dia,"_extra.csv"),row.names = F,fileEncoding = "WINDOWS-1252")
#write.csv(recompra_1,paste0("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Recompras_bimbonet/Recompras_TCONECTA_",year,"0",mes,"0",dia,"_extra.csv"),row.names = F,fileEncoding = "WINDOWS-1252")
#write.csv(cali1,paste0("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Recompras_bimbonet/cali_",year,"0",mes,"0",dia,".csv"),row.names = F,fileEncoding = "WINDOWS-1252")


table(recompra_1$producto,recompra_1$Liquidado)


# Limpieza final
#recompra_1 <- recompra_1 %>% 
#  filter(producto == 15109)
#length(recompra_1$bimboid)

recompra_1 <- recompra_1 %>% 
  distinct(No_Cliente,.keep_all = TRUE)

#cali1 <- cali1 %>% 
#  filter(Producto == 15109)
#length(cali1$bimboid)

cali1 <- cali1 %>% 
  distinct(No_Cliente,.keep_all = TRUE)

length(cali1$bimboid)

#renovaciones <- renovaciones %>% 
#  filter(producto == 15109)
length(renovaciones$bimboid)

renovaciones <- renovaciones %>% 
  distinct(No_Cliente,.keep_all = TRUE)

length(renovaciones$bimboid)

#Incrementos extra
#recompra_1 <- 

if(nchar(as.character(mes)) < 2)
{
  if(nchar(dia) < 2)
  {
    write.csv(renovaciones,paste0("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Recompras_bimbonet/Renovaciones_TCONECTA_",year,"0",mes,"0",dia,".csv"),row.names = F,fileEncoding = "WINDOWS-1252")
    write.csv(recompra_1,paste0("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Recompras_bimbonet/Recompras_TCONECTA_",year,"0",mes,"0",dia,".csv"),row.names = F,fileEncoding = "WINDOWS-1252")
    write.csv(cali1,paste0("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Recompras_bimbonet/Calificaciones_TCONECTA_",year,"0",mes,"0",dia,".csv"),row.names = F,fileEncoding = "WINDOWS-1252")
    write.csv(vip,paste0("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Recompras_bimbonet/VIP_TCONECTA_",year,"0",mes,"0",dia,".csv"),row.names = F,fileEncoding = "WINDOWS-1252")
    
  }else{
    write.csv(renovaciones,paste0("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Recompras_bimbonet/Renovaciones_TCONECTA_",year,"0",mes,dia,".csv"),row.names = F,fileEncoding = "WINDOWS-1252")
    write.csv(recompra_1,paste0("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Recompras_bimbonet/Recompras_TCONECTA_",year,"0",mes,dia,".csv"),row.names = F,fileEncoding = "WINDOWS-1252")
    write.csv(cali1,paste0("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Recompras_bimbonet/Calificaciones_TCONECTA_",year,"0",mes,dia,".csv"),row.names = F,fileEncoding = "WINDOWS-1252")
    write.csv(vip,paste0("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Recompras_bimbonet/VIP_TCONECTA_",year,"0",mes,dia,".csv"),row.names = F,fileEncoding = "WINDOWS-1252")
  }
}else{
  if(nchar(dia) < 2)
  {
    write.csv(renovaciones,paste0("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Recompras_bimbonet/Renovaciones_TCONECTA_",year,mes,"0",dia,".csv"),row.names = F,fileEncoding = "WINDOWS-1252")
    write.csv(recompra_1,paste0("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Recompras_bimbonet/Recompras_TCONECTA_",year,mes,"0",dia,".csv"),row.names = F,fileEncoding = "WINDOWS-1252")
    write.csv(cali1,paste0("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Recompras_bimbonet/Calificaciones_TCONECTA_",year,mes,"0",dia,".csv"),row.names = F,fileEncoding = "WINDOWS-1252")
    write.csv(vip,paste0("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Recompras_bimbonet/VIP_TCONECTA_",year,mes,"0",dia,".csv"),row.names = F,fileEncoding = "WINDOWS-1252")
    
  }else{
    write.csv(renovaciones,paste0("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Recompras_bimbonet/Renovaciones_TCONECTA_",year,mes,dia,".csv"),row.names = F,fileEncoding = "WINDOWS-1252")
    write.csv(recompra_1,paste0("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Recompras_bimbonet/Recompras_TCONECTA_",year,mes,dia,".csv"),row.names = F,fileEncoding = "WINDOWS-1252")
    write.csv(cali1,paste0("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Recompras_bimbonet/Calificaciones_TCONECTA_",year,mes,dia,".csv"),row.names = F,fileEncoding = "WINDOWS-1252")
    write.csv(vip,paste0("C:/Users/mfsierra/OneDrive - fincomun.com.mx/Recompras_bimbonet/VIP_TCONECTA_",year,mes,dia,".csv"),row.names = F,fileEncoding = "WINDOWS-1252")
    
  }
}

#Enviar correo ====

correos <- c(
  "besamano@fincomun.com.mx",
  "mreynolds@fincomun.com.mx",
  "yspargoa@fincomun.com.mx",
  "ecalderon@fincomun.com.mx",
  "cbello@fincomun.com.mx",
  "bc.mescamilla@fincomun.com.mx",
  "bestradav@fincomun.com.mx",
  "aantonio@fincomun.com.mx"
)

#correos <- c("mfsierra@fincomun.com.mx","ecalderon@fincomun.com.mx")
my_outlook <- get_business_outlook(tenant = "fincomun.com.mx")

# Formatear el texto HTML con el enlace
link_text <- '<a href="https://fincomuncommx.sharepoint.com/:f:/g/DERCC/Riesgos/EoBoNIuUSBhMo58Ysi4ckzwBbb8bM1tMkiZ8MuGnLEIzBQ?e=iecu89">Liga a la carpeta de Recompras</a>'

# Definir la firma preestablecida en formato HTML
firma <- '<p><strong>Quedo al pendiente de sus comentarios</strong></p>
          <p><strong>Saludos</strong></p>
          <p><strong>Martín Fernando Sierra Ortega</strong></p>'

# Crear el correo electrónico
my_email <- my_outlook$create_email(content_type = "html")$
  set_body(sprintf('<p><strong>Estimada Beatriz, </strong></p>
                  <p><strong>Por medio del presente se informa la actualización de la base de Recompras</strong>.</p>
                  <p>Este insumo contiene las ofertas prospectadas en los siguientes archivos:</p>
                  <p>- Recompras_TCONECTA.csv</p>
                  <p>- Renovaciones_TCONECTA.csv</p>
                  <p>- VIP_TCONECTA.csv</p>
                  <p>- Calificaciones_TCONECTA.csv</p>
                  %s %s', link_text, firma))$
  set_subject("Recompras T-CONECTA")$
  set_recipients(to = c(correos), cc = "mfsierra@fincomun.com.mx")

# Enviar el correo electrónico
my_email$send()











