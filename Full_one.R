#1. Matriz de produccion A del año 2017 de Chile----

a <- c(2033,	1,	7329,	36,	3,	279,	2,	2,	0,	15,	43,	39)
b <- c(93,	1993,	1391,	43,	101,	47,	33,	27,	11,	77,	29,	9)
c <- c(2389,	1155,	6603,	637,	4397,	2505,	1408,	222,	41,	540,	1244,	276)
d <- c(125,	1847,	1694	,4054	,128	,616	,315	,64	,107	,188	,451,	452)
e <- c(31	,17	,57,	108,	3352,	330,	184,	20,	2321,	88,	351,	294)
f <- c(632,	786	,2091,	248,	1290	,3390	,1930	,195,	58	,811	,1261	,240)
g <- c(514,	924,	2985,	302,	425,	3835,	4657,	639,	48,	1050,	551,	414)
h <- c(389	,157	,896,	249	,717	,1309	,613	,1604	,840	,466,	250,	20)
i <- c(57	,71,	295,	37,	81,	2026,	701,	149,	265,	582,	693,	107)
j <- c(458,	2795	,3551,	576	,1358	,3680	,2723	,1416	,357	,3109	,1279	,562)
k <- c(10	,34	,91,	10,	20,	101,	146,	40,	7	,52,	1024,	41)
l <- c(17	,60	,112	,22	,16	,122	,124	,11	,4,	22,	43,	43)
# son diez commodities, retira 2 mineria, 8 servicios financieros 
Stotal_ <- c(14866,29062, 53682, 13133, 25447, 41074, 30629, 13856, 18345, 25733, 29948, 12185) #suma total de produccion
Sconsumo <- c(1956,12, 15056, 2892,16,22496,10389,5855,13101,1515,13520,394) # consumo de hogares 
Ingreso <-
  c(2328,
    2763,
    5760,
    794,
    6104,
    10553,
    5577,
    3657,
    533,
    7913,
    16646,
    6917) #remuneraciones

Af <- rbind(a,b,c,d,e,f,g,h,i,j,k,l)
print(Af)
barplot(Af)
#se incluye valores finales Pro.f
#Af <- cbind(Af, Stotal_)

print(Af)
class(Af)
#generando la division solicitada el coeficiente 
Apon <- Af/Stotal_
print(Apon)
rowSums(Apon)
barplot(Apon)

#testiando
#generando matrix identidad
Itest <- diag(12)
#Io <- as.matrix(t(Io))
I <- as.matrix(Itest)
print(I)
#el resultado de la matriz de produccion en base a leontief nacional
Z <- (I-Apon[1:12,1:12])
leontief <- solve(Z)
dim(leontief)
class(leontief)
barplot(leontief)

rownames(leontief) <- c("ASP",
                        "MIN",
                        "IMN", 
                        "EGA", 
                        "CON", 
                        "CHR", 
                        "TCI",
                        "SEF",
                        "SIV", 
                        "SEN", 
                        "SPN", 
                        "ADM" 
)
colnames(leontief) <- c("ASP",
                        "MIN",
                        "IMN", 
                        "EGA", 
                        "CON", 
                        "CHR", 
                        "TCI",
                        "SEF",
                        "SIV", 
                        "SEN", 
                        "SPN", 
                        "ADM" 
)
print(leontief)
dim(leontief)
#FIN DE LO SOLICITADO. 
#-----2: Ingresos (V) 
#esi_2017_personas en una base de datos que proviene de R, en base al INE
# 3 para el calculo de de Vij
#tomamos la base de datos full personas
#2.-----Ingresos V- #########
# Código para cargar los datos
esi_2017_personas <- readRDS("esi_2017_personas.rds")
full <- esi_2017_personas
print(full)
# Agrupar por grupo y calcular la media

IT1 <- full[, 19] #edad
names(full[, 19])
IT2 <- full$ing_mon_cb #monto de ingreso #302
seq_along(full) #reenumeracion de columnas
numeracion_y_nombres <-
  paste(seq_along(full), colnames(full)) #numero de columna y nombre
print(numeracion_y_nombres)
IT3 <- full[, 308]
names(full[, 308]) # categorias iniciales de ingresos "Rama de actividad econ?mica de la empresa o instituci?n que le paga el sueldo o de la que es due?a la persona ocupada"

#install.packages("dplyr")
library(dplyr)

conteo_unicos <- IT3 %>%
  summarize(cantidad = n_distinct(IT3))
print(conteo_unicos) # 22 categorias. quitando NA, son 21 categorias.

#union de categorias
IT <- cbind(IT1, IT2, IT3)
print(IT)

#Cambiar el nombre de la columna "NombreOriginal"
colnames(IT)[colnames(IT) == "IT2"] <- "ingresos"
colnames(IT)[colnames(IT) == "r_p_rev4cl_caenes"] <- "categoria"
colnames(IT)[colnames(IT) == "IT1"] <- "edad"
names(IT)

IT <- na.omit(IT)#borrar NA porque estan sin categoria de ingresos
print(IT)
class(IT)

# Filtrar y sumar en funci?n de Nivel1 y Nivel2 # funciona con libreria dplyr OJO.
ITfiltrado <- IT %>%
  group_by(edad, categoria) %>%
  summarize(Suma_Valor = sum(ingresos))
#Pasando a data frame
class(ITfiltrado)
ITfiltrado <- as.data.frame(ITfiltrado)

# Ver el resultado
print(ITfiltrado)
cantidad_de_datos <- nrow(ITfiltrado)
print(cantidad_de_datos)

#ya limpia la base de datos se clasifica por edad.(1)
#retomamos la funcion creada en coeficiente de consumo para filtrar por edades
#SE TOMA UN CODIGO ANTERIOR PARA GENERAR LA NUEVA BASE DE DATOS DE GASTOS GF
filtrar <- function(base, columna, edadmax, edadmin) {
  if (is.numeric(base[,columna])) {
    return( base[base[,columna]<=edadmax & base[,columna]>=edadmin, ])
  } else {
    return(error)
  }
}
# para rango de edades
IT_29 <- filtrar(base=ITfiltrado, columna=1, edadmax = 29, edadmin = 15)
IT_30_39 <- filtrar(base=ITfiltrado, columna=1, edadmax = 39, edadmin = 30)
IT_40_49 <- filtrar(base=ITfiltrado, columna=1, edadmax = 49, edadmin = 40)
IT_50_59 <- filtrar(base=ITfiltrado, columna=1, edadmax = 59, edadmin = 50)
IT_60_69 <- filtrar(base=ITfiltrado, columna=1, edadmax = 69, edadmin = 60)
IT_70_79 <- filtrar(base=ITfiltrado, columna=1, edadmax = 79, edadmin = 70)
IT_80_inf <- filtrar(base=ITfiltrado, columna=1, edadmax = Inf, edadmin = 80)

#IT_average <- filtrar(base=ITfiltrado, columna=1, edadmax = Inf, edadmin =15)
#crear columnas 
# Utilizar la funci?n aggregate() para sumar por categor?a (ID)
IT_29 <- IT_29 %>% group_by(categoria) %>% summarize(IT_29 = sum(Suma_Valor))
IT_30_39 <- IT_30_39 %>% group_by(categoria) %>% summarize(IT_30_39 = sum(Suma_Valor))
IT_40_49 <- IT_40_49 %>% group_by(categoria) %>% summarize(IT_40_49 = sum(Suma_Valor))
IT_50_59 <- IT_50_59 %>% group_by(categoria) %>% summarize(IT_50_59 = sum(Suma_Valor))
IT_60_69 <- IT_60_69 %>% group_by(categoria) %>% summarize(IT_60_69 = sum(Suma_Valor))
IT_70_79 <- IT_70_79 %>% group_by(categoria) %>% summarize(IT_70_79 = sum(Suma_Valor))
IT_80_inf <- IT_80_inf %>% group_by(categoria) %>% summarize(IT_80_inf = sum(Suma_Valor))
# Aseg?rate de tener el data frame 'df' cargado o definido

# Crear una nueva fila para a?adir las categorias que tienen valor cero y no se agregan por omision segun el codigo anterior.
nueva_fila <- data.frame(categoria = 21, IT_70_79 = 0)
nueva_fila_4 <- data.frame(categoria = 4, IT_70_79 = 0)
# A?adir la nueva fila al data frame
IT_70_79 <- rbind(IT_70_79, nueva_fila, nueva_fila_4)
# Ordenar el data frame por id si es necesario
IT_70_79 <- IT_70_79[order(IT_70_79$categoria),]
# Verificar los cambios
print(IT_70_79)
#IT_80_inf
nueva_fila_80_21 <- data.frame(categoria = 21, IT_80_inf = 0)
nueva_fila_80_4 <- data.frame(categoria = 4, IT_80_inf = 0)
nueva_fila_80_10 <- data.frame(categoria = 10, IT_80_inf = 0)
#
IT_80_inf <-
  rbind(IT_80_inf,
        nueva_fila_80_21,
        nueva_fila_80_4,
        nueva_fila_80_10)
print(IT_80_inf)
# Ordenar el data frame por id si es necesario
IT_80_inf <- IT_80_inf[order(IT_80_inf$categoria),]
#
#Cambio a suma de fila para ponderar, los commodities
IT_full <-
  cbind(IT_29[, 2:2],
        IT_30_39[, 2:2],
        IT_40_49[, 2:2],
        IT_50_59[, 2:2],
        IT_60_69[, 2:2],
        IT_70_79[, 2:2],
        IT_80_inf[, 2:2])
print(IT_full)
#########2. Reordenando los grupos, desde 21 categorias a 12 categorias segun la clasificacion de matriz input- output
Cat1 <- IT_full[1:1, 1:7]
Cat2 <- IT_full[2:2, 1:7]
Cat3 <- IT_full[3:3, 1:7]
Cat4 <- as.data.frame(t(colSums(IT_full[4:5, 1:7]))) # suma de 4 y 5
rownames(Cat4) <- 4
print(Cat4)
Cat5 <- IT_full[5:5, 1:7]
Cat6 <- colSums(IT_full[6:7, 1:7])
Cat6.1 <- Cat6 + IT_full[9:9, 1:7]
rownames(Cat6.1) <- 6
Cat6 <- Cat6.1
Cat7 <- IT_full[8:8, 1:7] + IT_full[10:10, 1:7]
rownames(Cat7) <- 7
Cat8 <- IT_full[11:11, 1:7]
rownames(Cat8) <- 8
Cat9 <- IT_full[12:12, 1:7]
rownames(Cat9) <- 9
Cat10 <- as.data.frame(t(colSums(IT_full[13:14, 1:7])))
rownames(Cat10) <-  10
Cat12 <- colSums(IT_full[15:17, 1:7])
Cat12.1 <- colSums(IT_full[20:21, 1:7])
Cat12  <- as.data.frame(t(Cat12 + Cat12.1))
rownames(Cat12) <- 12
Cat11  <- as.data.frame(t(colSums(IT_full[18:19, 1:7])))
rownames(Cat11) <- 11
#uniendo
I_full_unif <-
  t(rbind(
    Cat1,
    Cat2,
    Cat3,
    Cat4,
    Cat5,
    Cat6,
    Cat7,
    Cat8,
    Cat9,
    Cat10,
    Cat11,
    Cat12
  ))
########## 3. Los datos ordenados y reclasificados se ponderan para generar los pesos por grupo de edad

class(I_full_unif)
print(I_full_unif)

#solo suma de columnas como metodo de comprobacion de totales
dim(I_full_unif) #debe ser 7 x 12
I_full_unif <- t(I_full_unif)

I_total <- colSums(I_full_unif)

class(I_total)

dim(I_total)

dim(I_full_unif)

# Convirtiendo cada elemento a porcentaje del total de su columna
matriz_porcentajes <-
  sweep(I_full_unif, 2, I_total, FUN = "/") # donde 1 es divido para filas y 2 es divido para columnas.
# aqui siendo V_{rn}, entonces la columna debe sumar uno para mantener los montos totales de cada categoria.
print(matriz_porcentajes)

rowSums(matriz_porcentajes)
colSums(matriz_porcentajes) #debe sumar 1
barplot(I_total)
barplot(I_full_unif)
barplot(matriz_porcentajes)
# Calculando los totales de cada columna
#matriz_datos <- rowSums(I_full_unif)

# Convirtiendo cada elemento a porcentaje del total de su columna
#matriz_porcentajes <- sweep(I_full_unif, 1, matriz_datos, FUN = "/")
#rowSums(matriz_porcentajes)
#dim(matriz_porcentajes)
##########4. reordenando los share de grupos de edades

#ahora se traspone para dar orden de input - output
#I_full_weith <- t(I_full_weith)
class(matriz_porcentajes)
print(matriz_porcentajes)
#I_full_grupos <- matriz_porcentajes
#ok_listo #NIVEL UNO EL SHARE DE INGRESO POR GRUPO DE CADA COMMODITIES

#iniciocalculo distinto

##########5. Share de cada categoria #sueldos.

share_ingreso <- t(Ingreso / (sum(Stotal_))) # el calculo a la miyazawa  es Ykj/Xj -- donde Stotal_ viene de linea 16 de matriz de producion del modelo
sum(Ingreso)

print(share_ingreso)
sum(share_ingreso) # 2.822291
class(share_ingreso)
dim(share_ingreso)
#V <- I_full_weith%*%share_ingreso
#sum(share_ingreso)# para comprobar porcentajes.
#V <- as.matrix(V)
#Renombrando las categorias
colnames(share_ingreso) <-
  c("I1",
    "I2",
    "I3",
    "I4",
    "I5",
    "I6",
    "I7",
    "I8",
    "I9",
    "I10",
    "I11",
    "I12")
print(share_ingreso)
summary(share_ingreso)
dim(share_ingreso)

#separando los datos para llevar a un bucle for
v1 <- share_ingreso[, 1:1]
v2 <- share_ingreso[,2:2]
v3 <- share_ingreso[,3:3]
v4 <- share_ingreso[,4:4]
v5 <- share_ingreso[,5:5]
v6 <- share_ingreso[6:6]
v7 <- share_ingreso[7:7]
v8 <- share_ingreso[8:8]
v9 <- share_ingreso[9:9]
v10 <- share_ingreso[10:10]
v11 <- share_ingreso[11:11]
v12 <- share_ingreso[12:12]

####es un calculo distinto - fin---

I_full_grupos <- matriz_porcentajes
dim(I_full_grupos)
class(I_full_grupos)
class(share_ingreso)
print(I_full_grupos)
I_full_grupos <- t(I_full_grupos)
rowSums(I_full_grupos)

# creo una funcion para doce categorias 
for (i in 1:12) {
  I_full_grupos[,i] <- I_full_grupos[,i] * get(paste("v", i, sep = ""))
} #cuidado con el nombre dado que el bucle no discrimina valores y necesita el dato de matriz porcentajes, sino cuando se corre 1 y otra vez, va cambiando los resultados y deja de cuadrar.

#comprobando que se mantienen los porcentajes 27-12-2023
colSums(matriz_porcentajes)

colSums(share_ingreso)

rowSums(I_full_grupos)

dim(matriz_porcentajes)

dim(share_ingreso)

print(I_full_grupos)
dim(I_full_grupos)

colSums(I_full_grupos)

sum(I_full_grupos) # 12

#rename
#I_full_grupos <- names()
print(I_full_grupos) # es el resultado final
rownames(I_full_grupos) <-   c("29s","39s","49s", "59s", "69s", "79s","80+")
#eliminando la categoria 2 y 8
#I_full_grupos <- I_full_grupos[,-c(2,8)]
barplot(I_full_grupos)
colnames(I_full_grupos) <- c("ASP",
                             "MIN",
                             "IMN", 
                             "EGA", 
                             "CON", 
                             "CHR", 
                             "TCI",
                             "SEF",
                             "SIV", 
                             "SEN", 
                             "SPN", 
                             "ADM" 
)
print(I_full_grupos)

#3.-----Consumo- C- ------------
#estimado segun la ecuacion de consumo por grupo y valor reales iniciales (montos)
library(readxl)
X26092023_final_clear_gastos_ingresos_precios_2017 <- read_excel("26092023_final_clear_gastos_2017.xlsx",
                                                                 col_types = c("text", "numeric", "numeric", 
                                                                               "numeric", "numeric", "numeric", 
                                                                               "numeric", "numeric", "numeric", 
                                                                               "numeric", "numeric", "numeric", 
                                                                               "numeric", "numeric", "numeric", 
                                                                               "numeric", "numeric", "numeric", 
                                                                               "numeric", "numeric", "numeric", 
                                                                               "numeric", "numeric", "numeric", 
                                                                               "numeric", "numeric", "numeric", 
                                                                               "numeric", "numeric", "numeric", 
                                                                               "numeric", "numeric", "numeric", 
                                                                               "numeric", "numeric", "numeric", 
                                                                               "numeric", "numeric", "numeric", 
                                                                               "numeric", "numeric", "numeric", 
                                                                               "numeric", "numeric"))


#filtramos los datos
Consumo_p <- X26092023_final_clear_gastos_ingresos_precios_2017
#Gasto <- as.data.frame(Gasto)

#Gasto <- as.matrix(Gasto)
CP1 <- Consumo_p[,4:4]
CP2 <- Consumo_p[,7:18]
CPT <- cbind(CP1,CP2)
print(CPT)
class(CPT)
CPT <- as.matrix(CPT)
any(is.na(CPT))
barplot(CPT[,2:13])
#suma de G1
#sum(as.numeric(cpt[, 2:2]), na.rm = TRUE)
# Las cuentas de consumo se prorratean a 10 sectores productivos de la matriz input-output, 12x12 del banco central de chile
# Obtener las dimensiones del data.frame existente

nrows <- nrow(CPT)
ncols <- ncol(CPT)
# Crear una matriz de NA con las mismas dimensiones
matriz_vacia <- matrix(NA, nrow = nrows, ncol = ncols)
# Convertir la matriz en un data.frame
df_vacio <- as.data.frame(matriz_vacia)
df_catp <- df_vacio
#Lista prorrateo
cpt <- as.data.frame(CPT)
names(df_catp)
names(cpt)
#edad
df_catp$V1 <- cpt$EDAD
#GP1 a V2 #categoria 1
df_catp$V2 <- cpt$"1GP"
#cat 2 mineria = vacio
#2-3 es cat 3
df_catp$V3 <- 0
df_catp$V4 <- cpt$"2GP" + cpt$"3GP"
# cat 4 al 50%
df_catp$V5 <- cpt$"4GP"*0.5
# categoria 5
df_catp$V6 <- cpt$"5GP"
# categoria 6
df_catp$V7 <- cpt$"11GP"
# categoria  7
df_catp$V8 <- cpt$"7GP" + cpt$"8GP"
#cat 8 financiera vacia
df_catp$V9 <- 0
# categoria 9 al 50%
df_catp$V10 <- cpt$"4GP"*0.5
# categoria 10
df_catp$V11 <- cpt$"10GP"
# categoria 11
df_catp$V12 <- cpt$"12GP"
#categoria 12
df_catp$V13 <- cpt$"6GP" +cpt$"9GP"


#fin de prorrateo

dim(df_catp)
print(df_catp)
class(df_catp)
nrow(df_catp) #son 14982
any(is.na(df_catp)) #an?lisis de datos, existen valores faltantes. si es FALSE entonces existen, si es TRUE no hay datos corruptos.
names(df_catp)
head(df_catp)
#eliminando los commodities sobrantes (ajuste a 10 categorias)
#df_catp <- df_catp[,-c(3,9)] #clear


#SE TOMA UN CODIGO ANTERIOR PARA GENERAR LA NUEVA BASE DE DATOS DE GASTOS GF
filtrar <- function(base, columna, edadmax, edadmin) {
  if (is.numeric(base[,columna])) {
    return( base[base[,columna]<=edadmax & base[,columna]>=edadmin, ])
  } else {
    return(error)
  }
}


# se comprueba su funcionamiento con el detalle que no se sabe si sirve para rango de edades
solo_29 <- filtrar(base=df_catp, columna=1, edadmax = 29, edadmin = 15)
solo_30_39 <- filtrar(base=df_catp, columna=1, edadmax = 39, edadmin = 30)
solo_40_49 <- filtrar(base=df_catp, columna=1, edadmax = 49, edadmin = 40)
solo_50_59 <- filtrar(base=df_catp, columna=1, edadmax = 59, edadmin = 50)
solo_60_69 <- filtrar(base=df_catp, columna=1, edadmax = 69, edadmin = 60)
solo_70_79 <- filtrar(base=df_catp, columna=1, edadmax = 79, edadmin = 70)
solo_80_inf <- filtrar(base=df_catp, columna=1, edadmax = Inf, edadmin = 80)
#solo_average <- filtrar(base=df_catp, columna=1, edadmax = Inf, edadmin =15)
#el resultado es el esperado, la funcion resulta en un exito.
#ahora calculo la suma de cada grupo
class(solo_29)
dim(solo_29)
#solo_29 <- as.numeric(solo_29)
suma_29 <- round(apply(solo_29[, 2:13], 2, sum, na.rm = TRUE))
#suma_29 <- suma_29/1000000
round(suma_29)
sum(suma_29)
#comprobacion del calculo de desviacion estandar
#desviacion_29 <- round(apply(solo_29[, 2:13], 2, sd, na.rm = TRUE))
#print(desviacion_29)
#funcion desviacion_29 para cada columna esta la desviacion estandar.
cuadro_2 <- function(data){
  medias <- round(apply(data[,2:13], 2, sum, na.rm=TRUE))
  #  desviaciones <- apply(data[,2:13], 2, sd, na.rm=TRUE)
  #resultado <- list(Suma = medias)#, Desviacion =desviaciones)
  #  return(resultado)
}
library(dplyr)
#cuadro <- cuadro_2(data=solo_29)
#print(cuadro)
#plot(cuadro$Media,type = "h", col = "red")
#?plot
G2p <- cuadro_2(data= solo_29)
G3p <- cuadro_2(data= solo_30_39)
G4p <- cuadro_2(data= solo_40_49)
G5p <- cuadro_2(data= solo_50_59)
G6p <- cuadro_2(data= solo_60_69)
G7p <- cuadro_2(data=solo_70_79)
G8p <- cuadro_2(data = solo_80_inf)
#Gpp <- cuadro_2(data= solo_average)

#G2p <- data.frame(Columna_a = unlist(G2p$Suma))
#G3p <- data.frame(Columna_b = unlist(G3p$Suma))
#G4p <- data.frame(Columna_c = unlist(G4p$Suma))
#G5p <- data.frame(Columna_d = unlist(G5p$Suma))
#G6p <- data.frame(Columna_e = unlist(G6p$Suma))
#G1p <- data.frame(Columna_f = unlist(G1p$Suma))

#matriz consumo Cij
cij <- cbind(G2p,G3p,G4p,G5p, G6p,G7p,G8p)
dim(cij)
barplot(cij)

matriz_suma_datos <- rowSums(cij)
dim(matriz_suma_datos)
class(cij)
# Convirtiendo cada elemento a porcentaje del total de su columna
matriz_cij <- sweep(cij, 1, matriz_suma_datos, FUN = "/") ###AQUI ESTA # es el porcentaje sobre que grupo consume mas en cada categoria
matriz_cij[is.na(matriz_cij)] <- 0

# Mostrando la matriz resultante
rowSums(matriz_cij)  

C_miyazawa <- Sconsumo/(sum(Ingreso)) #Sconsumo proviene desde linea 17, pesta?a Matriz de produccci?n del modelo , faltan 2 unidades monetarias de diferencia con input output
print(C_miyazawa) #indice miyazawa 
class(C_miyazawa)

C_miyazawa <- as.matrix(C_miyazawa)
dim(C_miyazawa)
sum(Ingreso)
sum(Sconsumo)
69545-87202
#eliminando los commodities sobrantes (ajuste a 10 categorias)
#C_miyazawa <- C_miyazawa[-c(2,8),] #clear
C_miyazawa <- as.matrix(C_miyazawa)
dim(C_miyazawa)
sum(C_miyazawa)
#full calculo matriz prorrateada
#se crea un bucle for para separar cada valor de Cik
for(i in 1:12) {
  # Extrae el valor de la i-?sima fila del data frame original
  valor <- C_miyazawa[i, ]
  # Crea un nuevo data frame con este valor
  nuevo_df <- data.frame(Valor = valor)
  # Asigna el nuevo data frame a una variable con nombre din?mico (h1, h2, ..., h12)
  assign(paste0("h", i), nuevo_df)
}

#class(smm)  
class(h1)
#smm <- as.data.frame(smm)                                
#smm <- as.matrix(smm)
h1 <- as.matrix(h1)
h2 <- as.matrix(h2)
h3 <- as.matrix(h3)
h4 <- as.matrix(h4)
h5 <- as.matrix(h5)
h6 <- as.matrix(h6)
h7 <- as.matrix(h7)
h8 <- as.matrix(h8)
h9 <- as.matrix(h9)
h10 <- as.matrix(h10)
h11 <- as.matrix(h11)
h12 <- as.matrix(h12)

class(matriz_cij)
#EL FIN 
c1 <- matriz_cij[1:1,]%*%h1
c2 <- matriz_cij[2:2,]%*%h2
c3 <- matriz_cij[3:3,]%*%h3
c4 <- matriz_cij[4:4,]%*%h4
c5 <- matriz_cij[5:5,]%*%h5
c6 <- matriz_cij[6:6,]%*%h6
c7 <- matriz_cij[7:7,]%*%h7
c8 <- matriz_cij[8:8,]%*%h8
c9 <- matriz_cij[9:9,]%*%h9
c10 <- matriz_cij[10:10,]%*%h10
c11 <- matriz_cij[11:11,]%*%h11
c12 <- matriz_cij[12:12,]%*%h12


c_full <- cbind(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12)


c_full <- t(c_full)
dim(c_full)
class(c_full)
rowSums(c_full)# el metodo cuadra #ESTE ES EL QUE FUNCIONA CORRECTAMENTE SIN BUG / FIRMADO 05-10-2024


#rename
rownames(c_full) <- c("ASP",
                      "MIN",
                      "IMN", 
                      "EGA", 
                      "CON", 
                      "CHR", 
                      "TCI",
                      "SEF",
                      "SIV", 
                      "SEN", 
                      "SPN", 
                      "ADM" 
)

colnames(c_full) <- c("29s","39s","49s","59s","69s","79s","80+")
print(c_full) #es el resultado del nivel 3. es el que se utilizara.
class(c_full)
print(c_full) #esta completa y prorrateada a solo 10 clases seg?n encuesta de consumo familiar.
barplot(c_full)
dim(c_full)
names(c_full)
#4.-----Matrices Miyazawa----
library(dplyr)
library(reshape2)

I <- diag(1, nrow = nrow(Apon), ncol = ncol(Apon))
B <- t(leontief)#la matriz base 12 x 12 coeficiente de produccion
#B <- B[-c(2,8),-c(2,8)]
print(B) # 10x10
#reducciendo B
dim(B)

I_full_grupos #ingresos share
dim(I_full_grupos)
V <- I_full_grupos # 7x10#La matriz de ingreso por categoria
dim(I_full_grupos)
#V <- V[,-c(2,8)] # la idea de cambiar el nombre, sirve para generar dos secciones de calculo. 
#V_analisis_d <- I_full_grupos 

c_full # la matriz ajustada a 10 x7 segun miyazawa
dim(c_full)
#c_full <- c_full[-c(2,8),]
C <- c_full #   7 x 10 # la matriz ajustada a 12 x7 segun miyazawa
#C_analisis_a <- round(c_full, digits=4)
#print(C_analisis_a)


#7x10, 10x10, 10x7 
loko2 <-V %*% B %*% C

library(matlib)
#if (!require(expm)) install.packages("expm")
library(expm)

Itestloko <- diag(1, nrow(loko2), ncol(loko2)) #matriz I diagonal
k_son <- solve(Itestloko - loko2)

print(k_son) # es la matriz miyazawa k

#modelo proyectado a 15 a?os
#loko_proy <- I_full_grupos %*% B %*% c_full_pro
#k_son_proy <- solve(Itestloko - loko_proy)

print(k_son)
class(k_son)
dim(k_son)


# (K) resultado final matrix interrelacional de ingresos
KVB <- k_son%*%V%*% B#(L) resultado de la "matriz de coeficientes entre grupos de ingresos" 
plot(KVB)
print(KVB)
#For example, in this illustration, a direct increase of $1 in income to households in
#group 1 leads to a 6.7 cent (k21) increase in income payments to households in group
Miyazawa_melt <- melt(KVB)
Miyazawa_melt_K <- melt(k_son)

redonde_miyazawa_industrial <- Miyazawa_melt %>% mutate(across(c("value"), round, 4))

redonde_miyazawa <- Miyazawa_melt_K%>% mutate(across(c("value"), round, 5))
# Convertir Var1 y Var2 de factores a n?meros
#redonde_miyazawa$value <- as.numeric(as.character(redonde_miyazawa$value))
# Cargar el paquete xtable
library(xtable)
# Convertir el dataframe a una tabla de LaTeX con xtable
tabla_latex <- xtable(k_son, digits=6)
# Imprimir la tabla en formato LaTeX
print(tabla_latex, comment = FALSE) # es tabla 2 del articulo

##################FIG 1 ##################
#C:/Users/COMPC/Documentos/Phd/Investigaciones/12_consumo_en_base_al_ciclo_de_vida/Input/codigo_figura_gastos_familia.R
##################TAB 1 ##################
#esta en el archivo tabla_1.r
##################TAB 2 ##################
###--- Creando tabla 2 ---###
# Instalar el paquete xtable si a?n no est? instalado
# install.packages("xtable")
# Cargar el paquete xtable
library(xtable)
# Convertir el dataframe a una tabla de LaTeX con xtable
tabla_latex <- xtable(k_son, digits=6)
# Imprimir la tabla en formato LaTeX
print(tabla_latex, comment = FALSE) # es tabla 2 del articulo
##################FIG 2 ---------
library(tibble)
library(ggplot2)
library(patchwork)
library(reshape2)

TotalC <-colSums(k_son)
#k_son <- rbind(k_son, TotalC)
TotalR <- rowSums(k_son)

#un simple grafico lineal - figure 2
Total_lineal <- data.frame(TotalC, TotalR)
print(Total_lineal)
colnames(Total_lineal) <- c("TotalC","TotalR")
print(Total_lineal)
Total_lineal$dif <- as.numeric(as.matrix(data.frame(TotalC-TotalR)))
print(Total_lineal)

# Crear una nueva columna "Grupo" con los nombres de las filas
Total_lineal$Grupo <- rownames(Total_lineal)

# Mover la columna "Grupo" al inicio del data.frame
Total_lineal <- Total_lineal[, c(ncol(Total_lineal), 1:(ncol(Total_lineal)-1))]

# Eliminar los nombres de las filas
rownames(Total_lineal) <- NULL


# Reestructurar los datos para que sean adecuados para ggplot2
datos_long <- melt(Total_lineal, id.vars = "Grupo")

# Crear el gr?fico de l?neas
dif_simple <- ggplot(Total_lineal, aes(x = Grupo, y = dif)) +
  geom_bar(stat = "identity", fill="grey")+
  geom_text(aes(label = round(dif, 4), y = dif/2), vjust = 0.5, size= 5) +  # Ajustar la posici?n y del texto
  annotate("text", x = "29s", y = 0.0045, size = 5, label = expression(bold("(b)"))) +
  # Agregar una leyenda interna al gráfico que explique los tipos de línea
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    plot.title = element_text(size = 14),  # Tama?o del t?tulo
    axis.title.x = element_text(size = 14),  # Tama?o de la etiqueta del eje X
    axis.title.y = element_text(size = 14)   # Tama?o de la etiqueta del eje Y
  ) +
  labs(title = "", x = "", y = "")

plot(dif_simple)
# Gráfico de líneas con etiquetas cerca de las líneas

Grupo_total_linea <- ggplot(datos_long[1:14,], aes(x = Grupo, y = value, group = variable, color = variable)) +
  geom_line(size = 0.9) +
  geom_point(size=0.9) + 
  annotate("text", x = "69s", y = 1.0093, size = 5, label = expression(bold("Income generate"))) +
  annotate("text", x = "49s", y = 1.0058, size = 5, label = expression(bold("Indirect income"))) +
  annotate("text", x = "29s", y = 1.01, size = 5, label = expression(bold("(a)"))) +
    scale_color_manual(values = c("red", "blue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        legend.position = "none",  # Eliminar la leyenda
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +  
  labs(title = "", x = "", y = "")

# Mostrar el gráfico
print(Grupo_total_linea)


#uniendo grafico de totales de matriz (K)

simple_analisis <- Grupo_total_linea / dif_simple 
simple_analisis

# Save the combined plot with a width of 2244 pixels
ggsave("Method/Imagenes/imagenes_final_ingles/Figure_2.png", plot = simple_analisis, width=7.48, height=9, dpi =300 )

##################FIG 3 ----
#Esta en python
#C:\Users\COMPC\Documentos\Phd\Investigaciones\12_consumo_en_base_al_ciclo_de_vida\Method\R-Python
##################FIG 4 ####
library(ggplot2)
library(reshape2)
#figura con los pesos de los grupos es figura 4. 
# Muestra un heatmap con etiquetas descriptivas y una escala de color m?s apropiada

KVB <- KVB[,-c(2,8)]

Miyazawa_melt <- melt(KVB)
Miyazawa_melt_K <- melt(k_son)

ggplot(Miyazawa_melt, aes(y = Var1, x = Var2, fill = value)) +
  geom_tile(color = "white") +  # Agrega bordes blancos entre las celdas para mayor claridad
  scale_fill_gradient(low = "white", high = "red", name = "Share") +  # Escala de colores mejorada
  geom_text(aes(label = sprintf("%.3f", value)), size = 4) +  # Agregar texto dentro de las celdas, 3 decimales
  labs(title = "",
       subtitle = "",
       x = "", y = "") +  # Etiquetas y t?tulos mejorados
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Rotar etiquetas del eje X para mayor claridad
        axis.text.y = element_text(size = 12),  # Aumentar tama?o de texto en el eje Y
        plot.title = element_text(size = 16, face = "bold"),  # Ajustar el tama?o del t?tulo
        plot.subtitle = element_text(size = 14))  # Ajustar el tama?o del subt?tulo

# Crear el gr?fico de l?neas
# Calcular las posiciones del tercer punto para las etiquetas
etiquetas_tercer_punto <- Miyazawa_melt %>% 
  group_by(Var2) %>% 
  slice(3)  # Selecciona la tercera fila por cada grupo

Miyazawa_melt$Var2 <- recode(Miyazawa_melt$Var2,
                                "ASP" = "AgroFishing",
                                "IMN" = "Manufacturing",
                                "EGA" = "Energy",
                                "CON" = "Construction",
                                "CHR" = "Commerce",
                                "TCI" = "Transport",
                                "SIV" = "Real Estate",
                                "SEN" = "Business Services",
                                "SPN" = "Personal Services",
                                "ADM" = "Public Admin")
produc <- ggplot(Miyazawa_melt, aes(x = Var1, y = value, group = Var2, color = Var2)) +
  geom_line(size = 1.5) +  # Ajustar el grosor de las líneas
  geom_point() +
  theme_minimal() +
  labs(title = "", 
       x = "", 
       y = "", 
       color = "Categoría") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 12),  # Rotar y aumentar el tamaño de las etiquetas del eje x
    axis.text.y = element_text(size = 12),  # Aumentar el tamaño de las etiquetas del eje y
    axis.title.x = element_text(size = 14),  # Aumentar el tamaño del título del eje x
    axis.title.y = element_text(size = 14),  # Aumentar el tamaño del título del eje y
    legend.position = "none",  # Eliminar la leyenda
    panel.background = element_rect(fill = "white", color = NA),  # Fondo blanco para el panel
    plot.background = element_rect(fill = "white", color = NA),    # Fondo blanco para todo el gráfico
    strip.text = element_text(size = 16)  # Aumentar el tamaño de las etiquetas de los facetas (Var2)
  ) +
  facet_wrap(~ Var2, scales = "fixed", nrow = 5, ncol = 2)  # Crear cajas individuales para cada Var2

produc

# Guardar el gráfico como archivo PNG
ggsave("Method/Imagenes/imagenes_final_ingles/Figure_4.png", plot = produc, width = 7.48, height = 15, dpi = 300)
#2. Similarly,
# pagina 275 de Miller&Blair 
#con estilo miyazawa consumo
##################FIG 5----
#proyeciones
#librerias de la seccion
library(tibble)
library(reshape2)
library(dplyr)
library(ggplot2)
library(patchwork)
# resultados economicos y proyecciones
c_full_proy <- c_full
#se guardan los datos
#cambio de consumo
c_full_proy <- list()

for(i in 1:15){
  # la base a modificar
  c_full_modificada <- c_full
  #la columna a ingrementar sin ser acumulativa
  c_full_modificada[,5:7] <- c_full_modificada[,5:7]*(1+0.01*i) # un 15% a aumento de la poblacion para 69 anos (5=69,6=79,7=80+)
  #donde se guardan las 15 bases de datos
  c_full_proy[[i]] <- c_full_modificada
}
dim(V)
dim(B)
dim(c_full_proy[[i]])

kBC_proy <- list()
for (i in 1:15){
  #el cambio de valores
  kBC_p <- V %*% B %*% c_full_proy[[i]]
  #bases multiplicadas
  kBC_proy[[i]] <- kBC_p
}
#desarrollo de las 15 k
Itestloko <- diag(1, nrow(loko2), ncol(loko2) )

# base de datos de las 15 matrices
k_proyec <- list()

for(i in 1:15){
    # el cambio de valores
  k_proy <- solve(Itestloko - kBC_proy[[i]])
  # todas las matrices k proyectadas
  k_proyec[[i]] <- k_proy 
}
# quedan creados los 15 periodos proyectados
dim(k_proyec[[1]])
colSums(k_proyec[[1]])
rowSums(k_proyec[[1]])
# suma de lineas


#un simple grafico lineal
#-----sumCols
Total_c_p <- list()

for(i in 1:15){
  Totalcproy <- colSums(k_proyec[[i]])
  Total_c_p[[i]] <- Totalcproy 
}

#----sumRow
Total_r_p <- list()

for(i in 1:15){
  Totalrproy <- rowSums(k_proyec[[i]])
  Total_r_p[[i]] <- Totalrproy
}

#graficando
#uniendo
Total_proy <- list()

for(i in 1:15){
  #uniendo los datos
  total_frame <- data.frame(Total_r_p[[i]], Total_c_p[[i]])
  Total_proy[[i]] <- total_frame
  colnames(Total_proy[[i]]) <- c("T_filas", "T_columnas") 
  # pasar nombre de filas a una columna con los nombres
  Total_proy[[i]] <- rownames_to_column(Total_proy[[i]], var ="grupo")
}
#que clase es cada elemento
sapply(Total_proy, class)

# Reestructurar los datos para que sean adecuados para ggplot2
Total_proy_long <- reshape2::melt(Total_proy, id.vars = "grupo")

########revision lineal
Total_proy_filtrado<- Total_proy_long  %>%
  group_by(variable, grupo) %>%
  slice(c(1, n()))  # Mantener solo la primera y ?ltima observaci?n de cada grupo

# Crear el gráfico con diferentes tipos de línea para las últimas dos series
Grupo_total_proyec <- ggplot(Total_proy_filtrado, aes(x = grupo, y = value, color = variable, group = interaction(variable, L1))) +
  
  # Usar geom_line con diferentes tipos de línea según si son las últimas dos series
  geom_line(aes(linetype = ifelse(L1 == min(L1),"dashed", "solid" )), size = 0.5) +  # "solid" para la primera serie, "dashed" para las últimas dos
  geom_point(shape = 24, size = 1) +
  
  # Colores y anotaciones
  scale_color_manual(values = c("red", "blue"), name = "Ingresos") +
  
  # Añadir una leyenda interna para explicar los diferentes tipos de líneas
  annotate("text", x = "49s", y = 1.002, label = "Solid line: Initial values \nDashed line: Projected", 
          size = 5, color = "black") +
    # Anotaciones específicas para ciertos puntos en el gráfico
  annotate("text", x = "29s", y = 1.012, size = 5, label = expression(bold("(a)"))) +  # Negrita usando expression
  #annotate("text", x = "59s", y = 1.012, size = 3, label = "15% growth in groups aged 60 and above ") +

  # Temas y estilos
  theme_minimal() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.4),
        plot.title = element_text(size = 10), # Ajuste del tamaño del título si se usa
        legend.position = "none") + # Ocultar leyenda predeterminada
  
  # Etiquetas y límites
  labs(title = "", x = "", y = "") +
  scale_y_continuous(limits = c(1, 1.0125))

# Mostrar el gráfico
print(Grupo_total_proyec)

# Mostrar el gr?fico
plot(Grupo_total_proyec) # final figura 5.2 proyectada

#cambio -----consumo baja 15% -figura 5.2 

c_full_proy_l <- list()

for(i in 1:15){
  # la base a modificar
  c_full_modificada_l <- c_full
  #la columna a ingrementar sin ser acumulativa
  c_full_modificada_l[5:7,] <- c_full_modificada_l[5:7,]/(1+0.01*i) # un 15% disminucion de la poblacion para 15 a?os
  #donde se guardan las 15 bases de datos
  c_full_proy_l[[i]] <- c_full_modificada_l
}

#desarrollo de los 15 kBC 

kBC_proy_l <- list()
for (i in 1:15){
  # la base a modificar
  kBC_proy_mo_l <- kBC_proy_l
  #el cambio de valores
  kBC_p_l <- V %*% B %*%c_full_proy_l[[i]]
  #bases multiplicadas
  kBC_proy_l[[i]] <- kBC_p_l
}

#desarrollo de las 15 k

Itestloko <- diag(1, nrow(loko2), ncol(loko2) )

# base de datos de las 15 matrices
k_proyec_l <- list()

for(i in 1:15){
  # la matrices a modificar
  kBC_proy_k_l <- kBC_proy_l
  # el cambio de valores
  k_proy_l <- solve(Itestloko - kBC_proy_l[[i]])
  # todas las matrices k proyectadas
  k_proyec_l[[i]] <- k_proy_l 
}

#un simple grafico lineal
#-----sumCols
Total_c_p_l <- list()

for(i in 1:15){
  Totalcproy_l <- colSums(k_proyec_l[[i]])
  Total_c_p_l[[i]] <- Totalcproy_l 
}
#----sumRow
Total_r_p_l <- list()

for(i in 1:15){
  Totalrproy_l <- rowSums(k_proyec_l[[i]])
  Total_r_p_l[[i]] <- Totalrproy_l
}

#graficando
#uniendo
library(tibble)
library(ggplot2)
library(reshape2)
library(dplyr)

Total_proy_l <- list()

for(i in 1:15){
  #uniendo los datos
  total_frame_l <- data.frame(Total_r_p_l[[i]], Total_c_p_l[[i]])
  Total_proy_l[[i]] <- total_frame_l
  colnames(Total_proy_l[[i]]) <- c("T_filas", "T_columnas") 
  # pasar nombre de filas a una columna con los nombres
  Total_proy_l[[i]] <- rownames_to_column(Total_proy_l[[i]], var ="grupo")
}
#que clase es cada elemento
sapply(Total_proy_l, class)


# Reestructurar los datos para que sean adecuados para ggplot2
Total_proy_long_l <- reshape2::melt(Total_proy_l, id.vars = "grupo")


# Filtrar solo la primera y ?ltima l?nea de cada tendencia (cada variable)
Total_proy_long_l_filtrado <- Total_proy_long_l %>%
  group_by(variable, grupo) %>%
  slice(c(1, n()))  # Mantener solo la primera y ?ltima observaci?n de cada grupo


# Crear el gráfico con solo la primera y última línea de cada tendencia, aplicando diferentes tipos de línea
Grupo_total_proyec_l <- ggplot(Total_proy_long_l_filtrado, 
                               aes(x = grupo, y = value, 
                                   color = variable, 
                                   group = interaction(variable, L1))) +
  
  # Aplicar diferentes tipos de línea: sólido para la primera, discontinuo para la última observación
  geom_line(aes(linetype = ifelse(L1 == max(L1), "solid", "dashed")), 
            size = 0.5) +  # Línea discontinua para las últimas observaciones
  
  # Agregar puntos en las líneas
  geom_point(shape = 25, size = 1) +  # Puntos triangulares vacíos (shape 25) y más grandes
  
  # Definir los colores manualmente para las líneas
  scale_color_manual(values = c("red", "blue"), name = "Income") +
  
  # Definir manualmente los tipos de líneas y eliminar la leyenda de linetype
  scale_linetype_manual(values = c("solid", "dashed"), guide = FALSE) +
  
  # Agregar anotaciones de texto en el gráfico
  annotate("text", x = "29s", y = 1.012, size = 5, label = expression(bold("(b)"))) +
  #annotate("text", x = "59s", y = 1.012, size = 3, label = "") +
    # Agregar una leyenda interna al gráfico que explique los tipos de línea
  annotate("text", x = "49s", y = 1.002, label = "Solid line: Initial values\nDashed line: Projected", 
           size = 5, color = "black") +
    # Aplicar un tema minimalista
  theme_minimal() +
  
  # Personalización del tema (tamaño de los textos y bordes)
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.4),  # Borde del gráfico
    legend.position = "none"  # Quitar la leyenda
  ) +
    # Etiquetas vacías para el título y los ejes
  labs(title = "", x = "", y = "") +
    # Definir los límites del eje Y
  scale_y_continuous(limits = c(1, 1.0125))
# Mostrar el gráfico
print(Grupo_total_proyec_l)

f.5 <- (Grupo_total_proyec / Grupo_total_proyec_l) #figura 5  Interrelaciones totales por efectos inducidos

print(f.5)

# Save the combined plot with a width of 2244 pixels
ggsave("Method/Imagenes/imagenes_final_ingles/Figure_5.png", plot = f.5, width = 7.48, height = 8, dpi = 300)

#-----     fin     -----#
##################FIG 6 ----
#inicio proyeccion a 15 años que incluye aumento 15% 69.79.80+ y disminucion tasa natalidad 5.699% 29s
library(tibble)
library(reshape2)
library(dplyr)
#anotamos el resultado de consumo al periodo 15
c_full_proy_nat <- c_full
#se guardan los datos
#cambio de consumo
c_full_proy_nat <- list()

#desarrollo de los 15 k

for(i in 1:15){
  # la base a modificar
  c_full_modificada <- c_full
  #la columna a ingrementar sin ser acumulativa
  c_full_modificada[5:7,] <- c_full_modificada[5:7,]*(1+0.01*i) # un 15% a aumento de la poblacion para 69 a?os
  c_full_modificada[1:1,] <- c_full_modificada[1:1,]/(1+0.00379*i) # un 15%de disminucion de la poblacion para 29s
  #donde se guardan las 15 bases de datos
  c_full_proy_nat[[i]] <- c_full_modificada
}

kBC_proy_n <- list()
for (i in 1:15){
  # la base a modificar
  kBC_proy_mo_n <- kBC_proy_n
  #el cambio de valores
  kBC_p_n <- V %*% B %*% c_full_proy_nat[[i]]
  #bases multiplicadas
  kBC_proy_n[[i]] <- kBC_p_n
}
#desarrollo de las 15 k
Itestloko <- diag(1, nrow(loko2), ncol(loko2) )
# base de datos de las 15 matrices
k_proyec_nk <- list()

for(i in 1:15){
  # la matrices a modificar
  kBC_proy_k <- kBC_proy
  # el cambio de valores
  k_proyec_k <- solve(Itestloko - kBC_proy_n[[i]])
  # todas las matrices k proyectadas
  k_proyec_nk[[i]] <- k_proyec_k 
}
# quedan creados los 15 periodos proyectados
dim(k_proyec_nk[[1]])
colSums(k_proyec_nk[[1]])
rowSums(k_proyec_nk[[1]])
# suma de lineas
#un simple grafico lineal
#-----sumCols
Total_c_p_nk <- list()
for(i in 1:15){
  Totalcproy_nk <- colSums(k_proyec_nk[[i]])
  Total_c_p_nk[[i]] <- Totalcproy_nk 
}

#----sumRow
Total_r_p_nk <- list()

for(i in 1:15){
  Totalrproy_nk <- rowSums(k_proyec_nk[[i]])
  Total_r_p_nk[[i]] <- Totalrproy_nk
}

#graficando
#uniendo

Total_proy_nk <- list()
for(i in 1:15){
  #uniendo los datos
  total_frame_nk <- data.frame(Total_r_p_nk[[i]], Total_c_p_nk[[i]])
  Total_proy_nk[[i]] <- total_frame_nk
  colnames(Total_proy_nk[[i]]) <- c("T_filas", "T_columnas") 
  # pasar nombre de filas a una columna con los nombres
  Total_proy_nk[[i]] <- rownames_to_column(Total_proy_nk[[i]], var ="grupo")
}
#que clase es cada elemento
sapply(Total_proy_nk, class)


# Reestructurar los datos para que sean adecuados para ggplot2
Total_proy_long_nk <- reshape2::melt(Total_proy_nk, id.vars = "grupo")

# Filtrar solo la primera y última línea de cada tendencia (cada variable)
Total_proy_long_nk1 <- Total_proy_long_nk %>%
  group_by(variable, grupo) %>%
  slice(c(1, n()))  # Mantener solo la primera y última observación de cada grupo

# Crear el gráfico con solo la primera y última línea de cada tendencia, aplicando diferentes tipos de línea
Grupo_total_proyec_nk2 <- 
    ggplot(Total_proy_long_nk1, 
         aes(x = grupo, y = value, 
         color = variable, 
         group = interaction(variable, L1))) +
  # Aplicar diferentes tipos de línea: sólido para la primera, discontinuo para la última observación
  geom_line(aes(linetype = ifelse(L1 == min(L1),"dashed" , "solid")), 
            size = 0.5) +  # Líneas sólidas y discontinuas para resaltar primeros y últimos valores
  # Agregar puntos en las líneas para los valores máximos y mínimos
  geom_point(aes(shape = as.factor(L1 == min(L1) | L1 == max(L1))), shape=24, size = 1) +  # Añadir puntos a los extremos
  # Definir los colores manualmente para las líneas
  scale_color_manual(values = c("red", "blue"), name = "Income") +
  # Definir manualmente los tipos de líneas y eliminar la leyenda de linetype
  scale_linetype_manual(values = c("solid", "dashed"), guide = FALSE) +
  # Agregar anotaciones de texto en el gráfico
  annotate("text", x = "29s", y = 1.012, size = 5, label = expression(bold("(a)"))) +
  # Agregar una leyenda interna al gráfico que explique los tipos de línea
  annotate("text", x = Inf, y = Inf, label = "Solid line: Initial\nDashed line: Projected", 
           hjust = 1.2, vjust = 1.2, size = 5, color = "black") +
    # Agregar una leyenda interna al gráfico que explique los tipos de línea
  # Aplicar un tema minimalista
  theme_minimal() +
  # Personalización del tema (tamaño de los textos y bordes)
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.4),  # Borde del gráfico
    legend.position = "none"  # Quitar la leyenda
  ) +
  # Etiquetas vacías para el título y los ejes
  labs(title = "", x = "", y = "") +
  
  # Definir los límites del eje Y
  scale_y_continuous(limits = c(1, 1.0125))
# Mostrar el gráfico
print(Grupo_total_proyec_nk2)

#(b) cambio -----consumo - baja 15% 
#cambio de consumo

c_full_proy_l_nk <- list()
#disminuci?n de la tasa de natalidad
for(i in 1:15){
  # la base a modificar
  c_full_modificada_l_nk <- c_full
  #la columna a ingrementar sin ser acumulativa
  c_full_modificada_l_nk[5:7,] <- c_full_modificada_l_nk[5:7,]/(1+0.01*i) # un 15% disminucion de la poblacion para 15 a?os
  c_full_modificada_l_nk[1:1,] <- c_full_modificada_l_nk[1:1,]/(1+0.00379*i) # un 15% a disminucion de la poblacion para 15 a?os
  #donde se guardan las 15 bases de datos
  c_full_proy_l_nk[[i]] <- c_full_modificada_l_nk
}
#desarrollo de los 15 kBC 

kBC_proy_l_nk1 <- list()
for (i in 1:15){
  # la base a modificar
  kBC_proy_l_nk <- kBC_proy_l_nk1
  #el cambio de valores
  kBC_p_l_nk1 <- V %*% B %*% c_full_proy_l_nk[[i]]
  #bases multiplicadas
  kBC_proy_l_nk1[[i]] <- kBC_p_l_nk1
}
dim(kBC_proy_l_nk1[[i]])

#desarrollo matriz identidad
Itestloko <- diag(1, nrow(loko2), ncol(loko2))
#resolviendo K= (I-A)^-1
k_proyec_l_nk2 <- list()

for(i in 1:15){
   # el cambio de valores
  k_proy_l_nk <- solve(Itestloko - kBC_proy_l_nk1[[i]])
  # todas las matrices k proyectadas
  k_proyec_l_nk2[[i]] <- k_proy_l_nk 
}
# fin del codigo de calculo de k proyectada
k_proyectada <- k_proyec_l_nk2

#un simple grafico lineal de suma de total i
#-----sumCols
Total_c_p_l_nk <- list()

for(i in 1:15){
  Totalcproy_l_nk <- colSums(k_proyectada[[i]])
  Total_c_p_l_nk[[i]] <- Totalcproy_l_nk 
}
#----sumRow
Total_r_p_l_nk <- list()

for(i in 1:15){
  Totalrproy_l_nk <- rowSums(k_proyectada[[i]])
  Total_r_p_l_nk[[i]] <- Totalrproy_l_nk
}

#uniendo

Total_proy_l_nk <- list()

for(i in 1:15){
  #uniendo los datos
  total_frame_l_nk <- data.frame(Total_r_p_l_nk[[i]], Total_c_p_l_nk[[i]])
  Total_proy_l_nk[[i]] <- total_frame_l_nk
  colnames(Total_proy_l_nk[[i]]) <- c("T_filas", "T_columnas") 
  # pasar nombre de filas a una columna con los nombres
  Total_proy_l_nk[[i]] <- rownames_to_column(Total_proy_l_nk[[i]], var ="grupo")
}
print(Total_proy_l_nk[[i]])

#que clase es cada elemento
sapply(Total_proy_l_nk, class)

# Reestructurar los datos para que sean adecuados para ggplot2
Total_proy_long_l_nk <- reshape2::melt(Total_proy_l_nk, id.vars = "grupo")

# Filtrar solo la primera y última línea de cada tendencia (cada variable)
Total_proy_long_nk_filtrado <- Total_proy_long_l_nk  %>%
  group_by(variable, grupo) %>%
  slice(c(1, n()))  # Mantener solo la primera y última observación de cada grupo

# Crear el gráfico con solo la primera y última línea de cada tendencia, aplicando diferentes tipos de línea
Grupo_total_proyec_nk3 <- ggplot(Total_proy_long_nk_filtrado, 
                                 aes(x = grupo, y = value, 
                                     color = variable, 
                                     group = interaction(variable, L1))) +
    # Aplicar diferentes tipos de línea: sólido para la primera, discontinuo para la última observación
  geom_line(aes(linetype = ifelse(L1 == min(L1), "dashed", "solid")), 
            size = 0.5) +  # Líneas sólidas y discontinuas para resaltar primeros y últimos valores
    # Agregar puntos en las líneas para los valores máximos y mínimos
  geom_point(aes(shape = as.factor(L1 == min(L1) | L1 == max(L1))), shape=24, size = 1) +  # Añadir puntos a los extremos
    # Definir los colores manualmente para las líneas
  scale_color_manual(values = c("red", "blue"), name = "Income") +
    # Definir manualmente los tipos de líneas y eliminar la leyenda de linetype
  scale_linetype_manual(values = c("solid", "dashed"), guide = FALSE) +
    # Agregar anotaciones de texto en el gráfico
  annotate("text", x = "29s", y = 1.012, size = 5, label = expression(bold("(b)"))) +
    # Agregar una leyenda interna al gráfico que explique los tipos de línea
  annotate("text", x = Inf, y = Inf, label = "Solid line: Initial  \nDashed line: Projected", 
           hjust = 1.2, vjust = 1.2, size = 5, color = "black") +
  
  # Aplicar un tema minimalista
  theme_minimal() +
  
  # Personalización del tema (tamaño de los textos y bordes)
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.4),  # Borde del gráfico
    legend.position = "none"  # Quitar la leyenda
  ) +
    # Etiquetas vacías para el título y los ejes
  labs(title = "", x = "", y = "") +
    # Definir los límites del eje Y
  scale_y_continuous(limits = c(1, 1.0125))
# Mostrar el gráfico
print(Grupo_total_proyec_nk3) #grafico (b) 6 (b)

f.6 <- Grupo_total_proyec_nk2 / Grupo_total_proyec_nk3 #figura 5  Interrelaciones totales por efectos inducidos

print(f.6)

# Save the combined plot with a width of 2244 pixels
ggsave("Method/Imagenes/imagenes_final_ingles/Figure_6.png", plot=f.6, width=7.48, height = 9, dpi=300)

#-----     fin

##################FIG 7 ---------
dim(k_proyec[[i]])
class(k_proyec)
dim(V)
#salida y analisis de interrelaciones entre los grupos. 
#k_proyec es la proyeccion 15% poblacion 60+
#k_proyectada es la proyeccion 15% crecimiento de la poblacion 60+ y una baja en la tasa de natalidad.

multi_ingreso_proy <- list()
for(i in 1:15){
  ingreso_proy_kvb <-  k_proyec[[i]]%*%V%*%B
  multi_ingreso_proy[[i]] <- ingreso_proy_kvb
}

class(k_proyec)
class(k_proyectada)
k_proyec[[i]] - k_proyectada[[i]]
#
#suma total multiingreso columnas (sectores productivos) 
library(tibble) # Aseg?rate de tener tibble instalado y cargado

total_proy_multi_ingreso <- list()

for(i in 1:15){
  # Sumar las columnas del data frame
  sumcol_proy <- colSums(multi_ingreso_proy[[i]])
  # Convertir el vector de sumas en un data frame
  df_sumcol_proy <- as.data.frame(sumcol_proy)
  # Nombrar las columnas del data frame
  #colnames(df_sumcol_proy) <- names(sumcol_proy)
  # Convertir una de las columnas en nombres de fila
  total_proy_multi_ingreso[[i]] <- rownames_to_column(df_sumcol_proy, var="grupo")
}
#pasando datos a columnas
total_multi_ingreso_proy_col <- melt(total_proy_multi_ingreso, id.vars = "grupo") 


# Si no es as?, debes leer o crear el data.frame con tus datos

ggplot(total_multi_ingreso_proy_col, aes(x = grupo, y = value, fill = factor(L1))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Comparaci?n de Values por Grupo y L1",
       x = "Grupo",
       y = "Value",
       fill = "L1") +
  theme_minimal()

# grafico calor multi_ingreso_proy

multi_ingreso_proy_ggplot <- melt(multi_ingreso_proy, id.vars = "produccion")
kvbm_p <- ggplot(multi_ingreso_proy_ggplot, aes(y = Var1, x = Var2, fill = value, color =L1)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "#666f88") +
  geom_text(aes(label = round(value, 5)), vjust = 1) +
  theme_minimal() +
  labs(x = "", y = "", fill = "") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        panel.border = element_rect(colour = "black", fill=NA, size=1))+
  scale_x_discrete(position = "top")  # Mover las etiquetas del eje X a la parte superior# A?ade el recuadro
#plot.background = element_rect(colour = "black", fill=NA)) # Opcional: Cambia el fondo
# Graficar
plot(kvbm_p)
# un analisis linea de la proyeccion, pero carga a 4 años
# Convertir la lista de data frames en un ?nico data frame
df <- do.call(rbind, lapply(seq_along(multi_ingreso_proy_ggplot), function(i) {
  cbind(total_proy_multi_ingreso[[i]], Periodo = i)
}))
# Crear el gr?fico de l?neas
ggplot(df, aes(x = Periodo, y = sumcol_proy, group = grupo, color = grupo)) +
  geom_line() +
  geom_point() +
  labs(title = "Evoluci?n de sumcol_proy por Grupo a lo Largo del Tiempo",
       x = "Periodo",
       y = "Suma de sumcol_proy",
       color = "Grupo") +
  theme_minimal()


#------GRAFO FIGURA 7-----#####
#esta en python, el codigo y la imagen en la carpeta.
##################FIG 8 ------
#multiingreso proyectado a 15 años por cambios en el consumo
multi_ingreso_proy[[15]]
library(reshape2)
library(ggplot2)
multi_ingreso_15 <- melt(multi_ingreso_proy[[15]])
print(multi_ingreso_15)

kvbm_15 <- ggplot(multi_ingreso_15, aes(y = Var1, x = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "#666f88") +
  geom_text(aes(label = round(value, 5)), vjust = 1) +
  theme_minimal() +
  labs(x = "", y = "", fill = "") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        panel.border = element_rect(colour = "black", fill=NA, size=1))+
  scale_x_discrete(position = "top")  # Mover las etiquetas del eje X a la parte superior# A?ade el recuadro
#plot.background = element_rect(colour = "black", fill=NA)) # Opcional: Cambia el fondo

# Graficar
plot(kvbm_15)

#solo la diferencia
multi_ingreso_proy_15 <- multi_ingreso_proy[[15]][,-c(2,8)]
kvbm_15_dif <- (multi_ingreso_proy_15)-KVB
print(kvbm_15_dif)
kvbm_15_dif <- kvbm_15_dif*10^5 

kvbm_15_dif_melt <- melt(kvbm_15_dif)
kvbm_15_dif_map <- ggplot(kvbm_15_dif_melt, aes(y = Var1, x = Var2, fill = value)) +
  geom_tile(color = "black") +  # Añadir bordes negros a las celdas
  scale_fill_gradient(low = "white", high = "#98F5FF", name = "Differences") +  # Añadir nombre a la leyenda
  geom_text(aes(label = round(value, 6)), vjust = 1, color = "black", size = 4) +  # Ajustar tamaño y color del texto
  theme_minimal() +
  labs(x = "", y = "", fill = "") +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold", size = 12),  # Centrar texto del eje X y agrandarlo
    axis.text.y = element_text(face = "bold", size = 12),  # Agrandar texto del eje Y
    panel.border = element_rect(colour = "black", fill = NA, size = 1),  # Añadir borde negro al gráfico
    plot.background = element_rect(fill = "white", color = "black", size = 1),  # Añadir fondo blanco y borde negro
    panel.grid.major = element_blank(),  # Eliminar las líneas de cuadrícula
    panel.grid.minor = element_blank()  # Eliminar las líneas de cuadrícula menores
  ) +
  scale_x_discrete(position = "top")  # Mover las etiquetas del eje X a la parte superior

# Mostrar el gráfico
print(kvbm_15_dif_map)

# Graficar#### es la figura 8
plot(kvbm_15_dif_map)
#
kvbm_15_dif  <- melt(kvbm_15_dif )

# Crear el gráfico de líneas con cajas individuales para cada categoría, sin etiqueta interior y con fondo blanco
produc_proyectada <- ggplot(kvbm_15_dif , aes(x = Var1, y = value, group = Var2, color = Var2)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "", 
       x = "", 
       y = "", 
       color = "Categoría") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),  # Rotar las etiquetas del eje x
    legend.position = "none",  # Eliminar la leyenda
    panel.background = element_rect(fill = "white", color = NA),  # Fondo blanco para el panel
    plot.background = element_rect(fill = "white", color = NA)    # Fondo blanco para todo el gráfico
  ) +
  facet_wrap(~ Var2, scales =  "fixed", nrow = 2, ncol = 5)  # Crear cajas individuales para cada Var2

produc_proyectada

##

kvbm_15_dif_melt <- melt(kvbm_15_dif*10^6)

#KVB$Var2                   <- recode(KVB$Var2,
#                                     "ASP" = "AgroFishing",
#                                     "IMN" = "Manufacturing",
#                                     "EGA" = "Energy",
#                                     "CON" = "Construction",
#                                     "CHR" = "Commerce",
#                                     "TCI" = "Transport",
#                                     "SIV" = "RealEstate",
#                                     "SEN" = "BusinessServices",
#                                     "SPN" = "PersonalServices",
#                                     "ADM" = "PublicAdmin")

# Recode the 'Var2' column in kvbm_15_dif

kvbm_15_dif_melt$Var2 <- recode(kvbm_15_dif_melt$Var2,
                           "ASP" = "AgroFishing",
                           "IMN" = "Manufacturing",
                           "EGA" = "Energy",
                           "CON" = "Construction",
                           "CHR" = "Commerce",
                           "TCI" = "Transport",
                           "SIV" = "Real Estate",
                           "SEN" = "Business Services",
                           "SPN" = "Personal Services",
                           "ADM" = "Public Admin")

# Añadir la columna 'Fuente' para identificar los datos base y proyectados
#Miyazawa_melt$Fuente <- 'Initial'
kvbm_15_dif_melt$Fuente <- 'Projected'

# Combinar ambas bases de datos en un solo data.frame
#datos_combinados <- rbind(Miyazawa_melt, kvbm_15_dif_melt)

# Crear el gráfico de líneas con ggplot
# Multiplicar los valores pequeños por 10^6
kvbm_15_dif_melt$value <- kvbm_15_dif_melt$value * 10^6

# Crear un dataframe de anotaciones para cada faceta
anotaciones <- data.frame(
  Var2 = c("AgroFishing", "Manufacturing", "Energy", "Construction", "Commerce", 
           "Transport", "RealEstate", "BusinessServices", "PersonalServices", "PublicAdmin"),
  label = c("AgroFishing", "Manufacturing", "Energy", "Construction", "Commerce", 
            "Transport", "Real Estate", "Business Services", "Personal Services", "Public Admin"),
  x = c("49s", "49s", "49s", "49s", "49s", "49s", "49s", "49s", "49s", "49s"),  # Coordenadas X
  y = c(1, 1.1, 1.05, 1.02, 1.03, 1.08, 1.04, 1.09, 1.01, 1.07)  # Coordenadas Y
)

# Crear el gráfico de líneas con ggplot
# Crear un dataframe para las posiciones de las etiquetas de categorías (opcional si necesitas controlarlo)
nombres_finales <- kvbm_15_dif_melt %>%
  group_by(Var2) %>%
  filter(Var1 == "80+")  # Tomar los valores correspondientes al año "80+" para etiquetar al final de la línea

# Crear un dataframe para las posiciones de las etiquetas de categorías (opcional si necesitas controlarlo)
nombres_finales <- kvbm_15_dif_melt %>%
  group_by(Var2) %>%
  filter(Var1 == "80+")  # Tomar los valores correspondientes al año "80+" para etiquetar al final de la línea

# Crear el gráfico de líneas con etiquetas para los valores y nombres de categorías
produc.final <- ggplot(kvbm_15_dif_melt, aes(x = Var1, y = value, color = Fuente, group = interaction(Var2, Fuente))) +
  
  # Añadir las líneas para ambas bases de datos
  geom_line(size = 0.5) +
  
  # Añadir los puntos a las líneas
  geom_point(size = 1) +
  
# Agregar etiquetas para los valores numéricos cerca de los puntos
#  geom_text(aes(label = round(value, 2)), 
#            vjust = 0.5,  # Ajustar la posición vertical para que las etiquetas queden justo encima de los puntos
#            size = 3,      # Tamaño del texto
#            color = "black") +  # Color de las etiquetas
  
  # Agregar etiquetas para los nombres de las categorías"
  geom_text(data = nombres_finales, aes(label = Var2), 
            hjust = 1,  # Ajustar la posición horizontal para colocar los nombres a la derecha de los puntos
            vjust = -1.2,   # Posición vertical cerca del valor
            size = 2.5,    # Tamaño del texto de las etiquetas
            color = "blue",  # Color de las etiquetas de nombres
            fontface = "bold") +  # Estilo de fuente para los nombres
  
  # Usar tema minimalista
  theme_minimal() +
  
  # Etiqueta en el eje Y
  labs(x = "", y = "Value (scaled by 10^6)", color = "Share") +
  
  # Personalización del tema
  theme(
    axis.title.y = element_text(size = 6),  # Tamaño del título del eje Y
    axis.title.x = element_text(size = 12),   # Tamaño del título del eje X
    strip.text = element_blank(),  # Eliminar los títulos de las facetas (opcional)
    axis.text.x = element_text(angle = 90, hjust = 1),  # Rotar las etiquetas del eje X para mayor claridad
    axis.text.y = element_text(size = 8),  # Ajustar el tamaño de las etiquetas del eje Y
    panel.spacing = unit(1, "lines"),  # Aumentar el espacio entre las facetas
    panel.background = element_rect(fill = "white", color = NA),  # Fondo blanco para el panel
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none"  # Eliminar la leyenda
  ) +
  
  # Crear facetas individuales por categoría (10 categorías en 2 filas y 5 columnas)
  facet_wrap(~ Var2, scales = "fixed", nrow = 2, ncol = 5)

# Mostrar el gráfico final
print(produc.final)

ggsave("Method/Imagenes/imagenes_final_ingles/Figure_8.png", plot=produc.final, width=7.48, height = 4, dpi=300)


#-----------Anexos---------####
#-------A-1-----------####
library(xtable)
# Convertir ----------     LaTeX con xtable     ------#
#1 cuadro 15 de disminucion 
#pasar a data.frame
k_proyec_l_latex <- as.data.frame(k_proyec_l[[15]])
k_latex_proy_l <- xtable(k_proyec_l_latex, digits =5)
# Imprimir el c?digo LaTeX del dataframe
print(k_latex_proy_l, digits=5, include.rownames = TRUE, floating = FALSE)

#-------A-2------diferencia low change #####
#desarrollado para el año 1
diferencia_k_pro_l_kson <- k_proyec_l[[1]]-k_son
print(diferencia_k_pro_l_kson)
#-------A-3------reduccion para visibilidad #### 
dif_dis_l <- diferencia_k_pro_l_kson/(10^(-3))
diferencia_k_pro_l_kson_latex <- xtable(dif_dis_l, digits=5)
print(diferencia_k_pro_l_kson_latex)
#-------A-4------proyeccion año 15####
diferencia_k_pro_l_kson_15 <- k_proyec_l[[15]]-k_son
print(diferencia_k_pro_l_kson_15)
#-------A-5------ diferencia (1-15) ---####
dif_dis_l_15 <- diferencia_k_pro_l_kson_15/(10^(-3))
diferencia_k_pro_l_kson_latex_15 <- xtable(dif_dis_l_15, digits=5)
print(diferencia_k_pro_l_kson_latex_15)
#--------A-6-------Diferencia de aumento 15% -----#
k_p_hi_1 <- k_proyec[[1]]-k_son
k_p_hi_15 <- k_proyec[[15]]-k_son
print(k_p_hi_15)
