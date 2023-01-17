

if (!require(devtools)) {
  install.packages("devtools")
}
devtools::install_github('diegovalle/mxmaps')


library(testthat)
library(mxmaps)

rm(list=ls())
setwd("~/ENIGH")

############################
#viviendas (info geografica)
############################
hogar_2016 = read.csv('~/ENIGH/2016/concentradohogar.csv',sep=',')
hogar_2018 = read.csv('~/ENIGH/2018/concentradohogar.csv',sep=',')
hogar_2020 = read.csv('~/ENIGH/2020/concentradohogar.csv',sep=',')

hogar_2016 = hogar_2016[, c('folioviv', 'foliohog', 'ubica_geo', 'factor', 'tot_integ', 'hombres', 'mujeres', 'mayores', 'menores')]
hogar_2018 = hogar_2018[, c('folioviv', 'foliohog', 'ubica_geo', 'factor', 'tot_integ', 'hombres', 'mujeres', 'mayores', 'menores')]
hogar_2020 = hogar_2020[, c('folioviv', 'foliohog', 'ubica_geo', 'factor', 'tot_integ', 'hombres', 'mujeres', 'mayores', 'menores')]

hogar_2016 <- within(hogar_2016, ubica_geo[nchar(ubica_geo)==8] <- paste('0', ubica_geo, sep = ""))
hogar_2018 <- within(hogar_2018, ubica_geo[nchar(ubica_geo)==4] <- paste('0', ubica_geo, sep = ""))
hogar_2020 <- within(hogar_2020, ubica_geo[nchar(ubica_geo)==4] <- paste('0', ubica_geo, sep = ""))


hogar_2016['state_code'] <- as.character(substr(hogar_2016$ubica_geo, 0, 2))
hogar_2016['municipio_code'] <- as.character(substr(hogar_2016$ubica_geo, 3, 5))

hogar_2018['state_code'] <- as.character(substr(hogar_2018$ubica_geo, 0, 2))
hogar_2018['municipio_code'] <- as.character(substr(hogar_2018$ubica_geo, 3, 5))

hogar_2020['state_code'] <- as.character(substr(hogar_2020$ubica_geo, 0, 2))
hogar_2020['municipio_code'] <- as.character(substr(hogar_2020$ubica_geo, 3, 5))


############################
#informacion de gasto de los hogares
############################
#base 2020
#deflactor_2016 = 1.199754326
#deflactor_2018 = 1.070986953
#deflactor_2020 = 1.0

#base 2018
#deflactor_2016 = 1.120232438
#deflactor_2018 = 1.0
#deflactor_2020 = 0.933718191

#base 2016
deflactor_2016 = 1.0
deflactor_2018 = 0.892671883
deflactor_2020 = 0.833503975

############################
#ingreso de los hogares
############################
ingreso_2016 = read.csv('~/ENIGH/2016/ingresos.csv',sep=',')
ingreso_2018 = read.csv('~/ENIGH/2018/ingresos.csv',sep=',')
ingreso_2020 = read.csv('~/ENIGH/2020/ingresos.csv',sep=',')

ingreso_2016['ing_tri_2016'] <- ingreso_2016$ing_tri * deflactor_2016
ingreso_2018['ing_tri_2018'] <- ingreso_2018$ing_tri * deflactor_2018
ingreso_2020['ing_tri_2020'] <- ingreso_2020$ing_tri * deflactor_2020

hogar_ingreso_2016 <- aggregate(ingreso_2016$ing_tri_2016, by=list(ingreso_2016$folioviv, ingreso_2016$foliohog), FUN=sum)
names(hogar_ingreso_2016)[1] ="folioviv"
names(hogar_ingreso_2016)[2] ="foliohog"
names(hogar_ingreso_2016)[3] ="ing_tri_2016"

hogar_ingreso_2018 <- aggregate(ingreso_2018$ing_tri_2018, by=list(ingreso_2018$folioviv, ingreso_2018$foliohog), FUN=sum)
names(hogar_ingreso_2018)[1] ="folioviv"
names(hogar_ingreso_2018)[2] ="foliohog"
names(hogar_ingreso_2018)[3] ="ing_tri_2018"

hogar_ingreso_2020 <- aggregate(ingreso_2020$ing_tri_2020, by=list(ingreso_2020$folioviv, ingreso_2020$foliohog), FUN=sum)
names(hogar_ingreso_2020)[1] ="folioviv"
names(hogar_ingreso_2020)[2] ="foliohog"
names(hogar_ingreso_2020)[3] ="ing_tri_2020"


############################
#1er join
############################
df_2016 <- merge(x=hogar_2016,y=hogar_ingreso_2016, 
                by=c("folioviv","foliohog"), all.x=TRUE)

df_2018 <- merge(x=hogar_2018,y=hogar_ingreso_2018, 
                 by=c("folioviv","foliohog"), all.x=TRUE)

df_2020 <- merge(x=hogar_2020,y=hogar_ingreso_2020, 
                 by=c("folioviv","foliohog"), all.x=TRUE)



############################
#gastos de los hogares
############################
gasto_hogar_2016 = read.csv('~/ENIGH/2016/gastoshogar.csv',sep=',')
gasto_hogar_2018 = read.csv('~/ENIGH/2018/gastoshogar.csv',sep=',')
gasto_hogar_2020 = read.csv('~/ENIGH/2020/gastoshogar.csv',sep=',')

############################  
#deflactar inflacion
############################
gasto_hogar_2016['gasto_tri_2016'] <- gasto_hogar_2016$gasto_tri * deflactor_2016
gasto_hogar_2018['gasto_tri_2018'] <- gasto_hogar_2018$gasto_tri * deflactor_2018
gasto_hogar_2020['gasto_tri_2020'] <- gasto_hogar_2020$gasto_tri * deflactor_2020

#clave del tipo de gasto
gasto_hogar_2016['alpha_clave'] <- substr(gasto_hogar_2016$clave, 1, 1)
gasto_hogar_2016['num_clave'] <- as.numeric(substr(gasto_hogar_2016$clave, 2, 4))

gasto_hogar_2018['alpha_clave'] <- substr(gasto_hogar_2018$clave, 1, 1)
gasto_hogar_2018['num_clave'] <- as.numeric(substr(gasto_hogar_2018$clave, 2, 4))

gasto_hogar_2020['alpha_clave'] <- substr(gasto_hogar_2020$clave, 1, 1)
gasto_hogar_2020['num_clave'] <- as.numeric(substr(gasto_hogar_2020$clave, 2, 4))


#gasto monetario en salud
#G1 Gasto monetario en bienes y servicios para el hogar
gasto_salud_2016 <- gasto_hogar_2016[gasto_hogar_2016$tipo_gasto=='G1' & gasto_hogar_2016$alpha_clave=='J' & gasto_hogar_2016$num_clave >=1 &  gasto_hogar_2016$num_clave <=72,]
gasto_salud_2018 <- gasto_hogar_2018[gasto_hogar_2018$tipo_gasto=='G1' & gasto_hogar_2018$alpha_clave=='J' & gasto_hogar_2018$num_clave >=1 &  gasto_hogar_2018$num_clave <=72,]
gasto_salud_2020 <- gasto_hogar_2020[gasto_hogar_2020$tipo_gasto=='G1' & gasto_hogar_2020$alpha_clave=='J' & gasto_hogar_2020$num_clave >=1 &  gasto_hogar_2020$num_clave <=72,]


#gasto en salud por hogar
#2016
hogar_salud_2016 <- aggregate(gasto_salud_2016$gasto_tri_2016,
                              by=list(gasto_salud_2016$folioviv, gasto_salud_2016$foliohog), FUN=sum)
names(hogar_salud_2016)[1] ="folioviv"
names(hogar_salud_2016)[2] ="foliohog"
names(hogar_salud_2016)[3] ="salud_2016"


df_2016 <- merge(x=df_2016,y=hogar_salud_2016, 
                  by=c("folioviv","foliohog"), all.x=TRUE)


#2018
hogar_salud_2018 <- aggregate(gasto_salud_2018$gasto_tri_2018,
                              by=list(gasto_salud_2018$folioviv, gasto_salud_2018$foliohog), FUN=sum)
names(hogar_salud_2018)[1] ="folioviv"
names(hogar_salud_2018)[2] ="foliohog"
names(hogar_salud_2018)[3] ="salud_2018"


df_2018 <- merge(x=df_2018,y=hogar_salud_2018, 
                 by=c("folioviv","foliohog"), all.x=TRUE)


#2020
hogar_salud_2020 <- aggregate(gasto_salud_2020$gasto_tri_2020,
                              by=list(gasto_salud_2020$folioviv, gasto_salud_2020$foliohog), FUN=sum)
names(hogar_salud_2020)[1] ="folioviv"
names(hogar_salud_2020)[2] ="foliohog"
names(hogar_salud_2020)[3] ="salud_2020"


df_2020 <- merge(x=df_2020,y=hogar_salud_2020, 
                       by=c("folioviv","foliohog"), all.x=TRUE)



#gasto monetario en alimentos
gasto_alimento_2016 <- gasto_hogar_2016[gasto_hogar_2016$tipo_gasto=='G1' & gasto_hogar_2016$alpha_clave=='A' & gasto_hogar_2016$num_clave >=1 &  gasto_hogar_2016$num_clave <=212,]
gasto_alimento_2018 <- gasto_hogar_2018[gasto_hogar_2018$tipo_gasto=='G1' & gasto_hogar_2018$alpha_clave=='A' & gasto_hogar_2018$num_clave >=1 &  gasto_hogar_2018$num_clave <=212,]
gasto_alimento_2020 <- gasto_hogar_2020[gasto_hogar_2020$tipo_gasto=='G1' & gasto_hogar_2020$alpha_clave=='A' & gasto_hogar_2020$num_clave >=1 &  gasto_hogar_2020$num_clave <=212,]


#gasto en alimentos por hogar
#2016
hogar_alimento_2016 <- aggregate(gasto_alimento_2016$gasto_tri_2016,
                              by=list(gasto_alimento_2016$folioviv, gasto_alimento_2016$foliohog), FUN=sum)
names(hogar_alimento_2016)[1] ="folioviv"
names(hogar_alimento_2016)[2] ="foliohog"
names(hogar_alimento_2016)[3] ="alimento_2016"


df_2016 <- merge(x=df_2016,y=hogar_alimento_2016, 
                 by=c("folioviv","foliohog"), all.x=TRUE)


#2018
hogar_alimento_2018 <- aggregate(gasto_alimento_2018$gasto_tri_2018,
                              by=list(gasto_alimento_2018$folioviv, gasto_alimento_2018$foliohog), FUN=sum)
names(hogar_alimento_2018)[1] ="folioviv"
names(hogar_alimento_2018)[2] ="foliohog"
names(hogar_alimento_2018)[3] ="alimento_2018"


df_2018 <- merge(x=df_2018,y=hogar_alimento_2018, 
                 by=c("folioviv","foliohog"), all.x=TRUE)


#2020
hogar_alimento_2020 <- aggregate(gasto_alimento_2020$gasto_tri_2020,
                              by=list(gasto_alimento_2020$folioviv, gasto_alimento_2020$foliohog), FUN=sum)
names(hogar_alimento_2020)[1] ="folioviv"
names(hogar_alimento_2020)[2] ="foliohog"
names(hogar_alimento_2020)[3] ="alimento_2020"


df_2020 <- merge(x=df_2020,y=hogar_alimento_2020, 
                 by=c("folioviv","foliohog"), all.x=TRUE)



#gasto monetario en educacion
gasto_educacion_2016 <- gasto_hogar_2016[gasto_hogar_2016$tipo_gasto=='G1' & gasto_hogar_2016$alpha_clave=='E' & gasto_hogar_2016$num_clave >=1 &  gasto_hogar_2016$num_clave <=21,]
gasto_educacion_2018 <- gasto_hogar_2018[gasto_hogar_2018$tipo_gasto=='G1' & gasto_hogar_2018$alpha_clave=='E' & gasto_hogar_2018$num_clave >=1 &  gasto_hogar_2018$num_clave <=21,]
gasto_educacion_2020 <- gasto_hogar_2020[gasto_hogar_2020$tipo_gasto=='G1' & gasto_hogar_2020$alpha_clave=='E' & gasto_hogar_2020$num_clave >=1 &  gasto_hogar_2020$num_clave <=21,]


#gasto en educacion por hogar
#2016
hogar_educacion_2016 <- aggregate(gasto_educacion_2016$gasto_tri_2016,
                                 by=list(gasto_educacion_2016$folioviv, gasto_educacion_2016$foliohog), FUN=sum)
names(hogar_educacion_2016)[1] ="folioviv"
names(hogar_educacion_2016)[2] ="foliohog"
names(hogar_educacion_2016)[3] ="educacion_2016"


df_2016 <- merge(x=df_2016,y=hogar_educacion_2016, 
                 by=c("folioviv","foliohog"), all.x=TRUE)


#2018
hogar_educacion_2018 <- aggregate(gasto_educacion_2018$gasto_tri_2018,
                                 by=list(gasto_educacion_2018$folioviv, gasto_educacion_2018$foliohog), FUN=sum)
names(hogar_educacion_2018)[1] ="folioviv"
names(hogar_educacion_2018)[2] ="foliohog"
names(hogar_educacion_2018)[3] ="educacion_2018"


df_2018 <- merge(x=df_2018,y=hogar_educacion_2018, 
                 by=c("folioviv","foliohog"), all.x=TRUE)


#2020
hogar_educacion_2020 <- aggregate(gasto_educacion_2020$gasto_tri_2020,
                                 by=list(gasto_educacion_2020$folioviv, gasto_educacion_2020$foliohog), FUN=sum)
names(hogar_educacion_2020)[1] ="folioviv"
names(hogar_educacion_2020)[2] ="foliohog"
names(hogar_educacion_2020)[3] ="educacion_2020"


df_2020 <- merge(x=df_2020,y=hogar_educacion_2020, 
                 by=c("folioviv","foliohog"), all.x=TRUE)



#gasto monetario en bebidas alcoholicas
gasto_bebida_2016 <- gasto_hogar_2016[gasto_hogar_2016$tipo_gasto=='G1' & gasto_hogar_2016$alpha_clave=='A' & gasto_hogar_2016$num_clave >=223 &  gasto_hogar_2016$num_clave <=238,]
gasto_bebida_2018 <- gasto_hogar_2018[gasto_hogar_2018$tipo_gasto=='G1' & gasto_hogar_2018$alpha_clave=='A' & gasto_hogar_2018$num_clave >=223 &  gasto_hogar_2018$num_clave <=238,]
gasto_bebida_2020 <- gasto_hogar_2020[gasto_hogar_2020$tipo_gasto=='G1' & gasto_hogar_2020$alpha_clave=='A' & gasto_hogar_2020$num_clave >=223 &  gasto_hogar_2020$num_clave <=238,]

#gasto en bebidas alcoholicas por hogar
#2016
hogar_bebida_2016 <- aggregate(gasto_bebida_2016$gasto_tri_2016,
                                  by=list(gasto_bebida_2016$folioviv, gasto_bebida_2016$foliohog), FUN=sum)
names(hogar_bebida_2016)[1] ="folioviv"
names(hogar_bebida_2016)[2] ="foliohog"
names(hogar_bebida_2016)[3] ="bebida_2016"


df_2016 <- merge(x=df_2016,y=hogar_bebida_2016, 
                 by=c("folioviv","foliohog"), all.x=TRUE)


#2018
hogar_bebida_2018 <- aggregate(gasto_bebida_2018$gasto_tri_2018,
                                  by=list(gasto_bebida_2018$folioviv, gasto_bebida_2018$foliohog), FUN=sum)
names(hogar_bebida_2018)[1] ="folioviv"
names(hogar_bebida_2018)[2] ="foliohog"
names(hogar_bebida_2018)[3] ="bebida_2018"


df_2018 <- merge(x=df_2018,y=hogar_bebida_2018, 
                 by=c("folioviv","foliohog"), all.x=TRUE)


#2020
hogar_bebida_2020 <- aggregate(gasto_bebida_2020$gasto_tri_2020,
                                  by=list(gasto_bebida_2020$folioviv, gasto_bebida_2020$foliohog), FUN=sum)
names(hogar_bebida_2020)[1] ="folioviv"
names(hogar_bebida_2020)[2] ="foliohog"
names(hogar_bebida_2020)[3] ="bebida_2020"


df_2020 <- merge(x=df_2020,y=hogar_bebida_2020, 
                 by=c("folioviv","foliohog"), all.x=TRUE)



#gasto monetario en bebidas tabaco
gasto_tabaco_2016 <- gasto_hogar_2016[gasto_hogar_2016$tipo_gasto=='G1' & gasto_hogar_2016$alpha_clave=='A' & gasto_hogar_2016$num_clave >=239 &  gasto_hogar_2016$num_clave <=241,]
gasto_tabaco_2018 <- gasto_hogar_2018[gasto_hogar_2018$tipo_gasto=='G1' & gasto_hogar_2018$alpha_clave=='A' & gasto_hogar_2018$num_clave >=239 &  gasto_hogar_2018$num_clave <=241,]
gasto_tabaco_2020 <- gasto_hogar_2020[gasto_hogar_2020$tipo_gasto=='G1' & gasto_hogar_2020$alpha_clave=='A' & gasto_hogar_2020$num_clave >=239 &  gasto_hogar_2020$num_clave <=241,]

#gasto en bebidas alcoholicas por hogar
#2016
hogar_tabaco_2016 <- aggregate(gasto_tabaco_2016$gasto_tri_2016,
                               by=list(gasto_tabaco_2016$folioviv, gasto_tabaco_2016$foliohog), FUN=sum)
names(hogar_tabaco_2016)[1] ="folioviv"
names(hogar_tabaco_2016)[2] ="foliohog"
names(hogar_tabaco_2016)[3] ="tabaco_2016"


df_2016 <- merge(x=df_2016,y=hogar_tabaco_2016, 
                 by=c("folioviv","foliohog"), all.x=TRUE)


#2018
hogar_tabaco_2018 <- aggregate(gasto_tabaco_2018$gasto_tri_2018,
                               by=list(gasto_tabaco_2018$folioviv, gasto_tabaco_2018$foliohog), FUN=sum)
names(hogar_tabaco_2018)[1] ="folioviv"
names(hogar_tabaco_2018)[2] ="foliohog"
names(hogar_tabaco_2018)[3] ="tabaco_2018"


df_2018 <- merge(x=df_2018,y=hogar_tabaco_2018, 
                 by=c("folioviv","foliohog"), all.x=TRUE)


#2020
hogar_tabaco_2020 <- aggregate(gasto_tabaco_2020$gasto_tri_2020,
                               by=list(gasto_tabaco_2020$folioviv, gasto_tabaco_2020$foliohog), FUN=sum)
names(hogar_tabaco_2020)[1] ="folioviv"
names(hogar_tabaco_2020)[2] ="foliohog"
names(hogar_tabaco_2020)[3] ="tabaco_2020"


df_2020 <- merge(x=df_2020,y=hogar_tabaco_2020, 
                 by=c("folioviv","foliohog"), all.x=TRUE)



#variables calculadas
df_2016[is.na(df_2016)] <- 0
df_2018[is.na(df_2018)] <- 0
df_2020[is.na(df_2020)] <- 0


df_2016['ing_disponible_2016'] <- df_2016$ing_tri_2016 - (df_2016$alimento_2016 + df_2016$educacion_2016 + df_2016$bebida_2016 + df_2016$tabaco_2016)
df_2018['ing_disponible_2018'] <- df_2018$ing_tri_2018 - (df_2018$alimento_2018 + df_2018$educacion_2018 + df_2018$bebida_2018 + df_2018$tabaco_2018)
df_2020['ing_disponible_2020'] <- df_2020$ing_tri_2020 - (df_2020$alimento_2020 + df_2020$educacion_2020 + df_2020$bebida_2020 + df_2020$tabaco_2020)



#########################
#agrupar por estado
########################
estado_2016 <- aggregate(cbind(df_2016$tot_integ,
                               df_2016$hombres,
                               df_2016$mujeres,
                               df_2016$mayores,
                               df_2016$menores,
                               df_2016$ing_tri_2016,
                               df_2016$salud_2016,
                               df_2016$alimento_2016,
                               df_2016$educacion_2016,
                               df_2016$bebida_2016,
                               df_2016$tabaco_2016,
                               df_2016$ing_disponible_2016),
                         by=list(df_2016$state_code), FUN=sum)

names(estado_2016)[1] ="region"
names(estado_2016)[2] ="tot_integ_2016"
names(estado_2016)[3] ="hombres_2016"
names(estado_2016)[4] ="mujeres_2016"
names(estado_2016)[5] ="mayores_2016"
names(estado_2016)[6] ="menores_2016"
names(estado_2016)[7] ="ing_tri_2016"
names(estado_2016)[8] ="salud_2016"
names(estado_2016)[9] ="alimento_2016"
names(estado_2016)[10] ="educacion_2016"
names(estado_2016)[11] ="bebida_2016"
names(estado_2016)[12] ="tabaco_2016"
names(estado_2016)[13] ="ing_disponible_2016"


estado_2018 <- aggregate(cbind(df_2018$tot_integ,
                               df_2018$hombres,
                               df_2018$mujeres,
                               df_2018$mayores,
                               df_2018$menores,
                               df_2018$ing_tri_2018,
                               df_2018$salud_2018,
                               df_2018$alimento_2018,
                               df_2018$educacion_2018,
                               df_2018$bebida_2018,
                               df_2018$tabaco_2018,
                               df_2018$ing_disponible_2018),
                         by=list(df_2018$state_code), FUN=sum)

names(estado_2018)[1] ="region"
names(estado_2018)[2] ="tot_integ_2018"
names(estado_2018)[3] ="hombres_2018"
names(estado_2018)[4] ="mujeres_2018"
names(estado_2018)[5] ="mayores_2018"
names(estado_2018)[6] ="menores_2018"
names(estado_2018)[7] ="ing_tri_2018"
names(estado_2018)[8] ="salud_2018"
names(estado_2018)[9] ="alimento_2018"
names(estado_2018)[10] ="educacion_2018"
names(estado_2018)[11] ="bebida_2018"
names(estado_2018)[12] ="tabaco_2018"
names(estado_2018)[13] ="ing_disponible_2018"


estado_2020 <- aggregate(cbind(df_2020$tot_integ,
                               df_2020$hombres,
                               df_2020$mujeres,
                               df_2020$mayores,
                               df_2020$menores,
                               df_2020$ing_tri_2020,
                               df_2020$salud_2020,
                               df_2020$alimento_2020,
                               df_2020$educacion_2020,
                               df_2020$bebida_2020,
                               df_2020$tabaco_2020,
                               df_2020$ing_disponible_2020),
                         by=list(df_2020$state_code), FUN=sum)

names(estado_2020)[1] ="region"
names(estado_2020)[2] ="tot_integ_2020"
names(estado_2020)[3] ="hombres_2020"
names(estado_2020)[4] ="mujeres_2020"
names(estado_2020)[5] ="mayores_2020"
names(estado_2020)[6] ="menores_2020"
names(estado_2020)[7] ="ing_tri_2020"
names(estado_2020)[8] ="salud_2020"
names(estado_2020)[9] ="alimento_2020"
names(estado_2020)[10] ="educacion_2020"
names(estado_2020)[11] ="bebida_2020"
names(estado_2020)[12] ="tabaco_2020"
names(estado_2020)[13] ="ing_disponible_2020"



df_mxstate_2020 <- merge(x=df_mxstate_2020,y=estado_2016, 
                         by=c("region"), all.x=TRUE)

df_mxstate_2020 <- merge(x=df_mxstate_2020,y=estado_2018, 
                         by=c("region"), all.x=TRUE)

df_mxstate_2020 <- merge(x=df_mxstate_2020,y=estado_2020, 
                         by=c("region"), all.x=TRUE)




x <- df_mxstate_2020$salud_2020/df_mxstate_2020$ing_disponible_2020
h<-hist(x, breaks=6, col="gray", xlab="Gasto de Bolsillo en Salud",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2) 


write.csv(df_mxstate_2020, "df_mxstate_2020.csv", row.names=TRUE)


