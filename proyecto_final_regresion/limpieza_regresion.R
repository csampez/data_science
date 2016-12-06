source('utils.R')

############################################################

moto_oficial     moto_de_alquiler  moto_particular

### CALIDAD AIRE ###


### VEHICULOS ### 
folder<-'vehiculos_de_motor_registrados_vmrc/conjunto_de_datos/'
paths<- paste(folder,dir(folder),sep='')

cat_ent<-get_clean('vehiculos_de_motor_registrados_vmrc/catalogos/tc_entidad.csv',c('character','character'))
cat_mpo<-get_clean('vehiculos_de_motor_registrados_vmrc/catalogos/tc_municipio.csv',c('character','character','character')) 
names(cat_mpo)[1]<-'ID_ENTIDAD'
cat_mpo <- left_join(cat_mpo,cat_ent,by='ID_ENTIDAD') 


all_data_motor<-bind_rows(lapply(paths,get_data)) %>% mutate(ID_ENTIDAD = str_pad(string=ID_ENTIDAD, width=2, side='left', pad='0'),
					ID_MUNICIPIO = str_pad(string=ID_MUNICIPIO, width=3, side='left', pad='0') )  %>% select(-ESTATUS)

motor_final<-left_join(all_data_motor,cat_mpo, by=c('ID_ENTIDAD','ID_MUNICIPIO')) %>% select(-ID_ESTATUS_CIFRA) %>% filter( NOM_ENTIDAD %in% c("Ciudad de México","México"))
names(motor_final)<-tolower(names(motor_final))
motor_final <- motor_final %>% mutate(id_entmun=paste(motor_final$id_entidad,motor_final$id_municipio,sep=''))  %>%
							select(-id_entidad,-id_municipio) %>% rename(year=año)
#get_clean('vehiculos_de_motor_registrados_vmrc/catalogos/tc_estatus_cifra.csv',c('character','character','character'))

motor_final <- summary


motor_final <- motor_final %>% mutate( 
									auto = auto_oficial + auto_publico + auto_particular,
									cam_pas = cam_pas_oficial + cam_pas_publico + cam_pas_particular,
									cyc = cyc_carga_oficial + cyc_carga_publico + cyc_carga_particular,
									moto = moto_oficial + moto_de_alquiler + moto_particular) %>% select(auto,cam_pas,cyc,moto,nom_municipio,nom_entidad,id_entmun,year)



## CLAVES DE LA ZONA METROPOLITANA

 

#### CONTAMINANTES ####
estacion<-read.csv('contaminantes_aire/cat_estacion.csv', fileEncoding='iso-8859-1',skip=1) 
units<-read.csv('contaminantes_aire/cat_unidades.csv', fileEncoding='iso-8859-1')
pars<-read.csv('contaminantes_aire/cat_parametros.csv', fileEncoding='iso-8859-1',skip=1)
plomo<-read.csv('contaminantes_aire/red_manual_plomo.csv', skip=8)
parti<-read.csv('contaminantes_aire/red_manual_particulas_susp.csv', skip=8)
cvegeo_est <- read.csv('cvegeo_estaciones.csv', stringsAsFactor=FALSE, as.is=TRUE)



all_aire<-bind_rows(plomo,parti) %>% mutate(Date=as.factor(Date), cve_station=as.factor(cve_station), cve_parameter=as.factor(cve_parameter))

all_aire<-all_aire %>% mutate(year=substr(Date,7,10)) %>% group_by(cve_station,year,cve_parameter) %>%
		 summarise(valor=sum(value,na.rm=TRUE),mean_valor=mean(value,na.rm=TRUE), unit=mean(unit,na.rm=TRUE))

aire_valor<-left_join(all_aire, units, by=c('unit'='id_unidad')) %>% left_join(.,estacion, by=c('cve_station'='cve_estac')) %>% select(-id_station,-alt,-obs_estac,-unit, -mean_valor) %>% 
spread(cve_parameter, valor) %>% ungroup

aire_muvalor<-left_join(all_aire, units, by=c('unit'='id_unidad')) %>% left_join(.,estacion, by=c('cve_station'='cve_estac')) %>% select(-id_station,-alt,-obs_estac,-unit, -valor)%>% 
spread(.,key=cve_parameter, value=mean_valor) %>% ungroup

abiertas<-estacion %>% filter(obs_estac=='') %>% select(cve_estac) 
print(abiertas)
#%>% filter(cve_station %in% abiertas$cve_estac)

table(all_aire$cve_station, all_aire$cve_parameter)

names(aire_muvalor)<-c("cve_station", 
						 "year",
						 "clave_unidad" ,
						 "nombre_unidad",
						 "nom_estac",  
						 "longitud",
						 "latitud",
						 "mean_PbPM10",
						 "mean_PbPST",   
						 "mean_PM10" ,
						 "mean_PM2.5",
						 "mean_PST" )

aire_final<-left_join( aire_muvalor, aire_valor, by=c("cve_station", 
											 "year",
											 "clave_unidad" ,
											 "nombre_unidad",
											 "nom_estac",  
											 "longitud",
											 "latitud") ) %>% left_join(.,cvegeo_est,by=c('cve_station'='cve_estac')) %>%
		 mutate(id_entidad=substr(cvegeo,1,2), id_municipio=substr(cvegeo,3,5), id_entmun=substr(cvegeo,1,5)) %>%
		 select(-longitud,-latitud, -id_entidad,-id_municipio,-cvegeo,-cve_station,-nom_estac) %>% 
		 group_by(id_entmun, year,clave_unidad,nombre_unidad) %>%
		 summarise_each(funs(max(.,na.rm=TRUE)))

names(aire_final) <-  tolower(names(aire_final))


get_suple<-function(xx){
	cerradas[which.min((cerradas$longitud-xx[1])**2 + (cerradas$latitud-xx[2])**2),]
}


ggplot_missing(aire_final)

dataset<-left_join(aire_final %>% ungroup %>% mutate(year_num=as.numeric(year), year=as.Date(paste(year,'-01-01',sep=''))),motor_final %>% mutate(year=as.Date(paste(year,'-01-01',sep=''))),by=c('id_entmun','year'))
 
datacorr <- dataset %>% select(-mean_pbpm10, -mean_pbpst, -pst, -mean_pst, -pbpm10, -pbpst) 
datacorr[,sapply(datacorr,is.numeric)]<-sapply(datacorr[,sapply(datacorr,is.numeric)],scale)

ggpairs(datacorr %>% select(-year,-clave_unidad, -nombre_unidad, -nom_municipio,-nom_entidad, -id_entmun), 
	lower=list(continuous = wrap(ggally_cor, size = 0.1)),
    upper=list(continuous = wrap(ggally_points, size = 1, color = "blue"))
)

#suben?
plot(aire_final$year,aire_final$pm2.5)

#bajan?
plot(aire_final$year,aire_final$pm2.5)




## SELECCION DE PAREJAS
data_pre <- dataset %>% select(pm10,pm2.5,auto,cam_pas,cyc,moto,year,id_entmun)


write.csv(data_pre%>% na.omit, 'regresion_airecdmx',row.names=FALSE)



data_pre <- dataset %>% select(pm2.5,auto,cam_pas,cyc,moto,year,id_entmun) %>% gather(key=,value=)


#cor(na.omit(numerico) %>% as.matrix))
ggplot(dataset, aes(x=cam_pas_oficial+cam_pas_pu
ggpairs(numerico %>% select(-moto_de_alquiler,-fecha), lower=list(continuous='cor'),
    upper=list(continuous = 'points')
)blico+cam_pas_particular, y=pm2.5, color=year)) + geom_point() + facet_grid(id_entmun~.,scales = "free")
ggplot(dataset, aes(x=cam_pas_oficial, y=pbpm10)) + geom_point()

#import pandas as pd
#import numpy as np 
#estaciones=pd.read_csv('../../../../proyecto_final_regresion/coordenadas_estaciones.csv') #.rename()

#executor = ThreadPoolExecutor()
#result = list(executor.map(func_subm, estaciones[['latitud','longitud','cve_estac']].itertuples()))
#print(time.time() - now)	

#pd.concat(result).to_csv('../../../../proyecto_final_regresion/cvegeo_estaciones.csv',index=None)


 