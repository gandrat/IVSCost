

library(sf)
library(dplyr)

rm(list=ls()) ## Limpando o workspace
##Carregando os dados---------------
load('output_data/setores_ivs.Rda') #Dados calculados na rotina 1 (PCA e IVS)
load('input_data/malha_territorial_sf.Rda') #Shapes de malhas territoriais

#Setores------------
set<-merge(set_sf,setores, by=c('cod_setor','cod_mun'))
write_sf(set,'output_data/shapes/setores_ivs.shp')
setores<-set



#Municipios-----------------
setores$geometry<-NULL#removendo a geometria dos setores
muni<-setores%>%select(-cod_setor)%>%
  group_by(nm_mun,nm_micro,nm_meso,cod_uf,cod_mun)%>%
  summarise_all(~ mean(.x, na.rm = TRUE))

muni<-merge(muni_sf,muni,by=c('cod_mun','nm_mun'))
plot(muni%>%select(ivs1))
write_sf(muni,'output_data/shapes/municipios_ivs.shp')


#Microrregiões------

micro<-setores%>%select(-tipo,-cod_setor,-cod_mun,-nm_mun)%>%
  group_by(nm_micro,nm_meso,cod_uf)%>%
  summarise_all(~ mean(.x, na.rm = TRUE))

micro<-merge(micro_sf,micro,by=c('nm_micro','cod_uf'))
write_sf(micro,'output_data/shapes/micro_ivs.shp')

plot(micro%>%select(ivs1))


#Mesorregiões-----

meso<-setores%>%select(-tipo,-cod_setor,-cod_mun,-nm_mun,-nm_micro)%>%
  group_by(nm_meso,cod_uf)%>%
  summarise_all(~ mean(.x, na.rm = TRUE))

meso<-merge(meso_sf,meso,by=c('nm_meso','cod_uf'))
plot(meso%>%select(ivs1))
write_sf(meso,'output_data/shapes/meso_ivs.shp')



#Estados-----

uf<-setores%>%select(-tipo,-cod_setor,-cod_mun,-nm_mun,-nm_micro,-nm_meso)%>%
  group_by(cod_uf)%>%
  summarise_all(~ mean(.x, na.rm = TRUE))

uf<-merge(uf_sf,uf,by=c('cod_uf'))
plot(uf%>%select(ivs1))
write_sf(uf,'output_data/shapes/uf_ivs.shp')

save(meso,micro,muni,uf,setores,file='output_data/malha_territorial_ivs.Rda')


