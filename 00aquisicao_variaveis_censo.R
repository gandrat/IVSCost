#Obtenção de Variáveis do Censo para o cálculo do Índice de Vulnerabilidade Social Costeiro
#Aplicação nos municípios costeiros do Brasil
#Menor unidade de análise: setor censitário


#Carregando pacotes------------
#packages<-c('ggplot2','utils','readxl','dplyr','sf','matrixStats','classInt','reshape','nngeo','sf','Rarity')

rm(list=ls()) ## Removendo as variáveis

packages<-c('ggplot2','readxl','dplyr','sf','gdata')
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

##Selecionando apenas municípios costeiros
##Shape editado por Cibele
m<-read_sf('input_data/municipios_defrontantes2010.shp')
# m[which(m$CD_GCMUN=='4220000'),'CD_GCMUN']<-'4207007'
coast_mun<-as.numeric(m$GEOCODIGO)
coast_states<-as.numeric(unique(substring(m$GEOCODIGO,0,2)))
# coast_mun<-append(coast_mun,"4202305")


#Shape: setores censitarios de 2010--------------
#Os dados daque em diante não estão no projeto do github, por questões de espaço de armazenamento
files<-list.files('/home/gandra/Documents/github/censoIBGE/input_data/IBGE_raw_data/setores',
                  pattern = ".shp",full.names = T)

m<-list()
i=2
for(i in 1:length(files)){
  print(files[i])
  df<-read_sf(files[i],options="ENCODING=Windows-1252")
  df<-st_transform(df,crs=4674)
  df<-df%>%transmute(cod_setor=CD_GEOCODI,tipo=TIPO,
                     cod_mun=CD_GEOCODM,nome_mun=NM_MUNICIP,
                     geometry=geometry)
  
  # plot(df[1])
  m[[i]] <- df
}
set_sf = as.data.frame(do.call(rbind, m))
set_sf = st_sf(set_sf)
set_sf<-set_sf%>%filter(cod_mun %in% coast_mun)

# plot(set_sf[1])

#Conversão para shape
write_sf(set_sf,"output_data/setores_costeiros.shp",delete_layer = T)

#Fazendo um índice dos codigos dos setores para eliminar das análise futuras
coast_setor<-set_sf$cod_setor

#Planilha Básico----------------
files<-list.files('/home/gandra/Documents/github/censoIBGE/input_data/IBGE_raw_data',pattern = "Basico",full.names = T)

muni<-list()
i=15
for(i in 1:length(files)){
  df<-read_xls(files[i])
  length(unique(df$Nome_do_municipio))
  df[is.na(df)] <- 0
  df<-df%>%filter(Cod_setor %in% coast_setor)%>%mutate(cod_setor=Cod_setor,
                                                       ndom=V001, mordom=V003, 
                                                       poptotal=V005,
                                                       cod_mun=Cod_municipio,
                                                       situacao=Situacao_setor,
                                                       nome=Nome_do_municipio)
  df<-df%>%select(cod_mun,nome,cod_setor, situacao,poptotal,ndom,mordom)
  muni[[i]] <- df
}
munibasico = as.data.frame(do.call(rbind, muni))
save.image()


#Planilha Domicilio 02--------------
files<-list.files('/home/gandra/Documents/github/censoIBGE/input_data/IBGE_raw_data',
                  pattern = "Domicilio02",full.names = T)

muni<-list()
for(i in 1:length(files)){
  df<-read_xls(files[i])
  df[is.na(df)] <- 0
  df<-df%>%filter(Cod_setor %in% coast_setor)%>%transmute(cod_setor=Cod_setor,domcolet=as.numeric(V001),
                                                          # poptotal=as.numeric(V002),
                                                       domprop=as.numeric(V006)+as.numeric(V007), 
                                                       domalug=as.numeric(V008),cagua=as.numeric(V012),
                                                       cbanhesg=as.numeric(V017),sbanh=as.numeric(V023),
                                                       ccoletalixo=as.numeric(V030),
                                                       cenergia=as.numeric(V039), senergia=as.numeric(V041),
                                                       homens=as.numeric(V046),mulheres=as.numeric(V090))
  
  muni[[i]] <- df
}
munidom2 = as.data.frame(do.call(rbind, muni))
save.image()

#Planilha Pessoa01--------------
files<-list.files('/home/gandra/Documents/github/censoIBGE/input_data/IBGE_raw_data',
                  pattern = "Pessoa01",full.names = T)
i=15
muni<-list()
for(i in 1:length(files)){
  
  df<-read_xls(files[i])
  df<-as.data.frame(lapply(df,as.numeric))
  df[is.na(df)] <- 0
  df<-df%>%filter(Cod_setor %in% coast_setor)
  df<-df%>%transmute(cod_setor=Cod_setor,
                     alfabet=V001)
  
  muni[[i]] <- df
}
munipessoa1 = as.data.frame(do.call(rbind, muni))
save.image()

#Planilha Pessoa13--------------
files<-list.files('/home/gandra/Documents/github/censoIBGE/input_data/IBGE_raw_data',
                  pattern = "Pessoa13",full.names = T)
i=15
muni<-list()
for(i in 1:length(files)){
  
  df<-read_xls(files[i])
  grep("V134", colnames(df))
  grep("V039", colnames(df))
  df<-as.data.frame(lapply(df,as.numeric))
  df[is.na(df)] <- 0
  df<-df%>%filter(Cod_setor %in% coast_setor)
  df<-df%>%transmute(cod_setor=Cod_setor,
                  criancas=V022+V035+V036+V037+V038+V039+V040+V041+V042+V043+V044,
                  idosos=rowSums(df[,c(96:136)]),
                  pmais5=rowSums(df[,c(41:136)]))
  
  muni[[i]] <- df
}
munipessoa13 = as.data.frame(do.call(rbind, muni))
save.image()

#Planilha DomicilioRenda--------------
files<-list.files('/home/gandra/Documents/github/censoIBGE/input_data/IBGE_raw_data',
                  pattern = "DomicilioRenda",full.names = T)

muni<-list()
for(i in 1:length(files)){
  df<-read_xls(files[i])
  df<-as.data.frame(lapply(df,as.numeric))
  df[is.na(df)] <- 0
  df<-df%>%filter(Cod_setor %in% coast_setor)
  df<-df%>%transmute(cod_setor=Cod_setor,
                  renda=V003,
                  srenda=V014,
                  abaixopob=V005)
  df<-df%>%select(cod_setor,renda,srenda)
  muni[[i]] <- df
}
munidomrenda = as.data.frame(do.call(rbind, muni))

save.image()

#Mesclando todas as planilhas------

setores<-merge(munibasico,munidomrenda,by='cod_setor')
setores<-merge(setores,munipessoa1,by='cod_setor')
setores<-merge(setores,munidom2,by='cod_setor')
setores<-merge(setores,munipessoa13,by='cod_setor')
setores_sf<-merge(set_sf,setores,by='cod_setor')

keep(setores,setores_sf,sure = T)
write.csv(setores,"output_data/setores_indicadores.csv")
write_sf(setores_sf,"output_data/setores_indicadores.shp")
save.image('descritores_IVSCost.RData')
