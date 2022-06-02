#Obtenção de Variáveis do Censo para o cálculo do Índice de Vulnerabilidade Social Costeiro
#Aplicação nos municípios costeiros do Brasil
#Menor unidade de análise: setor censitário
#Dados e shapes da divisão política de 2010


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


#Shape Setores--------------
#Os dados daque em diante não estão no projeto do github, por questões de espaço de armazenamento
files<-list.files('/home/gandra/github/censoIBGE/input_data/IBGE_raw_data/setores',
                  pattern = ".shp",full.names = T)

m<-list()
i=2
for(i in 1:length(files)){
  print(files[i])
  df<-read_sf(files[i],options="ENCODING=Windows-1252")
  df<-st_transform(df,crs=4674)
  # substring(df$CD_GEOCODM,1,2)
  df<-df%>%transmute(cod_setor=as.numeric(CD_GEOCODI),
                     cod_uf=as.numeric(substring(df$CD_GEOCODI,1,2)),
                     tipo=TIPO,
                     nm_meso=NM_MESO,
                     nm_micro=NM_MICRO,
                     cod_mun=CD_GEOCODM,
                     nm_mun=NM_MUNICIP,
                     geometry=geometry)
  
  # plot(df[5])
  m[[i]] <- df
}

set_sf = as.data.frame(do.call(rbind, m))
set_sf = st_sf(set_sf)
#Filtrando municipios costeiros
set_sf<-set_sf%>%filter(cod_mun %in% coast_mun)

# plot(set_sf[1])

#Conversão para shape
write_sf(set_sf,"output_data/setores_costeiros.shp",delete_layer = T)

#Fazendo um índice dos codigos dos setores para eliminar das análise futuras
coast_setor<-set_sf$cod_setor

#Shape Mesoregiões-----------
files<-list.files('/home/gandra/github/censoIBGE/input_data/mesoregioes_shp',
                  pattern = ".shp",full.names = T, recursive = T)

m<-list()
i=1
for(i in 1:length(files)){
  print(files[i])
  df<-read_sf(files[i],options="ENCODING=Windows-1252")
  df<-st_transform(df,crs=4674)
  # substring(df$CD_GEOCODM,1,2)
  # plot(df)
  df<-df%>%transmute(nm_meso=NM_MESO,
                     cod_uf=as.numeric(df$CD_GEOCODU),
                     geometry=geometry)
  
  # plot(df[5])
  m[[i]] <- df
}

meso_sf = as.data.frame(do.call(rbind, m))
meso_sf = st_sf(meso_sf)

meso_sf<-meso_sf%>%filter(nm_meso %in% set_sf$nm_meso)
plot(meso_sf)


#Shape Estados-----------
files<-list.files('/home/gandra/github/censoIBGE/input_data/estados_shp',
                  pattern = ".shp",full.names = T, recursive = T)

m<-list()
i=1
for(i in 1:length(files)){
  print(files[i])
  df<-read_sf(files[i],options="ENCODING=Windows-1252")
  df<-st_transform(df,crs=4674)
  # substring(df$CD_GEOCODM,1,2)
  plot(df)
  df<-df%>%transmute(cod_uf=as.numeric(df$CD_GEOCODU),
                     nm_uf=NM_ESTADO,
                     nm_regiao=NM_REGIAO,
                     geometry=geometry)
  
  # plot(df[5])
  m[[i]] <- df
}

uf_sf = as.data.frame(do.call(rbind, m))
uf_sf = st_sf(uf_sf)

uf_sf<-uf_sf%>%filter(cod_uf %in% set_sf$cod_uf)
plot(uf_sf)


#Shape Micro-----------
files<-list.files('/home/gandra/github/censoIBGE/input_data/microregioes_shp',
                  pattern = ".shp",full.names = T, recursive = T)

m<-list()
i=1
for(i in 1:length(files)){
  print(files[i])
  df<-read_sf(files[i],options="ENCODING=Windows-1252")
  df<-st_transform(df,crs=4674)
  # substring(df$CD_GEOCODM,1,2)
  # plot(df)
  df<-df%>%transmute(cod_uf=as.numeric(df$CD_GEOCODU),
                     nm_micro=NM_MICRO,
                     geometry=geometry)
  
  # plot(df[5])
  m[[i]] <- df
}

micro_sf = as.data.frame(do.call(rbind, m))
micro_sf = st_sf(micro_sf)

micro_sf<-micro_sf%>%filter(nm_micro %in% set_sf$nm_micro, cod_uf %in% set_sf$cod_uf)
plot(micro_sf)

#Shape Municipios-----------
files<-list.files('/home/gandra/github/censoIBGE/input_data/municipios_shp',
                  pattern = ".shp",full.names = T, recursive = T)

m<-list()
i=1
for(i in 1:length(files)){
  print(files[i])
  df<-read_sf(files[i],options="ENCODING=Windows-1252")
  df<-st_transform(df,crs=4674)
  # substring(df$CD_GEOCODM,1,2)
  df<-df%>%transmute(cod_mun=as.numeric(df$CD_GEOCODM),
                     nm_mun=NM_MUNICIP,
                     geometry=geometry)
  m[[i]] <- df
}

muni_sf = as.data.frame(do.call(rbind, m))
muni_sf = st_sf(muni_sf)

muni_sf<-muni_sf%>%filter(cod_mun %in% set_sf$cod_mun)
plot(muni_sf)

save(set_sf,meso_sf,uf_sf,micro_sf,muni_sf,file = 'output_data/malha_territorial_sf.Rda')
############################################################
#Básico----------------
files<-list.files('/home/gandra/github/censoIBGE/input_data/IBGE_raw_data',
                  pattern = "Basico",full.names = T)

muni<-list()
i=15
for(i in 1:length(files)){
  df<-read_xls(files[i])
  length(unique(df$Nome_do_municipio))
  df[is.na(df)] <- 0
  df<-df%>%filter(Cod_setor %in% coast_setor)%>%transmute(cod_setor=Cod_setor,
                                                       ndom=V001, 
                                                       mordom=V003, 
                                                       poptotal=V002,
                                                       cod_mun=Cod_municipio,
                                                       situacao=Situacao_setor,
                                                       nome=Nome_do_municipio)
  
  muni[[i]] <- df
}
munibasico = as.data.frame(do.call(rbind, muni))

save.image()


#Domicilio 02--------------
files<-list.files('/home/gandra/github/censoIBGE/input_data/IBGE_raw_data',
                  pattern = "Domicilio02",full.names = T)

muni<-list()
for(i in 1:length(files)){
  df<-read_xls(files[i])
  df[is.na(df)] <- 0
  df<-df%>%filter(Cod_setor %in% coast_setor)%>%transmute(cod_setor=Cod_setor,
                                                          poptotaldom=as.numeric(V001),#este seria a população?
                                                          domprop=as.numeric(V006)+as.numeric(V007),
                                                          domalug=as.numeric(V008),
                                                          cagua=as.numeric(V012),
                                                          cbanhesg=as.numeric(V017),
                                                          sbanh=as.numeric(V023),
                                                          ccoletalixo=as.numeric(V030),
                                                          cenergia=as.numeric(V039),
                                                          senergia=as.numeric(V041),
                                                          homens=as.numeric(V046),
                                                          mulheres=as.numeric(V090))
  
  muni[[i]] <- df
}
munidom2 = as.data.frame(do.call(rbind, muni))
save.image()

#Pessoa01--------------
files<-list.files('/home/gandra/github/censoIBGE/input_data/IBGE_raw_data',
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

#Pessoa03--------------
files<-list.files('/home/gandra/github/censoIBGE/input_data/IBGE_raw_data',
                  pattern = "Pessoa03",full.names = T)
i=15
muni<-list()
for(i in 1:length(files)){
  df<-read_xls(files[i])
  df<-as.data.frame(lapply(df,as.numeric))
  df[is.na(df)] <- 0
  df<-df%>%filter(Cod_setor %in% coast_setor)
  df<-df%>%transmute(cod_setor=Cod_setor,
                     raca=V003+V004+V005+V006)
  
  muni[[i]] <- df
}
munipessoa03 = as.data.frame(do.call(rbind, muni))
save.image()


#Pessoa13--------------
files<-list.files('/home/gandra/github/censoIBGE/input_data/IBGE_raw_data',
                  pattern = "Pessoa13",full.names = T)
i=15
muni<-list()
for(i in 1:length(files)){
  
  df<-read_xls(files[i])
  grep("V094", colnames(df))
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

#DomicilioRenda--------------
files<-list.files('/home/gandra/github/censoIBGE/input_data/IBGE_raw_data',
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
  muni[[i]] <- df
}
munidomrenda = as.data.frame(do.call(rbind, muni))

save.image()

#PessoaRenda--------------
files<-list.files('/home/gandra/github/censoIBGE/input_data/IBGE_raw_data',
                  pattern = "PessoaRenda",full.names = T)

muni<-list()
for(i in 1:length(files)){
  df<-read_xls(files[i])
  df<-as.data.frame(lapply(df,as.numeric))
  df[is.na(df)] <- 0
  df<-df%>%filter(Cod_setor %in% coast_setor)
  df<-df%>%transmute(cod_setor=Cod_setor,
                     rendavul=V001)
  muni[[i]] <- df
}
munipessoarenda = as.data.frame(do.call(rbind, muni))

save.image()

#Entorno--------------
files<-list.files('/home/gandra/github/censoIBGE/input_data/IBGE_raw_data',
                  pattern = "Entorno03",full.names = T)

muni<-list()
for(i in 1:length(files)){
  df<-read_xls(files[i])
  df<-as.data.frame(lapply(df,as.numeric))
  df[is.na(df)] <- 0
  df<-df%>%filter(Cod_setor %in% coast_setor)
  df<-df%>%transmute(cod_setor=Cod_setor,
                     sempav=V436+V438+V440)
  muni[[i]] <- df
}
munientorno = as.data.frame(do.call(rbind, muni))

save.image()

#Mesclando todas as planilhas------
load('.RData')
setores<-merge(munibasico,munidomrenda,by='cod_setor')
setores<-merge(setores,munipessoa1,by='cod_setor')
setores<-merge(setores,munipessoa03,by='cod_setor')
setores<-merge(setores,munidom2,by='cod_setor')
setores<-merge(setores,munipessoa13,by='cod_setor')
setores<-merge(setores,munipessoarenda,by='cod_setor')
setores<-merge(setores,munientorno,by='cod_setor')
setores_sf<-merge(set_sf,setores,by=c('cod_setor','cod_mun'))

save.image()
keep(setores,setores_sf,sure = T)
write.csv(setores,"output_data/setores_indicadoresV2.csv")
write_sf(setores_sf,"output_data/setores_indicadoresV2.shp")
save.image('input_data/descritores_IVSCostV2.RData')


