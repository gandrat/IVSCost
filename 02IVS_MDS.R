#Obtenção de Variáveis do Censo para o cálculo do Índice de Vulnerabilidade Social Costeiro
#Aplicação nos municípios costeiros do Brasil
#Menor unidade de análise: setor censitário

rm(list=ls()) ## Removendo as variáveis

#Carregando pacotes------------


#Rodar apenas na primeira vez
# install.packages("devtools")
# library("devtools")
# install_github("kassambara/factoextra")

##
packages<-c('ggplot2','readxl','dplyr','sf','corrplot','factoextra','reshape','smacof','gdata')
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

##Carregando os dados---------------
load('input_data/descritores_IVSCostV2.RData')

#V2: Selecionando  variáveis--------------
set<-setores%>%transmute(cod_setor=cod_setor,
                         cod_state=substring(cod_mun,1,2),
                         cod_region=substring(cod_mun,1,1),
                         cod_mun=cod_mun,
                         nome_mun=nome,
                         cd01=mordom,
                         cd02=domalug/poptotal,
                         # cd03=renda/poptotal,
                         # cd04=abaixopob/ndom,
                         # cd05=srenda/ndom,
                         cp01=mulheres/poptotal,
                         cp02=(criancas+idosos)/poptotal,
                         cp03=raca/poptotal,
                         cp04=(pmais5-alfabet)/poptotal,
                         cp05=rendavul/poptotal,
                         ci01=(poptotal-cagua)/poptotal,
                         ci02=(poptotal-cbanhesg)/poptotal,
                         ci03=(poptotal-ccoletalixo)/poptotal,
                         ci04=sempav/poptotal
)
unique(set$cod_region)

keep(set, sure=T)


# set<-setores%>%transmute(cod_setor=cod_setor,cod_mun=cod_mun,nome_mun=nome,cd01=mordom,cd02=domalug/poptotal,cd03=renda,cd04=abaixopob/ndom,cd05=srenda/ndom,cp01=mulheres/poptotal,cp02=(criancas+idosos)/poptotal,cp03=raca/poptotal,cp04=pmais5-alfabet,ci01=(poptotal-cagua)/poptotal,ci02=(poptotal-cbanhesg)/poptotal,ci03=(poptotal-ccoletalixo)/poptotal,ci04=senergia/poptotal)

#Removendo setores com percentuais maiores que 1
set<-set%>%filter(cd02<=1,  cp01<=1,cp02<=1,cp03<=1, ci01<=1,
                       ci02<=1,ci03<=1)
set<-set[complete.cases(set),] #mantem apenas os registros que possuem valores válidos em TODAS as variáveis

#selecionando um numero randômico de observações para possibilitar o MDS
set<-set%>%group_by(cod_region)%>%sample_n(1000)

#MDS: Domicilios----------------

mdsd<-set[complete.cases(set),c('cd01','cd02')]
set_scale<-scale(mdsd)
d <- dist(set_scale) # euclidean distances between the rows

fitd<-mds(d,type='ratio')
fitd
jpeg('figures/mds_domicilios.jpg',width=15,height = 10,units='cm',res=300)
plot(fitd, plot.type = "Shepard",
     main = "Shepard Diagram (ratio MDS)")
dev.off()

#MDS: Pessoais----------------
mdsp<-set[complete.cases(set),c('cp01','cp02','cp03','cp04','cp05')]
set_scale<-scale(mdsp)
d <- dist(set_scale) # euclidean distances between the rows

fitp<-mds(d,type='ratio')
fitp
jpeg('figures/mds_pessoas.jpg',width=15,height = 10,units='cm',res=300)
plot(fitp, plot.type = "Shepard",
     main = "Shepard Diagram (ratio MDS)")
dev.off()

#MDS: Infraestrutura----------------
rm(d)
mdsi<-set[complete.cases(set),c('ci01','ci02','ci03','ci04')]

set_scale<-scale(mdsi)
d <- dist(set_scale) # euclidean distances between the rows

fiti<-mds(d,type='ratio')
fiti
jpeg('figures/mds_infra.jpg',width=15,height = 10,units='cm',res=300)
plot(fiti, plot.type = "Shepard",
     main = "Shepard Diagram (ratio MDS)")
dev.off()
rm(d)
save.image('output_data/mds.RData')