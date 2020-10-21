#Obtenção de Variáveis do Censo para o cálculo do Índice de Vulnerabilidade Social Costeiro
#Aplicação nos municípios costeiros do Brasil
#Menor unidade de análise: setor censitário

rm(list=ls()) ## Removendo as variáveis

#Carregando pacotes------------

packages<-c('dplyr','smacof')
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

##Carregando os dados---------------
load('input_data/descritores_IVSCostV2.RData')

#Selecionando  variáveis--------------
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

#Removendo setores com percentuais maiores que 1
set<-set%>%filter(cd02<=1,  cp01<=1,cp02<=1,cp03<=1, ci01<=1,
                       ci02<=1,ci03<=1)
set<-set[complete.cases(set),] #mantem apenas os registros que possuem valores válidos em TODAS as variáveis


#selecionando um numero randômico de observações para possibilitar o MDS
# set<-set%>%group_by(cod_region)%>%sample_n(1000)

#MDS: Condições Pessoais e de Domicílios----------------
mdsp<-set[complete.cases(set),c('cp01','cp02','cp03','cp04','cp05','cd01','cd02')]
set_scale<-scale(mdsp)

#Modo R
dr<-dist(t(set_scale))
fitpr<-mds(dr,type='ratio')

fitpr$delta
fitpr$stress
summary(fitpr)
fitpr

jpeg('figures/mds_dim_pessoas_modoR.jpg',width=15,height = 10,units='cm',res=300)
plot(fitpr, plot.dim = c(1,2), main = "MDS: Condições Pessoais e de Domicílios")
dev.off()

jpeg('figures/mds_shep_pessoas_modoR.jpg',width=15,height = 10,units='cm',res=300)
plot(fitpr, plot.type = "Shepard",
     main = "Condições Pessoais e de Domicílios (7 variáveis)")
dev.off()


#MDS: Infraestrutura----------------
mdsi<-set[complete.cases(set),c('ci01','ci02','ci03','ci04')]

set_scale<-scale(mdsi)

#Modo R
dr<-dist(t(set_scale))
fitir<-mds(dr,type='ratio')

fitir$delta
fitir$stress
summary(fitir)
fitir
jpeg('figures/mds_infra_modoR.jpg',width=15,height = 10,units='cm',res=300)
plot(fitir, plot.dim = c(1,2), main = "MDS: Condições de Infraestrutura")
dev.off()

jpeg('figures/mds_shep_infra_modoR.jpg',width=15,height = 10,units='cm',res=300)
plot(fitir, plot.type = "Shepard",
     main = "Condições de Infraestrutura (4 variáveis)")
dev.off()

#Saving data in workspace---------
save.image('output_data/mdsV2.RData')
