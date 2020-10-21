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
summary(setores)

summary(setores$renda/setores$poptotal)

summary(setores$poptotal-setores$poptotaldom)

# #Selecionando variáveis--------------
# set<-setores%>%transmute(cod_setor=cod_setor,
#                          cod_mun=cod_mun,
#                          nome_mun=nome,
#                          cd01=mordom,
#                          cd02=domalug/poptotal,
#                          cd03=renda/poptotal,
#                          cd04=abaixopob/ndom,
#                          cd05=srenda/ndom,
#                          cp01=mulheres/poptotal,
#                          cp02=(criancas+idosos)/poptotal,
#                          cp03=raca/poptotal,
#                          cp04=(pmais5-alfabet)/poptotal,
#                          ci01=(poptotal-cagua)/poptotal,
#                          ci02=(poptotal-cbanhesg)/poptotal,
#                          ci03=(poptotal-ccoletalixo)/poptotal
# )


#V2: Selecionando  variáveis--------------
set<-setores%>%transmute(cod_setor=cod_setor,
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

# set<-setores%>%transmute(cod_setor=cod_setor,cod_mun=cod_mun,nome_mun=nome,cd01=mordom,cd02=domalug/poptotal,cd03=renda,cd04=abaixopob/ndom,cd05=srenda/ndom,cp01=mulheres/poptotal,cp02=(criancas+idosos)/poptotal,cp03=raca/poptotal,cp04=pmais5-alfabet,ci01=(poptotal-cagua)/poptotal,ci02=(poptotal-cbanhesg)/poptotal,ci03=(poptotal-ccoletalixo)/poptotal,ci04=senergia/poptotal)

write.csv(set,'output_data/setores_pca_ivscost_v2.csv')

#Removendo setores com percentuais maiores que 1
summary(set)
set<-set%>%filter(cd02<=1,  cp01<=1,cp02<=1,cp03<=1, ci01<=1,
                       ci02<=1,ci03<=1)
set<-set[complete.cases(set),4:14] #mantem apenas os registros que possuem valores válidos em TODAS as variáveis
summary(set)
nrow(setores)-nrow(set)

#Histogramas

set.m<-melt(set)

ggplot(set.m,aes(x=value))+geom_histogram()+
  facet_wrap(~variable,scales='free')


#Correlograma-------------------

M<-cor(set,method='spearman')

jpeg('figures/correlograma_varbrutasV2.jpg',width=25,height = 25,units='cm',res=300)
corrplot(M,type='upper',method = 'number')
dev.off()

#PCA: Geral-----------
pc<-prcomp(set,scale = TRUE)

fviz_screeplot(pc, choice='eigenvalue', geom='line')+
  ylab('Variância')+xlab('PCs')+ggtitle(NULL)+
  geom_hline(yintercept=1, linetype='dashed')
ggsave('figures/pca_screeV2.jpg', width=15, heigh=8, units='cm',dpi=150)

#Roda PCA
fviz_pca_var(pc,axes = c(1,2),
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
             title='')+
  theme(text=element_text(family='Times',size=10),
        legend.position = 'none')
ggsave("figures/pca_wheelV2.jpg", dpi=500, units='cm', width=14, height=14)

#PCA: Domiciliares-----------------
setd<-set[c('cd01','cd02')]
pc<-prcomp(setd,scale = TRUE)


fviz_screeplot(pc, choice='eigenvalue', geom='line')+
  ylab('Variância')+xlab('PCs')+ggtitle(NULL)+
  geom_hline(yintercept=1, linetype='dashed')
ggsave('figures/cd_pca_screeV2.jpg', width=15, heigh=8, units='cm',dpi=150)


#Roda PCA
fviz_pca_var(pc,axes = c(1,2),
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
             title='')+
  theme(text=element_text(family='Times',size=10),
        legend.position = 'none')
ggsave("figures/cd_pca_wheelV2.jpg", dpi=500, units='cm', width=14, height=14)

#PCA: Pessoais-----------------
setp<-set[c('cp01','cp02','cp03','cp04','cp05')]
pc<-prcomp(setp,scale = TRUE)


fviz_screeplot(pc, choice='eigenvalue', geom='line')+
  ylab('Variância')+xlab('PCs')+ggtitle(NULL)+
  geom_hline(yintercept=1, linetype='dashed')
ggsave('figures/cp_pca_screeV2.jpg', width=15, heigh=8, units='cm',dpi=150)


#Roda PCA
fviz_pca_var(pc,axes = c(1,2),
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
             title='')+
  theme(text=element_text(family='Times',size=10),
        legend.position = 'none')
ggsave("figures/cp_pca_wheelV2.jpg", dpi=500, units='cm', width=14, height=14)

#PCA: Infraestrutura-----------------
seti<-set[c('ci01','ci02','ci03','ci04')]
pc<-prcomp(seti,scale = TRUE)


fviz_screeplot(pc, choice='eigenvalue', geom='line')+
  ylab('Variância')+xlab('PCs')+ggtitle(NULL)+
  geom_hline(yintercept=1, linetype='dashed')
ggsave('figures/ci_pca_screeV2.jpg', width=15, heigh=8, units='cm',dpi=150)


#Roda PCA
fviz_pca_var(pc,axes = c(1,2),
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
             title='')+
  theme(text=element_text(family='Times',size=10),
        legend.position = 'none')
ggsave("figures/ci_pca_wheelV2.jpg", dpi=500, units='cm', width=14, height=14)