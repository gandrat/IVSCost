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
packages<-c('ggplot2','readxl','dplyr','sf','corrplot','factoextra','reshape')
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

##Carregando os dados---------------
load('input_data/descritores_IVSCost.RData')


#Erro (correção temporária)
setores$poptotal=setores$domcolet


#Selecionando e renomeando variáveis--------------
set<-setores%>%transmute(cd01=mordom,
                         cd02=domalug/poptotal,
                         cd03=renda,
                         cd04=abaixopob/ndom,
                         cd05=srenda/ndom,
                         cp01=mulheres/poptotal,
                         cp02=(criancas+idosos)/poptotal,
                         cp03=raca/poptotal,
                         cp04=pmais5-alfabet,
                         ci01=(poptotal-cagua)/poptotal,
                         ci02=(poptotal-cbanhesg)/poptotal,
                         ci03=(poptotal-ccoletalixo)/poptotal,
                         ci04=senergia/poptotal
)

#Histogramas
ggplot(set,aes(x=cd01))+geom_histogram()
set.m<-melt(set)

ggplot(set.m,aes(x=value))+geom_histogram()+
  facet_wrap(~variable,scales='free')

#Correlograma-------------------
setcor=set[complete.cases(set),] #mantem apenas os registros que possuem valores válidos em TODAS as variáveis
M<-cor(setcor,method='spearman')

jpeg('figures/correlograma_varbrutas.jpg',width=25,height = 25,units='cm',res=300)
corrplot(M,type='upper',method = 'number')
dev.off()

#PCA-----------
set<-set[complete.cases(set),]
pc<-prcomp(set,scale = TRUE)
plot(pc)

# ggbiplot(pc, choices=1:2, obs.scale = 1, var.scale = 1,
#          groups = set$regiao,ellipse = T,varname.size = 4,varname.adjust = 2, alpha=.1) +
#   scale_color_brewer(palette = rev("Set1"), name='') +
#   ylim(c(-6,6))+
#   xlim(c(-8,8))+
#   
#   theme(legend.direction = 'horizontal', legend.position = 'top')+
#   xlab('PC1 (53%)')+
#   ylab('PC2 (10%)')
# ggsave("output/artigo/soc_pca_biplot.jpg", dpi=200, units='cm', width=16, height=10)


fviz_screeplot(pc, choice='eigenvalue', geom='line')+
  ylab('Variância')+xlab('PCs')+ggtitle(NULL)+
  geom_hline(yintercept=1, linetype='dashed')
ggsave('figures/pca_scree_varindex.jpg', width=15, heigh=8, units='cm',dpi=150)


#Roda PCA
fviz_pca_var(pc,axes = c(1,2),
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
             title='')+
  theme(text=element_text(family='Times',size=10),
        legend.position = 'none')
ggsave("figures/pca_wheel_varindex.jpg", dpi=500, units='cm', width=14, height=14)

#PCA: Características Domiciliares-----------------
setd<-set[complete.cases(set),1:4]
pc<-prcomp(setd,scale = TRUE)


# ggbiplot(pc, choices=1:2, obs.scale = 1, var.scale = 1,
#          groups = set$regiao,ellipse = T,varname.size = 4,varname.adjust = 2, alpha=.1) +
#   scale_color_brewer(palette = rev("Set1"), name='') +
#   ylim(c(-6,6))+
#   xlim(c(-8,8))+
#   
#   theme(legend.direction = 'horizontal', legend.position = 'top')+
#   xlab('PC1 (53%)')+
#   ylab('PC2 (10%)')
# ggsave("output/artigo/soc_pca_biplot.jpg", dpi=200, units='cm', width=16, height=10)


fviz_screeplot(pc, choice='eigenvalue', geom='line')+
  ylab('Variância')+xlab('PCs')+ggtitle(NULL)+
  geom_hline(yintercept=1, linetype='dashed')
ggsave('figures/pca_scree_varindex.jpg', width=15, heigh=8, units='cm',dpi=150)


#Roda PCA
fviz_pca_var(pc,axes = c(1,2),
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
             title='')+
  theme(text=element_text(family='Times',size=10),
        legend.position = 'none')
ggsave("figures/cd_pca_wheel_varindex.jpg", dpi=500, units='cm', width=14, height=14)
