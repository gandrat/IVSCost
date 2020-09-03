#Obtenção de Variáveis do Censo para o cálculo do Índice de Vulnerabilidade Social Costeiro
#Aplicação nos municípios costeiros do Brasil
#Menor unidade de análise: setor censitário


#Carregando pacotes------------
rm(list=ls()) ## Removendo as variáveis

#Rodar apenas na primeira vez
# install.packages("devtools")
# library("devtools")
# install_github("kassambara/factoextra")

##
packages<-c('ggplot2','readxl','dplyr','sf','corrplot','factoextra')
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

##Carregando os dados---------------
load('input_data/descritores_IVSCost.RData')

#Selecionando e renomeando variáveis
set<-setores%>%select(ndom,mordom,domcolet,poptotal,
                       domalug,mulheres,raca,criancas,
                      idosos,pmais5,alfabet,renda,abaixopob,
                      srenda,cagua,cbanhesg,ccoletalixo,
                      senergia)

#Correlograma-------------------
setcor=set[complete.cases(set),]
M<-cor(setcor,method='spearman')

jpeg('figures/correlograma_varbrutas.jpg',width=25,height = 25,units='cm',res=300)
corrplot(M,type='upper',method = 'color')
dev.off()

#PCA-----------
set<-set[complete.cases(set),]
pc<-prcomp(set,scale = TRUE)
plot(pc, type='lines')

ggbiplot(pc, choices=1:2, obs.scale = 1, var.scale = 1,
         groups = set$regiao,ellipse = T,varname.size = 4,varname.adjust = 2, alpha=.1) +
  scale_color_brewer(palette = rev("Set1"), name='') +
  ylim(c(-6,6))+
  xlim(c(-8,8))+
  
  theme(legend.direction = 'horizontal', legend.position = 'top')+
  xlab('PC1 (53%)')+
  ylab('PC2 (10%)')
# ggsave("output/artigo/soc_pca_biplot.jpg", dpi=200, units='cm', width=16, height=10)


fviz_screeplot(pc, choice='eigenvalue', geom='line')+
  ylab('Variância')+xlab('PCs')+ggtitle(NULL)+
  geom_hline(yintercept=1, linetype='dashed')
ggsave('output/artigo/scree_pca.jpg', width=15, heigh=8, units='cm',dpi=150)


#Roda PCA
fviz_pca_var(pc,axes = c(1,2),
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
             title='')+
  theme(text=element_text(family='Times',size=10),
        legend.position = 'none')+
  xlab('PC1 (70%)')+ylab('PC2 (7.9%)')
ggsave("output/artigo/soc_pca_variaveis_v2.jpg", dpi=500, units='cm', width=14, height=14)
