#Obtenção de Variáveis do Censo para o cálculo do Índice de Vulnerabilidade Social Costeiro
#Aplicação nos municípios costeiros do Brasil
#Menor unidade de análise: setor censitário

rm(list=ls()) ## Removendo as variáveis

#Carregando pacotes------------


#Rodar apenas na primeira vez
# install.packages("devtools")
# library("devtools")
# install_github("kassambara/factoextra")

# library(devtools)
# install_github("vqv/ggbiplot")

# Conferindo instalação de pacotes uma a um
# library (ggplot2)
# install.packages ("ggplot2")

##
packages<-c('ggplot2','dplyr','corrplot','factoextra','reshape','smacof','gdata', 'ggbiplot')
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})


rm(list=ls()) ## Limpando o workspace


theme_set(
  theme_bw(base_size = 10)+
    theme(text=element_text(family="Times"))
)

##Carregando os dados---------------
load('input_data/descritores_IVSCostV2.RData')

rm(setores_sf)

#V5: Selecionando  variáveis--------------
set<-setores%>%transmute(cod_setor=cod_setor,
                         cod_mun=cod_mun,
                         nome_mun=nome,
                         cod_state=substring(as.character(cod_setor),1,2),
                         cod_regiao=substring(as.character(cod_setor),1,1),
                         cd01=mordom,
                         cd02=1-(domalug/poptotal),
                         # cd03=renda/poptotal,
                         # cd04=abaixopob/ndom,
                         # cd05=srenda/ndom,
                         # cp01=mulheres/poptotal,
                         cp01=rendavul/poptotal,
                         cp02=(criancas+idosos)/poptotal,
                         cp03=raca/poptotal,
                         cp04=(pmais5-alfabet)/poptotal,
                         ci01=(poptotal-cagua)/poptotal,
                         ci02=(poptotal-cbanhesg)/poptotal,
                         ci03=(poptotal-ccoletalixo)/poptotal,
                         ci04=sempav/poptotal
)

set<-set%>%mutate(regiao=case_when(cod_regiao==1 ~ 'N',
                                   cod_regiao==2 ~ 'NE',
                                   cod_regiao==3 ~ 'SE',
                                   cod_regiao==4 ~ 'S')) 
set<-set%>%mutate(cd01=(cd01-min(cd01))/(max(cd01)-min(cd01)))



#Removendo setores com percentuais maiores que 1
summary(set)
set<-set%>%filter(cd02<=1,cp02<=1,cp03<=1, ci01<=1,
                       ci02<=1,ci03<=1)
set<-set[complete.cases(set),] #mantem apenas os registros que possuem valores válidos em TODAS as variáveis

seti<-set%>%select(-cod_mun, -cod_state, -nome_mun,  -cod_regiao, -regiao)


#Histogramas---------------
set.m<-melt(seti,id='cod_setor')
ggplot(set.m,aes(x=value))+geom_histogram()+
  facet_wrap(~variable,scales='free',ncol = 3)+
  theme_bw()+xlab(NULL)+ylab(NULL)
ggsave('figures/histogramas_v5.jpg',dpi=200, units='cm', width=15, height=14)

#Correlograma-------------------
seti<-seti%>%select(-regiao)

M<-cor(seti,method='pearson')

jpeg('figures/correlograma_varbrutas_v5.jpg',width=15,height = 15,units='cm',res=300)
corrplot(M,type='upper',method = 'number')
dev.off()

#PCA: Geral-----------
pc<-prcomp(seti[-1],scale = TRUE)

summary(pc)

#Obtendo indicadores do PCA
pca<-get_pca(pc, element = c("var", "ind"))
pc_contrib<-as.data.frame(pca$contrib)
pc_cor<-as.data.frame(pca$cor)
pcgeral<-as.data.frame(pc$x)

#Screeplot
fviz_screeplot(pc, choice='eigenvalue', geom='line')+
  ylab('Variância')+xlab('PCs')+ggtitle(NULL)+
  geom_hline(yintercept=1, linetype='dashed')
ggsave('figures/pca_screeV5.jpg', width=15, heigh=8, units='cm',dpi=150)

#Roda PCA
fviz_pca_var(pc,axes = c(1,2),
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
             title='')+
  theme(text=element_text(family='Times',size=10),
        legend.position = 'none')
ggsave("figures/pca_wheelV5.jpg", dpi=500, units='cm', width=14, height=14)

#Biplot
ggbiplot(pc, choices=c(1,2), obs.scale = 1, var.scale = 1,
         groups = set$regiao,ellipse = T,varname.size = 4,
         varname.adjust = 2, alpha=.05) +
  scale_color_brewer(palette = rev("YlOrBr"), name='') +
  ylim(c(-6,6))+
  xlim(c(-7,7))+
  # theme(legend.direction = 'horizontal', legend.position = 'top')+
  theme_bw()

ggsave("figures/pca_biplot_v5.jpg",dpi=300, units = 'cm', width = 14, height = 14)

#Calculando o IVS: Condições pessoais


#PCA: Pessoais cp +cd -----------------
setp<-seti[c('cp02','cp03','cp04','cp01','cd01','cd02')]
pc<-prcomp(setp,scale = TRUE)

summary(pc)

#Obtendo indicadores do PCA
pca<-get_pca(pc, element = c("var", "ind"))
pc_contrib<-as.data.frame(pca$contrib)
pc_cor<-as.data.frame(pca$cor)
pcpessoais<-as.data.frame(pc$x)

#Screeplot
fviz_screeplot(pc, choice='eigenvalue', geom='line')+
  ylab('Variância')+xlab('PCs')+ggtitle(NULL)+
  geom_hline(yintercept=1, linetype='dashed')
ggsave('figures/cpd_pca_screeV5.jpg', width=15, heigh=8, units='cm',dpi=150)


#Roda PCA
fviz_pca_var(pc,axes = c(1,2),
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
             title='')+
  theme(text=element_text(family='Times',size=10),
        legend.position = 'none')
ggsave("figures/cpd_pca_wheelV5.jpg", dpi=500, units='cm', width=14, height=14)
summary(pc)

#Biplot
ggbiplot(pc,choices=c(1,2),
         groups = set$regiao,ellipse = T, ellipse.prob=.7,
         varname.size = 4, alpha=0,
         sec.axis='v') +
  scale_color_brewer(palette = 7, name='') +
  ylim(c(-3,3))+
  xlim(c(-3,3))+
  # theme(legend.direction = 'horizontal', legend.position = 'top')+
  theme_bw()

ggsave("figures/cpd_biplot_v5.jpg",dpi=300, units = 'cm', width = 14, height = 14)

#Facet por estados
ggbiplot(pc,choices=c(1,2),
         ellipse = T,groups = set$cod_state,
         varname.size = 4, alpha=0,
         sec.axis='v') +
  # scale_color_brewer(palette = 7, name='') +
  # facet_wrap(~set$cod_state)+
  ylim(c(-3,3))+
  xlim(c(-3,3))+
  # theme(legend.direction = 'horizontal', legend.position = 'top')+
  theme_bw()

#PCA: Infraestrutura-----------------
setin<-seti[c('ci01','ci02','ci03','ci04')]
pc<-prcomp(setin,scale = TRUE)



#Obtendo indicadores do PCA
pca<-get_pca(pc, element = c("var", "ind"))
pc_contrib<-as.data.frame(pca$contrib)
pc_cor<-as.data.frame(pca$cor)
pcinfra<-as.data.frame(pc$x)


fviz_screeplot(pc, choice='eigenvalue', geom='line')+
  ylab('Variância')+xlab('PCs')+ggtitle(NULL)+
  geom_hline(yintercept=1, linetype='dashed')
ggsave('figures/ci_pca_screeV5.jpg', width=15, heigh=8, units='cm',dpi=150)



#Roda PCA
fviz_pca_var(pc,axes = c(1,2),
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
             title='')+
  theme(text=element_text(family='Times',size=10),
        legend.position = 'none')
ggsave("figures/ci_pca_wheelV5.jpg", dpi=500, units='cm', width=14, height=14)

#Biplot
ggbiplot(pc,choices=c(1,2),
         groups = set$regiao,ellipse = T, ellipse.prob=.7,
         varname.size = 4, alpha=0,
         sec.axis='v') +
  scale_color_brewer(palette = 7, name='') +
  ylim(c(-3,3))+
  xlim(c(-3,3))+
  # theme(legend.direction = 'horizontal', legend.position = 'top')+
  theme_bw()


ggsave("figures/ci_biplot_V5.jpg",dpi=300, units = 'cm', width = 14, height = 14)


#Merge entre PCAs e setores-------------
pc_ci<-pcinfra%>%transmute(ci_pc1=PC1,
                             ci_pc2=PC2,
                             ci_pc3=PC3)
pc_cp<-pcpessoais%>%transmute(cp_pc1=PC1,
                           cp_pc2=PC2,
                           cp_pc3=PC3)
pc_g<-pcgeral%>%transmute(all_pc1=PC1,
                              all_pc2=PC2,
                              all_pc3=PC3)

seti<-cbind(seti,pc_cp,pc_ci,pc_g)

#Cálculo do IVS Opção 1-----------------------

seti<-seti%>%mutate(ivs1=all_pc1)#PC1 de todas as variáveis em conjunto

#R2 para IVS1----------
summary(lm(cd01 ~ ivs1, data=seti))
summary(lm(cd02 ~ ivs1, data=seti))
summary(lm(cp01 ~ ivs1, data=seti))
summary(lm(cp02 ~ ivs1, data=seti))
summary(lm(cp03 ~ ivs1, data=seti))
summary(lm(cp04 ~ ivs1, data=seti))
summary(lm(ci01 ~ ivs1, data=seti))
summary(lm(ci02 ~ ivs1, data=seti))
summary(lm(ci03 ~ ivs1, data=seti))
summary(lm(ci04 ~ ivs1, data=seti))


#Cálculo do IVS Opção 2-----------------------
seti<-seti%>%mutate(ivs2=(cp_pc1+ci_pc1)/2)#Média da somas da PC1cpd e PC1ci

#R2 para IVS2----------
summary(lm(cd01 ~ ivs2, data=seti))
summary(lm(cd02 ~ ivs2, data=seti))
summary(lm(cp01 ~ ivs2, data=seti))
summary(lm(cp02 ~ ivs2, data=seti))
summary(lm(cp03 ~ ivs2, data=seti))
summary(lm(cp04 ~ ivs2, data=seti))
summary(lm(ci01 ~ ivs2, data=seti))
summary(lm(ci02 ~ ivs2, data=seti))
summary(lm(ci03 ~ ivs2, data=seti))
summary(lm(ci04 ~ ivs2, data=seti))

#Salvando o DF de setores-------------
head(setores)#variáveis brutas
head(seti)#PCA e IVS

setores<-merge(setores,seti,by=c('cod_setor'),all.x=T)

save(setores,file='output_data/setores_ivs.Rda')
