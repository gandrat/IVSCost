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

##
packages<-c('ggplot2','readxl','dplyr','sf','corrplot','factoextra','reshape','smacof','gdata','ggbiplot')
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

rm(list=ls()) ## Limpando o workspace

##Carregando os dados---------------
load('input_data/descritores_IVSCostV2.RData')
load('output_data/malha_territorial_sf.Rda')


#V4: Selecionando  variáveis--------------
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

# set<-setores%>%transmute(cod_setor=cod_setor,cod_mun=cod_mun,nome_mun=nome,cd01=mordom,cd02=domalug/poptotal,cd03=renda,cd04=abaixopob/ndom,cd05=srenda/ndom,cp01=mulheres/poptotal,cp02=(criancas+idosos)/poptotal,cp03=raca/poptotal,cp04=pmais5-alfabet,ci01=(poptotal-cagua)/poptotal,ci02=(poptotal-cbanhesg)/poptotal,ci03=(poptotal-ccoletalixo)/poptotal,ci04=senergia/poptotal)
table(set$regiao)

write.csv(set,'output_data/setores_pca_ivscost_v4.csv')

#Carregando shapes (estados, mesoregiões, municipios)
#Removendo setores com percentuais maiores que 1
summary(set)
set<-set%>%filter(cd02<=1,cp02<=1,cp03<=1, ci01<=1,
                       ci02<=1,ci03<=1)
set<-set[complete.cases(set),] #mantem apenas os registros que possuem valores válidos em TODAS as variáveis

seti<-set%>%select(-cod_setor,-cod_mun, -cod_state, -nome_mun,  -cod_regiao)


#Histogramas---------------
set.m<-melt(seti)
ggplot(set.m,aes(x=value))+geom_histogram()+
  facet_wrap(~variable,scales='free',ncol = 3)+
  theme_bw()+xlab(NULL)+ylab(NULL)
ggsave('figures/histogramas_v4.jpg',dpi=200, units='cm', width=15, height=14)

ggplot(set.m,aes(x=regiao,y=value))+geom_boxplot(outlier.size=.1)+
  facet_wrap(~variable,scales='free',ncol = 3)+
  theme_bw()+xlab(NULL)+ylab(NULL)
ggsave('figures/boxplot_regiao_v4.jpg',dpi=200, units='cm', width=15, height=14)

#Correlograma-------------------
seti<-seti%>%select(-regiao)

M<-cor(seti,method='pearson')

jpeg('figures/correlograma_varbrutas_v4.jpg',width=15,height = 15,units='cm',res=300)
corrplot(M,type='upper',method = 'number')
dev.off()

#PCA: Geral-----------
pc<-prcomp(seti,scale = TRUE)

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
ggsave('figures/pca_screeV4.jpg', width=15, heigh=8, units='cm',dpi=150)

#Roda PCA
fviz_pca_var(pc,axes = c(1,2),
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
             title='')+
  theme(text=element_text(family='Times',size=10),
        legend.position = 'none')
ggsave("figures/pca_wheelV4.jpg", dpi=500, units='cm', width=14, height=14)

#Biplot
ggbiplot(pc, choices=c(1,2), obs.scale = 1, var.scale = 1,
         groups = set$regiao,ellipse = T,varname.size = 4,
         varname.adjust = 2, alpha=.05) +
  scale_color_brewer(palette = rev("YlOrBr"), name='') +
  ylim(c(-6,6))+
  xlim(c(-7,7))+
  # theme(legend.direction = 'horizontal', legend.position = 'top')+
  theme_bw()

ggsave("figures/pca_biplot_v4.jpg",dpi=300, units = 'cm', width = 14, height = 14)

#Calculando o IVS: Condições pessoais
pc$rotation
ivsp<-as.data.frame(pc$x)

set$ivsp=pc$x


#PCA: Pessoais-----------------
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
ggsave('figures/cpd_pca_screeV4.jpg', width=15, heigh=8, units='cm',dpi=150)


#Roda PCA
fviz_pca_var(pc,axes = c(1,2),
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
             title='')+
  theme(text=element_text(family='Times',size=10),
        legend.position = 'none')
ggsave("figures/cpd_pca_wheelV4.jpg", dpi=500, units='cm', width=14, height=14)
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

ggsave("figures/cpd_biplot_v4.jpg",dpi=300, units = 'cm', width = 14, height = 14)

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
ggsave('figures/ci_pca_screeV4.jpg', width=15, heigh=8, units='cm',dpi=150)



#Roda PCA
fviz_pca_var(pc,axes = c(1,2),
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
             title='')+
  theme(text=element_text(family='Times',size=10),
        legend.position = 'none')
ggsave("figures/ci_pca_wheelV4.jpg", dpi=500, units='cm', width=14, height=14)

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


ggsave("figures/ci_biplot_V4.jpg",dpi=300, units = 'cm', width = 14, height = 14)



#Montando tabela com TODOS os dados---------
#Padronização Z
setores<-set%>%mutate(cd01n=as.numeric(scale(cd01)),
                      cd02n=as.numeric(scale(cd02)),
                      cp01n=as.numeric(scale(cp01)),
                      cp02n=as.numeric(scale(cp02)),
                      cp03n=as.numeric(scale(cp03)),
                      cp04n=as.numeric(scale(cp04)),
                      ci01n=as.numeric(scale(ci01)),
                      ci02n=as.numeric(scale(ci02)),
                      ci03n=as.numeric(scale(ci03)),
                      ci04n=as.numeric(scale(ci04)))

summary(setores)

#Merge entre PCAs e setores-------------
pc_ci<-pcinfra%>%transmute(ci_pc1=PC1,
                             ci_pc2=PC2,
                             ci_pc3=PC3)
pc_cp<-pcpessoais%>%transmute(cp_pc1=PC1,
                           cp_pc2=PC2,
                           cp_pc3=PC3)
setores<-cbind(setores,pc_cp,pc_ci)

#Boxplot das componentes principais
setores.m<-melt(setores%>%select(regiao,cp_pc1,cp_pc2,ci_pc1,ci_pc2))
ggplot(setores.m,aes(x=regiao,y=value))+geom_boxplot(outlier.size=.2)+
  facet_wrap(~variable,scales='free')+
  theme_bw()+xlab(NULL)+ylab(NULL)
ggsave('figures/boxplots_pc_v4.jpg',dpi=200, units='cm', width=15, height=14)

#Montando o SHP com as malhas-----------------
setores<-merge(set_sf,setores%>%select(-cod_mun,-nome_mun,-cod_state), by='cod_setor')

#Setores
setores2<-setores
setores2$geometry<-NULL
muni<-setores2%>%select(-tipo,-cod_setor)%>%
  group_by(cod_mun,nm_mun,nm_micro,nm_meso,cod_uf,cod_regiao,regiao)%>%
  summarise_all(~ mean(.x, na.rm = TRUE))

muni<-merge(muni_sf,muni,by=c('cod_mun','nm_mun'))
write_sf(muni,'output_data/setores_pc.shp')

plot(muni%>%select(cp_pc1))


#Microrregiões
micro<-setores2%>%select(-tipo,-cod_setor,-cod_mun,-nm_mun)%>%
  group_by(nm_micro,nm_meso,cod_uf,cod_regiao,regiao)%>%
  summarise_all(~ mean(.x, na.rm = TRUE))

micro<-merge(micro_sf,micro,by=c('nm_micro'))
write_sf(micro,'output_data/micro_pc.shp')

plot(micro%>%select(cp_pc1))

#Mesorregiões
meso<-setores2%>%select(-tipo,-cod_setor,-cod_mun,-nm_mun,-nm_micro)%>%
  group_by(nm_meso,cod_uf,cod_regiao,regiao)%>%
  summarise_all(~ mean(.x, na.rm = TRUE))

meso<-merge(meso_sf,meso,by=c('nm_meso'))
write_sf(meso,'output_data/meso_pc.shp')

plot(meso%>%select(cp_pc1))

#Estados
uf<-setores2%>%select(-tipo,-cod_setor,-cod_mun,-nm_mun,-nm_micro,-nm_meso)%>%
  group_by(cod_uf,cod_regiao,regiao)%>%
  summarise_all(~ mean(.x, na.rm = TRUE))

uf<-merge(uf_sf,uf,by=c('cod_uf'))
write_sf(uf,'output_data/uf_pc.shp')

plot(uf%>%select(cp_pc1))
