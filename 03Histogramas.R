##Histogramas para os descritores (Brutos)

packages<-c('ggplot2','dplyr','reshape')
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

load('input_data/descritores_IVSCostV2.RData')

set<-setores%>%transmute(cod_setor=cod_setor,
                         ndom=ndom,
                         mordom=mordom,
                         domalug=domalug/ndom,
                         rendavul=rendavul/poptotal,
                         renda=renda/poptotal,
                         mulheres=mulheres/poptotal,
                         raca=raca/poptotal,
                         idade_vul=(criancas+idosos)/poptotal,
                         analfa=alfabet/poptotal,
                         sagua=1-(cagua/poptotal),
                         sbanhesg=sbanh/poptotal,
                         scoleta=1-(ccoletalixo/poptotal),
                         senergia=1-(cenergia/poptotal),
                         spav=sempav/poptotal)

summary(set)

#Reorganizando a tabela para fazer o gráfico por facets
set.m<-melt(set,id='cod_setor')

ggplot(set.m,aes(x=value))+geom_histogram()+
  facet_wrap(~variable,scales = 'free')+
  xlab(NULL)
