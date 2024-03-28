install.packages('FactoMineR')
install.packages('factoextra')
install.packages('tidyverse')
install.packages('ggplot2')

require(FactoMineR)
require(factoextra)
require(tidyverse)
require(ggplot2)

df_enem_anacor <- read.csv("C:/Users/mih98/Downloads/anacor - enem/df_enem_anacor.csv", header=TRUE, sep = ";")

#Análise de correspondência simples - Dependência escolar por sucesso
res_ca <- table(df_enem_anacor$NM_DEPENDENCIA_ADM_ESC, df_enem_anacor$SUCESSO)
res_ca

dependencia <- CA(res_ca)

#Análise de correspondência simples - Sucesso por raça/cor
res_ca2 <- table(df_enem_anacor$SUCESSO, df_enem_anacor$NM_COR_RACA)
res_ca2

dependencia2 <- CA(res_ca2)

#Análise de correspondência múltipla - Dependência escolar por renda e cor/raça

df2 = df_enem_anacor[, c('NM_DEPENDENCIA_ADM_ESC', 'NM_COR_RACA', 'Q006')]

#MCA
res_mca <- MCA(df2, graph = TRUE)

#Scree-plot
fviz_screeplot(res_mca, addlabels = TRUE)

#Gráfico das variáveis
fviz_mca_var(res_mca, choice = "mca.cor",
             repel = TRUE,
             ggtheme = theme_minimal()) + labs(title = "MCA - Variáveis")

#Gráfico das categorias
fviz_mca_var(res_mca, col.var = "black", shape.var = 1,
             repel = TRUE)

#Cos2
#Representa a qualidade da representação das variáveis no mapa de fatores
#Mede o grau de associação entre as categorias de variáveis e um determinado eixo
fviz_cos2(res_mca, choice = "var", axes = 1:2) + labs(x="", y="", title="")

#Contribuição
fviz_contrib(res_mca, choice = "var", axes = 1:2, top = 15, ggtheme = theme_minimal(base_size = 18)) + labs(y="Contriuições (%)", title = "%")

#Gráfico de Correspondência - pontos e elipses
fviz_mca_biplot(res_mca,
                geom.ind = "point",
                habillage = 3,
                addEllipses = TRUE,
                col.var = "Black",
                palette = c("red","blue","green","yellow","orange","purple","pink","brown","black","violet","gray","cyan","magenta","turquoise","gold","salmon","maroon"),
                title = "",
                legend.title = "Escola",
                mean.point = FALSE,
                repel = TRUE)

#Análise de correspondência múltipla - Dependência escolar por renda e nota
df3 <- df_enem_anacor[, c('NM_DEPENDENCIA_ADM_ESC', 'Q006', 'SUCESSO')]

res_mca2 <- MCA(df3, graph = FALSE)

#Scree-plot
fviz_screeplot(res_mca2, addlabels = TRUE)

#Gráfico das variáveis
fviz_mca_var(res_mca2, choice = "mca.cor",
             repel = TRUE,
             ggtheme = theme_minimal()) + labs(title = "MCA - Variáveis")

#Gráfico das categorias
fviz_mca_var(res_mca2, col.var = "black", shape.var = 1,
             repel = TRUE)


