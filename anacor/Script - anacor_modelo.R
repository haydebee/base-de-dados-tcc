#Instalação e carregamento dos pacotes utilizados
pacotes <- c("plotly", #plataforma gráfica
            "tidyverse", #carregar outros pacotes do R
            "ggrepel", #geoms de texto e rótulo para o 'ggplot2' que ajudam a evitar
            "knitr", "kableExtra", #formatação de tabelas
            "sjPlot", #elaboração de tabelas de contingência
            "FactoMineR", #função 'CA' para elaboração direta da Anacor
            "amap", #funções 'matlogic' e 'burt' para matrizes binárias e de Burt
            "ade4") #função 'dudi.acm' para elaboração da ACM

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for (i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) } else {
    sapply(pacotes, require, character =T)
  }


#Carregando a base de dados
df_basico <- read.csv("C:/Users/mih98/Downloads/Projeto TCC - Análise de Correspondecia/anacor/df_basico_final.csv", header=TRUE)    

#Tabela de contingência com frequências absolutas observadas
tabela_contingencia <- table(df_basico$NM_DEPENDENCIA,
                             df_basico$TEM_AGUA_POTAVEL)
tabela_contingencia

#Definição da quantidade de observações na tabela de contingência
n <- sum(tabela_contingencia)
n

#Estatística qui_quadrado e teste
qui2 <- chisq.test(x = tabela_contingencia)
qui2

#Tabela de contingência com frequências absolutas observadas
qui2$observed

#Tabela de contingência com frequências absolutas esperadas
qui2$expected

#Tabela de contingência com frequências absolutas observadas e esperadas
sjt.xtab(var.row = df_basico$NM_DEPENDENCIA,
         var.col = df_basico$TEM_AGUA_POTAVEL,
         show.exp = TRUE)

#Resíduos - diferenças entre frequências absolutas observadas e esperadas
qui2$observed - qui2$expected

#Valores de qui-quadrado por célula
((qui2$observed - qui2$expected)^2)/qui2$expected

#Resíduos padronizados
qui2$residuals

#Resíduos padronizados ajustados
#Importa somente os valores > 1,96%
qui2$stdres

#Mapa de calor dos resíduos padronizados ajustados
data.frame(qui2$stdres) %>%
  rename(NM_DEPENDENCIA = 1,
         TEM_AGUA_POTAVEL = 2) %>% 
  ggplot(aes(x=fct_rev(NM_DEPENDENCIA), y = TEM_AGUA_POTAVEL,
             fill = Freq, label = round(Freq, 3))) + 
  geom_tile() + 
  geom_text(size = 5) +
  scale_fill_gradient2(low = "white",
                       mid = "white",
                       high = "blue",
                       midpoint = 1.96) + 
  labs(x = 'Dependência Escolar', y = 'Possui Água Potável', fill = "Res. Pad. Ajustados") + 
  coord_flip() + 
  theme_bw()

#Análise da associação por meio do mapa perceptual
#Definição da matriz A
#Resíduos padronizados (qui2$residuals) divididos pela raiz quadrado do tamanho da amostra
matrizA <- qui2$residuals/sqrt(n)
matrizA

#Definição da matriz W
#Multiplicação da matriz A transposta pela matriz A
matrizW <- t(matrizA) %*% matrizA
matrizW

#Definição da quantidade de dimensões
qtde_dimensoes <- min(nrow(matrizW) -1, ncol(matrizW) -1)
qtde_dimensoes
#menor valor entre 3 e 2 é 2

#Definição de valores singulares
VS_AV <- svd(matrizA, nu = qtde_dimensoes, nv = qtde_dimensoes)

#Valores singulares de cada dimensão
valores_singulares <- VS_AV$d[1:qtde_dimensoes]
valores_singulares
#raiz do lambda quadrado

#Autovalores (eigenvalues) de cada dimensão
eigenvalues <- (valores_singulares)^2
eigenvalues
#primeiro autovalor é sempre o maior e é o eixo X e o menor valor é o eixo Y

#Cálculo da inércia principal total (a partir do qui-quadrado)
inercia_total <- as.numeric(qui2$statistic/sum(tabela_contingencia))
inercia_total

#Cálculo da variância explicada em cada dimensão
variancia_explicada <- eigenvalues / inercia_total
variancia_explicada

#Cálculo das massas das colunas (column profiles)
soma_colunas <- apply(tabela_contingencia, MARGIN = 1, FUN = sum)
soma_colunas

#Massas das colunas (column profiles)
massa_colunas <- soma_colunas / n
massa_colunas

#Cálculo das massas das linhas (row profiles)
soma_linhas <- apply(tabela_contingencia, MARGIN = 2, FUN = sum)
soma_linhas

#Massas das linhas (row_profiles)
massa_linhas <- soma_linhas / n
massa_linhas

#Autovetores v das dimensões
autovetor_v <- VS_AV$v
autovetor_v

#Autovetores u das dimensões
autovetor_u <- VS_AV$u
autovetor_u

#Calculando as coordenadas para plotar as categorias no mapa perceptual

#Variável em linha na tabela de contingência ('NM_DEPENDENCIA')
#Coordenadas das abcissas
coord_abcissas_nm_dependencia <- sqrt(valores_singulares[1] * massa_colunas^0.5) * autovetor_u[,1]
coord_abcissas_nm_dependencia

# Coordenadas das ordenadas
coord_ordenadas_nm_dependencia <- sqrt(valores_singulares[2]) * (massa_colunas^-0.5) * autovetor_u[,2]
coord_ordenadas_nm_dependencia

# Variável em coluna na tabela de contingência ('TEM_AGUA_POTAVEL')
# Coordenadas das abcissas
coord_abcissas_agua_potavel <- sqrt(valores_singulares[1]) * (massa_linhas^-0.5) * autovetor_v[,1]
coord_abcissas_agua_potavel

# Coordenadas das ordenadas
coord_ordenadas_agua_potavel <- sqrt(valores_singulares[2]) * (massa_linhas^-0.5) * autovetor_v[,2]
coord_ordenadas_agua_potavel

#Mapa perceptual
cbind.data.frame(coord_abcissas_nm_dependencia, coord_ordenadas_nm_dependencia,
                 coord_abcissas_agua_potavel, coord_ordenadas_agua_potavel) %>%
  rename(dim_1_dependencia = 1,
         dim_2_dependencia = 2,
         dim_1_agua = 3,
         dim_2_agua = 4) %>%
  rownames_to_column() %>%
  setNames(make.names(names(.), unique = TRUE)) %>%
  mutate(TEM_AGUA_POTAVEL = rownames(data.frame(coord_abcissas_agua_potavel,
                                                coord_ordenadas_agua_potavel))) %>%
  rename(NM_DEPENDENCIA = 1,
         dim_1_dependencia = 2,
         dim_2_dependencia = 3,
         dim_1_agua = 4,
         dim_2_agua = 5) %>%
  ggplot() +
  geom_point(aes(x = dim_1_dependencia, y = dim_2_dependencia),
             color = "deeppink1",
             fill = "deeppink1",
             shape = 24,
             size = 4) +
  geom_text_repel(aes(x = dim_1_dependencia, y = dim_2_dependencia, label = NM_DEPENDENCIA)) +
  geom_point(aes(x = dim_1_agua, y = dim_2_agua),
             color = "turquoise3",
             fill = "turquoise3",
             shape = 21,
             size = 4) +
  geom_text_repel(aes(x = dim_1_agua, y = dim_2_agua, label = TEM_AGUA_POTAVEL)) +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(variancia_explicada[1] * 100, 2),"%")),
       y = paste("Dimensão 2:", paste0(round(variancia_explicada[2] * 100, 2),"%"))) +
  theme_bw()

#O resultado pode ser obtido por meio da função 'CA' do pacote FactoMineR 
anacor <- CA(tabela_contingencia, graph = TRUE)
