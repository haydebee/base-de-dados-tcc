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

#INFRAESTRUTURA BÁSICA DAS ESCOLAS - ANACOR

#Carregando a base de dados
df_basico <- read.csv("C:/Users/mih98/Downloads/Projeto TCC - Análise de Correspondecia/anacor/df_basico_final.csv", header=TRUE)

##############################################################
# ÁGUA POTÁVEL
##############################################################

#Tabela de contingência com frequências absolutas observadas
tabela_agua_potavel <- table(df_basico$NM_DEPENDENCIA,
                             df_basico$TEM_AGUA_POTAVEL)
tabela_agua_potavel

#Estatística qui_quadrado e teste
qui2_agua_potavel <- chisq.test(x = tabela_agua_potavel)
qui2_agua_potavel

#Tabela de contingência com frequências absolutas observadas
qui2_agua_potavel$observed

#Tabela de contingência com frequências absolutas esperadas
qui2_agua_potavel$expected

#Tabela de contingência com frequências absolutas observadas e esperadas
sjt.xtab(var.row = df_basico$NM_DEPENDENCIA,
         var.col = df_basico$TEM_AGUA_POTAVEL,
         show.exp = TRUE)

#Análise de Correspondência - Água Potável nas Escolas
anacor <- CA(tabela_agua_potavel, graph = TRUE)

##############################################################
# ÁGUA INEXISTENTE
##############################################################

tabela_agua_inexistente <- table(df_basico$NM_DEPENDENCIA,
                             df_basico$AGUA_INEXISTENTE)
tabela_agua_inexistente

qui2_agua_inexistente <- chisq.test(x = tabela_agua_inexistente)
qui2_agua_inexistente

qui2_agua_inexistente$observed

qui2_agua_inexistente$expected

sjt.xtab(var.row = df_basico$NM_DEPENDENCIA,
         var.col = df_basico$AGUA_INEXISTENTE,
         show.exp = TRUE)

anacor1 <- CA(tabela_agua_inexistente, graph = TRUE)

##############################################################
# ENERGIA REDE PÚBLICA
##############################################################

tabela_energia <- table(df_basico$NM_DEPENDENCIA,
                                 df_basico$ENERGIA_REDE_PUBLICA)
tabela_energia

qui2_energia <- chisq.test(x = tabela_energia)
qui2_energia

qui2_energia$observed

qui2_energia$expected

sjt.xtab(var.row = df_basico$NM_DEPENDENCIA,
         var.col = df_basico$ENERGIA_REDE_PUBLICA,
         show.exp = TRUE)

anacor2 <- CA(tabela_energia, graph = TRUE)

##############################################################
# ENERGIA INEXISTENTE
##############################################################

tabela_energia_inexistente <- table(df_basico$NM_DEPENDENCIA,
                        df_basico$ENERGIA_INEXISTENTE)
tabela_energia_inexistente

qui2_energia_inexistente <- chisq.test(x = tabela_energia_inexistente)
qui2_energia_inexistente

qui2_energia_inexistente$observed

qui2_energia_inexistente$expected

sjt.xtab(var.row = df_basico$NM_DEPENDENCIA,
         var.col = df_basico$ENERGIA_INEXISTENTE,
         show.exp = TRUE)

anacor3 <- CA(tabela_energia_inexistente, graph = TRUE)

##############################################################
# ESGOTO REDE PÚBLICA
##############################################################

tabela_esgoto <- table(df_basico$NM_DEPENDENCIA,
                                    df_basico$ESGOTO_REDE_PUBLICA)
tabela_esgoto

qui2_esgoto <- chisq.test(x = tabela_esgoto)
qui2_esgoto

qui2_esgoto$observed

qui2_esgoto$expected

sjt.xtab(var.row = df_basico$NM_DEPENDENCIA,
         var.col = df_basico$ESGOTO_REDE_PUBLICA,
         show.exp = TRUE)

anacor4 <- CA(tabela_esgoto, graph = TRUE)

##############################################################
# ESGOTO INEXISTENTE
##############################################################

tabela_esgoto_inexistente <- table(df_basico$NM_DEPENDENCIA,
                       df_basico$ESGOTO_INEXISTENTE)
tabela_esgoto_inexistente

qui2_esgoto_inexistente <- chisq.test(x = tabela_esgoto_inexistente)
qui2_esgoto_inexistente

qui2_esgoto_inexistente$observed

qui2_esgoto_inexistente$expected

sjt.xtab(var.row = df_basico$NM_DEPENDENCIA,
         var.col = df_basico$ESGOTO_INEXISTENTE,
         show.exp = TRUE)

anacor5 <- CA(tabela_esgoto_inexistente, graph = TRUE)

# *********************************************************** #

#DEPENDENCIAS FÍSICAS EXISTENTES E UTILIZADAS NA ESCOLA

#Carregando a base de dados
df_depend <- read.csv("C:/Users/mih98/Downloads/Projeto TCC - Análise de Correspondecia/anacor/df_depend_final.csv", header=TRUE)

##############################################################
# AREA VERDE
##############################################################

tabela_area_verde <- table(df_depend$NM_DEPENDENCIA,
                                   df_depend$AREA_VERDE)
tabela_area_verde

qui2_area_verde <- chisq.test(x = tabela_area_verde)
qui2_area_verde

qui2_area_verde$observed

qui2_area_verde$expected

sjt.xtab(var.row = df_depend$NM_DEPENDENCIA,
         var.col = df_depend$AREA_VERDE,
         show.exp = TRUE)

anacor6 <- CA(tabela_area_verde, graph = TRUE)

##############################################################
# BANHEIRO
##############################################################

tabela_banheiro <- table(df_depend$NM_DEPENDENCIA,
                           df_depend$BANHEIRO)
tabela_banheiro

qui2_banheiro <- chisq.test(x = tabela_banheiro)
qui2_banheiro

qui2_banheiro$observed

qui2_banheiro$expected

sjt.xtab(var.row = df_depend$NM_DEPENDENCIA,
         var.col = df_depend$BANHEIRO,
         show.exp = TRUE)

anacor7 <- CA(tabela_banheiro, graph = TRUE)

##############################################################
# AUDITORIO
##############################################################

tabela_auditorio <- table(df_depend$NM_DEPENDENCIA,
                         df_depend$AUDITORIO)
tabela_auditorio

qui2_auditorio <- chisq.test(x = tabela_auditorio)
qui2_auditorio

qui2_auditorio$observed

qui2_auditorio$expected

sjt.xtab(var.row = df_depend$NM_DEPENDENCIA,
         var.col = df_depend$AUDITORIO,
         show.exp = TRUE)

anacor8 <- CA(tabela_auditorio, graph = TRUE)

##############################################################
# BIBLIOTECA E/OU SALA DE LEITURA
##############################################################

tabela_biblioteca <- table(df_depend$NM_DEPENDENCIA,
                          df_depend$BIBLIOTECA_SALA_LEITURA)
tabela_biblioteca

qui2_biblioteca <- chisq.test(x = tabela_biblioteca)
qui2_biblioteca

qui2_biblioteca$observed

qui2_biblioteca$expected

sjt.xtab(var.row = df_depend$NM_DEPENDENCIA,
         var.col = df_depend$BIBLIOTECA_SALA_LEITURA,
         show.exp = TRUE)

anacor9 <- CA(tabela_biblioteca, graph = TRUE)

##############################################################
# COZINHA
##############################################################

tabela_cozinha <- table(df_depend$NM_DEPENDENCIA,
                           df_depend$COZINHA)
tabela_cozinha

qui2_cozinha <- chisq.test(x = tabela_cozinha)
qui2_cozinha

qui2_cozinha$observed

qui2_cozinha$expected

sjt.xtab(var.row = df_depend$NM_DEPENDENCIA,
         var.col = df_depend$COZINHA,
         show.exp = TRUE)

anacor10 <- CA(tabela_cozinha, graph = TRUE)

##############################################################
# LABORATÓRIO DE CIÊNCIAS 
##############################################################

tabela_lab_ciencias <- table(df_depend$NM_DEPENDENCIA,
                        df_depend$LAB_CIENCIAS)
tabela_lab_ciencias

qui2_lab_ciencias <- chisq.test(x = tabela_lab_ciencias)
qui2_lab_ciencias

qui2_lab_ciencias$observed

qui2_lab_ciencias$expected

sjt.xtab(var.row = df_depend$NM_DEPENDENCIA,
         var.col = df_depend$LAB_CIENCIAS,
         show.exp = TRUE)

anacor11 <- CA(tabela_lab_ciencias, graph = TRUE)

##############################################################
# LABORATÓRIO DE INFORMÁTICA
##############################################################

tabela_lab_informatica <- table(df_depend$NM_DEPENDENCIA,
                            df_depend$LAB_INFORMATICA)
tabela_lab_informatica

qui2_lab_informatica <- chisq.test(x = tabela_lab_informatica)
qui2_lab_informatica

qui2_lab_informatica$observed

qui2_lab_informatica$expected

sjt.xtab(var.row = df_depend$NM_DEPENDENCIA,
         var.col = df_depend$LAB_INFORMATICA,
         show.exp = TRUE)

anacor12 <- CA(tabela_lab_informatica, graph = TRUE)

##############################################################
# LABORATÓRIO ESPECÍFICO PARA EDUCAÇÃO PROFISSIONAL
##############################################################

tabela_lab_edu_prof <- table(df_depend$NM_DEPENDENCIA,
                                df_depend$LAB_EDUC_PROF)
tabela_lab_edu_prof

qui2_lab_edu_prof <- chisq.test(x = tabela_lab_edu_prof)
qui2_lab_edu_prof

qui2_lab_edu_prof$observed

qui2_lab_edu_prof$expected

sjt.xtab(var.row = df_depend$NM_DEPENDENCIA,
         var.col = df_depend$LAB_EDUC_PROF,
         show.exp = TRUE)

anacor13 <- CA(tabela_lab_edu_prof, graph = TRUE)

##############################################################
# PARQUE INFANTIL
##############################################################

tabela_parque_inf <- table(df_depend$NM_DEPENDENCIA,
                             df_depend$PARQUE_INFANTIL)
tabela_parque_inf

qui2_parque_inf <- chisq.test(x = tabela_parque_inf)
qui2_parque_inf

qui2_parque_inf$observed

qui2_parque_inf$expected

sjt.xtab(var.row = df_depend$NM_DEPENDENCIA,
         var.col = df_depend$PARQUE_INFANTIL,
         show.exp = TRUE)

anacor14 <- CA(tabela_parque_inf, graph = TRUE)

##############################################################
# PISCINA
##############################################################

tabela_piscina <- table(df_depend$NM_DEPENDENCIA,
                           df_depend$PISCINA)
tabela_piscina

qui2_piscina <- chisq.test(x = tabela_piscina)
qui2_piscina

qui2_piscina$observed

qui2_piscina$expected

sjt.xtab(var.row = df_depend$NM_DEPENDENCIA,
         var.col = df_depend$PISCINA,
         show.exp = TRUE)

anacor15 <- CA(tabela_piscina, graph = TRUE)

##############################################################
# QUADRA DE ESPORTES
##############################################################

tabela_quadra_esportes <- table(df_depend$NM_DEPENDENCIA,
                        df_depend$QUADRA_ESPORTES)
tabela_quadra_esportes

qui2_quadra_esportes <- chisq.test(x = tabela_quadra_esportes)
qui2_quadra_esportes

qui2_quadra_esportes$observed

qui2_quadra_esportes$expected

sjt.xtab(var.row = df_depend$NM_DEPENDENCIA,
         var.col = df_depend$QUADRA_ESPORTES,
         show.exp = TRUE)

anacor16 <- CA(tabela_quadra_esportes, graph = TRUE)

##############################################################
# REFEITÓRIO
##############################################################

tabela_refeitorio <- table(df_depend$NM_DEPENDENCIA,
                                df_depend$REFEITORIO)
tabela_refeitorio

qui2_refeitorio <- chisq.test(x = tabela_refeitorio)
qui2_refeitorio

qui2_refeitorio$observed

qui2_refeitorio$expected

sjt.xtab(var.row = df_depend$NM_DEPENDENCIA,
         var.col = df_depend$REFEITORIO,
         show.exp = TRUE)

anacor17 <- CA(tabela_refeitorio, graph = TRUE)

##############################################################
# SALA/ATELIÊ DE ARTES
##############################################################

tabela_artes <- table(df_depend$NM_DEPENDENCIA,
                           df_depend$SALA_ATELIE_ARTES)
tabela_artes

qui2_artes <- chisq.test(x = tabela_artes)
qui2_artes

qui2_artes$observed

qui2_artes$expected

sjt.xtab(var.row = df_depend$NM_DEPENDENCIA,
         var.col = df_depend$SALA_ATELIE_ARTES,
         show.exp = TRUE)

anacor18 <- CA(tabela_artes, graph = TRUE)

##############################################################
# SALA DE MÚSICA/CORAL
##############################################################

tabela_musica <- table(df_depend$NM_DEPENDENCIA,
                      df_depend$SALA_MUSICA_CORAL)
tabela_musica

qui2_musica <- chisq.test(x = tabela_musica)
qui2_musica

qui2_musica$observed

qui2_musica$expected

sjt.xtab(var.row = df_depend$NM_DEPENDENCIA,
         var.col = df_depend$SALA_MUSICA_CORAL,
         show.exp = TRUE)

anacor19 <- CA(tabela_musica, graph = TRUE)

##############################################################
# SALA/ESTÚDIO DE DANÇA
##############################################################

tabela_danca <- table(df_depend$NM_DEPENDENCIA,
                       df_depend$SALA_ESTUDIO_DANCA)
tabela_danca

qui2_danca <- chisq.test(x = tabela_danca)
qui2_danca

qui2_danca$observed

qui2_danca$expected

sjt.xtab(var.row = df_depend$NM_DEPENDENCIA,
         var.col = df_depend$SALA_ESTUDIO_DANCA,
         show.exp = TRUE)

anacor20 <- CA(tabela_danca, graph = TRUE)

##############################################################
# SALA MULTIUSO
##############################################################

tabela_multiuso <- table(df_depend$NM_DEPENDENCIA,
                      df_depend$SALA_MULTIUSO)
tabela_multiuso

qui2_multiuso <- chisq.test(x = tabela_multiuso)
qui2_multiuso

qui2_multiuso$observed

qui2_multiuso$expected

sjt.xtab(var.row = df_depend$NM_DEPENDENCIA,
         var.col = df_depend$SALA_MULTIUSO,
         show.exp = TRUE)

anacor21 <- CA(tabela_multiuso, graph = TRUE)

##############################################################
# TERREIRÃO (ÁREA PARA PRÁTICA DESPORTIVA E RECREAÇÃO)
##############################################################

tabela_terreirao <- table(df_depend$NM_DEPENDENCIA,
                         df_depend$TERREIRAO)
tabela_terreirao

qui2_terreirao <- chisq.test(x = tabela_terreirao)
qui2_terreirao

qui2_terreirao$observed

qui2_terreirao$expected

sjt.xtab(var.row = df_depend$NM_DEPENDENCIA,
         var.col = df_depend$TERREIRAO,
         show.exp = TRUE)

anacor22 <- CA(tabela_terreirao, graph = TRUE)

# *********************************************************** #

#EQUIPAMENTOS EXISTENTES NA ESCOLA PARA O PROCESSO DE APRENDIZAGEM

#Carregando a base de dados
df_equip <- read.csv("C:/Users/mih98/Downloads/Projeto TCC - Análise de Correspondecia/anacor/df_equip_final.csv", header=TRUE)

##############################################################
# DESKTOP EM USO PELOS ALUNOS
##############################################################

tabela_desktop <- table(df_equip$NM_DEPENDENCIA,
                          df_equip$DESKTOP_ALUNO)
tabela_desktop

qui2_desktop <- chisq.test(x = tabela_desktop)
qui2_desktop

qui2_desktop$observed

qui2_desktop$expected

sjt.xtab(var.row = df_equip$NM_DEPENDENCIA,
         var.col = df_equip$DESKTOP_ALUNO,
         show.exp = TRUE)

anacor23 <- CA(tabela_desktop, graph = TRUE)

##############################################################
# DVD/BLU-RAY
##############################################################

tabela_dvd <- table(df_equip$NM_DEPENDENCIA,
                        df_equip$EQUIP_DVD)
tabela_dvd

qui2_dvd <- chisq.test(x = tabela_dvd)
qui2_dvd

qui2_dvd$observed

qui2_dvd$expected

sjt.xtab(var.row = df_equip$NM_DEPENDENCIA,
         var.col = df_equip$EQUIP_DVD,
         show.exp = TRUE)

anacor24 <- CA(tabela_dvd, graph = TRUE)

##############################################################
# APARELHO DE SOM
##############################################################

tabela_som <- table(df_equip$NM_DEPENDENCIA,
                    df_equip$EQUIP_SOM)
tabela_som

qui2_som <- chisq.test(x = tabela_som)
qui2_som

qui2_som$observed

qui2_som$expected

sjt.xtab(var.row = df_equip$NM_DEPENDENCIA,
         var.col = df_equip$EQUIP_SOM,
         show.exp = TRUE)

anacor25 <- CA(tabela_som, graph = TRUE)

##############################################################
# APARELHO DE TELEVISÃO
##############################################################

tabela_televisao <- table(df_equip$NM_DEPENDENCIA,
                    df_equip$EQUIP_TV)
tabela_televisao

qui2_televisao <- chisq.test(x = tabela_televisao)
qui2_televisao

qui2_televisao$observed

qui2_televisao$expected

sjt.xtab(var.row = df_equip$NM_DEPENDENCIA,
         var.col = df_equip$EQUIP_TV,
         show.exp = TRUE)

anacor26 <- CA(tabela_televisao, graph = TRUE)

##############################################################
# LOUSA DIGITAL
##############################################################

tabela_lousa <- table(df_equip$NM_DEPENDENCIA,
                          df_equip$EQUIP_LOUSA_DIGITAL)
tabela_lousa

qui2_lousa <- chisq.test(x = tabela_lousa)
qui2_lousa

qui2_lousa$observed

qui2_lousa$expected

sjt.xtab(var.row = df_equip$NM_DEPENDENCIA,
         var.col = df_equip$EQUIP_LOUSA_DIGITAL,
         show.exp = TRUE)

anacor27 <- CA(tabela_lousa, graph = TRUE)

##############################################################
# EQUIPAMENTO MULTIMÍDIA - DATASHOW
##############################################################

tabela_datashow <- table(df_equip$NM_DEPENDENCIA,
                      df_equip$EQUIP_DATASHOW)
tabela_datashow

qui2_datashow <- chisq.test(x = tabela_datashow)
qui2_datashow

qui2_datashow$observed

qui2_datashow$expected

sjt.xtab(var.row = df_equip$NM_DEPENDENCIA,
         var.col = df_equip$EQUIP_DATASHOW,
         show.exp = TRUE)

anacor28 <- CA(tabela_datashow, graph = TRUE)

##############################################################
# COMPUTADOR PORTÁTIL EM USO PELOS ALUNOS
##############################################################

tabela_comp_portatil <- table(df_equip$NM_DEPENDENCIA,
                         df_equip$COMP_PORTATIL_ALUNO)
tabela_comp_portatil

qui2_comp_portatil <- chisq.test(x = tabela_comp_portatil)
qui2_comp_portatil

qui2_comp_portatil$observed

qui2_comp_portatil$expected

sjt.xtab(var.row = df_equip$NM_DEPENDENCIA,
         var.col = df_equip$COMP_PORTATIL_ALUNO,
         show.exp = TRUE)

anacor29 <- CA(tabela_comp_portatil, graph = TRUE)

##############################################################
# TABLET
##############################################################

tabela_tablet <- table(df_equip$NM_DEPENDENCIA,
                              df_equip$TABLET_ALUNO)
tabela_tablet

qui2_tablet <- chisq.test(x = tabela_tablet)
qui2_tablet

qui2_tablet$observed

qui2_tablet$expected

sjt.xtab(var.row = df_equip$NM_DEPENDENCIA,
         var.col = df_equip$TABLET_ALUNO,
         show.exp = TRUE)

anacor30 <- CA(tabela_tablet, graph = TRUE)

##############################################################
# INTERNET
##############################################################

tabela_internet <- table(df_equip$NM_DEPENDENCIA,
                       df_equip$INTERNET)
tabela_internet

qui2_internet <- chisq.test(x = tabela_internet)
qui2_internet

qui2_internet$observed

qui2_internet$expected

sjt.xtab(var.row = df_equip$NM_DEPENDENCIA,
         var.col = df_equip$INTERNET,
         show.exp = TRUE)

anacor31 <- CA(tabela_internet, graph = TRUE)

##############################################################
# INTERNET - PARA USO DOS ALUNOS
##############################################################

tabela_internet_alunos <- table(df_equip$NM_DEPENDENCIA,
                         df_equip$INTERNET_ALUNOS)
tabela_internet_alunos

qui2_internet_alunos <- chisq.test(x = tabela_internet_alunos)
qui2_internet_alunos

qui2_internet_alunos$observed

qui2_internet_alunos$expected

sjt.xtab(var.row = df_equip$NM_DEPENDENCIA,
         var.col = df_equip$INTERNET_ALUNOS,
         show.exp = TRUE)

anacor32 <- CA(tabela_internet_alunos, graph = TRUE)

##############################################################
# INTERNET - BANDA LARGA
##############################################################

tabela_banda_larga <- table(df_equip$NM_DEPENDENCIA,
                                df_equip$BANDA_LARGA)
tabela_banda_larga

qui2_banda_larga <- chisq.test(x = tabela_banda_larga)
qui2_banda_larga

qui2_banda_larga$observed

qui2_banda_larga$expected

sjt.xtab(var.row = df_equip$NM_DEPENDENCIA,
         var.col = df_equip$BANDA_LARGA,
         show.exp = TRUE)

anacor33 <- CA(tabela_banda_larga, graph = TRUE)

# *********************************************************** #

#PROFISSIONAIS QUE ATUAM NA ESCOLA

#Carregando a base de dados
df_profissionais <- read.csv("C:/Users/mih98/Downloads/Projeto TCC - Análise de Correspondecia/anacor/df_profissionais_final.csv", header=TRUE)

##############################################################
# PSICÓLOGO ESCOLAR
##############################################################

tabela_psicologo <- table(df_profissionais$NM_DEPENDENCIA,
                            df_profissionais$PSICOLOGO)
tabela_psicologo

qui2_psicologo <- chisq.test(x = tabela_psicologo)
qui2_psicologo

qui2_psicologo$observed

qui2_psicologo$expected

sjt.xtab(var.row = df_profissionais$NM_DEPENDENCIA,
         var.col = df_profissionais$PSICOLOGO,
         show.exp = TRUE)

anacor34 <- CA(tabela_psicologo, graph = TRUE)

##############################################################
# NUTRICIONISTA
##############################################################

tabela_nutricionista <- table(df_profissionais$NM_DEPENDENCIA,
                          df_profissionais$NUTRICIONISTA)
tabela_nutricionista

qui2_nutricionista <- chisq.test(x = tabela_nutricionista)
qui2_nutricionista

qui2_nutricionista$observed

qui2_nutricionista$expected

sjt.xtab(var.row = df_profissionais$NM_DEPENDENCIA,
         var.col = df_profissionais$NUTRICIONISTA,
         show.exp = TRUE)

anacor35 <- CA(tabela_nutricionista, graph = TRUE)

##############################################################
# FONAUDIÓLOGO
##############################################################

tabela_fonaudiologo <- table(df_profissionais$NM_DEPENDENCIA,
                              df_profissionais$FONAUDIOLOGO)
tabela_fonaudiologo

qui2_fonaudiologo <- chisq.test(x = tabela_fonaudiologo)
qui2_fonaudiologo

qui2_fonaudiologo$observed

qui2_fonaudiologo$expected

sjt.xtab(var.row = df_profissionais$NM_DEPENDENCIA,
         var.col = df_profissionais$FONAUDIOLOGO,
         show.exp = TRUE)

anacor36 <- CA(tabela_fonaudiologo, graph = TRUE)

##############################################################
# PROFISSIONAIS DE APOIO E SUPERVISÃO PEDAGÓGICA
##############################################################

tabela_pedagogo <- table(df_profissionais$NM_DEPENDENCIA,
                             df_profissionais$PEDAGOGO)
tabela_pedagogo

qui2_pedagogo <- chisq.test(x = tabela_pedagogo)
qui2_pedagogo

qui2_pedagogo$observed

qui2_pedagogo$expected

sjt.xtab(var.row = df_profissionais$NM_DEPENDENCIA,
         var.col = df_profissionais$PEDAGOGO,
         show.exp = TRUE)

anacor37 <- CA(tabela_pedagogo, graph = TRUE)

##############################################################
# SEGURANÇA
##############################################################

tabela_seguranca <- table(df_profissionais$NM_DEPENDENCIA,
                         df_profissionais$SEGURANCA)
tabela_seguranca

qui2_seguranca <- chisq.test(x = tabela_seguranca)
qui2_seguranca

qui2_seguranca$observed

qui2_seguranca$expected

sjt.xtab(var.row = df_profissionais$NM_DEPENDENCIA,
         var.col = df_profissionais$SEGURANCA,
         show.exp = TRUE)

anacor38 <- CA(tabela_seguranca, graph = TRUE)


# *********************************************************** #

#INSTRUMENTOS E MATERIAIS SOCIOCULTURAIS

#Carregando a base de dados
df_materiais_socio <- read.csv("C:/Users/mih98/Downloads/Projeto TCC - Análise de Correspondecia/anacor/df_materiais_socio_final.csv", header=TRUE)

##############################################################
# ACERVO MULTIMIDIA
##############################################################

tabela_acervo_multimidia <- table(df_materiais_socio$NM_DEPENDENCIA,
                          df_materiais_socio$ACERVO_MULTIMIDIA)
tabela_acervo_multimidia

qui2_acervo_multimidia <- chisq.test(x = tabela_acervo_multimidia)
qui2_acervo_multimidia

qui2_acervo_multimidia$observed

qui2_acervo_multimidia$expected

sjt.xtab(var.row = df_materiais_socio$NM_DEPENDENCIA,
         var.col = df_materiais_socio$ACERVO_MULTIMIDIA,
         show.exp = TRUE)

anacor39 <- CA(tabela_acervo_multimidia, graph = TRUE)

##############################################################
# BRINQUEDOS PARA EDUCAÇÃO INFANTIL
##############################################################

tabela_brinquedos <- table(df_materiais_socio$NM_DEPENDENCIA,
                                  df_materiais_socio$BRINQUEDOS_EDU_INF)
tabela_brinquedos

qui2_brinquedos <- chisq.test(x = tabela_brinquedos)
qui2_brinquedos

qui2_brinquedos$observed

qui2_brinquedos$expected

sjt.xtab(var.row = df_materiais_socio$NM_DEPENDENCIA,
         var.col = df_materiais_socio$BRINQUEDOS_EDU_INF,
         show.exp = TRUE)

anacor40 <- CA(tabela_brinquedos, graph = TRUE)

##############################################################
# CONJUNTO DE MATERIAIS CIENTÍFICOS
##############################################################

tabela_mat_cientifico <- table(df_materiais_socio$NM_DEPENDENCIA,
                           df_materiais_socio$MAT_CIENTIFICO)
tabela_mat_cientifico

qui2_mat_cientifico <- chisq.test(x = tabela_mat_cientifico)
qui2_mat_cientifico

qui2_mat_cientifico$observed

qui2_mat_cientifico$expected

sjt.xtab(var.row = df_materiais_socio$NM_DEPENDENCIA,
         var.col = df_materiais_socio$MAT_CIENTIFICO,
         show.exp = TRUE)

anacor41 <- CA(tabela_mat_cientifico, graph = TRUE)

##############################################################
# EQUIPAMENTO PARA AMPLIFICAÇÃO E DIFUSÃO DE SOM/ÁUDIO
##############################################################

tabela_mat_difusao_som_aud <- table(df_materiais_socio$NM_DEPENDENCIA,
                               df_materiais_socio$MAT_DIFUSAO_SOM_AUD)
tabela_mat_difusao_som_aud

qui2_mat_difusao_som_aud <- chisq.test(x = tabela_mat_difusao_som_aud)
qui2_mat_difusao_som_aud

qui2_mat_difusao_som_aud$observed

qui2_mat_difusao_som_aud$expected

sjt.xtab(var.row = df_materiais_socio$NM_DEPENDENCIA,
         var.col = df_materiais_socio$MAT_DIFUSAO_SOM_AUD,
         show.exp = TRUE)

anacor42 <- CA(tabela_mat_difusao_som_aud, graph = TRUE)

##############################################################
# INSTRUMENTOS MUSICAIS
##############################################################

tabela_mat_instr_musica <- table(df_materiais_socio$NM_DEPENDENCIA,
                                    df_materiais_socio$MAT_INSTR_MUSICA)
tabela_mat_instr_musica

qui2_mat_instr_musica <- chisq.test(x = tabela_mat_instr_musica)
qui2_mat_instr_musica

qui2_mat_instr_musica$observed

qui2_mat_instr_musica$expected

sjt.xtab(var.row = df_materiais_socio$NM_DEPENDENCIA,
         var.col = df_materiais_socio$MAT_INSTR_MUSICA,
         show.exp = TRUE)

anacor43 <- CA(tabela_mat_instr_musica, graph = TRUE)

##############################################################
# JOGOS EDUCATIVOS
##############################################################

tabela_jogos_edu <- table(df_materiais_socio$NM_DEPENDENCIA,
                                 df_materiais_socio$MAT_JOGOS_EDU)
tabela_jogos_edu

qui2_jogos_edu <- chisq.test(x = tabela_jogos_edu)
qui2_jogos_edu

qui2_jogos_edu$observed

qui2_jogos_edu$expected

sjt.xtab(var.row = df_materiais_socio$NM_DEPENDENCIA,
         var.col = df_materiais_socio$MAT_JOGOS_EDU,
         show.exp = TRUE)

anacor44 <- CA(tabela_jogos_edu, graph = TRUE)

##############################################################
# MATERIAIS PARA ATIVIDADES CULTURAIS E ARTISTÍCAS
##############################################################

tabela_mat_artisticas <- table(df_materiais_socio$NM_DEPENDENCIA,
                          df_materiais_socio$MAT_ARTISTICAS)
tabela_mat_artisticas

qui2_mat_artisticas <- chisq.test(x = tabela_mat_artisticas)
qui2_mat_artisticas

qui2_mat_artisticas$observed

qui2_mat_artisticas$expected

sjt.xtab(var.row = df_materiais_socio$NM_DEPENDENCIA,
         var.col = df_materiais_socio$MAT_ARTISTICAS,
         show.exp = TRUE)

anacor45 <- CA(tabela_mat_artisticas, graph = TRUE)

##############################################################
# MATERIAIS PARA EDUCAÇÃO PROFISSIONAL
##############################################################

tabela_mat_profissional <- table(df_materiais_socio$NM_DEPENDENCIA,
                               df_materiais_socio$MAT_PROFISSIONAL)
tabela_mat_profissional

qui2_mat_profissional <- chisq.test(x = tabela_mat_profissional)
qui2_mat_profissional

qui2_mat_profissional$observed

qui2_mat_profissional$expected

sjt.xtab(var.row = df_materiais_socio$NM_DEPENDENCIA,
         var.col = df_materiais_socio$MAT_PROFISSIONAL,
         show.exp = TRUE)

anacor46 <- CA(tabela_mat_profissional, graph = TRUE)

##############################################################
# MATERIAIS PARA PRÁTICA DESPORTIVA E RECREAÇÃO
##############################################################

tabela_mat_desportiva <- table(df_materiais_socio$NM_DEPENDENCIA,
                                 df_materiais_socio$MAT_DESPORTIVA)
tabela_mat_desportiva

qui2_mat_desportiva <- chisq.test(x = tabela_mat_desportiva)
qui2_mat_desportiva

qui2_mat_desportiva$observed

qui2_mat_desportiva$expected

sjt.xtab(var.row = df_materiais_socio$NM_DEPENDENCIA,
         var.col = df_materiais_socio$MAT_DESPORTIVA,
         show.exp = TRUE)

anacor47 <- CA(tabela_mat_desportiva, graph = TRUE)

##############################################################
# MATERIAIS INDÍGENA
##############################################################

tabela_mat_indigena <- table(df_materiais_socio$NM_DEPENDENCIA,
                               df_materiais_socio$MAT_INDIGENA)
tabela_mat_indigena

qui2_mat_indigena <- chisq.test(x = tabela_mat_indigena)
qui2_mat_indigena

qui2_mat_indigena$observed

qui2_mat_indigena$expected

sjt.xtab(var.row = df_materiais_socio$NM_DEPENDENCIA,
         var.col = df_materiais_socio$MAT_INDIGENA,
         show.exp = TRUE)

anacor48 <- CA(tabela_mat_indigena, graph = TRUE)

##############################################################
# MATERIAIS PEDAGÓGICOS PARA A EDUCAÇÃO DAS RELAÇÕES ÉTNICO-RACIAIS
##############################################################

tabela_mat_etnico <- table(df_materiais_socio$NM_DEPENDENCIA,
                             df_materiais_socio$MAT_ETNICO)
tabela_mat_etnico

qui2_mat_etnico <- chisq.test(x = tabela_mat_etnico)
qui2_mat_etnico

qui2_mat_etnico$observed

qui2_mat_etnico$expected

sjt.xtab(var.row = df_materiais_socio$NM_DEPENDENCIA,
         var.col = df_materiais_socio$MAT_ETNICO,
         show.exp = TRUE)

anacor49 <- CA(tabela_mat_etnico, graph = TRUE)

##############################################################
# MATERIAIS PEDAGÓGICOS PARA A EDUCAÇÃO DO CAMPO
##############################################################

tabela_mat_campo <- table(df_materiais_socio$NM_DEPENDENCIA,
                           df_materiais_socio$MAT_CAMPO)
tabela_mat_campo

qui2_mat_campo <- chisq.test(x = tabela_mat_campo)
qui2_mat_campo

qui2_mat_campo$observed

qui2_mat_campo$expected

sjt.xtab(var.row = df_materiais_socio$NM_DEPENDENCIA,
         var.col = df_materiais_socio$MAT_CAMPO,
         show.exp = TRUE)

anacor50 <- CA(tabela_mat_campo, graph = TRUE)























