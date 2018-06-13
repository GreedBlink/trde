# trabalho 3 - Jonatha Azevedo e Leonardo Filgueira
# delineamento em quadrados latinos

# Experimento: Comparar a produção de 8 variedades de cana-de-açúcar.
# A fim de controlar o efeito de fertilidade do solo, foi utilizado
# o delineamento em blocos, dividindo a área em linhas em colunas,
# de modo a obter uma planta por célula (linha, coluna).

# Unidade experimental: cana de acucar
# Variável resposta: peso da producao
# Níveis: variedades da cana de acucar

require(dplyr)
require(tidyr)
require(ggplot2)

set.seed(1)

linha <- rep(paste0("L", 1:8), each = 8)
coluna <- rep(paste0("C", 1:8), 8)

dados <- data_frame(linha, coluna)

dados <- dados %>% 
  mutate(variedade = c(LETTERS[c(1:8, 
                                 8, 1:7,
                                 7:8, 1:6,
                                 6:8, 1:5,
                                 5:8, 1:4,
                                 4:8, 1:3,
                                 3:8, 1:2,
                                 2:8, 1)])) # %>% 
  # spread(key = coluna, value = variedade)
  
# média global
mu <- 300

dados <- dados %>% 
  mutate(efeito_linha = case_when(
    linha == "L1" ~ 10,
    linha == "L2" ~ 50,
    linha == "L3" ~ 95,
    linha == "L4" ~ 100,
    linha == "L5" ~ 80,
    linha == "L6" ~ 30,
    linha == "L7" ~ 10,
    linha == "L8" ~ 0),
  efeito_coluna = case_when(
    coluna == "C1" ~ -25,
    coluna == "C2" ~ -50,
    coluna == "C3" ~ -10,
    coluna == "C4" ~ 35,
    coluna == "C5" ~ 85,
    coluna == "C6" ~ 100,
    coluna == "C7" ~ 50,
    coluna == "C8" ~ 0),
  efeito_linha_coluna = efeito_linha + efeito_coluna,
  peso = case_when(
    variedade == "A" ~  mu + efeito_linha_coluna + rnorm(8, 50, 10),
    variedade == "B" ~  mu + efeito_linha_coluna + rnorm(8, 130, 10),
    variedade == "C" ~  mu + efeito_linha_coluna + rnorm(8, 100, 10),
    variedade == "D" ~  mu + efeito_linha_coluna + rnorm(8, 0, 10),
    variedade == "E" ~  mu + efeito_linha_coluna + rnorm(8, -50, 10),
    variedade == "F" ~  mu + efeito_linha_coluna + rnorm(8, -100, 10),
    variedade == "G" ~  mu + efeito_linha_coluna + rnorm(8, -110, 10),
    variedade == "H" ~  mu + efeito_linha_coluna + rnorm(8, 10, 10))
  )

dados %>% 
  ggplot(aes(x = variedade, y = peso)) + 
  geom_boxplot()

dados %>% 
  ggplot(aes(x = coluna, y = peso)) + 
  geom_boxplot()

dados %>% 
  ggplot(aes(x = linha, y = peso)) + 
  geom_boxplot()



# Ajustando o modelo ------------------------------------------------------

# H_0: mi = mj , para todo i != j , i,j = 1,2,....,8 (variedades da cana)
# H_1: pelo menos um par diferente 

# nivel de significancia adotado: 5%

mod <- aov(peso ~ variedade + linha + coluna ,data = dados)
anova(mod)


# Análise dos resíduos ------------------------------------------------

par(mfrow=c(2,2))
plot(mod)


# Homocedasticidade, Normalidade e Independência --------------------------

residuos <- (mod$residuals)

par(mfrow=c(2,2))

names(residuos) <- NULL

# grafico dos residuos pelos pesos

plot(dados$peso,residuos)
title("Resíduos vs pesos \n Homocedasticidade")

preditos <- (mod$fitted.values)

# grafico dos residuos pelos pesos 

plot(residuos,preditos)
title("Resíduos vs pesos \n Independência")

qqnorm(residuos,ylab="Residuos", main=NULL)
qqline(residuos,col  = 2)
title("Grafico Normal de \n Probabilidade dos Resíduos")

# outra forma de verificar a normalidade graficamente

hist(residuos,prob = T)
curve(dnorm(x,mean(residuos),sd(residuos)),add = T,col = 2)
par(mfrow=c(2,1))


# analizando os graficos, podemos verificar que os residuos parecem ser 
# normalmente distribuidos

# Teste de normalidade - shapiro ------------------------------------------

# H_0:  os dados seguem uma distribuicao de probabildiade normal
# H_1: os dados nao seguem uma distribuicao de probabildiade normal

# nivel de significancia adotado: 5%
shapiro.test(residuos)

# com o p-valor > 0.05 , nao rejeitamos a hipotose de normalidade nos residuos 

# padronizando e buscando outliers nos residuos

respad <- (residuos/sqrt(anova(mod)$"Mean Sq"[4]))
boxplot(respad)
title("Resíduos Padronizados - outliers")

outlier<-c(max(respad),min(respad))
outlier



# Teste para Comparações Múltiplas ----------------------------------------



compMult <- TukeyHSD(mod, "variedade", ord=T)
compMult
plot(compMult)

# hipoteses do teste HSD de Tukey


# H_0: mu_i = mu_j , i != j
# H_1: mu_i != mu_j

# nivel de significancia adotado: 5%

aux <- as.data.frame(compMult$variedade) %>% 
  tibble::rownames_to_column("dupla")

aux  %>% 
 select(pvalor = 5, everything()) %>%  
    filter(pvalor > .05) 

# Podemos ver que nao rejitamos a hipotese nula para os pares de tratamento
# F-G e H-D. Isso siginigica que para todas as outras combinacoes de 
# variedades de cana de acucar, existe diferenca significativa no peso da 
# producao. 

duplas <- aux %>% filter(lwr > 0 , upr > 0) %>% 
    mutate(dupals_split = stringr::str_split(dupla,pattern = "-"))

k <- duplas$dupals_split

# os tratamentos da direita afetam o peso da produacao pra cima e os
# tratamentos da esquerda afetam o peso da producao pra baixo. 

# Conclusao ---------------------------------------------------------------

# Foi importante usar o delineamento por quadrado latino pois verificamos efeito da linha e da coluna 
# (o conjunto de terra onde foi plantado a cana) sobre o peso da produção de cana de acucar, como 
#indica o resultado da analise de variancia. 

