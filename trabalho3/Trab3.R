# Trabalho 3 - Jonatha Azevedo e Leonardo Filgueira

# Delineamento em quadrados latinos

# Experimento: Comparar a produção de 8 variedades de cana-de-açúcar.
# A fim de controlar o efeito de fertilidade do solo, foi utilizado
# o delineamento em quadrados latinos, dividindo a área em linhas em colunas,
# de modo a obter uma planta por quadrado (linha, coluna).

# Unidade experimental: cana de acucar
# Variável resposta: peso da producao
# Fator: variedades da cana de acucar
# Níveis: A, B, C, D, E, F, G, H

require(dplyr)
require(tidyr)
require(ggplot2)


# Simulação ---------------------------------------------------------------

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
# Podemos perceber que as diferentes variedades apresentam diferentes 
# distribuições de peso da produção

dados %>% 
  ggplot(aes(x = coluna, y = peso)) + 
  geom_boxplot()
# Percebemos que as colunas dos quadrados parecem interferir na produção

dados %>% 
  ggplot(aes(x = linha, y = peso)) + 
  geom_boxplot()
# Assim também as linhas dos quarados parecem interferir no peso produzido.


# Ajustando o modelo ------------------------------------------------------

#Modelo:
# Y_(i,j)[k] = mu + L_i + C_j + tau_k + erro_(i,j)[k]
# Onde Y_(i,j)[k] é o valor da variável resposta na i-ésima linha,
# j-ésima coluna, sob o efeito do nível k do fator.
# mu é a média geral, L_i é o efeito da i-ésima linha,
# C_j é o efeito da j-ésima coluna,
# tau_k é o efeito do k-ésimo nível do fator e 
# erro_(i,j)[k] é o erro aleatório, sendo erro_(i,j)[k] ~ N(0, sigma^2)

# H_0: mi = mj , para todo i != j , i,j = 1,2,....,8 (variedades da cana)
# H_1: pelo menos um par diferente 

# nivel de significancia adotado: 5%

mod <- aov(peso ~ variedade + linha + coluna, data = dados)
anova(mod)

# Como o p-valor é menor que o nível de significância, temos evidências 
# estatísticas para rejeitar H_0. Além disso, percebemos que a 
# região onde a cana foi plantada também infuencia na produção.

# Análise dos resíduos ------------------------------------------------

par(mfrow=c(2,2))
plot(mod)

# Homocedasticidade, Normalidade e Independência --------------------------

residuos <- (mod$residuals)

par(mfrow=c(2,2))

# grafico dos residuos pelos pesos

plot(dados$peso,residuos)
title("Resíduos vs pesos \n Homocedasticidade")

preditos <- (mod$fitted.values)

# grafico dos residuos pelos pesos 

plot(residuos,preditos)
title("Resíduos vs pesos \n Independência")

qqnorm(residuos,ylab="Residuos", main=NULL)
qqline(residuos,col  = 2)
title("Qqplot dos resíduos")

# outra forma de verificar a normalidade graficamente

hist(residuos,prob = T)
curve(dnorm(x,mean(residuos),sd(residuos)),add = T,col = 2)
par(mfrow=c(2,1))


# analizando os graficos, podemos verificar que os residuos parecem ser 
# normalmente distribuidos e independentes

# Teste de normalidade - shapiro ------------------------------------------

# H_0:  os dados vêm de uma população com distribuicao normal
# H_1: os dados nao vêm de uma população com distribuicao normal

# nivel de significancia adotado: 5%
shapiro.test(residuos)

# com o p-valor > 0.05 , nao rejeitamos a hipotose de normalidade nos residuos 

# padronizando e buscando outliers nos residuos
dev.off()
respad <- (residuos/sqrt(anova(mod)$"Mean Sq"[4]))
boxplot(respad)
title("Resíduos Padronizados")

outlier<-c(max(respad),min(respad))
outlier


# Teste para Comparações Múltiplas ----------------------------------------

# Hipoteses do teste HSD de Tukey
# H_0: mu_i = mu_j , i != j
# H_1: mu_i != mu_j

# Nível de significância ajustado: 5%/28 = 0.001785714

compMult <- TukeyHSD(mod, "variedade", ord = T)
compMult
plot(compMult) # Como há muitas comparações, o gráfico acaba não sendo
# tão informativo

aux <- as.data.frame(compMult$variedade) %>% 
  tibble::rownames_to_column("dupla")

aux  %>% 
 select(dupla, pvalor = 5, everything()) %>%  
    filter(pvalor > .05) 

# Podemos ver que nao rejitamos a hipotese nula para os pares de tratamento
# F-G e H-D. Isso siginigica que para todas as outras combinacoes de 
# variedades de cana de acucar, existe diferenca significativa no peso da 
# producao. Já entre as variedades F e G e entre as variedades H e D não 
# existe diferença significativa no peso da produção.

duplas <- aux %>%
  filter(lwr > 0 , upr > 0) %>% 
  mutate(duplas_split = stringr::str_split(dupla,pattern = "-"))

k <- duplas$duplas_split

# os tratamentos da esquerda afetam o peso da producao de maneira que
# o peso é maior que o peso da producao dos da direita

# Conclusao ---------------------------------------------------------------

# Foi importante usar o delineamento por quadrado latino pois verificamos efeito da linha e da coluna 
# (o conjunto de terra onde foi plantado a cana) sobre o peso da produção de cana de acucar, como 
# indica o resultado da analise de variancia. 

# Por fim, podemos concluir que diferentes variedades de cana de acucar
# levam a diferentes pesos da produção, com exceção da comparação entre
# as variedades F-G e entre as variedades H-D.