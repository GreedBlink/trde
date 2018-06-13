# trabalho 3
# delineamento em quadrados latinos

# Experimento: Comparar a produção de 8 variedades de cana-de-açúcar.
# A fim de controlar o efeito de fertilidade do solo, foi utilizado
# o delineamento em blocos, dividindo a área em linhas em colunas,
# de modo a obter uma planta por célula (linha, coluna).

# Unidade experimental: cana de acucar
# Variável resposta: 
# Níveis: variedades
# Blocos: 
# Número de blocos: 8
# Repetições por fator: 8

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

mod <- aov()
