require(dplyr)
require(ggplot2)
require(stringr)

base <- read.csv("trabalho1/estatistico/anliseagrsim/base_3.csv",
                 stringsAsFactors = F)

base <- base %>% 
  as_tibble()

boxplot(pesos~ind, data = base)

base %>% 
  mutate(ind = factor(ind, 
                      labels = c("Sem agrotóxico", "Metade da dose atualmente aplicada",
                                 "Dose atualmente aplicada"))) %>% 
  ggplot(aes(x = ind, y = pesos)) + 
  geom_boxplot(fill = "steelblue") + 
  theme_bw() + 
  labs(x = "Uso de agrotóxico", y = "Peso das laranjas") + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))

base %>% 
  mutate(ind = factor(ind, 
                      labels = c("Sem agrotóxico", "Metade da dose atualmente aplicada",
                                 "Dose atualmente aplicada"))) %>% 
  ggplot(aes(x = pesos, fill = ind)) + 
  geom_histogram(binwidth=.7, alpha=0.5, position="identity") + 
  theme_bw()

# Para todos os testes: alfa = 5%

base %>% 
  filter(ind == 1) %>% 
  pull(pesos) %>% 
  shapiro.test()

base %>% 
  filter(ind == 1) %>% 
  pull(pesos) %>% 
  qqnorm(pch = 20) 

base %>% 
  filter(ind == 1) %>% 
  pull(pesos) %>% 
  qqline(col = "blue")

base %>% 
  filter(ind == 2) %>% 
  pull(pesos) %>% 
  shapiro.test()

base %>% 
  filter(ind == 2) %>% 
  pull(pesos) %>% 
  qqnorm(pch = 20) 
base %>% 
  filter(ind == 2) %>% 
  pull(pesos) %>% 
  qqline(col = "blue")

base %>% 
  filter(ind == 3) %>% 
  pull(pesos) %>% 
  shapiro.test()

base %>% 
  filter(ind == 3) %>% 
  pull(pesos) %>% 
  qqnorm(pch = 20) 

base %>% 
  filter(ind == 3) %>% 
  pull(pesos) %>% 
  qqline(col = "blue")

# Normalidade rejeitada para o grupo 2 (sob alfa = 5%). 
# Temos, então, que a hipótese de normalidade da ANOVA foi violada.

# Abordagem não paramétrica

kruskal.test(pesos~ind, data = base)
# Algum par de grupos tem distribuições diferentes

wilcox.test(base$pesos[base$ind == 1], base$pesos[base$ind == 2])
# dist não diferem
wilcox.test(base$pesos[base$ind == 1], base$pesos[base$ind == 3])
# dist diferem
wilcox.test(base$pesos[base$ind == 2], base$pesos[base$ind == 3])
# dist diferem

# Então temos diferença significativa entre os grupos 1 e 3 e entre 2 e 3.
# Então podemos dizer que há diferença significativa do peso das laranjas com dose 
# atualmente aplicada de agrotóxicos, em relação às laranjas sem agrotóxicos 
# e com a metade da dose atualmente aplicada.


# modelo <- aov(pesos~ind, data = base)
# summary(modelo)
# anova(modelo)
# plot(modelo)
