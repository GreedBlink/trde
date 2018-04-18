#### Fazendo anova nos nossos dados simulados ####

dados <- read.csv2("dados_jonatha_leonardo.csv", fileEncoding = "UTF-8") %>% 
  as_tibble()

summary(dados)
boxplot(diferenca~grupo, data = dados)
# Verificando normalidade

# H0: dados vêm de população com distribuição normal
# H1: dados não vêm de população com distribuição normal
# Considerando alfa = 5%
dados %>% 
  filter(grupo == "Grupo 1") %>% 
  pull(diferenca) %>% 
  shapiro.test()

dados %>% 
  filter(grupo == "Grupo 2") %>% 
  pull(diferenca) %>% 
  shapiro.test()

dados %>% 
  filter(grupo == "Grupo 3") %>% 
  pull(diferenca) %>% 
  shapiro.test()

dados %>% 
  filter(grupo == "Grupo 4") %>% 
  pull(diferenca) %>% 
  shapiro.test()
# Para todos os grupos, considerando um nível de significância de 5%,
# não temos evidências para rejeitar H0. Logo, podemos supor que
# os dados vêm de população que segue distribuição normal.


#### Teste de homocedasticidade ####

# H0: sigma_1 = sigma_2 = sigma_3 = sigma_4
# H1: as variâncias não são todas iguais.

bartlett.test(diferenca~grupo, data = dados)

# Considerando alfa = 5%, não temos evidênicias para rejeitar a hipótese
# de que todas as variâncias são iguais. 

# Temos, então, as suposições necessárias para aplicar a 
### ANOVA

modelo <- aov(diferenca~grupo, data = dados)
anova(modelo)

# Considerando um nível de significância de 5%, temos evidências para
# rejeitar H0. Isso singifica que a sobredose de vitamina B12
# influenciou na diferença do diâmetro das artérias.

# De acordo com o boxplot do início do script, os pacientes que 
# receberam sobredose média tiveram diferenças do diâmetro da artéria 
# depois do tratamento maior, em relação ao diâmetro da artéria antes.

# Comparações múltiplas
TukeyHSD(modelo, ordered = T)
# Para todos os pares de grupos, há diferença significativa de médias
