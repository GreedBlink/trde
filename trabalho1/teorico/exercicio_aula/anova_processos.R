require(dplyr)

n <- 5

equipe <- as.factor(rep(LETTERS[1:3], each = 5))

processos <- c(550, 568, 537, 541, 553, 540, 579, 553, 545, 599,
               528, 571, 530, 510, 492)

dados <- tibble(equipe, processos)

# Visualizando 
boxplot(processos~equipe, data = dados)

# Testando hipótese de normalidade para cada equipe

# H_0: Dados provém de população com distribuição normal
# H_1: Dados não provém de população com distribuição normal
# nível de significância alfa = 5%

dados %>% 
  filter(equipe == "A") %>% 
  pull(processos) %>% 
  shapiro.test()

dados %>% 
  filter(equipe == "B") %>% 
  pull(processos) %>% 
  shapiro.test()

dados %>% 
  filter(equipe == "C") %>% 
  pull(processos) %>% 
  shapiro.test()

# Para todos os 3 grupos, temos um p-valor > alfa.
# Logo, sob um nível de significância de 5%, não temos
# evidências estatísticas para rejeitar H0.
# Então podemos supor que os dados provêm de uma população
# com distribuição normal.

### Testando homocedasticidade.
# Teste de Bartlett

# H_0: sigma_1 = sigma_2 = sigma_3
# H_1: Variância de algum grupo diferente.
bartlett.test(processos~equipe, data = dados)

# Sob um nível de significância de 5% não temos 
# evidências para dizer que as variâncias dos grupos não são iguais.

### ANOVA

# H0: mu_1 = mu_2 = mu_3
# H1: mu_i != mu_j, i, j = 1,2,3

modelo <- aov(processos~equipe, data = dados)
anova(modelo)

# Sob um nivel de significância de 5%, nao há evidências estatísticas
# para rejeitar H0. 
# Então não podemos dizer que há diferença significativa no número
# médio de processos analisados entre as 3 equipes.
