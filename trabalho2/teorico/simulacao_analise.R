# Simulacao 2 -------------------------------------------------------------
if(require(pacman)){
  p_load(ggplot2, tidyverse, data.table)
}else{
  install.packages("pacman")
  pacman::p_load(ggplot2, tidyverse, data.table)
}

set.seed(1)

delivery.df = tibble(
  Transp = c(rep("Serviço 1", 30), rep("Serviço 2", 30),
              rep("Serviço 3", 30), rep("Serviço 4",30)),
  Destino = c(rep(c("Filial 1", "Filial 2", "Filial 3",
                    "Filial 4"),30))
) %>% 
  arrange(Transp, Destino)

media_glob <- 5

nvl_1 <- round(rnorm(30, 2.5, 0.25), 2)
nvl_2 <- round(rnorm(30, 0, 0.25), 2)
nvl_3 <- round(rnorm(30, -1.5, 0.25), 2)
nvl_4 <- round(rnorm(30, 0, 0.25), 2)

tempos <- c(nvl_1, nvl_2, nvl_3, nvl_4)

delivery.df$tempo <- tempos

delivery.df <- data.table(delivery.df)

delivery.df[Destino %in% c("Filial 1", "Filial 2"), tempo := tempo + rnorm(60, 0.5, 0.1)]
delivery.df[Destino %in% c("Filial 3", "Filial 4"), tempo := tempo + rnorm(60, 1, 0.15)]

#### Análise ####

# Verificando número de observações por bloco
count(delivery.df, Destino)

# Verificando tempos por blocos
delivery.df %>%
  ggplot(aes(x = Destino, y = tempo)) + 
  geom_boxplot(fill = "steelblue") + 
  theme_bw() + 
  labs(x = "Filial", y = "Tempo de entrega (em horas)",
       title = "Tempo de entrega por filiais") + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))
# Neste gráfico podemos verificar se os blocos propostos
# (filiais) levam a uma diferença no tempo de entrega.
# As distribuições são diferentes, então vamos considerar
# os blocos na ANOVA.

# De qualquer forma, como temos um dataset bem pequeno
# não haverá problema de memória ou desempenho
# ao rodar a anova dessa maneira

# No gráfico de dispersão podemos tentar verificar alguma 
# diferença entre os tempos de entrega

ggplot(delivery.df, aes(Destino, tempo, colour = Transp)) +
  geom_point() + 
  labs(y = "Tempo (horas)",
       title = "Tempo de entrega de pacotes para filiais por transportadora") + 
  scale_color_discrete(name = "Serviço de \ntransportadora")

# Neste gráfico fica claro que o serviço 1 apresenta
# um serviço de entrega mais lento. 
# Por outro lado, o serviço 3 apresenta entregas muito rápidas,
# com as entregas realizadas em menos de uma hora.
# Já os serviços 2 e 4 apresentam tempos 
# parecidos de entrega.


# Neste gráfico podemos perceber uma indicacao de que as 
# diferencas entre os servicos variam, mesmo que
# suavemente, observando que a filial 3 recebe pacotes
# com tempos ligeiramente superiores.
# ANOVA.


# Para todos os testes: alfa = 5%

delivery.df %>% 
  filter(Transp == "Serviço 1") %>% 
  pull(tempo) %>% 
  shapiro.test()

delivery.df %>% 
  filter(Transp == "Serviço 1") %>% 
  pull(tempo) %>% 
  qqnorm(pch = 20) 

delivery.df %>% 
  filter(Transp == "Serviço 1") %>% 
  pull(tempo) %>% 
  qqline(col = "blue")

delivery.df %>% 
  filter(Transp == "Serviço 2") %>% 
  pull(tempo) %>%
  shapiro.test()


delivery.df %>% 
  filter(Transp == "Serviço 2") %>% 
  pull(tempo) %>% 
  qqnorm(pch = 20) 
delivery.df %>% 
  filter(Transp == "Serviço 2") %>% 
  pull(tempo) %>%
  qqline(col = "blue")



delivery.df %>% 
  filter(Transp == "Serviço 3") %>% 
  pull(tempo) %>%
  shapiro.test()

delivery.df %>% 
  filter(Transp == "Serviço 3") %>% 
  pull(tempo) %>%
  qqnorm(pch = 20) 

delivery.df %>% 
  filter(Transp == "Serviço 3") %>% 
  pull(tempo) %>%
  qqline(col = "blue")


delivery.df %>% 
  filter(Transp == "Serviço 4") %>% 
  pull(tempo) %>%
  shapiro.test()

delivery.df %>% 
  filter(Transp == "Serviço 4") %>% 
  pull(tempo) %>%
  qqnorm(pch = 20) 

delivery.df %>% 
  filter(Transp == "Serviço 4") %>% 
  pull(tempo) %>%
  qqline(col = "blue")


# Normalidade não rejeitada 


#### Teste de homocedasticidade ####

# H0: sigma_1 = sigma_2 = sigma_3 = sigma_4
# H1: as variâncias não são todas iguais.

bartlett.test(tempo~Destino, data = delivery.df)

# Considerando alfa = 5%, não temos evidênicias para rejeitar a hipótese
# de que todas as variâncias são iguais. 

delivery.mod1 = aov(tempo ~ Destino+Transp, data = delivery.df)
summary(delivery.mod1)

# temos fortes evidencias de que ha diferenca entre os servicos de entrega entre as quarto filiais
#

# Diagnostico do modelo ---------------------------------------------------


delivery.res = delivery.df
delivery.res$M1.Fit = fitted(delivery.mod1)
delivery.res$M1.Resid = resid(delivery.mod1)


# Grafico do modelo (residuos ~ fitteds values) ---------------------------


ggplot(delivery.res, aes(M1.Fit, M1.Resid, colour = Transp)) + 
  geom_point() +
  xlab("Fitted Values") + ylab("Residuals")

# separando os residuos por destino, para uma visualizacao diferente
ggplot(delivery.res, aes(M1.Fit, M1.Resid, colour = Transp)) +
  geom_point() + 
  xlab("Fitted Values") + 
  ylab("Residuals") +
  facet_wrap( ~ Destino)

ggplot(delivery.res, aes(M1.Fit, M1.Resid, colour = Transp)) +
  geom_point() +
  xlab("Fitted Values") + 
  ylab("Residuals")

ggplot(delivery.res, aes(M1.Fit, M1.Resid, colour = Transp)) +
  geom_point() + 
  xlab("Fitted Values") + 
  ylab("Residuals") +
  facet_wrap( ~ Destino)

ggplot(delivery.res, aes(M1.Fit, M1.Resid, colour = Destino)) +
  geom_point() + 
  xlab("Fitted Values") + 
  ylab("Residuals") +
  facet_wrap( ~ Transp)


ggplot(delivery.res, aes(sample = M1.Resid)) + 
  stat_qq()


hist(delivery.res$M1.Resid, prob = T,xlim = c(-1,1.5))
curve(dnorm(x, mean(delivery.res$M1.Resid), 
            sd(delivery.res$M1.Resid)), add= T,col = 2)


# O grafico acima e perto do comportamento que esperamos observar se os dados são 
# gerados ou aproximados de uma distribuicao normal.


# Teste de comparacoes  multiplas -----------------------------------------


TukeyHSD(delivery.mod1, which = "Transp")
# temos fortes evidencias muito forte das diferencas que observamos anteriormente

# adicionando as comparacoes
delivery.hsd = data.frame(TukeyHSD(delivery.mod1, which = "Transp")$Transp)
delivery.hsd$Comparison = row.names(delivery.hsd)


ggplot(delivery.hsd, aes(Comparison, y = diff, ymin = lwr, ymax = upr)) +
  geom_pointrange() + 
  ylab("Diferença da média de tempo de entrega por serviço") +
  coord_flip()

