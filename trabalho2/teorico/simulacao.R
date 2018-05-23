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

count(delivery.df, Destino)
count(delivery.df, Transp)

# No grafico de dispersao podemos tentar verificar alguma tendencia nos tempos de entrega

ggplot(delivery.df, aes(Destino, tempo, colour = Transp)) +
  geom_point()





#no grafico, percebemos que o servico 3 tem o menor tempo de entrega dentre todos e que o servico 
# 1 tem o pior tempo. E podemos perceber uma indicacao de que as diferencas entre os servicos 
# variam para as quatro filiais e podemos esperar que a interacao seja significativa no modelo
# ANOVA.

#### Teste de homocedasticidade ####

# H0: sigma_1 = sigma_2 = sigma_3 = sigma_4
# H1: as variâncias não são todas iguais.

bartlett.test(tempo~Destino, data = delivery.df)

# Considerando alfa = 5%, não temos evidênicias para rejeitar a hipótese
# de que todas as variâncias são iguais. 




# Considerando alfa = 5%


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

