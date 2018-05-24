# Simulacao 2 -------------------------------------------------------------
if(require(pacman)){
  p_load(ggplot2, tidyverse, data.table)
}else{
  install.packages("pacman")
  pacman::p_load(ggplot2, tidyverse, data.table)
}

set.seed(1)

delivery_df = tibble(
  Transp = c(rep("Serviço 1", 30*4), rep("Serviço 2", 30*4),
              rep("Serviço 3", 30*4), rep("Serviço 4",30*4)),
  Destino = c(rep(c("Filial 1", "Filial 2", "Filial 3",
                    "Filial 4"), 30*4))
) %>% 
  arrange(Transp, Destino)

media_glob <- 5

nvl_1 <- media_glob + round(rnorm(120, 2.5, 0.25), 2)
nvl_2 <- media_glob + round(rnorm(120, 0, 0.25), 2)
nvl_3 <- media_glob + round(rnorm(120, -0.5, 0.25), 2)
nvl_4 <- media_glob + round(rnorm(120, 0, 0.25), 2)

tempos <- c(nvl_1, nvl_2, nvl_3, nvl_4)

delivery_df$tempo <- tempos

delivery_df <- data.table(delivery_df)

delivery_df[Destino %in% c("Filial 1", "Filial 2"), tempo := tempo + rnorm(60, 0.5, 0.1)]
delivery_df[Destino %in% c("Filial 3", "Filial 4"), tempo := tempo + rnorm(60, 1, 0.15)]

#### Análise ####

# Verificando número de observações por bloco e nível
count(delivery_df, Destino, Transp)

# Verificando tempos por blocos
delivery_df %>%
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

ggplot(delivery_df, aes(Destino, tempo, colour = Transp)) +
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
# Podemos perceber uma indicacao de que as 
# diferencas entre os servicos variam, mesmo que
# suavemente, observando que a filial 3 recebe pacotes
# com tempos ligeiramente superiores.

delivery_df %>%
  ggplot(aes(x = Transp, y = tempo)) + 
  geom_boxplot(fill = "steelblue") + 
  theme_bw() + 
  labs(x = "Serviço de transportadora", 
       y = "Tempo de entrega (em horas)",
       title = "Tempo de entrega por transportadora") + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))
# Este gráfico mostra a diferença de tempos de entrega
# por transportadora. A conclusão confirma o que foi 
# observado no gráfico de pontos acima. O serviço 3 tem os
# menores tempos, os serviços 2 e 4 são equivalentes, 
# com relação ao tempo de entrega, e o serviço 1
# apresenta tempos de entrega superiores.
# Resta verificar se é significativa a diferença de
# tempos

# Vejamos os tempos mínimos, médios e máximos por
# transportadora
delivery_df %>% 
  group_by(Transp) %>% 
  summarise(tempo_min = min(tempo),
            tempo_medio = mean(tempo),
            tempo_max = max(tempo))

### Testes

# Para todos os testes: alfa = 5%

# Normalidade: 

#H0: Dados provêm de população com distribuição normal
#H1: Dados não provêm de população com distribuição normal

# Serviço 1 e filial 1
delivery_df %>% 
  filter(Transp == "Serviço 1",
         Destino == "Filial 1") %>% 
  pull(tempo) %>% 
  shapiro.test() # Não rejeitamos H0

delivery_df %>% 
  filter(Transp == "Serviço 1", 
         Destino == "Filial 1") %>% 
  pull(tempo) %>% 
  qqnorm(pch = 20) 

delivery_df %>% 
  filter(Transp == "Serviço 1",
         Destino == "Filial 1") %>% 
  pull(tempo) %>% 
  qqline(col = "blue")

# Serviço 1 e filial 2
delivery_df %>% 
  filter(Transp == "Serviço 1",
         Destino == "Filial 2") %>% 
  pull(tempo) %>% 
  shapiro.test() # Não rejeitamos H0

delivery_df %>% 
  filter(Transp == "Serviço 1", 
         Destino == "Filial 2") %>% 
  pull(tempo) %>% 
  qqnorm(pch = 20) 

delivery_df %>% 
  filter(Transp == "Serviço 1",
         Destino == "Filial 2") %>% 
  pull(tempo) %>% 
  qqline(col = "blue")

# Serviço 1 e filial 3
delivery_df %>% 
  filter(Transp == "Serviço 1",
         Destino == "Filial 3") %>% 
  pull(tempo) %>% 
  shapiro.test() # Não rejeitamos H0

delivery_df %>% 
  filter(Transp == "Serviço 1", 
         Destino == "Filial 3") %>% 
  pull(tempo) %>% 
  qqnorm(pch = 20) 

delivery_df %>% 
  filter(Transp == "Serviço 1",
         Destino == "Filial 3") %>% 
  pull(tempo) %>% 
  qqline(col = "blue")

# Serviço 1 e filial 4
delivery_df %>% 
  filter(Transp == "Serviço 1",
         Destino == "Filial 4") %>% 
  pull(tempo) %>% 
  shapiro.test() # Não rejeitamos H0

delivery_df %>% 
  filter(Transp == "Serviço 1", 
         Destino == "Filial 4") %>% 
  pull(tempo) %>% 
  qqnorm(pch = 20) 

delivery_df %>% 
  filter(Transp == "Serviço 1",
         Destino == "Filial 4") %>% 
  pull(tempo) %>% 
  qqline(col = "blue")

# Serviço 2 e filial 1
delivery_df %>% 
  filter(Transp == "Serviço 2",
         Destino == "Filial 1") %>% 
  pull(tempo) %>% 
  shapiro.test() # Não rejeitamos H0

delivery_df %>% 
  filter(Transp == "Serviço 2", 
         Destino == "Filial 1") %>% 
  pull(tempo) %>% 
  qqnorm(pch = 20) 

delivery_df %>% 
  filter(Transp == "Serviço 2",
         Destino == "Filial 1") %>% 
  pull(tempo) %>% 
  qqline(col = "blue")

# Serviço 2 e filial 2
delivery_df %>% 
  filter(Transp == "Serviço 2",
         Destino == "Filial 2") %>% 
  pull(tempo) %>% 
  shapiro.test() # Não rejeitamos H0

delivery_df %>% 
  filter(Transp == "Serviço 2", 
         Destino == "Filial 2") %>% 
  pull(tempo) %>% 
  qqnorm(pch = 20) 

delivery_df %>% 
  filter(Transp == "Serviço 2",
         Destino == "Filial 2") %>% 
  pull(tempo) %>% 
  qqline(col = "blue")

# Serviço 2 e filial 3
delivery_df %>% 
  filter(Transp == "Serviço 2",
         Destino == "Filial 3") %>% 
  pull(tempo) %>% 
  shapiro.test() # Não rejeitamos H0

delivery_df %>% 
  filter(Transp == "Serviço 2", 
         Destino == "Filial 3") %>% 
  pull(tempo) %>% 
  qqnorm(pch = 20) 

delivery_df %>% 
  filter(Transp == "Serviço 2",
         Destino == "Filial 3") %>% 
  pull(tempo) %>% 
  qqline(col = "blue")

# Serviço 2 e filial 4
delivery_df %>% 
  filter(Transp == "Serviço 2",
         Destino == "Filial 4") %>% 
  pull(tempo) %>% 
  shapiro.test() # Não rejeitamos H0

delivery_df %>% 
  filter(Transp == "Serviço 2", 
         Destino == "Filial 4") %>% 
  pull(tempo) %>% 
  qqnorm(pch = 20) 

delivery_df %>% 
  filter(Transp == "Serviço 2",
         Destino == "Filial 4") %>% 
  pull(tempo) %>% 
  qqline(col = "blue")

# Serviço 3 e filial 1
delivery_df %>% 
  filter(Transp == "Serviço 3",
         Destino == "Filial 1") %>% 
  pull(tempo) %>% 
  shapiro.test() # Não rejeitamos H0

delivery_df %>% 
  filter(Transp == "Serviço 3", 
         Destino == "Filial 1") %>% 
  pull(tempo) %>% 
  qqnorm(pch = 20) 

delivery_df %>% 
  filter(Transp == "Serviço 3",
         Destino == "Filial 1") %>% 
  pull(tempo) %>% 
  qqline(col = "blue")

# Serviço 3 e filial 2
delivery_df %>% 
  filter(Transp == "Serviço 3",
         Destino == "Filial 2") %>% 
  pull(tempo) %>% 
  shapiro.test() # Não rejeitamos H0

delivery_df %>% 
  filter(Transp == "Serviço 3", 
         Destino == "Filial 2") %>% 
  pull(tempo) %>% 
  qqnorm(pch = 20) 

delivery_df %>% 
  filter(Transp == "Serviço 3",
         Destino == "Filial 2") %>% 
  pull(tempo) %>% 
  qqline(col = "blue")

# Serviço 3 e filial 3
delivery_df %>% 
  filter(Transp == "Serviço 3",
         Destino == "Filial 3") %>% 
  pull(tempo) %>% 
  shapiro.test() # Não rejeitamos H0

delivery_df %>% 
  filter(Transp == "Serviço 3", 
         Destino == "Filial 3") %>% 
  pull(tempo) %>% 
  qqnorm(pch = 20) 

delivery_df %>% 
  filter(Transp == "Serviço 3",
         Destino == "Filial 3") %>% 
  pull(tempo) %>% 
  qqline(col = "blue")

# Serviço 3 e filial 4
delivery_df %>% 
  filter(Transp == "Serviço 3",
         Destino == "Filial 4") %>% 
  pull(tempo) %>% 
  shapiro.test() # Não rejeitamos H0

delivery_df %>% 
  filter(Transp == "Serviço 3", 
         Destino == "Filial 4") %>% 
  pull(tempo) %>% 
  qqnorm(pch = 20) 

delivery_df %>% 
  filter(Transp == "Serviço 3",
         Destino == "Filial 4") %>% 
  pull(tempo) %>% 
  qqline(col = "blue")

# Serviço 4 e filial 1
delivery_df %>% 
  filter(Transp == "Serviço 4",
         Destino == "Filial 1") %>% 
  pull(tempo) %>% 
  shapiro.test() # Não rejeitamos H0

delivery_df %>% 
  filter(Transp == "Serviço 4", 
         Destino == "Filial 1") %>% 
  pull(tempo) %>% 
  qqnorm(pch = 20) 

delivery_df %>% 
  filter(Transp == "Serviço 4",
         Destino == "Filial 1") %>% 
  pull(tempo) %>% 
  qqline(col = "blue")

# Serviço 4 e filial 2
delivery_df %>% 
  filter(Transp == "Serviço 4",
         Destino == "Filial 2") %>% 
  pull(tempo) %>% 
  shapiro.test() # Não rejeitamos H0

delivery_df %>% 
  filter(Transp == "Serviço 4", 
         Destino == "Filial 2") %>% 
  pull(tempo) %>% 
  qqnorm(pch = 20) 

delivery_df %>% 
  filter(Transp == "Serviço 4",
         Destino == "Filial 2") %>% 
  pull(tempo) %>% 
  qqline(col = "blue")

# Serviço 4 e filial 3
delivery_df %>% 
  filter(Transp == "Serviço 4",
         Destino == "Filial 3") %>% 
  pull(tempo) %>% 
  shapiro.test() # Não rejeitamos H0

delivery_df %>% 
  filter(Transp == "Serviço 4", 
         Destino == "Filial 3") %>% 
  pull(tempo) %>% 
  qqnorm(pch = 20) 

delivery_df %>% 
  filter(Transp == "Serviço 4",
         Destino == "Filial 3") %>% 
  pull(tempo) %>% 
  qqline(col = "blue")

# Serviço 4 e filial 4
delivery_df %>% 
  filter(Transp == "Serviço 4",
         Destino == "Filial 4") %>% 
  pull(tempo) %>% 
  shapiro.test() # Não rejeitamos H0

delivery_df %>% 
  filter(Transp == "Serviço 4", 
         Destino == "Filial 4") %>% 
  pull(tempo) %>% 
  qqnorm(pch = 20) 

delivery_df %>% 
  filter(Transp == "Serviço 4",
         Destino == "Filial 4") %>% 
  pull(tempo) %>% 
  qqline(col = "blue")

# Considerando nível de significância de 5%, não temos evidências
# estatísticas para rejeitar H0. Logo, não rejeitamos a hipótese
# de normalidade

#### Teste de homocedasticidade 

# H0: sigma_1 = sigma_2 = sigma_3 = sigma_4
# H1: as variâncias não são todas iguais.

bartlett.test(tempo~Transp, data = delivery_df)

# Considerando alfa = 5%, não temos evidênicias para rejeitar a hipótese
# de que todas as variâncias são iguais. 

# ANOVA

delivery_mod1 <- aov(tempo ~ Destino+Transp, data = delivery_df)
summary(delivery_mod1)

# Considerando p-valor de 5%, temos evidênicias de que há diferença 
# entre os servicos de entrega entre as quatro filiais.
# Também podemos perceber que foi importante considerar os blocos na 
# análise, já que as diferentes filiais levam a diferentes tempos de entrega

# Diagnostico do modelo -------------------------------------------------

delivery_res <- delivery_df
delivery_res$M1.Fit <- fitted(delivery_mod1)
delivery_res$M1.Resid <- resid(delivery_mod1)

# Grafico do modelo (residuos ~ fitteds values) -------------------------

ggplot(delivery_res, aes(M1.Fit, M1.Resid, colour = Transp)) + 
  geom_point() +
  xlab("Fitted Values") + ylab("Residuals")

# separando os residuos por destino, para uma visualizacao diferente

ggplot(delivery_res, aes(M1.Fit, M1.Resid, colour = Transp)) +
  geom_point() + 
  xlab("Fitted Values") + 
  ylab("Residuals") +
  facet_wrap( ~ Destino)

ggplot(delivery_res, aes(M1.Fit, M1.Resid, colour = Destino)) +
  geom_point() + 
  xlab("Fitted Values") + 
  ylab("Residuals") +
  facet_wrap( ~ Transp)


ggplot(delivery_res, aes(sample = M1.Resid)) + 
  stat_qq()


hist(delivery_res$M1.Resid, prob = T,xlim = c(-1,1.5))
curve(dnorm(x, mean(delivery_res$M1.Resid), 
            sd(delivery_res$M1.Resid)), add= T,col = 2)


# Os dois gráficos acima mostram o comportamento que esperamos observar se os dados são 
# gerados ou aproximados de uma distribuicao normal, porque
# os resíduos seguem distribuição normal


# Teste de comparacoes  multiplas -----------------------------------------
# ajustando nível de significância:
# alfa* = 5%/6 = 0.83%

TukeyHSD(delivery_mod1, which = "Transp")
# temos fortes evidencias das diferencas que observamos anteriormente
# Como já foi observado na análise descritiva, 
# os únicos serviços que não apresentam diferença significativa
# no tempo médio de entrega são os serviços 2 e 4

# adicionando as comparacoes
delivery_hsd <- data.frame(TukeyHSD(delivery_mod1, which = "Transp")$Transp)
delivery_hsd$Comparison = row.names(delivery.hsd)


ggplot(delivery_hsd, aes(Comparison, y = diff, ymin = lwr, ymax = upr)) +
  geom_pointrange() + 
  ylab("Diferença da média de tempo de entrega por serviço") +
  xlab("Comparação") +
  coord_flip()
# Nessa visualização confirmamos o que foi concluído no teste de comparações
# múltiplas: Com exceção da comparação entre os serviços 4 e 2,
# todas as combinações de serviços apresentam diferença significativa 
# de tempo de entrega.

#### Conclusão ####

# Sendo assim, escolheremos o serviço que obteve menor 
# tempo médio de entrega, que é o serviço 3.
# NEste cenário em que todas as transportadoras cobram
# um preço muito similar, então a transportadora que entrega
# os pacotes mais rápido deve ser a escolhida.

# Nota-se a importância de ter havido a divisão em blocos, 
# já que foi identificado que os blocos apresentam diferenças
# em relação aos tempos de viagem, e, ao considerar essas diferenças
# no modelo, pudemos obter conclusões mais acertivas.


