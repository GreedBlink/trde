#http://www.wekaleamstudios.co.uk/posts/two-way-analysis-of-variance-anova/
#https://www.r-bloggers.com/design-of-experiments-%E2%80%93-block-designs/
#http://www.r-tutor.com/elementary-statistics/analysis-variance/randomized-block-design
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

ggplot(delivery.df, aes(Destino, tempo, colour = Transp)) +
  geom_point()


delivery.mod1 = aov(tempo ~ Destino+Transp, data = delivery.df)
summary(delivery.mod1)

delivery.res = delivery.df
delivery.res$M1.Fit = fitted(delivery.mod1)
delivery.res$M1.Resid = resid(delivery.mod1)

ggplot(delivery.res, aes(M1.Fit, M1.Resid, colour = Transp)) + 
  geom_point() +
  xlab("Fitted Values") + ylab("Residuals")

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

hist(delivery.res$M1.Resid, prob = T)
curve(dnorm(x, mean(delivery.res$M1.Resid), 
            sd(delivery.res$M1.Resid)), add= T)

TukeyHSD(delivery.mod1, which = "Transp")


delivery.hsd = data.frame(TukeyHSD(delivery.mod1, which = "Transp")$Transp)
delivery.hsd$Comparison = row.names(delivery.hsd)


ggplot(delivery.hsd, aes(Comparison, y = diff, ymin = lwr, ymax = upr)) +
  geom_pointrange() + 
  ylab("Difference in Mean Delivery Time by Service") +
  coord_flip()

