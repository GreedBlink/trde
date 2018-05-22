#http://www.wekaleamstudios.co.uk/posts/two-way-analysis-of-variance-anova/
#https://www.r-bloggers.com/design-of-experiments-%E2%80%93-block-designs/
# Simulacao 2 -------------------------------------------------------------
if(require(pacman)){
  p_load(ggplot2,tidyverse)
}else{
  install.packages("pacman")
  pacman::p_load(ggplot2,tidyverse)
}

delivery.df = tibble(
  Service = c(rep("Pacote 1", 15), rep("Pacote 2", 15),
              rep("Pacote 3", 15), rep("Pacote 4",15)),
  Destino = c(rep(c("Filial 1", "Filial 2", "Filial 3",
                    "Filial 4", "Filial 5","Filial 6"),10)),
  Time = c(15.23, 14.32, 14.77, 15.12, 14.05,
           15.48, 14.13, 14.46, 15.62, 14.23, 15.19, 14.67, 14.48, 15.34, 14.22,
           16.66, 16.27, 16.35, 16.93, 15.05, 16.98, 16.43, 15.95, 16.73, 15.62,
           16.53, 16.26, 15.69, 16.97, 15.37, 17.12, 16.65, 15.73, 17.77, 15.52,
           16.15, 16.86, 15.18, 17.96, 15.26, 16.36, 16.44, 14.82, 17.62, 15.04,
           16.98, 16.93, 15.48, 17.12, 16.65, 15.73, 15.95, 17.96, 16.36, 14.05,
           16.43, 15.95, 16.73, 15.62, 16.53)
)


ggplot(delivery.df, aes(Time, Destino, colour = Service)) + geom_point()


delivery.mod1 = aov(Time ~ Destino*Service, data = delivery.df)
summary(delivery.mod1)



delivery.res = delivery.df
delivery.res$M1.Fit = fitted(delivery.mod1)
delivery.res$M1.Resid = resid(delivery.mod1)

ggplot(delivery.res, aes(M1.Fit, M1.Resid, colour = Service)) + geom_point() +
  xlab("Fitted Values") + ylab("Residuals")

ggplot(delivery.res, aes(M1.Fit, M1.Resid, colour = Service)) +
  geom_point() + xlab("Fitted Values") + ylab("Residuals") +
  facet_wrap( ~ Destino)

ggplot(delivery.res, aes(M1.Fit, M1.Resid, colour = Service)) + geom_point() +
  xlab("Fitted Values") + ylab("Residuals")

ggplot(delivery.res, aes(M1.Fit, M1.Resid, colour = Service)) +
  geom_point() + xlab("Fitted Values") + ylab("Residuals") +
  facet_wrap( ~ Destino)

ggplot(delivery.res, aes(M1.Fit, M1.Resid, colour = Destination)) +
  geom_point() + xlab("Fitted Values") + ylab("Residuals") +
  facet_wrap( ~ Service)


ggplot(delivery.res, aes(sample = M1.Resid)) + stat_qq()

TukeyHSD(delivery.mod1, which = "Service")


delivery.hsd = data.frame(TukeyHSD(delivery.mod1, which = "Service")$Service)
delivery.hsd$Comparison = row.names(delivery.hsd)


ggplot(delivery.hsd, aes(Comparison, y = diff, ymin = lwr, ymax = upr)) +
  geom_pointrange() + ylab("Difference in Mean Delivery Time by Service") +
  coord_flip()

