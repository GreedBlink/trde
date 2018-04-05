#### Banco de dados #####

require(dplyr)

pacientes <- tibble(
  id = 1:400,
  grupo = paste("Grupo", rep(1:4, each = 100)),
  dose = rep(c("Nenhuma", "Padrão", "Sobredose média", "Sobredose alta"), each = 100)
)

set.seed(26)
diferenca <- round(c(rnorm(100, 0, 0.3), rnorm(100, 2.5, 0.3), 
                     rnorm(100, 7, 0.3), rnorm(100, 1, 0.3)), 4)
pacientes <- pacientes %>% 
  bind_cols(as_tibble(diferenca)) %>% 
  rename(diferenca = value)

write.csv2(pacientes, file = "dados_jonatha_leonardo.csv", row.names = F,
           fileEncoding = "UTF-8")
