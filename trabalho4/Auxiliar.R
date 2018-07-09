teste <- PostHocTest(modelo, method = "duncan") # modelo é aov()

p.adjust(teste$Espec[, 4], method = "bonferroni")

teste_mod <- lapply(teste, function(x){p.adjust(x[, 4], method = "bonferroni")})

lapply(teste_mod, function(x){x < 0.05}))
unlist(teste_mod)[unlist(teste_mod) < 0.05]
names(unlist(teste_mod)[unlist(teste_mod) < 0.05])
