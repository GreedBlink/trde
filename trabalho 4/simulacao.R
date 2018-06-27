# Jonatha Azevedo e Leonardo Filgueira
# Simulacao do ultimo trabalho de delineamento de experimentos

set.seed(1)


# Contextualizacao --------------------------------------------------------

# Experimento sobre cerâmica de alto desempenho com o objetivo de caracterizar o 
# efeito de parâmetros de moagem sobre nitreto de silício sinterizado por reação, 
#

#nitreto de silicone ligado a reação e nitreto de silício sinterizado.


# Objetivo: Determinar o efeito de fatores de usinagem na resistência cerâmica
# Variável de resposta = média (mais de 15 repetições) da resistência cerâmica
# Número de observações = 30 (um planejamento fatorial completo 25)
 

#Fator A = Velocidade da Mesa (2 níveis: lento (.025 m / s), normal (.075 m / s) e rápido (.125 m / s))
#Fator B = Down Feed Rate (2 níveis: lento (0,05 mm) e rápido (0,125 mm))
#Fator aux1 = Grão de Roda (2 níveis: 140/170 e 80/100)
#Fator aux2 = Direção (2 níveis: longitudinal e transversal)
#Fator C = Lote (4 níveis: 1, 2, 3 e 4)
#Como dois fatores eram qualitativos (direção e lote) e era razoável esperar 
# efeitos monótonos dos fatores quantitativos, nenhuma corrida ao ponto central 
#foi incluída.


# Simulacao dos dados -----------------------------------------------------


