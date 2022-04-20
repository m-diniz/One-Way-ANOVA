### One-Way ANOVA
### Marcos Diniz
### Abril de 2022

# A análise de variância está interessada em testar se os resultados quanto a 
# UMA variável de interesse (por isso, one-way) diferem significativamente 
# entre grupos.

# Por que não calcular uma média somente? Pois temos que considerar também a
# dispersão dos dados. E é isso que ANOVA faz: verifica se a diferença das
# médias pode ou não ser explicada pela dispersão dos dados no grupo.


### Pacotes -----------------------------------------------------------------
rm(list = ls(all = TRUE))

library(tidyverse)
library(gplots)

### Dados -------------------------------------------------------------------
my_data <- PlantGrowth

# Amostra aleatória
set.seed(1234)
sample_n(my_data, 10)

# Mostra os grupos dos dados
levels(my_data$group)


# Reordena os grupos
my_data$group <- ordered(my_data$group,
                         levels = c("ctrl", "trt1", "trt2"))

# Estatísticas básicas
group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE)
  )

# Plota box-plot
boxplot(weight ~ group, data = my_data,
        xlab = "Treatment", ylab = "Weight",
        frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07"))


### Passo-a-passo: ----------------------------------------------------------
# Divide a variância entre os grupos com a variância dentro dos grupos

### Soma dos quadrados entre grupos


# Primeiro, precisamos da média de cada grupo
mu.group <- group_by(my_data, group) %>% 
  summarise(mean = mean(weight),
            obs = n())

# Soma dos quadrados entre grupos = desvio médio de cada grupo * número de
# observações
mu <- mean(mu.group$mean)
SQentre <- sum( ( mu.group$obs * ((mu.group$mean - mu)^2) ) )

### Soma dos quadrados dentro dos grupos
SQdentro <- group_by(my_data, group) %>% 
  summarise(variance = sum((weight - mean(weight))^2))

SQdentro <- sum(SQdentro$variance)


### Soma dos quadrados entre e dentro dos grupos deve ser igual a var total
SQT <- round( sum( (my_data$weight - mean(my_data$weight))^2), 5)
SQTaov <- round( (SQdentro + SQentre), 5)
SQT == SQTaov


# Sabemos que a divisão de uma variância por outra segue uma distribuição F
# com GLs associados as respectivas variâncias

# Graus de liberdade entre grupos: número de grupos - 1
GLentre <- length(unique(my_data$group)) -1

# Graus de liberade dentro dos grupos: número de observações - num grupos
GLdentro <- nrow(my_data) - length(unique(my_data$group))

### Estatística F
Fstat <- ( SQentre / GLentre ) / (SQdentro / GLdentro)


### Teste F com GLentre graus de liberade no numerador e GLdentro graus de
### liberdade no denominador

1 - pf(Fstat, GLentre, GLdentro)

## A hipótese nula de igualdade das médias dos grupos foi rejeitada, sugerindo
## que ao menos um dos grupos tem média diferente dos demais.

### Método direto ------------------------------------------------------------

# Análise de Variância
res.aov <- aov(weight ~ group, data = my_data)
# Summary of the analysis
summary(res.aov)


























