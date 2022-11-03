
# Fome e subnutrição em países democratas e autocratas -------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 02/11/22 ---------------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/hunger-and-undernourishment -----------------------------------------------------------------------

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------

### Ter uma dieta que é, ambos, suficiente em termos de requerimentos calóricos e diversa
### para satisfazer os requisitos nutricionais adicionais é essencial para uma boa saúde.
### Subnutrição, especialmente em crianças e mães, é um dos principais fatores de risco
### para morte e outras consequências de má saúde.

### As Nações Unidas tem estabelecido uma meta global como parte dos Objetivos do Desenvolvimento
### Sustentável para acabar com a fome até 2030. Atualmente nós estamos distantes de alcançar
### esse alvo.

### Em nossa pesquisa sobre fome e subnutrição nós observamos em como muitas pessoas estão
### subnutridas, onde elas vivem, subnutrição infantil, e insegurança alimentar em volta
### do mundo.
 
### 8,9% da população mundial está subnutrida. Isso significa que ela tem um retorno calórico
### abixo dos requerimentos mínimos.

### 663 milhões de pessoas no mundo está subnutrida.

### 22% de crianças menores que cinco anos estão 'raquíticas'. Elas são significativamente menores
### que a média para a idade delas, como uma consequência de uma má nutrição ou de infecções
### repetidas.

### 9% da população mundial - cerca de 697 milhões de pessoas estão severamente em insegurança
### alimentar.

### Uma a cada quatro pessoas no mundo - 1,9 bilhões - estão moderadamente ou severamente em
### insegurança alimnetar.

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(cols4all)
library(hrbrthemes)
library(ggthemes)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

fome <- read.csv("prevalence-of-undernourishment.csv")
view(fome)
names(fome)

# Manipular dados --------------------------------------------------------------------------------------------------------------------------

fome <- fome %>%
  select(-Code) %>%
  rename(por_subnut = Prevalence.of.undernourishment....of.population.) %>%
  view()

fome1 <- fome %>%
  filter(Entity %in% c("United States", "Germany", "Japan",
                       "China", "North Korea", "Cuba")) %>%
  group_by(Entity) %>%
  summarise(media = mean(por_subnut),
            sd = sd(por_subnut), n = n(),
            se = sd/sqrt(n)) %>%
  view()

fome2 <- fome %>%
  filter(Entity %in% c("United States", "Germany", "Japan",
                       "China", "North Korea", "Cuba")) %>%
  view()

fome3 <- fome %>%
  filter(Entity %in% c("United States", "China", "Brazil")) %>%
  view()

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

  
  
  