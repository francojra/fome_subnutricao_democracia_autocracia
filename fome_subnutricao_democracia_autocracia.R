
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

c4a("safe", 6)

ggplot(fome1, aes(x = fct_reorder(Entity, media), 
                  y = media, fill = Entity)) +
  geom_col(width = 0.9) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                width = 0.3, size = 0.8) +
  scale_fill_manual(values = c("#88CCEE", "#CC6677",
                               "#DDCC77", "#117733", 
                               "#332288", "#AA4499")) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  scale_x_discrete(labels = c("Cuba", "Alemanha", "Estados Unidos",
                              "Japão", "China", "Coreia do Norte")) +
  labs(x = "Países", y = "Subnutrição (%)") +
  theme_ipsum(axis_title_size = 16, axis_text_size = 14) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))

ggplot(fome2, aes(x = Year, y = por_subnut, 
                  group = Entity, color =  Entity)) +
  geom_point(shape = 15, size = 2.5) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("#88CCEE", "#CC6677",
                                "#DDCC77", "#117733", 
                                "#332288", "#AA4499"),
                     labels = c("China", "Cuba", "Alemanha",
                                "Japão", "Coreia do Norte", "Estados Unidos")) +
  labs(x = "Tempo (anos)", y = "Subnutrição (%)", col = "Países") +
  theme_ipsum(axis_title_size = 16, axis_text_size = 14) +
  theme(axis.text = element_text(color = "black"))
  
ggplot(fome3, aes(x = Year, y = por_subnut, 
                  group = Entity, color =  Entity)) +
  geom_line(size = 2) +
  scale_color_manual(values = c('#1B9E77', '#999999','#E69F00'),
                     labels = c("Brasil", "China", "Estados Unidos")) +
  labs(x = "Tempo (anos)", y = "Subnutrição (%)", col = "Países") +
  theme_light() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(color = "black", size = 15),
        legend.text = element_text(size = 12))


