library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rstanarm)

happiness <- read.csv("https://raw.githubusercontent.com/ruiyi29/678Final/main/2016.csv")
Happiness <- happiness %>% dplyr::select(Happiness.Score,Dystopia.Residual,Generosity,Trust..Government.Corruption.,Freedom,Health..Life.Expectancy.,Family,Economy..GDP.per.Capita.) %>% data.frame()

corrplot(cor(Happiness))

ggplot(data = happiness) + 
  aes(x = Economy..GDP.per.Capita. + 1, y = Happiness.Score) + 
  geom_point(aes(color = factor(Region)), size = 1) + 
  geom_smooth(aes(color = factor(Region)), method = "lm", se = FALSE, formula = 'y ~ x') + 
  labs(x = "GDP", y = "Happiness Score")

ggplot(data = happiness) + 
  aes(x = Family + 1, y = Happiness.Score) + 
  geom_point(aes(color = factor(Region)), size = 1) + 
  geom_smooth(aes(color = factor(Region)), method = "lm", se = FALSE, formula = 'y ~ x') + 
  labs(x = "Family", y = "Happiness Score")

ggplot(data = happiness) + 
  aes(x = Freedom + 1, y = Happiness.Score) + 
  geom_point(aes(color = factor(Region)), size = 1) + 
  geom_smooth(aes(color = factor(Region)), method = "lm", se = FALSE, formula = 'y ~ x') + 
  labs(x = "Freedom", y = "Happiness Score")

ggplot(data = happiness) + 
  aes(x = Trust..Government.Corruption. + 1, y = Happiness.Score) + 
  geom_point(aes(color = factor(Region)), size = 1) + 
  geom_smooth(aes(color = factor(Region)), method = "lm", se = FALSE, formula = 'y ~ x') + 
  labs(x = "Government Trust Level", y = "Happiness Score")

ggplot(data = happiness) + 
  aes(x = Generosity + 1, y = Happiness.Score) + 
  geom_point(aes(color = factor(Region)), size = 1) + 
  geom_smooth(aes(color = factor(Region)), method = "lm", se = FALSE, formula = 'y ~ x') + 
  labs(x = "Generosity", y = "Happiness Score")

ggplot(data = happiness) + 
  aes(x = Health..Life.Expectancy. + 1, y = Happiness.Score) + 
  geom_point(aes(color = factor(Region)), size = 1) + 
  geom_smooth(aes(color = factor(Region)), method = "lm", se = FALSE, formula = 'y ~ x') + 
  labs(x = "Life Expectancy", y = "Happiness Score")

happiness %>%
  group_by(Region) %>%
  summarise(mHappy = mean(Happiness.Score)) %>%
  ggplot(aes(reorder(Region, mHappy), mHappy)) +
  geom_point() +
  theme_bw() +
  coord_flip() +
  labs(title = "Happiness Score by Regions 2016",
       x = "", y = "Average happiness score")

happiness <- happiness %>%
  mutate(gdp = round(Economy..GDP.per.Capita./(Happiness.Score - Dystopia.Residual) * 100, 2),
         family = round(Family/(Happiness.Score - Dystopia.Residual) * 100, 2),
         life = round(Health..Life.Expectancy/(Happiness.Score - Dystopia.Residual) * 100, 2),
         freedom = round(Freedom/(Happiness.Score - Dystopia.Residual) * 100, 2),
         generosity = round(Generosity/(Happiness.Score - Dystopia.Residual) * 100, 2),
         corruption = round(Trust..Government.Corruption./(happiness.score - Dystopia.Residual) * 100, 2))
happiness %>%
  summarise(gdp = mean(vgdp),
            family = mean(vfamily),
            life = mean(vlife),
            freedom = mean(vfreedom),
            generosity = mean(vgenerosity),
            corruption = mean(vcorruption)) %>%
  pivot_longer(c(gdp, family, life,freedom,generosity, corruption),
               names_to = "f", values_to = "value") %>%
  ggplot(aes(reorder(f, value), value)) +
  geom_bar(stat = "identity", fill = "purple", width = 0.5, alpha = 0.8) +
  geom_text(aes(label = paste0(round(value, 2), "%"), vjust = -0.5)) +
  theme_bw() +
  labs(title = "Percentage of contribution to happiness score (factors)")

happiness %>%
  group_by(Region) %>%
  summarise(gdp = mean(vgdp),
            family = mean(vfamily),
            life = mean(vlife),
            freedom = mean(vfreedom),
            generosity = mean(vgenerosity),
            corruption = mean(vcorruption)) %>%
  pivot_longer(c(gdp, family, life,freedom,generosity, corruption),
               names_to = "f", values_to = "value") %>%
  ggplot(aes(reorder(f, value), value, color = Region)) +
  geom_line(aes(group = Region)) +
  labs(title = "Rate of contribution to happiness score (region)")




