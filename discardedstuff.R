### Investigating internal correlation between docs

## Cool stuff, very irrelevant. 

library(reshape)

corframe <- topicassigner %>% 
  group_by(document) %>% 
  pivot_wider(names_from = topic, values_from = gamma) %>% 
  ungroup()

corframe2 <- corframe %>% select(-(document))

corframe3 <- round(cor(corframe2), 4)

melted <- melt(corframe3)

ggplot(melted, aes(x = X1, y = X2, fill = value)) + 
  geom_tile() + 
  theme_bw() + 
  labs(title = "Correlation map between topic Gammas", x = "", 
       y = "")

## Interesting, but somewhat pointless as well

## Only the topics we are interested in

topic_assignment3 <- corframe %>% 
  select(`119`, `135`, `138`, `158`, `164`)

rouncor2 <- round(cor(topic_assignment3), 4)

melted2 <- melt(rouncor2)

ggplot(melted2, aes(x = as.character(X1), y = as.character(X2), fill = value)) + 
  geom_tile() + 
  theme_bw() + 
  labs(title = "Correlation map between topic Gammas", x = "", 
       y = "")

meancheck <- melted %>% 
  filter(value < 1) %>% 
  select(value)

mean(meancheck$value)

sd(meancheck$value)

hist(meancheck$value)

plot(density(meancheck$value))