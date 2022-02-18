library(readr)
library(here)
library(dplyr)
titanic2 <- read_csv(here("titanic2.csv"))

titanic2

#b)
dim(titanic2)
colnames(titanic2)

#d)
factor(titanic2$Embarked,levels = c("C","Q","S"))

#e)
titanic2 %>% group_by(Pclass) %>% as.data.frame() %>% summarize(mean = mean(Fare), median= median(Fare), Q1=quantile(Fare,.25),Q3=quantile(Fare,.75))

#part 2)
library(dplyr)
library(ggplot2)
titanic2$Pclass <- factor(titanic2$Pclass)
titanic2 %>% 
  filter(Fare > 0) %>% 
  ggplot(mapping = aes(x=Fare, fill=Pclass)) + 
  geom_density(alpha=0.3) + 
  scale_x_log10()

titanic2 %>% filter(Fare > 0) %>%
  ggplot(aes(x=Fare, fill=Pclass)) + 
  geom_histogram(position="identity", alpha=0.5, bins=75) + 
  scale_x_log10()

library(GGally)
titanic2 %>% select(-Name, -Ticket, -Cabin, -PassengerId) %>% ggpairs()
