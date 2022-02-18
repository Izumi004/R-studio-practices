data <- read.csv("titanic2.csv")

library(dplyr)
res <- data %>% filter(Sex == "female") 
me <- mean(res$Fare)
result <- res %>% filter(Fare > me)
summarise(result)
1-sum(result$Survived)/nrow(result)


data <- read.csv("titanic2.csv")

library(dplyr)
result <- data %>% filter(Sex == "female") %>% filter(Fare > mean(Fare)) 
frac <- 1-sum(result$Survived)/nrow(result)
return(frac)