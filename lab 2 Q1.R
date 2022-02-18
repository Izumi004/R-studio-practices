
library(dplyr)
readr::read_csv("student_data.csv") %>% select(-`Course ID`) %>% mutate(Grade = factor(Grade, levels=c("F","P","C","D","HD"))) -> sdata
library(knitr)
kable(sdata, booktabs=TRUE)

ccount <- table(Course=sdata$Course) %>% as.data.frame() %>% mutate(Percent = Freq/sum(Freq)*100)
kable(ccount, booktabs=TRUE)

ccount2 <- table(Course=sdata$Course,Grade=sdata$Grade)
kable(ccount2, booktabs=TRUE)

sdata %>% group_by(Sex) %>% summarize(mean(Score)) %>% kable(booktabs=TRUE)

sdata %>% group_by(Sex) %>% summarize(median(Score)) %>% kable(booktabs=TRUE)

sdata %>% group_by(Sex) %>% summarize(Q1=quantile(Score,.25),Q3=quantile(Score,.75)) %>% kable(booktabs=TRUE)

sdata %>% group_by(Sex) %>% summarize(IQR=diff(quantile(Score,c(.25,.75)))) %>% kable(booktabs=TRUE)

sdata %>% group_by(Sex) %>% summarize(var(Score),sd(Score)) %>% kable(booktabs=TRUE)