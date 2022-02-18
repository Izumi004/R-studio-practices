power <- read.table('power.txt', header = T)
power_mod_full = lm(PE ~ AT + V + AP + RH, data = power)
summary(power_mod_full)


anova(power_mod_full)
 mean(power$AT)
 
 power_mod_AT_V = lm(PE ~ AT + V, data = power)
 power_mod_AT = lm(PE ~ AT, data = power)
 anova(power_mod_AT, power_mod_AT_V)
 
 power_mod_AT = lm(PE ~ AP, data = power)
anova(power_mod_full, power_mod_AT)

predict(power_mod_full, newdata = list(AT = 24, V = 49, AP = 1013, RH = 44), interval = "prediction", level = .99)

pbinom(0,20,0.05)
