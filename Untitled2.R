hospital = read.table("hospital.txt", header = T)
hospital_mod = lm(y~. ,data = hospital)
e3 = residuals(hospital_mod)[3]
hospital_mod_m3 = lm(y~., data = hospitalm3)
hospitalm3 = hospital[-3,]
nd = hospital[3,]
e3m3 = hospital$y[3] - predict(hospital_mod_m3, newdata=nd)
e3m3

c(e3,e3m3)
