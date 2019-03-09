setwd("/Users/yaxincai/Desktop/DSC 423/group project/student")
math=read.csv("student-mat.csv",sep=";",header=T)
school=math[,1]
sex = math[,2]
age = math[,3]
address = math[,4]
famsize = math[,5]
Pstatus = math[,6]
Medu = math[,7]
Fedu = math[,8]
Mjob = math[,9]
Fjob = math[,10]
reason = math[,11]
guardian = math[,12]
traveltime = math[,13]
studytime = math[,14]
failures= math[,15]
schoolsup = math[,16]
famsup = math[,17]
paid= math[,18]
activities = math[,19]
nursery = math[,20]
higher = math[,21]
internet = math[,22]
romantic = math[,23]
famrel=math[,24]
freetime = math[,25]
goout = math[,26]
Dalc= math[,27]
Walc = math[,28]
health = math[,29]
absences = math[,30]
G1 = math[,31]
G2 = math[,32]
G3 = math[,33]
M1=lm(G3~as.factor(school)+as.factor(sex)+age+as.factor(address)+as.factor(famsize)+as.factor(Pstatus)+Medu+Fedu+as.factor(Mjob)
      +as.factor(Fjob)+as.factor(reason)+as.factor(guardian)+traveltime+studytime+failures+as.factor(schoolsup)+as.factor(famsup)
      +as.factor(paid)+as.factor(activities)+as.factor(nursery)+as.factor(higher)+as.factor(internet)+as.factor(romantic)+famrel
      +freetime+goout+Dalc+Walc+health+absences+G1+G2)
summary(M1)
M2=lm(G3~as.factor(school)+as.factor(sex)+age+as.factor(address)+as.factor(famsize)+as.factor(Pstatus)+Medu+Fedu+as.factor(Mjob)
      +as.factor(Fjob)+as.factor(reason)+traveltime+studytime+failures+as.factor(schoolsup)+as.factor(famsup)
      +as.factor(paid)+as.factor(activities)+as.factor(nursery)+as.factor(higher)+as.factor(internet)+as.factor(romantic)+famrel
      +freetime+goout+Dalc+Walc+health+absences+G1+G2)
summary(M2)
M3=lm(G3~as.factor(school)+as.factor(sex)+age+as.factor(address)+as.factor(famsize)+as.factor(Pstatus)+Medu+Fedu
      +as.factor(Fjob)+as.factor(reason)+traveltime+studytime+failures+as.factor(schoolsup)+as.factor(famsup)
      +as.factor(paid)+as.factor(activities)+as.factor(nursery)+as.factor(higher)+as.factor(internet)+as.factor(romantic)+famrel
      +freetime+goout+Dalc+Walc+health+absences+G1+G2)
summary(M3)
M4=lm(G3~as.factor(school)+as.factor(sex)+age+as.factor(address)+as.factor(Pstatus)+Medu+Fedu
      +as.factor(Fjob)+as.factor(reason)+traveltime+studytime+failures+as.factor(schoolsup)+as.factor(famsup)
      +as.factor(paid)+as.factor(activities)+as.factor(nursery)+as.factor(higher)+as.factor(internet)+as.factor(romantic)+famrel
      +freetime+goout+Dalc+Walc+health+absences+G1+G2)
summary(M4)
M5=lm(G3~as.factor(school)+as.factor(sex)+age+as.factor(address)+as.factor(Pstatus)+Medu+Fedu
     +as.factor(reason)+traveltime+studytime+failures+as.factor(schoolsup)+as.factor(famsup)
      +as.factor(paid)+as.factor(activities)+as.factor(nursery)+as.factor(higher)+as.factor(internet)+as.factor(romantic)+famrel
      +freetime+goout+Dalc+Walc+health+absences+G1+G2)
summary(M5)
M6=lm(G3~as.factor(school)+as.factor(sex)+age+as.factor(Pstatus)+Medu+Fedu
      +as.factor(reason)+traveltime+studytime+failures+as.factor(schoolsup)+as.factor(famsup)
      +as.factor(paid)+as.factor(activities)+as.factor(nursery)+as.factor(higher)+as.factor(internet)+as.factor(romantic)+famrel
      +freetime+goout+Dalc+Walc+health+absences+G1+G2)
summary(M6)
M7=lm(G3~as.factor(school)+as.factor(sex)+age+as.factor(Pstatus)+Medu+Fedu
     +traveltime+studytime+failures+as.factor(schoolsup)+as.factor(famsup)
      +as.factor(paid)+as.factor(activities)+as.factor(nursery)+as.factor(higher)+as.factor(internet)+as.factor(romantic)+famrel
      +freetime+goout+Dalc+Walc+health+absences+G1+G2)
summary(M7)
M8=lm(G3~as.factor(school)+as.factor(sex)+age+as.factor(Pstatus)+Medu+Fedu
      +studytime+failures+as.factor(schoolsup)+as.factor(famsup)
      +as.factor(paid)+as.factor(activities)+as.factor(nursery)+as.factor(higher)+as.factor(internet)+as.factor(romantic)+famrel
      +freetime+goout+Dalc+Walc+health+absences+G1+G2)
summary(M8)
M9=lm(G3~as.factor(school)+age+as.factor(Pstatus)+Medu+Fedu
      +studytime+failures+as.factor(schoolsup)+as.factor(famsup)
      +as.factor(paid)+as.factor(activities)+as.factor(nursery)+as.factor(higher)+as.factor(internet)+as.factor(romantic)+famrel
      +freetime+goout+Dalc+Walc+health+absences+G1+G2)
summary(M9)
M10=lm(G3~as.factor(school)+age+as.factor(Pstatus)+Medu+Fedu
      +studytime+failures+as.factor(schoolsup)+as.factor(famsup)
      +as.factor(paid)+as.factor(activities)+as.factor(nursery)+as.factor(internet)+as.factor(romantic)+famrel
      +freetime+goout+Dalc+Walc+health+absences+G1+G2)
summary(M10)
M11=lm(G3~as.factor(school)+age+as.factor(Pstatus)+Medu+Fedu
       +studytime+failures+as.factor(schoolsup)+as.factor(famsup)
       +as.factor(paid)+as.factor(activities)+as.factor(nursery)+as.factor(internet)+as.factor(romantic)+famrel
       +freetime+Dalc+Walc+health+absences+G1+G2)
summary(M11)
M12=lm(G3~as.factor(school)+age+as.factor(Pstatus)+Medu+Fedu
       +studytime+failures+as.factor(schoolsup)+as.factor(famsup)
       +as.factor(activities)+as.factor(nursery)+as.factor(internet)+as.factor(romantic)+famrel
       +freetime+Dalc+Walc+health+absences+G1+G2)
summary(M12)
M13=lm(G3~as.factor(school)+age+Medu+Fedu
       +studytime+failures+as.factor(schoolsup)+as.factor(famsup)
       +as.factor(activities)+as.factor(nursery)+as.factor(internet)+as.factor(romantic)+famrel
       +freetime+Dalc+Walc+health+absences+G1+G2)
summary(M13)
M14=lm(G3~as.factor(school)+age+Medu+Fedu
       +studytime+failures+as.factor(schoolsup)+as.factor(famsup)
       +as.factor(activities)+as.factor(nursery)+as.factor(romantic)+famrel
       +freetime+Dalc+Walc+health+absences+G1+G2)
summary(M14)
M15=lm(G3~as.factor(school)+age+Medu+Fedu
       +studytime+failures+as.factor(schoolsup)+as.factor(famsup)
       +as.factor(activities)+as.factor(nursery)+as.factor(romantic)+famrel
       +Dalc+Walc+health+absences+G1+G2)
summary(M15)
M16=lm(G3~as.factor(school)+age+Medu+Fedu
       +studytime+failures+as.factor(schoolsup)
       +as.factor(activities)+as.factor(nursery)+as.factor(romantic)+famrel
       +Dalc+Walc+health+absences+G1+G2)
summary(M16)
M17=lm(G3~as.factor(school)+age+Medu+Fedu
       +failures+as.factor(schoolsup)
       +as.factor(activities)+as.factor(nursery)+as.factor(romantic)+famrel
       +Dalc+Walc+health+absences+G1+G2)
summary(M17)
M18=lm(G3~as.factor(school)+age+Medu+Fedu
       +failures+as.factor(schoolsup)
       +as.factor(activities)+as.factor(nursery)+as.factor(romantic)+famrel
       +Walc+health+absences+G1+G2)
summary(M18)
M19=lm(G3~as.factor(school)+age+Medu+Fedu
       +failures+as.factor(schoolsup)
       +as.factor(activities)+as.factor(romantic)+famrel
       +Walc+health+absences+G1+G2)
summary(M19)
M20=lm(G3~as.factor(school)+age+Medu+Fedu
       +failures+as.factor(schoolsup)
       +as.factor(activities)+as.factor(romantic)+famrel
       +Walc+absences+G1+G2)
summary(M20)
M21=lm(G3~as.factor(school)+age+Fedu
       +failures+as.factor(schoolsup)
       +as.factor(activities)+as.factor(romantic)+famrel
       +Walc+absences+G1+G2)
summary(M21)
M22=lm(G3~as.factor(school)+age
       +failures+as.factor(schoolsup)
       +as.factor(activities)+as.factor(romantic)+famrel
       +Walc+absences+G1+G2)
summary(M22)
M23=lm(G3~as.factor(school)+age
       +failures
       +as.factor(activities)+as.factor(romantic)+famrel
       +Walc+absences+G1+G2)
summary(M23)
M24=lm(G3~as.factor(school)+age
      
       +as.factor(activities)+as.factor(romantic)+famrel
       +Walc+absences+G1+G2)
summary(M24)
M25=lm(G3~age
       
       +as.factor(activities)+as.factor(romantic)+famrel
       +Walc+absences+G1+G2)
summary(M25)
M26=lm(G3~age
       
       +as.factor(activities)+famrel
       +Walc+absences+G1+G2)
summary(M26)
M27=lm(G3~age
       
       +as.factor(activities)+famrel
       +absences+G1+G2)
summary(M27)
M28=lm(G3~age
       
      +famrel
       +absences+G1+G2)
summary(M28)   #model
vif(M28)      # check multicollinearity
qqnorm(rstandard(M28))
summary(influence.measures(M28))
rstandard(M28)
