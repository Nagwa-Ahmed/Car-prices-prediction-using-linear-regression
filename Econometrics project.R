setwd("D:/Econometrics project")
library('readxl')
?ifelse
##projectData=read_excel("D:/Econometrics project/car project econo - Copy.xlsx")
##datanew=projectData[sample(nrow(projectData),300),]
 

##
ourData=read_excel("D:/Econometrics project/our data.xlsx")
#ourData=as.data.frame(ourData)


##
Model8=lm(price~Mileage , data=ourData)
summary(Model8)

# Question1

ourData$Buick=ifelse(ourData$Make=='Buick',c(1),c(0))
ourData$Cadillac=ifelse(ourData$Make=='Cadillac',c(1),c(0))
ourData$Pontiac=ifelse(ourData$Make=='Pontiac',c(1),c(0))
ourData$SAAB=ifelse(ourData$Make=='SAAB',c(1),c(0))
ourData$Saturn=ifelse(ourData$Make=='Saturn',c(1),c(0))

Model1=lm(price~ Buick+Cadillac+Pontiac+SAAB
          +Saturn, data=ourData)
summary(Model1)

#Qustion2

ourData$Convertible=ifelse(ourData$Type=='Convertible',c(1),c(0))
ourData$Coupe=ifelse(ourData$Type=='Coupe',c(1),c(0))
ourData$Hatchback=ifelse(ourData$Type=='Hatchback',c(1),c(0))
ourData$Wagon=ifelse(ourData$Type=='Wagon',c(1),c(0))
ourData$high=ifelse(ourData$Cylinder=='high',c(1),c(0))
ourData$moderate=ifelse(ourData$Cylinder=='moderate',c(1),c(0))

Model2=lm(price~ Buick+Cadillac+Pontiac+SAAB
          +Saturn+Convertible+Coupe+Hatchback+Wagon
          +high+moderate, data=ourData)
summary(Model2)

#Qustion3

ourData$Buickhigh=ourData$Buick*ourData$high
ourData$Buickmoderate=ourData$Buick*ourData$moderate
sum(ourData$Buickhigh)
Model3=lm(price~ Buick+Cadillac+Pontiac+SAAB
          +Saturn+high+moderate+Buickhigh+Buickmoderate+
            Cadillac*high+Cadillac*moderate+Pontiac*high
          +Pontiac*moderate+SAAB*high+SAAB*moderate
          +Saturn*high+Saturn*moderate, data=ourData)
summary(Model3)


#Question4

Model4=lm(price~Buick+Cadillac+Pontiac+SAAB
          +Saturn+Mileage+Doors,data=ourData)
summary(Model4)


#Question5

ourData$BuickMileage=ourData$Buick*ourData$Mileage
ourData$CadillacMileage=ourData$Cadillac*ourData$Mileage
ourData$PontiacMileage=ourData$Pontiac*ourData$Mileage
ourData$SAABMileage=ourData$SAAB*ourData$Mileage
ourData$SaturnMileage=ourData$Saturn*ourData$Mileage

Model5=lm(price~Buick+Cadillac+Pontiac+SAAB
          +Saturn+Mileage+BuickMileage+CadillacMileage
          +PontiacMileage+SAABMileage+SaturnMileage,data=ourData)
summary(Model5)



# Qustion 7
ourData$Dummy= ifelse(ourData$Mileage>15000,c(1),c(0))
#print(ourData$Dummy)
#sum(ourData$Dummy)

ourData$MileageMinus=ourData$Mileage-15000

ourData$DummyMileageMinus=ourData$Dummy*ourData$MileageMinus

Model7=lm(price~Mileage+DummyMileageMinus , data=ourData)
summary(Model7)

round(6.626e+00,3)
round(4.359e-05,4)

##
Model8=lm(price~Mileage , data=ourData)
summary(Model8)

"""ourData$high1=as.factor(ourData$high)
ourData$moderate1=as.factor(ourData$moderate)
M=lm(price~high1+moderate1,data = ourData)
N=lm(price~high+moderate,data = ourData)
Summary(M)
Summary(N)"""






