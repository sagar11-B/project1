
#-----ATTRITION PROJECT------#

install.packages("stats")
install.packages("dplyr")

library(stats)#---For Data Exploration
library(dplyr)#---For Data Manipulation

getwd()
setwd("c:\\project")
getwd()

mydata<-read.csv("Attrition Case Study.csv")#Data is Loaded into mydata object from csv file
head(mydata)

str(mydata)

sum(is.na(mydata))
summary(mydata)

#----UNIVARIATE ANALYSIS

#----C

#---1
#----ATTRITION
#SUMMARIZATION
#---COUNT,PROPORTION,%,RATIO,MODE

table(mydata$Attrition)#count
table(mydata$Attrition)[1]/table(mydata$Attrition)[2]#ratio
Attrition_matrix<-as.matrix(table(mydata$Attrition))
Attrition_matrix
prop.table(table(mydata$Attrition))#proportion
round(prop.table(table(mydata$Attrition))*100,2)#percentage
table(mydata$Attrition)==max(table(mydata$Attrition))

#VISUALIZATION
#---PIECHART,BARCHART
#----ATTRITION VARIABLE
pie(Attrition_matrix,radius=1,main="Piechart of Attrition",
    col=c("red","blue"),
    labels = levels(mydata$Attrition),
    clockwise = TRUE,init.angle=0)

par(mfrow=c(1,2))
barplot(Attrition_matrix,beside =TRUE)
barplot(Attrition_matrix,beside=TRUE,
        space=0.5,
        main="Barplot of Attrition",
        xlab="Attrition",
        ylab="Frequency",
        #names.arg=c("A","N")
        names.arg = levels(mydata$Attrition),
        col = c("red","blue")
)


palette()

#---2
#----BUSINESS TRAVEL VARIABLE
#SUMMARIZATION
#---COUNT,PROPORTION,%,RATIO,MODE

table(mydata$BusinessTravel)#count
table(mydata$BusinessTravel)[1]/table(mydata$BusinessTravel)[2]#ratio
table(mydata$BusinessTravel)[2]/table(mydata$BusinessTravel)[3]#ratio
table(mydata$BusinessTravel)[1]/table(mydata$BusinessTravel)[3]#ratio
BusinessTravel_matrix<-as.matrix(table(mydata$BusinessTravel))
BusinessTravel_matrix
prop.table(table(mydata$BusinessTravel))#proportion
round(prop.table(table(mydata$BusinessTravel))*100,2)#percentage
table(mydata$BusinessTravel)==max(table(mydata$BusinessTravel))

#VISUALIZATION
#---PIECHART,BARCHART
#----ATTRITION VARIABLE
pie(BusinessTravel_matrix,radius=1,main="Piechart of BusinessTravel",
    col=c("red","blue","green"),
    labels = levels(mydata$BusinessTravel),
    clockwise = TRUE,init.angle=0)

par(mfrow=c(1,2))
barplot(BusinessTravel_matrix,beside =TRUE)
barplot(BusinessTravel_matrix,beside=TRUE,
        space=0.5,
        main="Barplot of BusinessTravel",
        xlab="BusinessTravel",
        ylab="Frequency",
        #names.arg=c("A","N")
        names.arg = levels(mydata$BusinessTravel),
        col = c("red","blue","green")
)


#---3
#----DEPARTMENT
#SUMMARIZATION
#---COUNT,PROPORTION,%,RATIO,MODE

table(mydata$Department)#count
table(mydata$Department)[1]/table(mydata$Department)[2]#ratio
table(mydata$Department)[2]/table(mydata$Department)[3]#ratio
table(mydata$Department)[1]/table(mydata$Department)[3]#ratio
Department_matrix<-as.matrix(table(mydata$Department))
Department_matrix
prop.table(table(mydata$Department))#proportion
round(prop.table(table(mydata$Department))*100,2)#percentage
table(mydata$Department)==max(table(mydata$Department))

#VISUALIZATION
#---PIECHART,BARCHART
#----ATTRITION VARIABLE
pie(Department_matrix,radius=1,main="Piechart of Department",
    col=c("red","blue","green"),
    labels = levels(mydata$Department),
    clockwise = TRUE,init.angle=0)

par(mfrow=c(1,2))
barplot(Department_matrix,beside =TRUE)
barplot(Department_matrix,beside=TRUE,
        space=0.5,
        main="Barplot of Department",
        xlab="Department",
        ylab="Frequency",
        #names.arg=c("A","N")
        names.arg = levels(mydata$Department),
        col = c("red","blue","green")
)

#---4
#----EDUCATION
#SUMMARIZATION
#---COUNT,PROPORTION,%,RATIO,MODE

table(mydata$Education)#count
table(mydata$Education)[1]/table(mydata$Education)[2]#ratio
table(mydata$Education)[2]/table(mydata$Education)[3]#ratio
table(mydata$Education)[3]/table(mydata$Education)[4]#ratio
Education_matrix<-as.matrix(table(mydata$Education))
Education_matrix
prop.table(table(mydata$Education))#proportion
round(prop.table(table(mydata$Education))*100,2)#percentage
#table(mydata$Department)==max(table(mydata$Department))

#VISUALIZATION
#---PIECHART,BARCHART
#----ATTRITION VARIABLE
pie(Education_matrix,radius=1,main="Piechart of Education",
    col=c("red","blue","green","orange","black"),
    labels = levels(mydata$Education),
    clockwise = TRUE,init.angle=0)

par(mfrow=c(1,2))
barplot(Education_matrix,beside =TRUE)
barplot(Education_matrix,beside=TRUE,
        space=0.5,
        main="Barplot of Education",
        xlab="Education",
        ylab="Frequency",
        #names.arg=c("A","N")
        names.arg = levels(mydata$Education),
        col = c("red","blue","green","orange","black")
)

#---5
#----EDUCATION FIELD
#SUMMARIZATION
#---COUNT,PROPORTION,%,RATIO,MODE

table(mydata$EducationField)#count
table(mydata$EducationField)[1]/table(mydata$EducationField)[2]#ratio
table(mydata$EducationField)[2]/table(mydata$EducationField)[3]#ratio
table(mydata$EducationField)[3]/table(mydata$EducationField)[4]#ratio
table(mydata$EducationField)[4]/table(mydata$EducationField)[5]#ratio
table(mydata$EducationField)[5]/table(mydata$EducationField)[6]#ratio
EducationField_matrix<-as.matrix(table(mydata$EducationField))
EducationField_matrix
prop.table(table(mydata$EducationField))#proportion
round(prop.table(table(mydata$EducationField))*100,2)#percentage
#table(mydata$Department)==max(table(mydata$Department))

#VISUALIZATION
#---PIECHART,BARCHART
#----ATTRITION VARIABLE
pie(EducationField_matrix,radius=1,main="Piechart of EducationField",
    col=c("red","blue","green","yellow","white","grey"),
    labels = levels(mydata$EducationField),
    clockwise = TRUE,init.angle=0)

par(mfrow=c(1,2))
barplot(EducationField_matrix,beside =TRUE)
barplot(EducationField_matrix,beside=TRUE,
        space=0.5,
        main="Barplot of EducationField",
        xlab="EducationField",
        ylab="Frequency",
        #names.arg=c("A","N")
        names.arg = levels(mydata$EducationField),
        col = c("red","blue","green","yellow","white","grey")
)

#---6
#----ENVIRONMENT SATISFACTION
#SUMMARIZATION
#---COUNT,PROPORTION,%,RATIO,MODE

table(mydata$EnvironmentSatisfaction)#count
table(mydata$EnvironmentSatisfaction)[1]/table(mydata$EnvironmentSatisfaction)[2]#ratio
table(mydata$EnvironmentSatisfaction)[2]/table(mydata$EnvironmentSatisfaction)[3]#ratio
table(mydata$EnvironmentSatisfaction)[3]/table(mydata$EnvironmentSatisfaction)[4]#ratio
#table(mydata$EducationField)[4]/table(mydata$EducationField)[5]#ratio
#table(mydata$EducationField)[5]/table(mydata$EducationField)[6]#ratio
EnvironmentSatisfaction_matrix<-as.matrix(table(mydata$EnvironmentSatisfaction))
EnvironmentSatisfaction_matrix
prop.table(table(mydata$EnvironmentSatisfaction))#proportion
round(prop.table(table(mydata$EnvironmentSatisfaction))*100,2)#percentage
#table(mydata$Department)==max(table(mydata$Department))

#VISUALIZATION
#---PIECHART,BARCHART
#----ATTRITION VARIABLE
pie(EnvironmentSatisfaction_matrix,radius=1,main="Piechart of EnvironmentSatisfaction",
    col=c("red","blue","green","yelow"),
    labels = levels(mydata$EnvironmentSatisfaction),
    clockwise = TRUE,init.angle=0)

par(mfrow=c(1,2))
barplot(EnvironmentSatisfaction_matrix,beside =TRUE)
barplot(EnvironmentSatisfaction_matrix,beside=TRUE,
        space=0.5,
        main="Barplot of EnvironmentSatisfaction",
        xlab="EnvironmentSatisfaction",
        ylab="Frequency",
        #names.arg=c("A","N")
        names.arg = levels(mydata$EnvironmentSatisfaction),
        col = c("red","blue","green","yelow")
)


#---7
#----GENDER
#SUMMARIZATION
#---COUNT,PROPORTION,%,RATIO,MODE

table(mydata$Gender)#count
table(mydata$Gender)[1]/table(mydata$Gender)[2]#ratio
Gender_matrix<-as.matrix(table(mydata$Gender))
Gender_matrix
prop.table(table(mydata$Gender))#proportion
round(prop.table(table(mydata$Gender)*100,2))#percentage
#table(mydata$Gender)==max(table(mydata$Gender))

#VISUALIZATION
#---PIECHART,BARCHART
pie(Gender_matrix,radius=1,main="Piechart of Gender",
    col=c("red","blue"),
    labels = levels(mydata$Gender),
    clockwise = TRUE,init.angle=0)

par(mfrow=c(1,2))
barplot(Gender_matrix,beside =TRUE)
barplot(Gender_matrix,beside=TRUE,
        space=0.5,
        main="Barplot of Gender",
        xlab="Gender",
        ylab="Frequency",
        #names.arg=c("A","N")
        names.arg = levels(mydata$Gender),
        col = c("blue","red")
)


#---8
#----JOB INVOLVEMENT
#SUMMARIZATION
#---COUNT,PROPORTION,%,RATIO,MODE

table(mydata$JobInvolvement)#count
table(mydata$JobInvolvement)[1]/table(mydata$JobInvolvement)[2]#ratio
table(mydata$JobInvolvement)[2]/table(mydata$JobInvolvement)[3]#ratio
table(mydata$JobInvolvement)[3]/table(mydata$JobInvolvement)[4]#ratio
JobInvolvement_matrix<-as.matrix(table(mydata$JobInvolvement))
JobInvolvement_matrix
prop.table(table(mydata$JobInvolvement))#proportion
round(prop.table(table(mydata$JobInvolvement))*100,2)#percentage
#table(mydata$Department)==max(table(mydata$Department))

#VISUALIZATION
#---PIECHART,BARCHART
pie(JobInvolvement_matrix,radius=1,main="Piechart of JobInvolvement",
    col=c("red","blue","green","yelow"),
    labels = levels(mydata$JobInvolvement),
    clockwise = TRUE,init.angle=0)

par(mfrow=c(1,2))
barplot(JobInvolvement_matrix,beside =TRUE)
barplot(JobInvolvement_matrix,beside=TRUE,
        space=0.5,
        main="Barplot of JobInvolvement",
        xlab="JobInvolvement",
        ylab="Frequency",
        #names.arg=c("A","N")
        names.arg = levels(mydata$JobInvolvement),
        col = c("red","blue","green","yelow")
)

#---9
#----JOB LEVEL
#SUMMARIZATION
#---COUNT,PROPORTION,%,RATIO,MODE

table(mydata$JobLevel)#count
table(mydata$JobLevel)[1]/table(mydata$JobLevel)[2]#ratio
table(mydata$JobLevel)[2]/table(mydata$JobLevel)[3]#ratio
table(mydata$JobLevel)[3]/table(mydata$JobLevel)[4]#ratio
JobLevel_matrix<-as.matrix(table(mydata$JobLevel))
JobLevel_matrix
prop.table(table(mydata$JobLevel))#proportion
round(prop.table(table(mydata$JobLevel))*100,2)#percentage
#table(mydata$Department)==max(table(mydata$Department))

#VISUALIZATION
#---PIECHART,BARCHART
pie(JobLevel_matrix,radius=1,main="Piechart of JobLevel",
    col=c("red","blue","green","yelow"),
    labels = levels(mydata$JobLevel),
    clockwise = TRUE,init.angle=0)

par(mfrow=c(1,2))
barplot(JobLevel_matrix,beside =TRUE)
barplot(JobLevel_matrix,beside=TRUE,
        space=0.5,
        main="Barplot of JobLevel",
        xlab="JobLevel",
        ylab="Frequency",
        #names.arg=c("A","N")
        names.arg = levels(mydata$JobLevel),
        col = c("red","blue","green","yelow")
)


#---10
#----JOB ROLE
#SUMMARIZATION
#---COUNT,PROPORTION,%,RATIO,MODE

table(mydata$JobRole)#count
table(mydata$JobRole)[1]/table(mydata$JobRole)[2]#ratio
table(mydata$JobRole)[2]/table(mydata$JobRole)[3]#ratio
table(mydata$JobRole)[3]/table(mydata$JobRole)[4]#ratio
table(mydata$JobRole)[4]/table(mydata$JobRole)[5]#ratio
table(mydata$JobRole)[5]/table(mydata$JobRole)[6]#ratio
table(mydata$JobRole)[6]/table(mydata$JobRole)[7]#ratio
table(mydata$JobRole)[7]/table(mydata$JobRole)[8]#ratio
table(mydata$JobRole)[8]/table(mydata$JobRole)[9]#ratio
table(mydata$JobRole)[5]/table(mydata$JobRole)[6]#ratio
JobRole_matrix<-as.matrix(table(mydata$EducationField))
JobRole_matrix
prop.table(table(mydata$JobRole))#proportion
round(prop.table(table(mydata$JobRole))*100,2)#percentage
#table(mydata$Department)==max(table(mydata$Department))

#VISUALIZATION
#---PIECHART,BARCHART
#----ATTRITION VARIABLE
pie(JobRole_matrix,radius=1,main="Piechart of JobRole",
    col=c("red","blue","green","yelow","white","grey","purpple","black","brown"),
    labels = levels(mydata$JobRole),
    clockwise = TRUE,init.angle=0)

par(mfrow=c(1,2))
barplot(JobRole_matrix,beside =TRUE)
barplot(JobRole_matrix,beside=TRUE,
        space=0.5,
        main="Barplot of JobRole",
        xlab="JobRole",
        ylab="Frequency",
        #names.arg=c("A","N")
        names.arg = levels(mydata$JobRole),
        col = c("red","blue","green","yelow","white","grey","purpple","black","brown")
)


#---11
#----JOB SATISFACTION
#SUMMARIZATION
#---COUNT,PROPORTION,%,RATIO,MODE

table(mydata$JobSatisfaction)#count
table(mydata$JobSatisfaction)[1]/table(mydata$JobSatisfaction)[2]#ratio
table(mydata$JobSatisfaction)[2]/table(mydata$JobSatisfaction)[3]#ratio
table(mydata$JobSatisfaction)[3]/table(mydata$JobSatisfaction)[4]#ratio
JobSatisfaction_matrix<-as.matrix(table(mydata$JobSatisfaction))
JobSatisfaction_matrix
prop.table(table(mydata$JobSatisfaction))#proportion
round(prop.table(table(mydata$JobSatisfaction))*100,2)#percentage
#table(mydata$Department)==max(table(mydata$Department))

#VISUALIZATION
#---PIECHART,BARCHART
pie(JobSatisfaction_matrix,radius=1,main="Piechart of JobSatisfaction",
    col=c("red","blue","green","yelow"),
    labels = levels(mydata$JobSatisfaction),
    clockwise = TRUE,init.angle=0)

par(mfrow=c(1,2))
barplot(JobSatisfaction_matrix,beside =TRUE)
barplot(JobSatisfaction_matrix,beside=TRUE,
        space=0.5,
        main="Barplot of JobSatisfaction",
        xlab="JobSatisfaction",
        ylab="Frequency",
        #names.arg=c("A","N")
        names.arg = levels(mydata$JobSatisfaction),
        col = c("red","blue","green","yelow")
)

#---12
#----MARITAL STATUS
#SUMMARIZATION
#---COUNT,PROPORTION,%,RATIO,MODE

table(mydata$MaritalStatus)#count
table(mydata$MaritalStatus)[1]/table(mydata$MaritalStatus)[2]#ratio
table(mydata$MaritalStatus)[2]/table(mydata$MaritalStatus)[3]#ratio
table(mydata$MaritalStatus)[1]/table(mydata$MaritalStatus)[3]#ratio
MaritalStatus_matrix<-as.matrix(table(mydata$MaritalStatus))
MaritalStatus_matrix
prop.table(table(mydata$MaritalStatus))#proportion
round(prop.table(table(mydata$MaritalStatus))*100,2)#percentage
#table(mydata$Department)==max(table(mydata$Department))

#VISUALIZATION
#---PIECHART,BARCHART
#----ATTRITION VARIABLE
pie(MaritalStatus_matrix,radius=1,main="Piechart of MaritalStatus",
    col=c("red","blue","green"),
    labels = levels(mydata$MaritalStatus),
    clockwise = TRUE,init.angle=0)

par(mfrow=c(1,2))
barplot(MaritalStatus_matrix,beside =TRUE)
barplot(MaritalStatus_matrix,beside=TRUE,
        space=0.5,
        main="Barplot of MaritalStatus",
        xlab="MaritalStatus",
        ylab="Frequency",
        #names.arg=c("A","N")
        names.arg = levels(mydata$MaritalStatus),
        col = c("red","blue","green")
)


#---13
#----OVER TIME
#SUMMARIZATION
#---COUNT,PROPORTION,%,RATIO,MODE

table(mydata$OverTime)#count
table(mydata$OverTime)[1]/table(mydata$OverTime)[2]#ratio
OverTime_matrix<-as.matrix(table(mydata$OverTime))
OverTime_matrix
prop.table(table(mydata$OverTime))#proportion
round(prop.table(table(mydata$OverTime))*100,2)#percentage
#table(mydata$Department)==max(table(mydata$Department))

#VISUALIZATION
#---PIECHART,BARCHART
#----ATTRITION VARIABLE
pie(OverTime_matrix,radius=1,main="Piechart of OverTime",
    col=c("red","blue"),
    labels = levels(mydata$OverTime),
    clockwise = TRUE,init.angle=0)

par(mfrow=c(1,2))
barplot(OverTime_matrix,beside =TRUE)
barplot(OverTime_matrix,beside=TRUE,
        space=0.5,
        main="Barplot of OverTime",
        xlab="OverTime",
        ylab="Frequency",
        #names.arg=c("A","N")
        names.arg = levels(mydata$OverTime),
        col = c("red","blue")
)


#---14
#----PERFORMANCE RATING
#SUMMARIZATION
#---COUNT,PROPORTION,%,RATIO,MODE

table(mydata$PerformanceRating)#count
table(mydata$PerformanceRating)[1]/table(mydata$PerformanceRating)[2]#ratio
table(mydata$PerformanceRating)[2]/table(mydata$PerformanceRating)[3]#ratio
table(mydata$PerformanceRating)[3]/table(mydata$PerformanceRating)[4]#ratio
table(mydata$PerformanceRating)[4]/table(mydata$PerformanceRating)[5]#ratio
PerformanceRating_matrix<-as.matrix(table(mydata$PerformanceRating))
PerformanceRating_matrix
prop.table(table(mydata$PerformanceRating))#proportion
round(prop.table(table(mydata$PerformanceRating))*100,2)#percentage
#table(mydata$Department)==max(table(mydata$Department))

#VISUALIZATION
#---PIECHART,BARCHART
pie(PerformanceRating_matrix,radius=1,main="Piechart of PerformanceRating",
    col=c("red","blue","green","yelow","orange"),
    labels = levels(mydata$PerformanceRating),
    clockwise = TRUE,init.angle=0)

par(mfrow=c(1,2))
barplot(PerformanceRating_matrix,beside =TRUE)
barplot(PerformanceRating_matrix,beside=TRUE,
        space=0.5,
        main="Barplot of PerformanceRating",
        xlab="PerformanceRating",
        ylab="Frequency",
        #names.arg=c("A","N")
        names.arg = levels(mydata$PerformanceRating),
        col = c("red","blue","green","yelow","orange")
)


#---15
#----RELATIONSHIP SATISFACTION
#SUMMARIZATION
#---COUNT,PROPORTION,%,RATIO,MODE

table(mydata$RelationshipSatisfaction)#count
table(mydata$RelationshipSatisfaction)[1]/table(mydata$RelationshipSatisfaction)[2]#ratio
table(mydata$RelationshipSatisfaction)[2]/table(mydata$RelationshipSatisfaction)[3]#ratio
table(mydata$RelationshipSatisfaction)[3]/table(mydata$RelationshipSatisfaction)[4]#ratio
RelationshipSatisfaction_matrix<-as.matrix(table(mydata$RelationshipSatisfaction))
RelationshipSatisfaction_matrix
prop.table(table(mydata$RelationshipSatisfaction))#proportion
round(prop.table(table(mydata$RelationshipSatisfaction))*100,2)#percentage
#table(mydata$Department)==max(table(mydata$Department))

#VISUALIZATION
#---PIECHART,BARCHART
pie(RelationshipSatisfaction_matrix,radius=1,main="Piechart of RelationshipSatisfaction",
    col=c("red","blue","green","yelow"),
    labels = levels(mydata$RelationshipSatisfaction),
    clockwise = TRUE,init.angle=0)

par(mfrow=c(1,2))
barplot(RelationshipSatisfaction_matrix,beside =TRUE)
barplot(RelationshipSatisfaction_matrix,beside=TRUE,
        space=0.5,
        main="Barplot of RelationshipSatisfaction",
        xlab="RelationshipSatisfaction",
        ylab="Frequency",
        #names.arg=c("A","N")
        names.arg = levels(mydata$RelationshipSatisfaction),
        col = c("red","blue","green","yelow")
)

#---16
#----STOCK OPTION LEVEL
#SUMMARIZATION
#---COUNT,PROPORTION,%,RATIO,MODE

table(mydata$StockOptionLevel)#count
table(mydata$StockOptionLevel)[1]/table(mydata$StockOptionLevel)[2]#ratio
table(mydata$StockOptionLevel)[2]/table(mydata$StockOptionLevel)[3]#ratio
table(mydata$StockOptionLevel)[3]/table(mydata$StockOptionLevel)[4]#ratio
StockOptionLevel_matrix<-as.matrix(table(mydata$StockOptionLevel))
StockOptionLevel_matrix
prop.table(table(mydata$StockOptionLevel))#proportion
round(prop.table(table(mydata$StockOptionLevel))*100,2)#percentage
#table(mydata$Department)==max(table(mydata$Department))

#VISUALIZATION
#---PIECHART,BARCHART
pie(StockOptionLevel_matrix,radius=1,main="Piechart of StockOptionLevel",
    col=c("red","blue","green","yelow"),
    labels = levels(mydata$StockOptionLevel),
    clockwise = TRUE,init.angle=0)

par(mfrow=c(1,2))
barplot(StockOptionLevel_matrix,beside =TRUE)
barplot(StockOptionLevel_matrix,beside=TRUE,
        space=0.5,
        main="Barplot of StockOptionLevel",
        xlab="StockOptionLevel",
        ylab="Frequency",
        #names.arg=c("A","N")
        names.arg = levels(mydata$StockOptionLevel),
        col = c("red","blue","green","yelow")
)


#---17
#----WORK LIFE BALANCE
#SUMMARIZATION
#---COUNT,PROPORTION,%,RATIO,MODE

table(mydata$WorkLifeBalance)#count
table(mydata$WorkLifeBalance)[1]/table(mydata$WorkLifeBalance)[2]#ratio
table(mydata$WorkLifeBalance)[2]/table(mydata$WorkLifeBalance)[3]#ratio
table(mydata$WorkLifeBalance)[3]/table(mydata$WorkLifeBalance)[4]#ratio
WorkLifeBalance_matrix<-as.matrix(table(mydata$WorkLifeBalance))
WorkLifeBalance_matrix
prop.table(table(mydata$WorkLifeBalance))#proportion
round(prop.table(table(mydata$WorkLifeBalance))*100,2)#percentage
#table(mydata$Department)==max(table(mydata$Department))

#VISUALIZATION
#---PIECHART,BARCHART
pie(WorkLifeBalance_matrix,radius=1,main="Piechart of WorkLifeBalance",
    col=c("red","blue","green","yelow"),
    labels = levels(mydata$WorkLifeBalance),
    clockwise = TRUE,init.angle=0)

par(mfrow=c(1,2))
barplot(WorkLifeBalance_matrix,beside =TRUE)
barplot(WorkLifeBalance_matrix,beside=TRUE,
        space=0.5,
        main="Barplot of WorkLifeBalance",
        xlab="WorkLifeBalance",
        ylab="Frequency",
        #names.arg=c("A","N")
        names.arg = levels(mydata$WorkLifeBalance),
        col = c("red","blue","green","yelow")
)



#----Q

#---1
#----AGE
#---Summarization
mean(mydata$Age)
median(mydata$Age)
min(mydata$Age)
max(mydata$Age)
range(mydata$Age)
sd(mydata$Age)
var(mydata$Age)
IQR(mydata$Age)
fivenum(mydata$Age)
summary(mydata$Age)
is.na(mydata$Age)

#VISUALIZATION
hist(mydata$Age)

abline(v=mean(mydata$Age),col = "blue")
abline(v=mean(mydata$Age) + 2*sd(mydata$Age),col = "blue")
#abline(v=mean(mydata$Age) + 3*sd(mydata$Age),col = "blue")
text(37,300,"Mean")
text(55,300,"Sd+2")
#text(2000,120,"Sd+3")

boxplot(mydata$Age,col = "red")
text(1.3,fivenum(mydata$Age),fivenum(mydata$Age))
text(1.3,fivenum(mydata$Age),c("Min","Q1","Mid","Q3","Max"))
boxplot(mydata$Age,col = "red")$out
boxplot(mydata$Age,col = "red")$stats
#mydata$Age[mydata$Age>boxplot(mydata$Age ,col = "red")]

par(mfrow=c(1,2))
png(filename ="plot1.png",
    height=480,width=480,units = "px")
dev.off()


#---2
#----DAILY RATE
#---Summarization
mean(mydata$DailyRate)
median(mydata$DailyRate)
min(mydata$DailyRate)
max(mydata$DailyRate)
range(mydata$DailyRate)
sd(mydata$DailyRate)
var(mydata$DailyRate)
IQR(mydata$DailyRate)
fivenum(mydata$DailyRate)
summary(mydata$DailyRate)
is.na(mydata$DailyRate)

#VISUALIZATION
hist(mydata$DailyRate)

abline(v=mean(mydata$DailyRate),col = "red")
abline(v=mean(mydata$DailyRate) + 2*sd(mydata$DailyRate),col = "red")
abline(v=mean(mydata$DailyRate) + 3*sd(mydata$DailyRate),col = "red")
text(800,100,"Mean")
#text(55,300,"Sd+2")
#text(2000,120,"Sd+3")

boxplot(mydata$DailyRate,col = "red")
text(1.3,fivenum(mydata$DailyRate),fivenum(mydata$DailyRate))
text(1.3,fivenum(mydata$DailyRate),c("Min","Q1","Mid","Q3","Max"))
boxplot(mydata$DailyRate,col = "red")$out
boxplot(mydata$DailyRate,col = "red")$stats
#mydata$Age[mydata$Age>boxplot(mydata$Age ,col = "red")]

#---3
#----DISTANCE FROM HOME
#---Summarization
mean(mydata$DistanceFromHome)
median(mydata$DistanceFromHome)
min(mydata$DistanceFromHome)
max(mydata$DistanceFromHome)
range(mydata$DistanceFromHome)
sd(mydata$DistanceFromHome)
var(mydata$DistanceFromHome)
IQR(mydata$DistanceFromHome)
fivenum(mydata$DistanceFromHome)
summary(mydata$DistanceFromHome)
is.na(mydata$DistanceFromHome)

#VISUALIZATION
hist(mydata$DistanceFromHome)

abline(v=mean(mydata$DistanceFromHome),col = "red")
abline(v=mean(mydata$DistanceFromHome) + 2*sd(mydata$DistanceFromHome),col = "red")
abline(v=mean(mydata$DistanceFromHome) + 3*sd(mydata$DistanceFromHome),col = "red")
text(8,300,"Mean")
text(26,300,"Sd+2")
#text(2000,120,"Sd+3")

boxplot(mydata$DistanceFromHome,col = "red")
text(1.3,fivenum(mydata$DistanceFromHome),fivenum(mydata$DistanceFromHome))
text(1.3,fivenum(mydata$DistanceFromHome),c("Min","Q1","Mid","Q3","Max"))
boxplot(mydata$DistanceFromHome,col = "red")$out
boxplot(mydata$DistanceFromHome,col = "red")$stats


#---4
#----EMPLOYEE NUMBER
#---Summarization
mean(mydata$EmployeeNumber)
median(mydata$EmployeeNumber)
min(mydata$EmployeeNumber)
max(mydata$EmployeeNumber)
range(mydata$EmployeeNumber)
sd(mydata$EmployeeNumber)
var(mydata$EmployeeNumber)
IQR(mydata$EmployeeNumber)
fivenum(mydata$EmployeeNumber)
summary(mydata$EmployeeNumber)
is.na(mydata$EmployeeNumber)

#VISUALIZATION
hist(mydata$EmployeeNumber)

abline(v=mean(mydata$EmployeeNumber),col = "blue")
abline(v=mean(mydata$EmployeeNumber) + 2*sd(mydata$EmployeeNumber),col = "blue")
#abline(v=mean(mydata$Age) + 3*sd(mydata$Age),col = "blue")
text(37,300,"Mean")
text(55,300,"Sd+2")
#text(2000,120,"Sd+3")

boxplot(mydata$EmployeeNumber,col = "red")
text(1.3,fivenum(mydata$EmployeeNumber),fivenum(mydata$EmployeeNumber))
text(1.3,fivenum(mydata$EmployeeNumber),c("Min","Q1","Mid","Q3","Max"))
boxplot(mydata$EmployeeNumber,col = "red")$out
boxplot(mydata$EmployeeNumber,col = "red")$stats


#---5
#----HOURLY RATE
#---Summarization
mean(mydata$HourlyRate)
median(mydata$HourlyRate)
min(mydata$HourlyRate)
max(mydata$HourlyRate)
range(mydata$HourlyRate)
sd(mydata$HourlyRate)
var(mydata$HourlyRate)
IQR(mydata$HourlyRate)
fivenum(mydata$HourlyRate)
summary(mydata$HourlyRate)
is.na(mydata$HourlyRate)

#VISUALIZATION
hist(mydata$HourlyRate)

abline(v=mean(mydata$HourlyRate),col = "red")
abline(v=mean(mydata$HourlyRate) + 2*sd(mydata$HourlyRate),col = "red")
abline(v=mean(mydata$HourlyRate) + 3*sd(mydata$HourlyRate),col = "red")
text(800,100,"Mean")
#text(55,300,"Sd+2")
#text(2000,120,"Sd+3")

boxplot(mydata$HourlyRate,col = "red")
text(1.3,fivenum(mydata$HourlyRate),fivenum(mydata$HourlyRate))
text(1.3,fivenum(mydata$HourlyRate),c("Min","Q1","Mid","Q3","Max"))
boxplot(mydata$HourlyRate,col = "red")$out
boxplot(mydata$HourlyRate,col = "red")$stats


#---6
#----MONTHLY INCOME
#---Summarization
mean(mydata$MonthlyIncome)
median(mydata$MonthlyIncome)
min(mydata$MonthlyIncome)
max(mydata$MonthlyIncome)
range(mydata$MonthlyIncome)
sd(mydata$MonthlyIncome)
var(mydata$MonthlyIncome)
IQR(mydata$MonthlyIncome)
fivenum(mydata$MonthlyIncome)
summary(mydata$MonthlyIncome)
is.na(mydata$MonthlyIncome)

#VISUALIZATION
hist(mydata$MonthlyIncome)

abline(v=mean(mydata$MonthlyIncome),col = "red")
abline(v=mean(mydata$MonthlyIncome) + 2*sd(mydata$MonthlyIncome),col = "red")
abline(v=mean(mydata$MonthlyIncome) + 3*sd(mydata$MonthlyIncome),col = "red")
text(800,100,"Mean")
#text(55,300,"Sd+2")
#text(2000,120,"Sd+3")

boxplot(mydata$MonthlyIncome,col = "red")
text(1.3,fivenum(mydata$MonthlyIncome),fivenum(mydata$MonthlyIncome))
text(1.3,fivenum(mydata$MonthlyIncome),c("Min","Q1","Mid","Q3","Max"))
boxplot(mydata$MonthlyIncome,col = "red")$out
boxplot(mydata$MonthlyIncome,col = "red")$stats


#---7
#----MONTHLY RATE
#---Summarization
mean(mydata$MonthlyRate)
median(mydata$MonthlyRate)
min(mydata$MonthlyRate)
max(mydata$MonthlyRate)
range(mydata$MonthlyRate)
sd(mydata$MonthlyRate)
var(mydata$MonthlyRate)
IQR(mydata$MonthlyRate)
fivenum(mydata$MonthlyRate)
summary(mydata$MonthlyRate)
is.na(mydata$MonthlyRate)

#VISUALIZATION
hist(mydata$MonthlyRate)

abline(v=mean(mydata$MonthlyRate),col = "red")
abline(v=mean(mydata$MonthlyRate) + 2*sd(mydata$MonthlyRate),col = "red")
abline(v=mean(mydata$MonthlyRate) + 3*sd(mydata$MonthlyRate),col = "red")
text(800,100,"Mean")
#text(55,300,"Sd+2")
#text(2000,120,"Sd+3")

boxplot(mydata$MonthlyRate,col = "red")
text(1.3,fivenum(mydata$MonthlyRate),fivenum(mydata$MonthlyRate))
text(1.3,fivenum(mydata$MonthlyRate),c("Min","Q1","Mid","Q3","Max"))
boxplot(mydata$MonthlyRate,col = "red")$out
boxplot(mydata$MonthlyRate,col = "red")$stats


#---8
#----NUM COMPANIES WORKED
#---Summarization
mean(mydata$NumCompaniesWorked)
median(mydata$NumCompaniesWorked)
min(mydata$NumCompaniesWorked)
max(mydata$NumCompaniesWorked)
range(mydata$NumCompaniesWorked)
sd(mydata$NumCompaniesWorked)
var(mydata$NumCompaniesWorked)
IQR(mydata$NumCompaniesWorked)
fivenum(mydata$NumCompaniesWorked)
summary(mydata$NumCompaniesWorked)
is.na(mydata$NumCompaniesWorked)

#VISUALIZATION
hist(mydata$NumCompaniesWorked)

abline(v=mean(mydata$NumCompaniesWorked),col = "red")
abline(v=mean(mydata$NumCompaniesWorked) + 2*sd(mydata$NumCompaniesWorked),col = "red")
abline(v=mean(mydata$NumCompaniesWorked) + 3*sd(mydata$NumCompaniesWorked),col = "red")
text(800,100,"Mean")
#text(55,300,"Sd+2")
#text(2000,120,"Sd+3")

boxplot(mydata$NumCompaniesWorked,col = "red")
text(1.3,fivenum(mydata$NumCompaniesWorked),fivenum(mydata$NumCompaniesWorked))
text(1.3,fivenum(mydata$NumCompaniesWorked),c("Min","Q1","Mid","Q3","Max"))
boxplot(mydata$NumCompaniesWorked,col = "red")$out
boxplot(mydata$NumCompaniesWorked,col = "red")$stats


#---9
#----PERCENR SALARY HIKE
#---Summarization
mean(mydata$PercentSalaryHike)
median(mydata$PercentSalaryHike)
min(mydata$PercentSalaryHike)
max(mydata$PercentSalaryHike)
range(mydata$PercentSalaryHike)
sd(mydata$PercentSalaryHike)
var(mydata$PercentSalaryHike)
IQR(mydata$PercentSalaryHike)
fivenum(mydata$PercentSalaryHike)
summary(mydata$PercentSalaryHike)
is.na(mydata$PercentSalaryHike)

#VISUALIZATION
hist(mydata$PercentSalaryHike)

abline(v=mean(mydata$PercentSalaryHike),col = "red")
abline(v=mean(mydata$PercentSalaryHike) + 2*sd(mydata$PercentSalaryHike),col = "red")
abline(v=mean(mydata$PercentSalaryHike) + 3*sd(mydata$PercentSalaryHike),col = "red")
text(800,100,"Mean")
#text(55,300,"Sd+2")
#text(2000,120,"Sd+3")

boxplot(mydata$PercentSalaryHike,col = "red")
text(1.3,fivenum(mydata$PercentSalaryHike),fivenum(mydata$PercentSalaryHike))
text(1.3,fivenum(mydata$PercentSalaryHike),c("Min","Q1","Mid","Q3","Max"))
boxplot(mydata$PercentSalaryHike,col = "red")$out
boxplot(mydata$PercentSalaryHike,col = "red")$stats


#---10
#----STANDARD HOURS
#---Summarization
mean(mydata$StandardHours)
median(mydata$StandardHours)
min(mydata$StandardHours)
max(mydata$StandardHours)
range(mydata$StandardHours)
sd(mydata$StandardHours)
var(mydata$StandardHours)
IQR(mydata$StandardHours)
fivenum(mydata$StandardHours)
summary(mydata$StandardHours)
is.na(mydata$StandardHours)

#VISUALIZATION
hist(mydata$StandardHours)

abline(v=mean(mydata$StandardHours),col = "red")
abline(v=mean(mydata$StandardHours) + 2*sd(mydata$StandardHours),col = "red")
abline(v=mean(mydata$StandardHours) + 3*sd(mydata$StandardHours),col = "red")
text(800,100,"Mean")
#text(55,300,"Sd+2")
#text(2000,120,"Sd+3")

boxplot(mydata$StandardHours,col = "red")
text(1.3,fivenum(mydata$StandardHours),fivenum(mydata$StandardHours))
text(1.3,fivenum(mydata$StandardHours),c("Min","Q1","Mid","Q3","Max"))
boxplot(mydata$StandardHours,col = "red")$out
boxplot(mydata$StandardHours,col = "red")$stats


#---11
#----TOTAL WORKING YEARS
#---Summarization
mean(mydata$TotalWorkingYears)
median(mydata$TotalWorkingYears)
min(mydata$TotalWorkingYears)
max(mydata$TotalWorkingYears)
range(mydata$TotalWorkingYears)
sd(mydata$TotalWorkingYears)
var(mydata$TotalWorkingYears)
IQR(mydata$TotalWorkingYears)
fivenum(mydata$TotalWorkingYears)
summary(mydata$TotalWorkingYears)
is.na(mydata$TotalWorkingYears)

#VISUALIZATION
hist(mydata$TotalWorkingYears)

abline(v=mean(mydata$TotalWorkingYears),col = "red")
abline(v=mean(mydata$TotalWorkingYears) + 2*sd(mydata$TotalWorkingYears),col = "red")
abline(v=mean(mydata$TotalWorkingYears) + 3*sd(mydata$TotalWorkingYears),col = "red")
text(800,100,"Mean")
#text(55,300,"Sd+2")
#text(2000,120,"Sd+3")

boxplot(mydata$TotalWorkingYears,col = "red")
text(1.3,fivenum(mydata$TotalWorkingYears),fivenum(mydata$TotalWorkingYears))
text(1.3,fivenum(mydata$TotalWorkingYears),c("Min","Q1","Mid","Q3","Max"))
boxplot(mydata$TotalWorkingYears,col = "red")$out
boxplot(mydata$TotalWorkingYears,col = "red")$stats



#---12
#----TRAINING TIMES LAST YEAR
#---Summarization
mean(mydata$TrainingTimesLastYear)
median(mydata$TrainingTimesLastYear)
min(mydata$TrainingTimesLastYear)
max(mydata$TrainingTimesLastYear)
range(mydata$TrainingTimesLastYear)
sd(mydata$TrainingTimesLastYear)
var(mydata$TrainingTimesLastYear)
IQR(mydata$TrainingTimesLastYear)
fivenum(mydata$TrainingTimesLastYear)
summary(mydata$TrainingTimesLastYear)
is.na(mydata$TrainingTimesLastYear)

#VISUALIZATION
hist(mydata$TrainingTimesLastYear)

abline(v=mean(mydata$TrainingTimesLastYear),col = "red")
abline(v=mean(mydata$TrainingTimesLastYear) + 2*sd(mydata$TrainingTimesLastYear),col = "red")
abline(v=mean(mydata$TrainingTimesLastYear) + 3*sd(mydata$TrainingTimesLastYear),col = "red")
text(800,100,"Mean")
#text(55,300,"Sd+2")
#text(2000,120,"Sd+3")

boxplot(mydata$TrainingTimesLastYear,col = "red")
text(1.3,fivenum(mydata$TrainingTimesLastYear),fivenum(mydata$TrainingTimesLastYear))
text(1.3,fivenum(mydata$TrainingTimesLastYear),c("Min","Q1","Mid","Q3","Max"))
boxplot(mydata$TrainingTimesLastYear,col = "red")$out
boxplot(mydata$TrainingTimesLastYear,col = "red")$stats


#---13
#----YEARS AT COMPANY
#---Summarization
mean(mydata$YearsAtCompany)
median(mydata$YearsAtCompany)
min(mydata$YearsAtCompany)
max(mydata$YearsAtCompany)
range(mydata$YearsAtCompany)
sd(mydata$YearsAtCompany)
var(mydata$YearsAtCompany)
IQR(mydata$YearsAtCompany)
fivenum(mydata$YearsAtCompany)
summary(mydata$YearsAtCompany)
is.na(mydata$YearsAtCompany)

#VISUALIZATION
hist(mydata$YearsAtCompany)

abline(v=mean(mydata$YearsAtCompany),col = "red")
abline(v=mean(mydata$YearsAtCompany) + 2*sd(mydata$YearsAtCompany),col = "red")
abline(v=mean(mydata$YearsAtCompany) + 3*sd(mydata$YearsAtCompany),col = "red")
text(800,100,"Mean")
#text(55,300,"Sd+2")
#text(2000,120,"Sd+3")

boxplot(mydata$YearsAtCompany,col = "red")
text(1.3,fivenum(mydata$YearsAtCompany),fivenum(mydata$YearsAtCompany))
text(1.3,fivenum(mydata$YearsAtCompany),c("Min","Q1","Mid","Q3","Max"))
boxplot(mydata$YearsAtCompany,col = "red")$out
boxplot(mydata$YearsAtCompany,col = "red")$stats

#---14
#----YEARS IN CURRENT ROLE
#---Summarization
mean(mydata$YearsInCurrentRole)
median(mydata$YearsInCurrentRole)
min(mydata$YearsInCurrentRole)
max(mydata$YearsInCurrentRole)
range(mydata$YearsInCurrentRole)
sd(mydata$YearsInCurrentRole)
var(mydata$YearsInCurrentRole)
IQR(mydata$YearsInCurrentRole)
fivenum(mydata$YearsInCurrentRole)
summary(mydata$YearsInCurrentRole)
is.na(mydata$YearsInCurrentRole)

#VISUALIZATION
hist(mydata$YearsInCurrentRole)

abline(v=mean(mydata$YearsInCurrentRole),col = "red")
abline(v=mean(mydata$YearsInCurrentRole) + 2*sd(mydata$YearsInCurrentRole),col = "red")
abline(v=mean(mydata$YearsInCurrentRole) + 3*sd(mydata$YearsInCurrentRole),col = "red")
text(800,100,"Mean")
#text(55,300,"Sd+2")
#text(2000,120,"Sd+3")

boxplot(mydata$YearsInCurrentRole,col = "red")
text(1.3,fivenum(mydata$YearsInCurrentRole),fivenum(mydata$YearsInCurrentRole))
text(1.3,fivenum(mydata$YearsInCurrentRole),c("Min","Q1","Mid","Q3","Max"))
boxplot(mydata$YearsInCurrentRole,col = "red")$out
boxplot(mydata$YearsInCurrentRole,col = "red")$stats


#---15
#----YEARS SINCE LAST PROMOTION
#---Summarization
mean(mydata$YearsSinceLastPromotion)
median(mydata$YearsSinceLastPromotion)
min(mydata$YearsSinceLastPromotion)
max(mydata$YearsSinceLastPromotion)
range(mydata$YearsSinceLastPromotion)
sd(mydata$YearsSinceLastPromotion)
var(mydata$YearsSinceLastPromotion)
IQR(mydata$YearsSinceLastPromotion)
fivenum(mydata$YearsSinceLastPromotion)
summary(mydata$YearsSinceLastPromotion)
is.na(mydata$YearsSinceLastPromotion)

#VISUALIZATION
hist(mydata$YearsSinceLastPromotion)

abline(v=mean(mydata$YearsSinceLastPromotion),col = "red")
abline(v=mean(mydata$YearsSinceLastPromotion) + 2*sd(mydata$YearsSinceLastPromotion),col = "red")
abline(v=mean(mydata$YearsSinceLastPromotion) + 3*sd(mydata$YearsSinceLastPromotion),col = "red")
text(800,100,"Mean")
#text(55,300,"Sd+2")
#text(2000,120,"Sd+3")

boxplot(mydata$YearsSinceLastPromotion,col = "red")
text(1.3,fivenum(mydata$YearsSinceLastPromotion),fivenum(mydata$YearsSinceLastPromotion))
text(1.3,fivenum(mydata$YearsSinceLastPromotion),c("Min","Q1","Mid","Q3","Max"))
boxplot(mydata$YearsSinceLastPromotion,col = "red")$out
boxplot(mydata$YearsSinceLastPromotion,col = "red")$stats


#---16
#----YEARS WITH CURRENT MANAGER
#---Summarization
mean(mydata$YearsWithCurrManager)
median(mydata$YearsWithCurrManager)
min(mydata$YearsWithCurrManager)
max(mydata$YearsWithCurrManager)
range(mydata$YearsWithCurrManager)
sd(mydata$YearsWithCurrManager)
var(mydata$YearsWithCurrManager)
IQR(mydata$YearsWithCurrManager)
fivenum(mydata$YearsWithCurrManager)
summary(mydata$YearsWithCurrManager)
is.na(mydata$YearsWithCurrManager)

#VISUALIZATION
hist(mydata$YearsWithCurrManager)

abline(v=mean(mydata$YearsWithCurrManager),col = "red")
abline(v=mean(mydata$YearsWithCurrManager) + 2*sd(mydata$YearsWithCurrManager),col = "red")
abline(v=mean(mydata$YearsWithCurrManager) + 3*sd(mydata$YearsWithCurrManager),col = "red")
text(800,100,"Mean")
#text(55,300,"Sd+2")
#text(2000,120,"Sd+3")

boxplot(mydata$YearsWithCurrManager,col = "red")
text(1.3,fivenum(mydata$YearsWithCurrManager),fivenum(mydata$YearsWithCurrManager))
text(1.3,fivenum(mydata$YearsWithCurrManager),c("Min","Q1","Mid","Q3","Max"))
boxplot(mydata$YearsWithCurrManager,col = "red")$out
boxplot(mydata$YearsWithCurrManager,col = "red")$stats


#----------------------BIVARIATE ANALYSIS----------------------#

#------C-->Q
#SUMMARIZATION 
#VISUALIZATION

#---JobInvolvement->Age
boxplot(mydata$Age~mydata$JobInvolvement,
        col=c(2,4))$stats
#anova F test
summary(aov(mydata$JobInvolvement~mydata$Age))

#---JobLevel->Age
boxplot(mydata$Age~mydata$JobLevel,
        col=c(2,4))$stats

#---JobRole->Age
boxplot(mydata$Age~mydata$JobRole,
        col=c(2,4))$stats

####--------DistanceFromHome
boxplot(mydata$DistanceFromHome~mydata$Attrition,col=c(2,4))
#anova F test
summary(aov(mydata$DistanceFromHome~mydata$Attrition))
#t<-table(mydata$DistanceFromHome,mydata$Attrition)
#barplot(t,col = c(2,4),beside = TRUE)
####--------MonthlyIncome
boxplot(mydata$MonthlyIncome~mydata$Attrition,col=c(2,4))$stats
#anova F test
summary(aov(mydata$MonthlyIncome~mydata$Attrition))

##-----MonthlyRate
boxplot(mydata$MonthlyRate~mydata$Attrition,col=c(2,4))$stats
#anova F test
summary(aov(mydata$MonthlyRate~mydata$Attrition))

####-------TotalWorkingYears
boxplot(mydata$TotalWorkingYears~mydata$Attrition,col=c(2,4))$stats
#anova F test
summary(aov(mydata$TotalWorkingYears~mydata$Attrition))

####-------YearsAtCompany
boxplot(mydata$YearsAtCompany~mydata$Attrition,col=c(2,4))$stats
#anova F test
summary(aov(mydata$YearsAtCompany~mydata$Attrition))

####-------YearsInCurrentRole
boxplot(mydata$YearsInCurrentRole~mydata$Attrition,col=c(2,4))$stats
#anova F test
summary(aov(mydata$YearsInCurrentRole~mydata$Attrition))

####--------YearsWithCurrManager
boxplot(mydata$YearsWithCurrManager~mydata$Attrition,col=c(2,4))$stats
#anova F test
summary(aov(mydata$YearsWithCurrManager~mydata$Attrition))



#C-->C
#SUMMARIZATION 
#Comparative Proportion
library(vcd)

####-------Attrition rate with BusinessTravel
mytable<-xtabs(~BusinessTravel + Attrition ,data=mydata)
prop.table(mytable,1)
#VISUALIZATION
#Contingency Table
chi.tab1<-table(mydata$BusinessTravel,mydata$Attrition)
#TOH
#Chi-Square Test
chi.test1<-chisq.test(chi.tab1)
chi.test1
chi.test1$observed
chi.test1$expected
chi.test1$residuals
sum((chi.test1$residuals)^2)
mosaic(mytable,shade = TRUE, main = "mydata")
?mosaic
library(ggplot2)

# grouped bar plot

ggplot(mydata,aes(Attrition, 
       BusinessTravel)) + 
    geom_bar(position = "stack")

####--------Department wise Attrition Rate
mytable<-xtabs(~Department + Attrition ,data=mydata)
prop.table(mytable,1)
chi.tab1<-table(mydata$Department,mydata$Attrition)
#TOH
#Chi-Square Test
chi.test1<-chisq.test(chi.tab1)
chi.test1
mosaic(mytable,shade = TRUE, main = "mydata")

##------Educationwise Attrition Rate
mytable<-xtabs(~Education + Attrition ,data=mydata)
prop.table(mytable,1)
chi.tab1<-table(mydata$Education,mydata$Attrition)
#TOH
#Chi-Square Test
chi.test1<-chisq.test(chi.tab1)
chi.test1
mosaic(mytable,shade = TRUE, main = "mydata")

####----------EducationFieldwise Attrition Rate
mytable<-xtabs(~EducationField + Attrition ,data=mydata)
prop.table(mytable,1)
chi.tab1<-table(mydata$EducationField,mydata$Attrition)
#TOH
#Chi-Square Test
chi.test1<-chisq.test(chi.tab1)
chi.test1
mosaic(mytable,shade = TRUE, main = "mydata")

####----------Effect of EnvironmentSatisfaction on Attrition
mytable<-xtabs(~EnvironmentSatisfaction + Attrition ,data=mydata)
prop.table(mytable,1)
chi.tab1<-table(mydata$EnvironmentSatisfaction,mydata$Attrition)
#TOH
#Chi-Square Test
chi.test1<-chisq.test(chi.tab1)
chi.test1
mosaic(mytable,shade = TRUE, main = "mydata")

##---Attrition Rate with Gender
mytable<-xtabs(~Gender + Attrition ,data=mydata)
prop.table(mytable,1)
chi.tab1<-table(mydata$Gender,mydata$Attrition)
#TOH
#Chi-Square Test
chi.test1<-chisq.test(chi.tab1)
chi.test1
mosaic(mytable,shade = TRUE, main = "mydata")

####--------Attrition Rate with JobInvolvement
mytable<-xtabs(~JobInvolvement + Attrition ,data=mydata)
prop.table(mytable,1)
chi.tab1<-table(mydata$JobInvolvement,mydata$Attrition)
#TOH
#Chi-Square Test
chi.test1<-chisq.test(chi.tab1)
chi.test1
mosaic(mytable,shade = TRUE, main = "mydata")

####--------Effect of JobLevel on Attrition rate
mytable<-xtabs(~JobLevel + Attrition ,data=mydata)
prop.table(mytable,1)
chi.tab1<-table(mydata$JobLevel,mydata$Attrition)
#TOH
#Chi-Square Test
chi.test1<-chisq.test(chi.tab1)
chi.test1
mosaic(mytable,shade = TRUE, main = "mydata")

####--------Effect of JobRole on Attrition rate
mytable<-xtabs(~JobRole + Attrition ,data=mydata)
prop.table(mytable,1)
chi.tab1<-table(mydata$JobRole,mydata$Attrition)
#TOH
#Chi-Square Test
chi.test1<-chisq.test(chi.tab1)
chi.test1
mosaic(mytable,shade = TRUE, main = "mydata")

####--------Effect of JobSatisfaction on Attrition rate
mytable<-xtabs(~JobSatisfaction + Attrition ,data=mydata)
prop.table(mytable,1)
chi.tab1<-table(mydata$JobSatisfaction,mydata$Attrition)
#TOH
#Chi-Square Test
chi.test1<-chisq.test(chi.tab1)
chi.test1
mosaic(mytable,shade = TRUE, main = "mydata")

####---------Effect of MaritalStatus on Attrition rate
mytable<-xtabs(~MaritalStatus + Attrition ,data=mydata)
prop.table(mytable,1)
mosaic(mytable,shade = TRUE, main = "mydata")

####---------Effect of OverTime on Attrition rate
mytable<-xtabs(~OverTime + Attrition ,data=mydata)
prop.table(mytable,1)
mosaic(mytable,shade = TRUE, main = "mydata")

##--Effect of PerformanceRating on Attrition rate
mytable<-xtabs(~PerformanceRating + Attrition ,data=mydata)
prop.table(mytable,1)
mosaic(mytable,shade = TRUE, main = "mydata")

####---------Effect of RelationSatisfaction on Attrition rate
mytable<-xtabs(~RelationshipSatisfaction + Attrition ,data=mydata)
prop.table(mytable,1)
mosaic(mytable,shade = TRUE, main = "mydata")

####---------Effect of WorkLifeBalance on Attrition rate
mytable<-xtabs(~WorkLifeBalance + Attrition ,data=mydata)
prop.table(mytable,1)
mosaic(mytable,shade = TRUE, main = "mydata")



#########---------------------------MODEL BUILDING---------------------------#########

set.seed(1)
a<-11:20
a
a.train<- sample(length(a),0.6*length(a) )
a.train
a[a.train]
a[-a.train]

nrow(mydata)
train<-sample(1:nrow(mydata),0.7*nrow(mydata))
train
test<-(-train)
test


####-------------Logistic Regression-------------#####
library(ROCR)

log.fit=glm(Attrition~BusinessTravel+DistanceFromHome+EnvironmentSatisfaction+JobInvolvement
            +JobLevel+JobSatisfaction+OverTime,data=mydata[train,],family=binomial)#Generalized Linear Model
summary(log.fit)
log.pred<-predict(log.fit,newdata = mydata[test,],type="response")
length(log.pred)

log.pred1<-predict(log.fit,newdata = mydata[train,],type="response")
ROCRPred<-prediction(log.pred1,mydata$Attrition[train])
ROCRPerf<-performance(ROCRPred,"tpr","fpr")

plot(ROCRPerf,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))

confmatrix<-table(ActualValue=mydata$Attrition[test],PredictedValue=log.pred >0.3)
sum(diag(confmatrix))
sum(confmatrix)
accuracy<-sum(diag((confmatrix))/sum(confmatrix))
print(accuracy)
error<-1-accuracy
print(error)

####-----Logistic Regression Model2
log.fit1=glm(Attrition~BusinessTravel+DistanceFromHome+Department+EducationField+
                EnvironmentSatisfaction+Gender+JobInvolvement+JobLevel+JobSatisfaction
                +MaritalStatus+OverTime+RelationshipSatisfaction+WorkLifeBalance
                ,data=mydata[train,],family=binomial)#Generalized Linear Model
summary(log.fit1)

log.pred<-predict(log.fit1,newdata = mydata[test,],type="response")
length(log.pred)

log.pred1<-predict(log.fit1,newdata = mydata[train,],type="response")
ROCRPred<-prediction(log.pred1,mydata$Attrition[train])
ROCRPerf<-performance(ROCRPred,"tpr","fpr")

plot(ROCRPerf,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))

confmatrix<-table(ActualValue=mydata$Attrition[test],PredictedValue=log.pred >0.3)
sum(diag(confmatrix))
sum(confmatrix)
accuracy<-sum(diag((confmatrix))/sum(confmatrix))
print(accuracy)
error<-1-accuracy
print(error)





####--------------------Linear Regression----------------####

mydatalm1<-lm(MonthlyIncome~TotalWorkingYears+YearsAtCompany,data=mydata)
summary(mydatalm1)
coef(mydatalm1)

#boxplot(mydata$Attrition,mydata$DistanceFromHome)
#abline(mydatalm1)


TSS<- sum((mydata$Attrition-mean(mydata$Attrition))^2)
TSS
#RSS(sum(y-yhat))
RSS1<-sum((mydata$Attrition-mydatalm1$fitted.values)^2)
RSS1
RSS1<-sum((mydatalm1$residuals)^2)
points(mydata$DistanceFromHome,mydatalm1$fitted.values,pch=3)
#RSQ
(TSS-RSS1)/TSS
#Adj RSQ
summary(mydatalm1)$adj.r.squared
#RSE-Residual Standard Error
summary(mydatalm1)$sigma
#Summary
summary(mydatalm1)
#Homoscedastic(mean(residual)=0,var=constant,residual=normal)
mean(mydatalm1$residuals)
shapiro.test(mydatalm1$residuals)#HO:Normal,Ha:Not Normal
#Plots
#plot(mydatalm1,1)
#plot(mydatalm1,3)
#Prediction





