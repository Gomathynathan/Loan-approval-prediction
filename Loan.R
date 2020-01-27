getwd()
setwd("C:/Users/GOMATHY/Desktop")
install.packages("pROC")
library(pROC)
install.packages("dplyr")
library(dplyr)
loan<-read.csv("loantrain.csv")
loantest<-read.csv("loantest.csv")
loantest$Loan_ID<-as.character(loantest$Loan_ID)

summary(loan$Gender)
loan$Gender[which(loan$Gender=="")]<-"Male"
loan$Gender<-factor(loan$Gender,levels = c("Male","Female"))
summary(loan$Gender)

summary(loan$Married)
loan$Married[which(loan$Married=="")]<-"Yes"
loan$Married<-factor(loan$Married,levels = c("Yes","No"),labels = c("Married","Not married"))
summary(loan$Dependents)
loan$Dependents[which(loan$Dependents=="")]<-"0"
loan$Dependents<-factor(loan$Dependents,levels = c("0","1","2","3+"))
summary(loan$Education)

loan$Education<-factor(loan$Education,levels = c("Graduate","Not Graduate"))

summary(loan$Self_Employed)
loan$Self_Employed[which(loan$Self_Employed=="")]<-"No"
loan$Self_Employed<-factor(loan$Self_Employed,levels = c("Yes","No"),labels = c("Self employed","Not self employed"))
summary(loan$Self_Employed)

loan$ApplicantIncome[which(is.na(loan$ApplicantIncome))]
loan$CoapplicantIncome[which(is.na(loan$CoapplicantIncome))]
sum((is.na(loan$LoanAmount)))

summary(loan$LoanAmount)
loan$LoanAmount[which(is.na(loan$LoanAmount))]<-128
summary((loan$LoanAmount))
sum((is.na(loan$Loan_Amount_Term)))
summary(loan$Loan_Amount_Term)
loan$Loan_Amount_Term[which(is.na(loan$Loan_Amount_Term))]<-360
## As the median and 3rd quartile of loan amount term was 360, we replace NA with 360

summary(loan$Credit_History)
loan$LoanAmount[which(is.na(loan$Credit_History))]
##Checking if the credit history NA people dont have a loan amount.But as they have loan amount
##and also median being 1, NA people's credit history should be 1
loan$Credit_History[which(is.na(loan$Credit_History))]<-"1"
loan$Credit_History<-factor(loan$Credit_History,levels = c("0","1"),labels = c("No credit history","Have credit history"))
levels(loan$Property_Area)
summary(loan)
str(loan)
loan$loanstatusnew<-ifelse(loan$Loan_Status=="Y",1,0)
loan$totalincome<-loan$ApplicantIncome+loan$CoapplicantIncome
##Test data imputation
loantest$Gender[which(loantest$Gender=="")]<-"Male"
loantest$Gender<-factor(loantest$Gender,levels = c("Male","Female"))


loantest$Married[which(loantest$Married=="")]<-"Yes"
loantest$Married<-factor(loantest$Married,levels = c("Yes","No"),labels = c("Married","Not married"))

loantest$Dependents[which(loantest$Dependents=="")]<-"0"
loantest$Dependents<-factor(loantest$Dependents,levels = c("0","1","2","3+"))

loantest$Self_Employed[which(loantest$Self_Employed=="")]<-"No"
loantest$Self_Employed<-factor(loantest$Self_Employed,levels = c("Yes","No"),labels = c("Self employed","Not self employed"))

loantest$totalincome<-loantest$ApplicantIncome+loantest$CoapplicantIncome


loantest$Credit_History[which(is.na(loantest$Credit_History))]<-"1"
loantest$Credit_History<-factor(loantest$Credit_History,levels = c("0","1"),labels = c("No credit history","Have credit history"))

loantest$Loan_Amount_Term[which(is.na(loantest$Loan_Amount_Term))]<-360


loantest$LoanAmount[which(is.na(loantest$LoanAmount))]<-128


summary(loantest)





##univariate analysis
pie(table(loan$Gender))
##Male apppliacnts were more
pie(table(loan$Married))
##Married applicants were more
    pie(table(loan$Dependents))
    ##More than 50 % applicants didnot have dependents
    pie(table(loan$Education))
    ##more than 50 % applicants were garduate
    pie(table(loan$Self_Employed))
    ##More than 75% applicants were not self employed
    pie(table(loan$Credit_History))
    ##More than 75% applicants had credit history
    pie(table(loan$Property_Area))
    ##About 35% applicants were from semi urban
    
    
    ##Bi variate analysis
boxplot(loan$totalincome~loan$Loan_Status,outline=FALSE,xlab="Loan Approved status",ylab="Total Income")
##Both loan approved and not approved hadsame median total income of about 6000.The 3rd qaurtile of 
##loan approved appicant's totla income was less than that of non-approved applicnats


boxplot(loan$LoanAmount~loan$Loan_Status,outline=FALSE)



married_loan<-table(loan$Loan_Status,loan$Married)
barplot(prop.table(married_loan,2),ylim=c(0,0.9),col=c("pink","cornflowerblue"),beside = TRUE,xlab=c("Martial status"),ylab = "Loan Approval status")
legend(3.03,0.9,legend = c("Loan notapproved","Loan Apporved"),col=c("pink","cornflowerblue"),lty = 18)

gender_loan<-table(loan$Loan_Status,loan$Gender)
barplot(prop.table(gender_loan,2),beside=TRUE,ylim=c(0,1),col=c("pink","cornflowerblue"),xlab = "Sex",ylab = "Loan Approval status")
legend(3,0.97,legend = c("Loan notapproved","Loan Apporved"),col=c("pink","cornflowerblue"),lty = 18)

edu_loan<-table(loan$Loan_Status,loan$Education)
barplot(prop.table(edu_loan,2),beside=TRUE,ylim=c(0,1),col=c("pink","cornflowerblue"),xlab = "Education",ylab = "Loan Approval status")
legend(3,0.97,legend = c("Loan notapproved","Loan Apporved"),col=c("pink","cornflowerblue"),lty = 18)

dep_loan<-table(loan$Loan_Status,loan$Dependents)
barplot(prop.table(dep_loan,2),beside=TRUE,ylim=c(0,1),col=c("pink","cornflowerblue"),xlab = "Dependents",ylab = "Loan Approval status")
legend(1,0.99,legend = c("Loan notapproved","Loan Apporved"),col=c("pink","cornflowerblue"),lty = 18)


SE_loan<-table(loan$Loan_Status,loan$Self_Employed)
barplot(prop.table(SE_loan,2),beside=TRUE,ylim=c(0,1),col=c("pink","cornflowerblue"),xlab = "Self Employed status",ylab = "Loan Approval status")
legend(3,0.97,legend = c("Loan notapproved","Loan Apporved"),col=c("pink","cornflowerblue"),lty = 18)

credit_loan<-table(loan$Loan_Status,loan$Credit_History)
barplot(prop.table(credit_loan,2),beside=TRUE,ylim=c(0,1),col=c("pink","cornflowerblue"),xlab = "Self Employed status",ylab = "Loan Approval status")
legend(2,1.06,legend = c("Loan notapproved","Loan Apporved"),col=c("pink","cornflowerblue"),lty = 18)

SE_loan<-table(loan$Loan_Status,loan$Self_Employed)
barplot(prop.table(SE_loan,2),beside=TRUE,ylim=c(0,1),col=c("pink","cornflowerblue"),xlab = "Self Employed status",ylab = "Loan Approval status")
legend(3,0.97,legend = c("Loan notapproved","Loan Apporved"),col=c("pink","cornflowerblue"),lty = 18)


cor(loan$LoanAmount,loan$totalincome)







##Logistic regression

loan<-loan[,-1]
 
loanfit<-glm(data=loan,loanstatusnew~(Gender+Married+Dependents+Education+Self_Employed+totalincome+LoanAmount+Loan_Amount_Term+Credit_History+Property_Area),family = binomial("logit"))
summary(loanfit)



#-------------------------------------
## Confusion matrix, ROC curve, and AUC
##-------------------------------------
confusion <- function(m, threshold = 0.5) {
    pred <- ifelse(m$fitted.values > threshold, 1, 0)
    confused <- matrix(NA, nrow = 2, ncol = 3)
    
    indx <- (m$y == 1)
    confused[1, 3] <- sum(indx)
    confused[2, 3] <- sum(!indx)
    
    confused[1, 1] <- sum(m$y[indx] == pred[indx])
    confused[1, 2] <- confused[1, 3] - confused[1, 1]
    confused[2, 2] <- sum(m$y[!indx] == pred[!indx]) 
    confused[2, 1] <- confused[2, 3] - confused[2, 2]
    
    rownames(confused) <- c("1", "0")
    colnames(confused) <- c("1", "0", "total")
    
    cat("Sensitivity =", confused[1, 1] / confused[1, 3], "\n")
    cat("Specificity =", confused[2, 2] / confused[2, 3], "\n")
    confused
}


confusion(loanfit1)

##The sensitivity(true positive rate)= 0.98
##Specifity(true negative rate) =0.43
##The specificity seems to be low
##ROC 
install.packages("pROC")
library(pROC)
roc(loan$loanstatusnew,loanfit$fitted.values,plot = TRUE,col=c("blue","black"))
legend(0.69,.29,legend = c("Empirical ROC curve","45-degree line"),col=c("blue","black"),lty = 26)
text(0.3,0.5,"AUC=0.7979")
##The model predicts 79% correctly 


##PRediciton


predict_prob<-predict(loanfit1,type = 'response',newdata = loantest)

predict_status<-ifelse(predict_prob>0.5,"Y","N")
comparelist<-cbind(loantest$Loan_ID,predict_status)
colnames(comparelist)<-c("Loan_ID","Loan_Status")

write.csv(comparelist,file="Logistic_finalmodel.csv")
##As the specificity is about 0.44, there is only 44% chance that the model will deduct the "not apporved" as "not approved"
##correctly.That's why some of the non approved status is predcited as "Approved"


