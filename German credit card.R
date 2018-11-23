#==============================================================================================
# WILL THE CUSTOMER DEFAULT ON THE LOAN?
#==============================================================================================

#1. DATA (GERMAN CREDIT DATA)
#=============================================================================================

credit = read.csv('C:\\Users\\Kalyan Rohith T G\\Documents\\Datasets\\germancredit.csv')
View(credit)

#2. Getting a basic understanding of the dataset
#=============================================================================================

dim(credit)
str(credit)

table(credit$Default_)

#3. ANALYZING THE BAD RATE ACCROSS SEVERAL INDIVIDUAL VARIABLES
#=============================================================================================

install.packages("sqldf")

require(sqldf)

#Bad rate for credit history

sqldf("SELECT history, round(avg(Default_)*100,2) AS 'Bad Rate',
      (100 - round(avg(Default_)*100,2)) AS 'Good Rate'
      FROM credit GROUP BY history")

#Calculating the Bad Rate for all the categorical variables

for(i in 2:ncol(credit))
{
  if(is.factor(credit[,i]))
  {
    varname = names(credit)[i]
    
    string = paste("SELECT", varname, ", round(avg(Default_)*100,2) AS 'Bad Rate', (100 - round(avg(Default_)*100,2)) AS 'Good Rate' FROM credit GROUP BY", varname)
    
    #print(sqldf(string))
    
    a = sqldf(string)
    print(a)
    
  }
}

#Exporting the tables in an excel workbook in separate worksheets

install.packages("xlsx")
library(xlsx)
wb = createWorkbook()

for(i in 2:ncol(credit))
{
  if(is.factor(credit[,i]))
  {
    
    varname = names(credit)[i]
    
    sheet = createSheet(wb, varname)
    
    string = paste("SELECT", varname, ", round(avg(Default_)*100,2) AS 'Bad Rate', (100 - round(avg(Default_)*100,2)) AS 'Good Rate' FROM credit GROUP BY", varname)
    
    addDataFrame(sqldf(string), sheet = sheet, startColumn = 2, row.names = F)
    
  }
}

saveWorkbook(wb, "Bad Rate.xlsx")

#3. EXPLORING THE DATA VISUALLY
#=============================================================================================

#A. UNIVARIATE ANALYSIS

#Histogram or barplots for numerical variables

for(i in 1:ncol(credit))
{
  if(is.numeric(credit[,i]))
  {
    if(length(unique(credit[,i])) > 10)
    {
      hist(credit[,i], main = names(credit)[i], xlab = names(credit)[i])
    }
      
    else if(length(unique(credit[,i])) < 10)
    {
      barplot(table(credit[,i]), main=names(credit)[i], xlab = names(credit)[i])
    }
  }
}

#Barplots for categorical varibales
for(i in 1:ncol(credit))
{
  if(is.factor(credit[,i]))
  {
     barplot(table(credit[,i]), main=names(credit)[i], xlab = names(credit)[i])
  }
}

#B. BIVARIATE ANALYSIS

#Side-by-side Boxplots for numerical variables
for(i in 2:ncol(credit))
{
  if(is.numeric(credit[,i]))
  {
    if(length(unique(credit[,i])) > 10)
    {
      boxplot(credit[,i] ~ credit$Default_, main = names(credit)[i], ylab = names(credit)[i])
    }
      
    else if(length(unique(credit[,i])) < 10)
    {
      barplot(table(credit[,i], credit$Default_), main=names(credit)[i], 
              xlab = names(credit)[i], beside = T, legend = rownames(table(credit[,i])))
    }
  }
}

#4. RE-GROUPING THE LEVELS OF THE CATEGORICAL VARIABLES
#=============================================================================================

library(xlsx)
library(InformationValue)

#Calculating the WOE table for the variable history

WOETable(credit$history, credit$Default_)

#Exporting the WOE Table for every categorical variables in excel workbook
wb = createWorkbook()
getwd()
for(i in 2:ncol(credit))
{
  if(is.factor(credit[,i]))
  {
    
    varname = names(credit)[i]
    
    sheet = createSheet(wb, varname)
    
    woe = WOETable(credit[,varname], credit$Default_)
    
    addDataFrame(woe, sheet = sheet, startColumn = 2, row.names = F)
    
  }
}

saveWorkbook(wb, "WOE Table 2.xlsx")

#re-leveling the credit history and a few other variables
credit2 = credit
str(credit2)

#VARIABLE: Credit_Default

credit2$Default_ = factor(credit2$Default_)

#VARIABLE: Credit history
credit2$history = factor(credit$history, levels=c("A30","A31","A32","A33","A34"))
levels(credit2$history) = c("good","good","poor","poor","terrible")

#VARIABLE; Credit_foreign
credit2$foreign_ = factor(credit2$foreign_, levels=c("A201","A202"),
                          labels=c("foreign","german"))
table(credit2$foreign_)
table(credit$foreign_)

#VARIABLE: Credit$Property
credit2$property_ = factor(credit2$property, levels=c("A121","A122","A123","A124"),
                          labels=c("real_estate","building society savings","car", "No property"))
#VARIABLE: Purpose
credit2$purpose = factor(credit$purpose, levels=c("A41","A48","A43","A42","A44","A49","A45","A40","A410","A46"))
levels(credit2$purpose) = c("Re-training","Used-car","Radio TV", rep("Furniture and Domestic app.",3),
                            rep("Business or Repairs",2), "New car", "Education and Others")   #[Check the no. of levels]

#Varaible Savings
credit2$savings = factor(credit2$savings, levels = c("A61","A62","A63","A64","A65"))
levels(credit2$savings) = c("Very low", "low", "Average",'Good', 'No savings acc')


#Variable Checking account
credit2$checkingstatus1 = factor(credit2$checkingstatus1, levels = c("A11","A12","A13","A14"))
levels(credit2$checkingstatus1) = c("Low_Deposit", "Average_Deposit", "High_Deposit","No_Deposit")

View(credit2)

#UNDERSTANDING VARIABLE IMPORTANCE
#=============================================================================================
library(InformationValue)

#Using IV to understand the imporance of the categorical variables

for(i in 2:ncol(credit))
{
  if(is.factor(credit[,i]))
  {
    varname = names(credit)[i]
    
    print(varname)
    print(IV(X=factor(credit[,varname]), Y=credit$Default_))
    
  }
}

#Using t-test to understand the imporance of the numerical variables

importance = c()

for(i in 2:ncol(credit))
{
  if(!is.factor(credit[,i]))
  {
    varname = names(credit)[i]
    
    p_value = t.test(credit[,varname] ~ credit$Default_)$p.value
    
    importance[varname] = round(p_value,5)
  
  }
}

(importance)

#TRAIN-TEST SPLIT
#=============================================================================================
library(caTools)
set.seed(88)
split <- sample.split(credit2$Default_, SplitRatio = 0.75)

#get training and test data
train <- subset(credit2, split == TRUE)
test  <- subset(credit2, split == FALSE)

View(train)

#FITTING A LOGISTIC REGRESSION MODEL
#=============================================================================================
names(credit2)

#Using some important variables to fit a Logistic Regression Model

#IMPORTANT CATEGORICAL VARIABLES: 
#("checkingstatus1", "history", "purpose_new", "savings", "property")

#IMPORTANT NUMERICAL VARIABLES: 
#("duration", "amount", "installment", "age")

cat_var = c("checkingstatus1", "history", "purpose", "savings", "property", 'Default_')
num_var = c("duration", "amount", "installment", "age")
credit_new = credit2[,c(cat_var, num_var)]
names(credit_new)

#Creating dummy variables
str(credit_new)
x = model.matrix(Default_ ~ ., credit_new)[,-1] #model.matrix automatically creates dummy variables for factor variables
View(x)   
y = credit_new$Default_

#get training and test data

library(caTools)
set.seed(88)
split <- sample.split(credit_new$Default_, SplitRatio = 0.75)
train <- subset(credit_new, split == TRUE)
test  <- subset(credit_new, split == FALSE)

View(train)
X_train = (model.matrix(Default_ ~ ., train)[,-1])
View(X_train)
X_test = (model.matrix(Default_ ~ ., test)[,-1])
y_train = (train$Default_)
y_test = test$Default_
#MODEL SELECTION
#=================================================================================================

names(X_test)
#Logistic regression on train dataset, fitting all the variables 
head(X_train)
fullmod = glm(Default_ ~ ., family = binomial, data = data.frame(Default_ = y_train, X_train))
?glm
summary(fullmod)

#AIC: 749.87 for full model

#Significant variables for loan default from this model are checkingstatus1High_Deposit, checkingstatus1No_Deposit
#historypoor, historyterrible, purposeFurniture and Domestic app., purposeBusiness or Repairs, purposeEducation and Others
# savingsNo savings acc and SavingsGood

#Comparing the model with Null model to check the AIC value and deviance values 

nothing <- glm(Default_ ~ 1, family = binomial, data = data.frame(Default_ = y_train, X_train))
summary(nothing) #AIC value is 916.3 greater than fullmod 

#Now using stepwise logistic regression

backwards = step(fullmod,direction = 'backward')

summary(backwards)
all = step(fullmod)

summary(all)
plot(all,scale='bic')
coef(all,8)

# Both backward and all in method of stepwise algorithm lead to a same AIC value of 739.77 with the variables
#checkingstatus1High_Deposit, checkingstatus1No_Deposit 
#historypoor  , historyterrible  , purposeFurniture.and.Domestic.app. 
#purposeBusiness.or.Repairs   , purposeEducation.and.Others , savingsGood 
#savingsNo.savings.acc  , duration , amount 
#installment and age 

#Predict and evaluate on test data set 

pred = predict(all, newdata = data.frame(X_test), type = 'response')
data.frame(y_test,pred)

library(ROCR)
ROCpred <- prediction(pred,test$Default_)
ROCperf <- performance(ROCpred,"tpr","fpr")
plot(ROCperf)
plot(ROCperf, colorize=T, 
     print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))

auroc = AUROC(test$Default_, pred)
auroc

#MODEL VALIDATION
#=================================================================================================

#threshold = 0.16
pred1 = ifelse(pred > 0.16, 1, 0)

#ASSESSING MODEL PERFORMANCE
#=================================================================================================

#Confusion matrix
table(test$Default_, pred1)

#Sensitivity
sensitivity(test$Default_, pred1)

#Specificity
specificity(test$Default_, pred1)

#Precision
precision(test$Default_, pred1)

#Youden's Index (Sensitivity + Specificity - 1)
youdensIndex(test$Default_, pred1)

#Mis-classification Error
misClassError(test$Default_, pred)

#Life chart and Gain Chart
#--------------------------------------------------------------------------
newdata = data.frame(y_test,pred)
View(newdata)
newdata = newdata[order(-newdata$pred), ]

nrow(newdata)/10
?rep
groups = rep(1:10,each=floor(nrow(newdata)/10))

extra = rep(10, nrow(newdata)-length(groups))
length(groups)
nrow(newdata)

groups = c(groups,extra)

newdata$groups = groups
View(newdata)
newdata

library(sqldf)
gainTable = sqldf("select groups, count(pred) as N, sum(pred) as N1 from newdata group by groups ")
gainTable$cumN1 = cumsum(gainTable$N1)
gainTable$Gain = round(gainTable$cumN1/sum(gainTable$N1)*100,3)
gainTable$Lift = round(gainTable$Gain/((1:10)*10),3)

gainTable

plot(gainTable$groups, gainTable$Gain, type="b", 
     main = "Gain Plot",
     xlab = "Groups", ylab = "Gain")

plot(gainTable$groups, gainTable$Lift, type="b", 
     main = "Lift Plot",
     xlab = "Groups", ylab = "Lift")
