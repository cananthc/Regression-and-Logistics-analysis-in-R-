#########################################################################################################
# Course         : ITMD 527-08 Data Analytics                                                           #
# Final Project  : Credit Card Default Payee Analysis						        #	
# Name           : Anantharaman Chandar									#	
# CWID:          : A20403439										#	
# Description    : R Code for the Final Project [Refer Project Document]				#
#													#	
#													#
#													#			
#########################################################################################################


#########################################################################################################
# Model : Logistic Regression 										#
# Description: To find is there any dependency on Default Payee Status and other attributes   	        #
# Attributes taken: Default Payee Status, Payee due status, Bill Payments, Statment Balance             #
#													#
#########################################################################################################


## Form a Analysis Data and a Dataset for Testing

install.packages("dplyr", dependencies = TRUE)
library(dplyr)

install.packages("magrittr", dependencies = TRUE)
library(magrittr)

install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)

install.packages("reshape" , dependencies = TRUE)
library(reshape)

install.packages("plotly", dependencies = TRUE)
library(plotly)

install.packages("tidyselect", dependencies = TRUE)
library(tidyselect)

install.packages("caret", dependencies = TRUE)
library(caret)

install.packages("reshape2", dependencies = TRUE)
library(reshape2)

install.packages("knitr", dependencies = TRUE )
library(knitr)

library(mlbench)


##Backup of the data set
> CreditCardData_EDA_1 <- CreditCardData_EDA

## Get the dimensions i.e. Records and Columns
> print(dim(CreditCardData_EDA_1))
[1] 19991    25

##Check for duplicates 
> print(sum(duplicated(CreditCardData_EDA_1)))
[1] 0

## Check for NA
> NullData <- sum(is.na(CreditCardData_EDA_1))
> print(NullData)
[1] 0

## Datatype for each column
> DataSet_Datatype <- apply(X = CreditCardData_EDA_1, MARGIN = 2,FUN = class)
> print(DataSet_Datatype)
          ï..ID       LIMIT_BAL             SEX       EDUCATION        MARRIAGE 
      "numeric"       "numeric"       "numeric"       "numeric"       "numeric" 
            AGE      REPAY_SEPT       REPAY_AUG       REPAY_JUL       REPAY_JUN 
      "numeric"       "numeric"       "numeric"       "numeric"       "numeric" 
      REPAY_MAY       REPAY_APR    BILLAMT_SEPT     BILLAMT_AUG     BILLAMT_JUL 
      "numeric"       "numeric"       "numeric"       "numeric"       "numeric" 
    BILLAMT_JUN    BILL_AMT_MAY    BILL_AMT_APR        PAY_SEPT         PAY_AUG 
      "numeric"       "numeric"       "numeric"       "numeric"       "numeric" 
        PAY_JUL         PAY_JUN         PAY_MAY         PAY_APR Default.Payment 
      "numeric"       "numeric"       "numeric"       "numeric"       "numeric" 
	  

#########################################################################################################
# Model : Logistic Regression 										#
# Description: -2 is not a valid Payee Status, so changing it to 1 (Indicates Default payee)   	        #
# Attributes taken: REPAY_*										#
#													#
#########################################################################################################

# Iterate the columns and pass it to a for loop to change values 
> REPAY_MODIFIED <- grep(names(CreditCardData_EDA_1), pattern = "REPAY_")
> View(REPAY_MODIFIED)
> print(REPAY_MODIFIED)
[1]  7  8  9 10 11 12

 for(i in REPAY_MODIFIED)
{
  CreditCardData_EDA_1[,i] <-  ifelse(CreditCardData_EDA_1[,i]==-2,1,CreditCardData_EDA_1[,i])
}

#########################################################################################################
# Model : Logistic Regression 										#
# Description: Statment Bill cannot be negative, so changing from -ve to +ve i.e. -312 to +312          #
# Attributes taken: BILLPAY_*								                #
#													#
#########################################################################################################

##Iterate the columns and pass it to a for loop to change values 
> BILLPAY_MODIFIED <- grep(names(CreditCardData_EDA_1),pattern = "BILL_AMT_")
> print(BILLPAY_MODIFIED)
[1] 13 14 15 16

for(i in BILLPAY_MODIFIED)
{
  CreditCardData_EDA_1[,i] <-  ifelse(CreditCardData_EDA_1[,i]<0, -CreditCardData_EDA_1[,i]  ,CreditCardData_EDA_1[,i])
}


####################################################################################################################
# Model : Logistic Regression 											   #
# Description: Split the data set into two data set for Logistic Regression (Data_Filtered and TestData_Filetered) #
# Attributes taken: Gender, Education, Relationship Status, Defaily Payment, Age			           #
#														   #
####################################################################################################################

## magrittr
## dplyr
##Data_Filtered First Data set
Data_Filtered <- CreditCardData_EDA_1 %>% mutate(SEX_Filtered = factor(ifelse(SEX ==1,"Male","Female"))) 
			                          %>% filter(EDUCATION !=0 & EDUCATION != 4 & EDUCATION != 5 & EDUCATION != 6)
									  %>% mutate(EDUCATION_Filtered = factor( ifelse(EDUCATION==1,"GS",ifelse(EDUCATION==2,"Uni","HS"))))
									  %>% filter(MARRIAGE == 1 | MARRIAGE == 2 ) 
									  %>% mutate(MARRIAGE_Filtered = factor( ifelse(MARRIAGE == 1,"Married","Single")))
									  %>% mutate( Default = factor(ifelse(Default.Payment==1,"Default","Reg"))) 
									  %>% mutate_at(.vars = grep(names(CreditCardData_EDA_1),pattern = "PAY_"), .funs =  factor)
									  
Data_Filtered <- Data_Filtered        %>% mutate( AGE_Range = factor(ifelse(  AGE >= 20 & AGE <= 30 , "20-30", 
                                                                     ifelse( AGE >= 30 & AGE <= 40 , "30-40",
                                                                     ifelse( AGE >= 40 & AGE <= 50,"40-50",
                                                                     ifelse( AGE >= 50 & AGE <= 60, "50-60", "70-80"))))))
									  
									  
									  
##TestData_Filtered First Data set 	
TestData_Filetered <- CreditCardData_EDA_1  %>% mutate(EDUCATION_FILTERED = ifelse(EDUCATION == 0 , 4,ifelse(EDUCATION >=4 , 4,EDUCATION)))
											%>% filter(MARRIAGE == 1 | MARRIAGE == 2 ) 
											%>% mutate(MARRIAGE_FILTERED=as.factor(MARRIAGE)) 
											%>% mutate(SEX_FILTERED = as.factor(SEX)) 
											%>% mutate(EDUCATION_FILTERED = as.factor(EDUCATION)) 
											%>% select(-ï..ID) 
											%>% mutate(Default_FILTERED = as.factor(Default.Payment)) 
											%>% select(-Default.Payment)


#####################################################################################################################
# Model : Logistic Regression 									          	    #
# Description:ggplot for Bill Statment based on Age, Education, Gender, Relationship Status                         #
# Attributes taken: Gender, Education, Relationship Status, Defauly Payment, Age			            #
#														    #
#####################################################################################################################
##ggplot
##library reshape
##Total Bill Based on Gender

> Bill_Statement <- Data_Filtered %>% select(SEX_Filtered,Default , contains( "BILL"))
> Bill_Statement_EDU <- Data_Filtered %>% select(EDUCATION_Filtered, Default, contains("BILL"))
> Bill_Statement_MAR <- Data_Filtered %>% select(MARRIAGE_Filtered, Default, contains("BILL"))
> Bill_Statement_Age <- Data_Filtered %>% select(AGE_Range, Default, contains("BILL"))

> ggplot2::ggplot(melt(Bill_Statement,id.vars = c("SEX_Filtered","Default"))) + geom_boxplot(aes(x=variable, y= value),fill="#F0E442", colour="blue")+xlab("Gender")+ylab("Amount")+ facet_grid(SEX_Filtered ~ Default)
##Check image

##Education
> ggplot2::ggplot(melt(Bill_Statement_EDU,id.vars = c("EDUCATION_Filtered","Default"))) + geom_boxplot(aes(x=variable, y= value),fill="#F0E442", colour="blue")+xlab("Education")+ylab("Amount")+ facet_grid(EDUCATION_Filtered ~ Default)

##Marriage
> ggplot(melt(Bill_Statement_MAR,id.vars = c("MARRIAGE_Filtered","Default"))) + geom_boxplot(aes(x=variable, y= value),fill="#F0E442", colour="blue")+xlab("Marriage")+ylab("Amount")+ facet_grid(MARRIAGE_Filtered ~ Default)

##Age
> ggplot(melt(Bill_Statement_Age,id.vars = c("AGE_Range","Default"))) + geom_boxplot(aes(x=variable, y= value),fill="#F0E442", colour="blue")+xlab("Age")+ylab("Amount")+ facet_grid(AGE_Range ~ Default)


#####################################################################################################################
# Model : Logistic Regression 											    #
# Description:ggplot for Payment Amount based on Age, Education, Gender, Relationship Status                        #
# Attributes taken: Gender, Education, Relationship Status, Defauly Payment, Age			            #
#														    #
#####################################################################################################################


> Payment_Amount <- Data_Filtered %>% select(SEX_Filtered,Default, PAY_APR, PAY_MAY, PAY_JUN, PAY_JUL, PAY_AUG, PAY_SEPT)
> Payment_Amount_EDU <- Data_Filtered %>% select(EDUCATION_Filtered, Default, PAY_SEPT, PAY_AUG,PAY_JUL, PAY_JUN,PAY_MAY, PAY_APR)
> Payment_Amount_MAR <- Data_Filtered %>% select(MARRIAGE_Filtered, Default, PAY_SEPT, PAY_AUG,PAY_JUL, PAY_JUN,PAY_MAY, PAY_APR) 
> Payment_Amount_AGE <- Data_Filtered %>% select(AGE_Range, Default, PAY_SEPT, PAY_AUG,PAY_JUL, PAY_JUN,PAY_MAY, PAY_APR)



##Total Payments Based on Gender
ggplot(melt(Payment_Amount,id.vars = c("SEX_Filtered","Default"))) + geom_boxplot(aes(x=variable, y= value))+xlab("Gender")+ylab("Amount")+ facet_grid(SEX_Filtered ~ Default)

##Education
ggplot(melt(Payment_Amount_EDU,id.vars = c("EDUCATION_Filtered","Default"))) + geom_boxplot(aes(x=variable, y= value))+xlab("Education")+ylab("Amount")+ facet_grid(EDUCATION_Filtered ~ Default)


##MARRIAGE
ggplot(melt(Payment_Amount_MAR,id.vars = c("Marriage_Filtered","Default"))) + geom_boxplot(aes(x=variable, y= value))+xlab("Marriage")+ylab("Amount")+ facet_grid(MARRIAGE_Filtered ~ Default)

##Age
ggplot(melt(Payment_Amount_AGE,id.vars = c("AGE_Range","Default"))) + geom_boxplot(aes(x=variable, y= value))+xlab("Age")+ylab("Amount")+ facet_grid(AGE_Range ~ Default)



#####################################################################################################################
# Model : Logistic Regression 											    #
# Description:Calculate Deafult Payee Percentage                                                                    #
# Attributes taken: Gender, Education, Relationship Status, Defauly Payment, Age			            #
#														    #
#####################################################################################################################

##Calculate Defualt Payyee Percentage
> Default_Payee_Percentage <- (sum(Data_Filtered$Default.Payment==1)/nrow(Data_Filtered))*100
> print(Default_Payee_Percentage)
[1] 21.0862



## Default Payee by Gender
#dplyr
#magrittr

#####################################################################################################################
# Model : Logistic Regression 											    #
# Description:Calculate Deafult Payee Percentage  based on Gender, Education                                        #
# Attributes taken: Gender, Education 			                                                            #
#												                    #
#####################################################################################################################

Data_Filtered %>% group_by(SEX_Filtered,Default)  %>%      
+     summarise("Total"=n()) %>% 
+     mutate( `Percentage` = (`Total`/sum(`Total`))*100) %>% filter(Default=="Default")
# A tibble: 2 x 4
# Groups:   SEX_Filtered [2]
  SEX_Filtered Default Total Percentage
        <fctr>  <fctr> <int>      <dbl>
		
		
1       Female Default  2268   19.51136
2         Male Default  1832   23.42711


## Default Payee by Education
> Data_Filtered %>% group_by(EDUCATION_Filtered,Default)  %>%      
+     summarise("Default Breakdown"=n()) %>% 
+     mutate(`P(Default|EDUCATION_Filtered)` = (`Default Breakdown`/sum(`Default Breakdown`))*100) %>% filter(Default=="Default")
# A tibble: 3 x 4
# Groups:   EDUCATION_Filtered [3]
  EDUCATION_Filtered Default `Default Breakdown` `P(Default|EDUCATION_Filtered)`
              <fctr>  <fctr>               <int>                           <dbl>
1                 GS Default                1383                        18.57623
2                 HS Default                 719                        24.48910
3                Uni Default                1998                        22.04568

#####################################################################################################################
# Model : Logistic Regression 											    #
# Description:Calculate Deafult Payee Count based on Gender, Education, Relationship Status                         #
# Attributes taken: Gender, Education, Relationship Status 							    #
#														    #
#####################################################################################################################

##Count of Default payee
ggplot2::ggplot(Data_Filtered)+geom_bar(aes(Default), fill="#E69F00", colour="black")


## Observations Count
ggplot(Data_Filtered)+geom_bar(aes(Default),fill="#E69F00", colour="black")+ facet_grid( SEX_Filtered ~. ) + ylab("Observations") + xlab("Status")


##Education based
ggplot(Data_Filtered)+geom_bar(aes(Default),fill="#E69F00", colour="black")+ facet_grid( EDUCATION_Filtered ~. ) + ylab("Observations") + xlab("Status")

##Marriage 
ggplot(Data_Filtered)+geom_bar(aes(Default),fill="#E69F00", colour="black")+ facet_grid(MARRIAGE_Filtered ~. ) + ylab("Observations") + xlab("Status")


#####################################################################################################################
# Model : Logistic Regression 											    #
# Description:Group Regular Payee and Default Payee for each month for Payment Due Status  (Apr - Sep)              #
# Attributes taken: Payment Due Status, Default Payee Status    						    #
#													            #
#####################################################################################################################


## Which month Default/Regular - SEP
ggplot(Data_Filtered)+geom_bar(aes(Default),fill="#56B4E9", colour="black")+ coord_flip()+facet_grid( REPAY_SEPT ~. ) + ylab("Observations") + xlab("Status")

## Which month Default/Regular - AUG
ggplot(Data_Filtered)+geom_bar(aes(Default),fill="#56B4E9", colour="black")+ coord_flip()+facet_grid( REPAY_AUG ~. ) + ylab("Observations") + xlab("Status")

## Which month Default/Regular - JUL
ggplot(Data_Filtered)+geom_bar(aes(Default),fill="#56B4E9", colour="black")+ coord_flip()+facet_grid( REPAY_JUL ~. ) + ylab("Observations") + xlab("Status")

## Which month Default/Regular - JUN
ggplot(Data_Filtered)+geom_bar(aes(Default),fill="#56B4E9", colour="black")+ coord_flip()+facet_grid( REPAY_JUN ~. ) + ylab("Observations") + xlab("Status")

## Which month Default/Regular - MAY
ggplot(Data_Filtered)+geom_bar(aes(Default),fill="#56B4E9", colour="black")+ coord_flip()+facet_grid( REPAY_MAY ~. ) + ylab("Observations") + xlab("Status")

## Which month Default/Regular - APR
ggplot(Data_Filtered)+geom_bar(aes(Default),fill="#56B4E9", colour="black")+ coord_flip()+facet_grid( REPAY_APR ~. ) + ylab("Observations") + xlab("Status")


#####################################################################################################################
# Model : Logistic Regression 											    #
# Description:Final Prediction Results for determining  default Payee                                               #
# Attributes taken: Default Payee Status    									    #																					    #
#####################################################################################################################
##caret


#Percent of Data Goes to Training and Testing
PercentOfDataGoesToTraining = .8

## Split the data in to Testing and Training 
TrainData_Index <- createDataPartition(TestData_Filetered$Default_FILTERED,p = PercentOfDataGoesToTraining, list = FALSE,times = 1)
Training_data <- 	[TrainData_Index,]
Testing_data <- TestData_Filetered[-TrainData_Index,]

FitControl <- trainControl(method = "repeatedcv",number = 10,repeats = 10)

## Attributes assigned to particular column
Training_data$AGE <- as.numeric( Training_data$AGE )
def <- Training_data$Default

## Log Model to calculate Default Payee Status based on other attributes
Log_Model <- train(Default_FILTERED ~. , data = Training_data, method="glm", family = binomial(link = "logit"),trControl = FitControl)



##Predict the output 
Training_Prediction <- predict(Log_Model,newdata = Training_data)
Testing_Prediction <- predict(Log_Model,newdata = Testing_data)

##Create Confusion Matrix for Training Data
confusionMatrix(Training_Prediction,Training_data$Default)

Confusion Matrix and Statistics
###O/p
          Reference
Prediction     0     1
         0 12233  2601
         1   266   696
                                          
               Accuracy : 0.8185          
                 95% CI : (0.8124, 0.8245)
    No Information Rate : 0.7913          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.2568          
 Mcnemar''s Test P-Value : < 2.2e-16       
                                          
            Sensitivity : 0.9787          
            Specificity : 0.2111          
         Pos Pred Value : 0.8247          
         Neg Pred Value : 0.7235          
             Prevalence : 0.7913          
         Detection Rate : 0.7744          
   Detection Prevalence : 0.9391          
      Balanced Accuracy : 0.5949          
                                          
       'Positive Class : 0'
	   
##Create Confusion Matrix for Testing Data	   
confusionMatrix(Testing_Prediction,Testing_data$Default)

Confusion Matrix and Statistics

          Reference
Prediction    0    1
         0 3045  636
         1   79  188
                                          
               Accuracy : 0.8189          
                 95% CI : (0.8065, 0.8308)
    No Information Rate : 0.7913          
    P-Value [Acc > NIR] : 7.853e-06       
                                          
                  Kappa : 0.2701          
 Mcnemar''	s Test P-Value : < 2.2e-16       
                                          
            Sensitivity : 0.9747          
            Specificity : 0.2282          
         Pos Pred Value : 0.8272          
         Neg Pred Value : 0.7041          
             Prevalence : 0.7913          
         Detection Rate : 0.7713          
   Detection Prevalence : 0.9324          
      Balanced Accuracy : 0.6014          
                                          
       Positive Class : 0
	   
	   