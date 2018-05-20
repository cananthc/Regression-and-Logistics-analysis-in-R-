##########################################################################################################################################
# Course         : ITMD 527-08 Data Analytics                                                                                            #
# Final Project  : Credit Card Default Payee Analysis						                        		 #	
# Name           : Anantharaman Chandar												  	 #	
# CWID:          : A20403439														 #				#																         #	
# Description    : R Code for the Final Project [Refer Project Document]								 #
#																	 #				#																         #	
#								 								         #				#			                                                                                                                 #
#																	 #				#					  												 #			
#																	 #				#				      													 #
##########################################################################################################################################


##Import the CreditCard Dataset which is in Quartile Ranges

> CreditCardData_EDA <- read.csv("E://IIT-C//Sem-I//Data Analytics//Fina_Proj//CC_DataSet_R_Quartiles.csv")

##Check Whether Data is Loaded or Not
View(CreditCardData)

## Check NA values
> sum(is.na(CreditCardData_EDA))
[1] 2405

## Tally no of rows
> nrow(CreditCardData_EDA)
[1] 20179


## Count of NA values
colSums(is.na(CreditCardData_EDA))
          ï..ID       LIMIT_BAL             SEX       EDUCATION        MARRIAGE             AGE 
              0              28              22               0               2               0 
     REPAY_SEPT       REPAY_AUG       REPAY_JUL       REPAY_JUN       REPAY_MAY       REPAY_APR 
            102             107             107             107             118             131 
   BILLAMT_SEPT     BILLAMT_AUG     BILLAMT_JUL     BILLAMT_JUN    BILL_AMT_MAY    BILL_AMT_APR 
            137             137             152             152             139             146 
       PAY_SEPT         PAY_AUG         PAY_JUL         PAY_JUN         PAY_MAY         PAY_APR 
            140             140             127             116             116              88 
Default.Payment 
             91 
			 
			 
## Remove all the NA values before EDA
> CreditCardData_EDA <- na.omit(CreditCardData_EDA)
> colSums(is.na(CreditCardData_EDA))
          ï..ID       LIMIT_BAL             SEX       EDUCATION        MARRIAGE             AGE 
              0               0               0               0               0               0 
     REPAY_SEPT       REPAY_AUG       REPAY_JUL       REPAY_JUN       REPAY_MAY       REPAY_APR 
              0               0               0               0               0               0 
   BILLAMT_SEPT     BILLAMT_AUG     BILLAMT_JUL     BILLAMT_JUN    BILL_AMT_MAY    BILL_AMT_APR 
              0               0               0               0               0               0 
       PAY_SEPT         PAY_AUG         PAY_JUL         PAY_JUN         PAY_MAY         PAY_APR 
              0               0               0               0               0               0 
Default.Payment 
              0 
			  
## After eliminating NA values
 > nrow(CreditCardData_EDA)
[1] 19991

## Verify if there is any NA values
> sum(is.na(CreditCardData_EDA))
[1] 0


## Assign Variables for the columns from the .csv fil

## Rename the Column Names as per R
RID <- CreditCardData_EDA$ï..ID
RLB <- CreditCardData_EDA$LIMIT_BAL
RSEX <- CreditCardData_EDA$SEX
REDU <- CreditCardData_EDA$EDUCATION
RMAR <- CreditCardData_EDA$MARRIAGE
RAGE <- CreditCardData_EDA$AGE
RDUESEP <- CreditCardData_EDA$REPAY_SEPT
RDUEAUG <- CreditCardData_EDA$REPAY_AUG
RDUEJUL <- CreditCardData_EDA$REPAY_JUL
RDUEJUN <- CreditCardData_EDA$REPAY_JUN
RDUEMAY <- CreditCardData_EDA$REPAY_MAY
RDUEAPR <- CreditCardData_EDA$REPAY_APR
RSTATSEP <- CreditCardData_EDA$BILLAMT_SEPT
RSTATAUG <- CreditCardData_EDA$BILLAMT_AUG
RSTATJUL <- CreditCardData_EDA$BILLAMT_JUL
RSTATJUN <- CreditCardData_EDA$BILLAMT_JUN
RSTATMAY <- CreditCardData_EDA$BILL_AMT_MAY
RSTATAPR <- CreditCardData_EDA$BILL_AMT_APR
RBLNCESEP <- CreditCardData_EDA$PAY_SEPT
RBLNCEAUG <- CreditCardData_EDA$PAY_AUG
RBLNCEJUL<- CreditCardData_EDA$PAY_JUL
RBLNCEJUN<- CreditCardData_EDA$PAY_JUN
RBLNCEMAY<- CreditCardData_EDA$PAY_MAY
RBLNCEAPR <- CreditCardData_EDA$PAY_APR
RDEFAULT <- CreditCardData_EDA$Default.Payment


#########################################################################################################
# Model : Linear Regression 										#								#													#
# Description: To find is there any dependency between Limit Balance with other attributes   	        #
# Attributes taken: Limit Balance vs (Age, Sex, Education, Relationship Status, Default Payee Status)   #
#													#								#													#
#########################################################################################################

## Linear Regression for Limit Balance ~ (RSEX, RAGE, REDU, RMAR, RDEFAULT)

#Age

> LRLBRAGE <- lm(RLB~RAGE)
> summary(LRLBRAGE)

Call:
lm(formula = RLB ~ RAGE)

Residuals:
    Min      1Q  Median      3Q     Max 
-181775 -110006  -18236   73533  813828 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 200032.2     5893.9   33.94   <2e-16 ***
RAGE          -294.9      161.2   -1.83   0.0673 .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 131300 on 19989 degrees of freedom
Multiple R-squared:  0.0001674,	Adjusted R-squared:  0.0001174 
F-statistic: 3.347 on 1 and 19989 DF,  p-value: 0.06733


#SEX
> LRLBRSEX <- lm(RLB~RSEX)
> summary(LRLBRSEX)

Call:
lm(formula = RLB ~ RSEX)

Residuals:
    Min      1Q  Median      3Q     Max 
-183759 -113759  -13759   77153  806241 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)   171935       3168  54.279  < 2e-16 ***
RSEX           10912       1894   5.761 8.47e-09 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 131200 on 19989 degrees of freedom
Multiple R-squared:  0.001658,	Adjusted R-squared:  0.001608 
F-statistic: 33.19 on 1 and 19989 DF,  p-value: 8.472e-09

# EDUCATION
> LRLBLEDU <- lm(RLB~REDU)
> summary(LRLBLEDU)

Call:
lm(formula = RLB ~ REDU)

Residuals:
    Min      1Q  Median      3Q     Max 
-214321 -101846  -23084   76916  776916 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)   264321       2271  116.41   <2e-16 ***
REDU          -41238       1147  -35.95   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 127300 on 19989 degrees of freedom
Multiple R-squared:  0.06073,	Adjusted R-squared:  0.06068 
F-statistic:  1292 on 1 and 19989 DF,  p-value: < 2.2e-16

#Marraige
> LRRLBRMAR <- lm(RLB~RMAR)
> summary(LRRLBRMAR)

Call:
lm(formula = RLB ~ RMAR)

Residuals:
    Min      1Q  Median      3Q     Max 
-183462 -110725  -17988   72012  809275 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)   193462       2800  69.092   <2e-16 ***
RMAR           -2737       1773  -1.544    0.123    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 131300 on 19989 degrees of freedom
Multiple R-squared:  0.0001192,	Adjusted R-squared:  6.92e-05 
F-statistic: 2.383 on 1 and 19989 DF,  p-value: 0.1226


# Default Payment
> LRRLBRDEF <- lm(RLB~RDEFAULT)
> summary(LRRLBRDEF)

Call:
lm(formula = RLB ~ RDEFAULT)

Residuals:
    Min      1Q  Median      3Q     Max 
-190021 -100021  -20021   79979  799979 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)   200021       1031  193.95   <2e-16 ***
RDEFAULT      -50971       2258  -22.58   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 129700 on 19989 degrees of freedom
Multiple R-squared:  0.02487,	Adjusted R-squared:  0.02482 
F-statistic: 509.8 on 1 and 19989 DF,  p-value: < 2.2e-16

#########################################################################################################
# Model : Linear Regression                                                                             #							
#    													#
# Description: To find is there any dependency between Limit Balance with other attributes   	        #
# Attributes taken: Default Payee Status vs (Age, Sex, Education, Relationship Status, Limit Balance)   #
#													#								#													#
#########################################################################################################


#SEX
> LRRDRSEX <- lm(RDEFAULT~RSEX)
> summary(LRRDRSEX)

Call:
lm(formula = RDEFAULT ~ RSEX)

Residuals:
    Min      1Q  Median      3Q     Max 
-0.2325 -0.2325 -0.1927 -0.1927  0.8073 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.272335   0.009797  27.797  < 2e-16 ***
RSEX        -0.039800   0.005858  -6.794 1.12e-11 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.4059 on 19989 degrees of freedom
Multiple R-squared:  0.002304,	Adjusted R-squared:  0.002254 
F-statistic: 46.16 on 1 and 19989 DF,  p-value: 1.12e-11

#Age
> LRRDRAGE <- lm(RDEFAULT~RAGE)
> summary(LRRDRAGE)

Call:
lm(formula = RDEFAULT ~ RAGE)

Residuals:
    Min      1Q  Median      3Q     Max 
-0.2352 -0.2151 -0.2018 -0.1906  0.8094 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 0.1281897  0.0182275   7.033 2.09e-12 ***
RAGE        0.0022294  0.0004985   4.473 7.77e-06 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.4062 on 19989 degrees of freedom
Multiple R-squared:  0.0009997,	Adjusted R-squared:  0.0009497 
F-statistic:    20 on 1 and 19989 DF,  p-value: 7.772e-06

#Education
> LRRDREDU <- lm(RDEFAULT~REDU)
> summary(LRRDREDU)

Call:
lm(formula = RDEFAULT ~ REDU)

Residuals:
    Min      1Q  Median      3Q     Max 
-0.2656 -0.2112 -0.1976 -0.1976  0.8024 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 0.183970   0.007246  25.388  < 2e-16 ***
REDU        0.013606   0.003661   3.717 0.000202 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.4063 on 19989 degrees of freedom
Multiple R-squared:  0.0006906,	Adjusted R-squared:  0.0006406 
F-statistic: 13.81 on 1 and 19989 DF,  p-value: 0.0002024

#MARRIAGE
> LRRDRMAR <- lm(RDEFAULT~RMAR)
> summary(LRRDRMAR)

Call:
lm(formula = RDEFAULT ~ RMAR)

Residuals:
    Min      1Q  Median      3Q     Max 
-0.2436 -0.2202 -0.1968 -0.1968  0.8267 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.243592   0.008660  28.129  < 2e-16 ***
RMAR        -0.023421   0.005482  -4.272 1.95e-05 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.4062 on 19989 degrees of freedom
Multiple R-squared:  0.0009121,	Adjusted R-squared:  0.0008622 
F-statistic: 18.25 on 1 and 19989 DF,  p-value: 1.947e-05

##Limit Balance
> LRRDRLB <- lm(RDEFAULT~RLB)
> summary(LRRDRLB)

Call:
lm(formula = RDEFAULT ~ RLB)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.29622 -0.24255 -0.19376 -0.07666  1.05995 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  3.011e-01  4.980e-03   60.46   <2e-16 ***
RLB         -4.879e-07  2.161e-08  -22.58   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.4013 on 19989 degrees of freedom
Multiple R-squared:  0.02487,	Adjusted R-squared:  0.02482 
F-statistic: 509.8 on 1 and 19989 DF,  p-value: < 2.2e-16

#########################################################################################################
# Model : Multilinear Regression 									#								#													#
# Description: To find is there any dependency for Limit Balance based on one or more attributes        #
# Attributes taken: Limit Balance vs All, Eliminate Statement Bill Amount, Eliminate expenses,          #
#			             *Expenses based on (Education, Gender), 				#
#				     (Age, Gender, Education, Relationship Status, Default Payee)       #
#  													#								#													#
#   													#								#													#
#########################################################################################################



#All Attributes

> MLRRLBRALL <- lm(RLB ~ RSEX + REDU + RMAR + RAGE + RDUESEP + RDUEAUG + RDUEJUL + RDUEJUN + RDUEMAY + RDUEAPR + RSTATSEP + RSTATAUG + RSTATJUL + RSTATJUN + RSTATMAY + RSTATAPR + RBLNCESEP +RBLNCEAUG + RBLNCEJUL + RBLNCEJUN+ RBLNCEMAY + RBLNCEAPR + RDEFAULT)
> summary(MLRRLBRALL)

Call:
lm(formula = RLB ~ RSEX + REDU + RMAR + RAGE + RDUESEP + RDUEAUG + 
    RDUEJUL + RDUEJUN + RDUEMAY + RDUEAPR + RSTATSEP + RSTATAUG + 
    RSTATJUL + RSTATJUN + RSTATMAY + RSTATAPR + RBLNCESEP + RBLNCEAUG + 
    RBLNCEJUL + RBLNCEJUN + RBLNCEMAY + RBLNCEAPR + RDEFAULT)

Residuals:
     Min       1Q   Median       3Q      Max 
-1078018   -78618   -21433    56978   566637 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  1.942e+05  7.110e+03  27.311  < 2e-16 ***
RSEX         5.088e+03  1.575e+03   3.231 0.001236 ** 
REDU        -3.240e+04  1.009e+03 -32.097  < 2e-16 ***
RMAR        -1.092e+04  1.557e+03  -7.016 2.36e-12 ***
RAGE         2.141e+02  1.423e+02   1.504 0.132586    
RDUESEP     -9.445e+03  9.534e+02  -9.907  < 2e-16 ***
RDUEAUG     -1.380e+04  1.153e+03 -11.968  < 2e-16 ***
RDUEJUL     -7.664e+03  1.250e+03  -6.132 8.83e-10 ***
RDUEJUN     -5.094e+03  1.368e+03  -3.725 0.000196 ***
RDUEMAY     -1.056e+03  1.494e+03  -0.707 0.479577    
RDUEAPR     -1.113e+04  1.234e+03  -9.019  < 2e-16 ***
RSTATSEP     3.813e-01  3.632e-02  10.500  < 2e-16 ***
RSTATAUG    -1.274e-01  5.069e-02  -2.512 0.011996 *  
RSTATJUL    -6.594e-02  4.827e-02  -1.366 0.171997    
RSTATJUN     1.559e-01  5.176e-02   3.013 0.002591 ** 
RSTATMAY     2.864e-01  5.905e-02   4.849 1.25e-06 ***
RSTATAPR     2.217e-03  4.653e-02   0.048 0.961996    
RBLNCESEP    3.807e-01  5.762e-02   6.607 4.03e-11 ***
RBLNCEAUG    1.688e-01  4.561e-02   3.701 0.000216 ***
RBLNCEJUL    2.497e-01  5.400e-02   4.623 3.80e-06 ***
RBLNCEJUN    3.660e-01  5.697e-02   6.424 1.36e-10 ***
RBLNCEMAY    7.576e-01  5.996e-02  12.635  < 2e-16 ***
RBLNCEAPR    6.443e-01  4.181e-02  15.412  < 2e-16 ***
RDEFAULT    -9.218e+03  2.001e+03  -4.607 4.11e-06 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 108000 on 19967 degrees of freedom
Multiple R-squared:  0.3248,	Adjusted R-squared:  	
F-statistic: 417.6 on 23 and 19967 DF,  p-value: < 2.2e-16


##Removed Last Month Payment Due Status(SEP to APR)
> MLRRLBRALL_1 <- lm(RLB ~ RSEX + REDU + RMAR + RAGE +  RSTATSEP + RSTATAUG + RSTATJUL + RSTATJUN + RSTATMAY + RSTATAPR + RBLNCESEP +RBLNCEAUG + RBLNCEJUL + RBLNCEJUN+ RBLNCEMAY + RBLNCEAPR + RDEFAULT)
> summary(MLRRLBRALL_1)

Call:
lm(formula = RLB ~ RSEX + REDU + RMAR + RAGE + RSTATSEP + RSTATAUG + 
    RSTATJUL + RSTATJUN + RSTATMAY + RSTATAPR + RBLNCESEP + RBLNCEAUG + 
    RBLNCEJUL + RBLNCEJUN + RBLNCEMAY + RBLNCEAPR + RDEFAULT)

Residuals:
     Min       1Q   Median       3Q      Max 
-1435950   -87617   -23743    65330   583423 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  2.141e+05  7.603e+03  28.160  < 2e-16 ***
RSEX         1.205e+04  1.681e+03   7.164 8.10e-13 ***
REDU        -3.998e+04  1.071e+03 -37.346  < 2e-16 ***
RMAR        -1.184e+04  1.668e+03  -7.099 1.30e-12 ***
RAGE         4.939e+02  1.524e+02   3.241 0.001193 ** 
RSTATSEP     4.185e-01  3.875e-02  10.801  < 2e-16 ***
RSTATAUG    -2.049e-01  5.418e-02  -3.781 0.000157 ***
RSTATJUL    -9.033e-02  5.166e-02  -1.748 0.080409 .  
RSTATJUN     1.315e-01  5.529e-02   2.378 0.017415 *  
RSTATMAY     2.299e-01  6.317e-02   3.640 0.000273 ***
RSTATAPR    -1.402e-01  4.967e-02  -2.823 0.004769 ** 
RBLNCESEP    5.266e-01  6.144e-02   8.571  < 2e-16 ***
RBLNCEAUG    2.251e-01  4.873e-02   4.619 3.88e-06 ***
RBLNCEJUL    3.961e-01  5.749e-02   6.891 5.71e-12 ***
RBLNCEJUN    5.065e-01  6.067e-02   8.347  < 2e-16 ***
RBLNCEMAY    9.757e-01  6.395e-02  15.258  < 2e-16 ***
RBLNCEAPR    7.124e-01  4.475e-02  15.920  < 2e-16 ***
RDEFAULT    -3.996e+04  2.032e+03 -19.667  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 115700 on 19973 degrees of freedom
Multiple R-squared:  0.2248,	Adjusted R-squared:  0.2241 
F-statistic: 340.6 on 17 and 19973 DF,  p-value: < 2.2e-16

## Remove Default Payee
> MLRRLBRALL_2 <- lm(RLB ~ RSEX + REDU + RMAR + RAGE +  RSTATSEP + RSTATAUG + RSTATJUL + RSTATJUN + RSTATMAY + RSTATAPR + RBLNCESEP +RBLNCEAUG + RBLNCEJUL + RBLNCEJUN+ RBLNCEMAY + RBLNCEAPR)
> summary(MLRRLBRALL_2)

Call:
lm(formula = RLB ~ RSEX + REDU + RMAR + RAGE + RSTATSEP + RSTATAUG + 
    RSTATJUL + RSTATJUN + RSTATMAY + RSTATAPR + RBLNCESEP + RBLNCEAUG + 
    RBLNCEJUL + RBLNCEJUN + RBLNCEMAY + RBLNCEAPR)

Residuals:
     Min       1Q   Median       3Q      Max 
-1534131   -90653   -23654    65610   592293 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  2.041e+05  7.658e+03  26.650  < 2e-16 ***
RSEX         1.368e+04  1.695e+03   8.071 7.38e-16 ***
REDU        -4.028e+04  1.081e+03 -37.268  < 2e-16 ***
RMAR        -1.114e+04  1.683e+03  -6.619 3.70e-11 ***
RAGE         4.339e+02  1.538e+02   2.821 0.004797 ** 
RSTATSEP     4.484e-01  3.909e-02  11.472  < 2e-16 ***
RSTATAUG    -2.292e-01  5.469e-02  -4.191 2.79e-05 ***
RSTATJUL    -8.644e-02  5.216e-02  -1.657 0.097483 .  
RSTATJUN     1.337e-01  5.582e-02   2.395 0.016608 *  
RSTATMAY     2.187e-01  6.377e-02   3.429 0.000606 ***
RSTATAPR    -1.532e-01  5.014e-02  -3.055 0.002251 ** 
RBLNCESEP    5.861e-01  6.195e-02   9.461  < 2e-16 ***
RBLNCEAUG    2.352e-01  4.920e-02   4.781 1.76e-06 ***
RBLNCEJUL    4.178e-01  5.803e-02   7.199 6.26e-13 ***
RBLNCEJUN    5.466e-01  6.122e-02   8.928  < 2e-16 ***
RBLNCEMAY    1.013e+00  6.453e-02  15.690  < 2e-16 ***
RBLNCEAPR    7.292e-01  4.517e-02  16.143  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 116800 on 19974 degrees of freedom
Multiple R-squared:  0.2098,	Adjusted R-squared:  0.2091 
F-statistic: 331.4 on 16 and 19974 DF,  p-value: < 2.2e-16

## Remove Statement Bill (SEP to APR)
> MLRRLBRALL_3 <- lm(RLB ~ RSEX + REDU + RMAR + RAGE + RBLNCESEP +RBLNCEAUG + RBLNCEJUL + RBLNCEJUN+ RBLNCEMAY + RBLNCEAPR)
> summary(MLRRLBRALL_3)

Call:
lm(formula = RLB ~ RSEX + REDU + RMAR + RAGE + RBLNCESEP + RBLNCEAUG + 
    RBLNCEJUL + RBLNCEJUN + RBLNCEMAY + RBLNCEAPR)

Residuals:
     Min       1Q   Median       3Q      Max 
-1789956   -93380   -20179    68349   599157 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  2.199e+05  7.802e+03  28.190  < 2e-16 ***
RSEX         1.203e+04  1.730e+03   6.955 3.63e-12 ***
REDU        -3.898e+04  1.102e+03 -35.389  < 2e-16 ***
RMAR        -1.094e+04  1.719e+03  -6.366 1.99e-10 ***
RAGE         3.817e+02  1.571e+02   2.430   0.0151 *  
RBLNCESEP    5.202e-01  5.257e-02   9.896  < 2e-16 ***
RBLNCEAUG    2.305e-01  3.517e-02   6.554 5.75e-11 ***
RBLNCEJUL    6.717e-01  4.703e-02  14.283  < 2e-16 ***
RBLNCEJUN    7.345e-01  5.096e-02  14.413  < 2e-16 ***
RBLNCEMAY    1.072e+00  5.359e-02  19.999  < 2e-16 ***
RBLNCEAPR    9.133e-01  4.449e-02  20.530  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 119300 on 19980 degrees of freedom
Multiple R-squared:  0.1752,	Adjusted R-squared:  0.1748 
F-statistic: 424.4 on 10 and 19980 DF,  p-value: < 2.2e-16


## Expenditure based on Eduation and Gender

> MLRRLBRALL_4 <- lm(RLB ~ RSEX + REDU + RBLNCESEP +RBLNCEAUG + RBLNCEJUL + RBLNCEJUN+ RBLNCEMAY + RBLNCEAPR)
> summary(MLRRLBRALL_4)

Call:
lm(formula = RLB ~ RSEX + REDU + RBLNCESEP + RBLNCEAUG + RBLNCEJUL + 
    RBLNCEJUN + RBLNCEMAY + RBLNCEAPR)

Residuals:
     Min       1Q   Median       3Q      Max 
-1804685   -93800   -21441    68593   588392 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  2.137e+05  3.492e+03  61.188  < 2e-16 ***
RSEX         1.260e+04  1.725e+03   7.304 2.89e-13 ***
REDU        -3.743e+04  1.080e+03 -34.649  < 2e-16 ***
RBLNCESEP    5.204e-01  5.264e-02   9.886  < 2e-16 ***
RBLNCEAUG    2.323e-01  3.522e-02   6.594 4.37e-11 ***
RBLNCEJUL    6.696e-01  4.709e-02  14.218  < 2e-16 ***
RBLNCEJUN    7.364e-01  5.104e-02  14.429  < 2e-16 ***
RBLNCEMAY    1.066e+00  5.366e-02  19.869  < 2e-16 ***
RBLNCEAPR    9.126e-01  4.455e-02  20.483  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 119500 on 19982 degrees of freedom
Multiple R-squared:  0.1726,	Adjusted R-squared:  0.1723 
F-statistic: 521.1 on 8 and 19982 DF,  p-value: < 2.2e-16

 
# Baseed on Education alone
> MLRRLBRALL_5 <- lm(RLB ~ REDU + RBLNCESEP +RBLNCEAUG + RBLNCEJUL + RBLNCEJUN+ RBLNCEMAY + RBLNCEAPR)
> summary(MLRRLBRALL_5)

Call:
lm(formula = RLB ~ REDU + RBLNCESEP + RBLNCEAUG + RBLNCEJUL + 
    RBLNCEJUN + RBLNCEMAY + RBLNCEAPR)

Residuals:
     Min       1Q   Median       3Q      Max 
-1800351   -94357   -19928    69529   592922 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  2.334e+05  2.219e+03 105.166  < 2e-16 ***
REDU        -3.719e+04  1.081e+03 -34.403  < 2e-16 ***
RBLNCESEP    5.226e-01  5.271e-02   9.914  < 2e-16 ***
RBLNCEAUG    2.329e-01  3.527e-02   6.603 4.12e-11 ***
RBLNCEJUL    6.688e-01  4.716e-02  14.182  < 2e-16 ***
RBLNCEJUN    7.349e-01  5.110e-02  14.381  < 2e-16 ***
RBLNCEMAY    1.068e+00  5.373e-02  19.880  < 2e-16 ***
RBLNCEAPR    9.122e-01  4.461e-02  20.447  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 119700 on 19983 degrees of freedom
Multiple R-squared:  0.1704,	Adjusted R-squared:  0.1701 
F-statistic: 586.4 on 7 and 19983 DF,  p-value: < 2.2e-16

## Based on Gender
> MLRRLBRALL_6 <- lm(RLB ~ RSEX + RBLNCESEP +RBLNCEAUG + RBLNCEJUL + RBLNCEJUN+ RBLNCEMAY + RBLNCEAPR)
> summary(MLRRLBRALL_6)

Call:
lm(formula = RLB ~ RSEX + RBLNCESEP + RBLNCEAUG + RBLNCEJUL + 
    RBLNCEJUN + RBLNCEMAY + RBLNCEAPR)

Residuals:
     Min       1Q   Median       3Q      Max 
-1878284  -103347   -19094    69800   603123 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 1.473e+05  3.007e+03  49.000  < 2e-16 ***
RSEX        1.083e+04  1.776e+03   6.099 1.09e-09 ***
RBLNCESEP   5.621e-01  5.418e-02  10.373  < 2e-16 ***
RBLNCEAUG   2.385e-01  3.626e-02   6.576 4.96e-11 ***
RBLNCEJUL   7.075e-01  4.847e-02  14.595  < 2e-16 ***
RBLNCEJUN   7.731e-01  5.253e-02  14.715  < 2e-16 ***
RBLNCEMAY   1.119e+00  5.522e-02  20.260  < 2e-16 ***
RBLNCEAPR   9.452e-01  4.586e-02  20.610  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 123000 on 19983 degrees of freedom
Multiple R-squared:  0.1229,	Adjusted R-squared:  0.1226 
F-statistic:   400 on 7 and 19983 DF,  p-value: < 2.2e-16

## Based on Gender
> MLRRLBRALL_6 <- lm(RLB ~ RSEX + RBLNCESEP +RBLNCEAUG + RBLNCEJUL + RBLNCEJUN+ RBLNCEMAY + RBLNCEAPR)
> summary(MLRRLBRALL_6)

Call:
lm(formula = RLB ~ RSEX + RBLNCESEP + RBLNCEAUG + RBLNCEJUL + 
    RBLNCEJUN + RBLNCEMAY + RBLNCEAPR)

Residuals:
     Min       1Q   Median       3Q      Max 
-1878284  -103347   -19094    69800   603123 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 1.473e+05  3.007e+03  49.000  < 2e-16 ***
RSEX        1.083e+04  1.776e+03   6.099 1.09e-09 ***
RBLNCESEP   5.621e-01  5.418e-02  10.373  < 2e-16 ***
RBLNCEAUG   2.385e-01  3.626e-02   6.576 4.96e-11 ***
RBLNCEJUL   7.075e-01  4.847e-02  14.595  < 2e-16 ***
RBLNCEJUN   7.731e-01  5.253e-02  14.715  < 2e-16 ***
RBLNCEMAY   1.119e+00  5.522e-02  20.260  < 2e-16 ***
RBLNCEAPR   9.452e-01  4.586e-02  20.610  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 123000 on 19983 degrees of freedom
Multiple R-squared:  0.1229,	Adjusted R-squared:  0.1226 
F-statistic:   400 on 7 and 19983 DF,  p-value: < 2.2e-16


## Limit balance baseed on  Age, Sex, Edu, MARRIAGE
> MLRRLBRALL_7 <- lm(RLB ~ RSEX + REDU + RMAR + RAGE )
> summary(MLRRLBRALL_7)

Call:
lm(formula = RLB ~ RSEX + REDU + RMAR + RAGE)

Residuals:
    Min      1Q  Median      3Q     Max 
-229324 -101049  -22211   74134  760029 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 242390.4     8290.0  29.239  < 2e-16 ***
RSEX         12483.2     1841.1   6.780 1.23e-11 ***
REDU        -43081.4     1169.2 -36.848  < 2e-16 ***
RMAR         -9335.4     1828.9  -5.104 3.35e-07 ***
RAGE           532.6      167.1   3.186  0.00144 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 127000 on 19986 degrees of freedom
Multiple R-squared:  0.06544,	Adjusted R-squared:  0.06526 
F-statistic: 349.9 on 4 and 19986 DF,  p-value: < 2.2e-16

## Limit balance based on Age, Sex, Edu, MARRIAGE, default Payee
> MLRRLBRALL_8 <- lm(RLB ~ RSEX + REDU + RMAR + RAGE + RDEFAULT)
> summary(MLRRLBRALL_8)

Call:
lm(formula = RLB ~ RSEX + REDU + RMAR + RAGE + RDEFAULT)

Residuals:
    Min      1Q  Median      3Q     Max 
-238454  -96411  -23663   72625  749937 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 254005.4     8205.1  30.957  < 2e-16 ***
RSEX         10470.2     1820.8   5.750 9.04e-09 ***
REDU        -42552.4     1155.1 -36.838  < 2e-16 ***
RMAR        -10269.9     1807.0  -5.683 1.34e-08 ***
RAGE           594.4      165.1   3.600 0.000319 ***
RDEFAULT    -48871.3     2187.8 -22.338  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 125400 on 19985 degrees of freedom
Multiple R-squared:  0.08821,	Adjusted R-squared:  0.08798 
F-statistic: 386.7 on 5 and 19985 DF,  p-value: < 2.2e-16

#########################################################################################################
# Model : Correaltion between Payments									#								#													#
# Description: To find is there any correlation betwenn Payments due, paid attributes                   #
# Attributes taken: Limit Balance vs Payments related attributes 					#
#  													#
#   													#
#########################################################################################################


## Find the Co-relation between payments
COR_TEST_PAYMENTS <- cor(subset(CreditCardData_EDA, select = c(LIMIT_BAL,BILLAMT_SEPT,BILLAMT_AUG,BILLAMT_JUL,BILLAMT_JUN,BILL_AMT_MAY,PAY_AUG,PAY_JUL,PAY_JUN,PAY_MAY,PAY_APR)))
> corrplot::corrplot(CORTEST, method = "number")








