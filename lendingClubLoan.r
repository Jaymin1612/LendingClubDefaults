
#Loading data
D<-read.csv("loan.csv",header=T)
attach(D)
### Data Frame 1 ###
# 887,379 Obserations &  47 Variables
df1=data.frame(addr_state,issue_d,policy_code,purpose,int_rate,grade,annual_inc,annual_inc_joint,application_type,collections_12_mths_ex_med,delinq_2yrs,dti,dti_joint,emp_length,funded_amnt,home_ownership,inq_last_6mths,installment,loan_amnt,loan_status,mths_since_last_delinq,mths_since_last_major_derog,mths_since_last_record,open_acc,out_prncp,pub_rec,revol_bal,revol_util,total_acc,total_pymnt,total_rec_late_fee,open_acc_6m,open_il_6m,open_il_12m,open_il_24m,mths_since_rcnt_il,total_bal_il,il_util,open_rv_12m,open_rv_24m,max_bal_bc,all_util,total_rev_hi_lim,inq_fi,total_cu_tl,inq_last_12m,acc_now_delinq,tot_coll_amt,tot_cur_bal)
detach(D)

#Keep good amount of observations and some NAs.
df2_1=df1[which(complete.cases(df1$mths_since_last_delinq)),]
df2_2=df2_1[which(complete.cases(df2_1$tot_cur_bal)),]
df2_3=df2_2[which(complete.cases(df2_2$revol_util)),]

### Dataframe 3 ###
#407,770 OBS and 28 Variables NO NAS
df3=df2_3[,c(-8,-13,-38,-36,-46,-42,-42,-41,-40,-39,-37,-35,-34,-33,-32,-45,-22,-23,-44)]
df3$loan_status<-factor(as.vector(df3$loan_status))

#Date
###Issue Date to Date varaible###
df3=data.frame(df3,date=gsub("-","-01-",df3$issue_d))
df3=data.frame(df3,date=as.Date(df3$date,"%b-%d-%Y"))
###



#########################################
###### REMAPING CATEGORICAL VARIABLES####
#########################################

### Removes Current, Issued, Grace Period Status###
df3=df3[which(!df3$loan_status %in% c("Current","Issued","In Grace Period")),]
aa=as.vector(df3$loan_status)

#Loan_Status
### Remaps Lates,Defaults,Charge Off to 1 Group "Deliquent" ####
aa[which(aa %in% c("Charged Off","Default","Late (16-30 days)","Late (31-120 days)"))]="Deliquent"
df3$loan_status<-factor(aa)

#STATES
A=table(df3$addr_state,df3$loan_status)[,1]
B=table(df3$addr_state,df3$loan_status)[,2]
C=A/(A+B)
#Cut-offs Based on quantile (0.25, 0.50 0.75)
#quantile(C)
A=names(C[which(C<=0.23)])
B=names(C[which(C>=0.26)])
D=names(C[which(C<0.26 & C>0.23)])
aa=as.vector(df3$addr_state)
aa[which(aa %in% A)]<-"Low Deliquency"
aa[which(aa %in% B)]<-"High Deliquency"
aa[which(aa %in% D)]<-"Med Deliquency"
df3$addr_state<-factor(aa)

#EMP_LENGTH
aa=as.vector(df3$emp_length)
aa[which(aa=="< 1 year")]<-"0.5"
aa[which(aa=="1 year")]<-"1"
aa[which(aa=="10+ years")]<-"10"
aa[which(aa=="2 years")]<-"2"
aa[which(aa=="3 years")]<-"3"
aa[which(aa=="4 years")]<-"4"
aa[which(aa=="5 years")]<-"5"
aa[which(aa=="6 years")]<-"6"
aa[which(aa=="7 years")]<-"7"
aa[which(aa=="8 years")]<-"8"
aa[which(aa=="9 years")]<-"9"
aa[which(aa=="n/a")]<-"0"
df3$emp_length<-as.numeric(aa)

dfCat = df3[, c("addr_state", "issue_d","purpose","grade","application_type",
                "home_ownership", "loan_status")]

dfNum = df3[, c("int_rate","annual_inc","collections_12_mths_ex_med","delinq_2yrs",
                "dti","emp_length", "funded_amnt","inq_last_6mths","installment",
                "loan_amnt","mths_since_last_delinq","open_acc","out_prncp",
                "pub_rec", "revol_bal", "revol_util", "total_acc","total_pymnt",
                "total_rec_late_fee", "total_rev_hi_lim","acc_now_delinq",
                "tot_coll_amt","tot_cur_bal")]
str(dfCat)
str(dfNum)
#Grade - Every grade has it's own distirbution. Don't worry about Grouping.

#Home Ownership- Should be kept separate.

#Purpose - Should discuss how to group (IF NECESSARY). Please Eveyone Examine then discuss.

#######################################################
#Before running any analysis technique, Let's create
#train and test sets
#######################################################

indexes1 = sample(1:nrow(dfCat), size = 0.3 * nrow(dfCat))
testCat = dfCat[indexes1, ]
dim(testCat)
trainCat = dfCat[-indexes1,]
dim(trainCat)

indexes2 = sample(1:nrow(dfNum), size = 0.3 * nrow(dfNum))
testNum = dfNum[indexes2, ]
dim(testNum)
trainNum = dfNum[-indexes2,]
dim(trainNum)
#######################################################



#######################################################
#Analysis part
#starts from here
#######################################################


library(corrplot)
library(psych)
library(car)
library(QuantPsyc)
library(leaps)

#PCA_Plot function

PCA_Plot = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = data.frame(pcaData$rotation, .names = row.names(pcaData$rotation))
  p + geom_text(data=loadings, mapping=aes(x = PC1, y = PC2, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC1", y = "PC2")
}

#######################################################

#Checking correlation in train set
corr.train = cor(trainNum)
corrplot(corr.train, method = "ellipse")
#From the correlation plot we can say that 
#   1) dti and annual_inc 
#   2) mths_since_last_delinq and delinq_2yrs
#are negatively correlated and rest are positively correlated.

#Checking correlation in test set
corr.test = cor(testNum)
corrplot(corr.test, method = "ellipse")
#And we can examine from both train and test correlation plot 
#that both are almost identical.


########## 1. Principle component analysis ##########


#Now we will perform principle componant analysis on train set 
p = prcomp(trainNum, center = TRUE, scale = TRUE)
summary(p) #10 components needed to account 70% of the variance in the train set
print(p)

plot(p)
PCA_Plot(p)
biplot(p)

#Now we will perform principle componant analysis on test set and compare result 
p1 = prcomp(testNum, center = TRUE, scale = TRUE)
summary(p1) #As expected, 10 components needed to account 70% of the variance in the test set
print(p1)

plot(p1)
PCA_Plot(p1)
biplot(p1)


########## 2. linear discriminant analysis ##########


#We will first store loan_status variable to numerical train and test sets
trainNum2 = trainNum
testNum2 = testNum
trainNum2$loan_status = trainCat$loan_status
testNum2$loan_status = testCat$loan_status

#Performing LDA function on train set
trainLDA = lda(loan_status ~ ., data = trainNum2)
trainLDA

#Predicting test data on the besis of trainLDA function
predictLDA = predict(trainLDA, newdata=testNum)$class
predictLDA

#Seeing how well our function performed
table(predictLDA, testNum2$loan_status)
###From the above table we can see that there are some prediction
###     for fully paid which were actually belongs to Deliquent

predictLDA2 = lda(loan_status ~ ., data=testNum2, CV=T)
predictLDA2

table(predictLDA2$class, testNum2$loan_status)
###We are still seeing some errors


########## 3. Canonical correlation ##########


##############
#Wilks function
ccaWilks = function(set1, set2, cca)
{
  ev = ((1 - cca$cor^2))
  ev
  
  n = dim(set1)[1]
  p = length(set1)
  q = length(set2)
  k = min(p, q)
  m = n - 3/2 - (p + q)/2
  m
  
  w = rev(cumprod(rev(ev)))
  
  # initialize
  d1 = d2 = f = vector("numeric", k)
  
  for (i in 1:k) 
  {
    s = sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5))
    si = 1/s
    d1[i] = p * q
    d2[i] = m * s - p * q/2 + 1
    r = (1 - w[i]^si)/w[i]^si
    f[i] = r * d2[i]/d1[i]
    p = p - 1
    q = q - 1
  }
  
  pv = pf(f, d1, d2, lower.tail = FALSE)
  dmat = cbind(WilksL = w, F = f, df1 = d1, df2 = d2, p = pv)
}
##############

cor(trainNum)
loan_amount = trainNum$loan_amnt
installment = trainNum$installment
total_rev_hi_lim = trainNum$total_rev_hi_lim
revol_bal = trainNum$revol_bal

cor(loan_amount, installment)
cor(total_rev_hi_lim, revol_bal)

amount = trainNum[, c("loan_amnt", "installment")]
revol = trainNum[, c("total_rev_hi_lim", "revol_bal")]

library(CCA)
matcor(amount,revol)

ccLoan = cc(amount, revol)
ccLoan$cor
ls(ccLoan)
ccLoan$xcoef
ccLoan$ycoef

#Trying Wilks function
wilksIris = ccaWilks(amount, revol, ccLoan)
round(wilksIris, 2)

# Now, let's calcualte the standardized coefficients
s1 = diag(sqrt(diag(cov(amount))))
s1 %*% ccLoan$xcoef

s2 = diag(sqrt(diag(cov(revol))))
s2 %*% ccLoan$ycoef


########## 4. K means clustering ##########


#I tried different combinations to see if the data is good enough for K means clustering
#   but I did not find any combination where I can see some clusters are forming.
trainNum2 = trainNum
testNum2 = testNum
trainNum2$purpose = trainCat$purpose
testNum2$purpose = testCat$purpose
plot(trainNum2$loan_amnt, trainNum2$total_pymnt)

library(ggplot2)
ggplot(data = trainNum2, aes(loan_amnt, total_pymnt, color = purpose)) + geom_point()


########## 5. Naive bayes ##########


library(e1071)

trainNum3 = trainNum
testNum3 = testNum
trainNum3$purpose = trainCat$purpose
testNum3$purpose = testCat$purpose

model <- naiveBayes(purpose ~ ., data = trainNum3)
class(model)
summary(model)
print(model)

tbl_list <- sapply(trainNum3[-10], table, trainNum3[ , 10])
tbl_list <- lapply(tbl_list, t)

cond_probs <- sapply(tbl_list, function(x) { 
  apply(x, 1, function(x) { 
    x / sum(x) }) })

cond_probs <- lapply(cond_probs, t)

print(cond_probs)

preds <- predict(model, newdata = testNum3)

conf_matrix <- table(preds, testNum3$purpose)


########## 6. Correspondence Analysis ##########


library(MASS)
library(ca)
str(trainCat)
tbl = table(trainCat$grade, trainCat$loan_status)
chisq.test(tbl)
#I found that loan status and grade have p-value much lower than 0.05.

fit = ca(tbl)
summary(fit)
fit
