# **********************************************************************************************
# # Installing required Package 
# **********************************************************************************************
if(!require(tidyverse))    install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret))        install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggthemes))     install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(ggcorrplot))   install.packages("ggcorrplot", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(magrittr))     install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rpart))        install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot))   install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(neighbr))      install.packages("neighbr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(formattable)) install.packages("formattable", repos = "http://cran.us.r-project.org")
if(!require(patchwork))     install.packages("patchwork", repos = "http://cran.us.r-project.org")
if(!require(VIM)) install.packages("hot.deck", repos = "http://cran.us.r-project.org")

#### Use the required library
library(tidyverse)
library(dplyr)
library(caret)
library(ggthemes)
library(ggcorrplot)
library(randomForest)
library(magrittr)
library(rpart)
library(rpart.plot)
library(neighbr)
library(kableExtra)
library(formattable)
library(corrgram) 
library(patchwork) 
library(ggplot2) 
library(e1071)
library(patchwork)
library(VIM)

# **********************************************************************************************
#### Set Local Variales 
# **********************************************************************************************

#### set_them has general properties of images and graphics
set_theme <- theme(plot.background = element_rect(fill="#F5FFFA",color = "darkblue"),
                   text = element_text(size=16), 
                   axis.title.x = element_text(size=16, color = "black"),
                   axis.title.y = element_text(size=16, color = "black"),
                   panel.border = element_rect(colour="black", linetype = "solid", fill=NA), 
                   plot.title = element_text(hjust = 0.5, size = 18), 
                   plot.caption = element_text(hjust = 0.5))

set_theme_cat <- theme(plot.background = element_rect(fill="#F5FFFA",color = "darkblue"),
                       plot.title = element_text(size=20, hjust=.5),
                       axis.title.x = element_text(size=18, color = "black"),
                       axis.title.y = element_text(size=18, color = "black"),
                       axis.text.x = element_text(size=16),
                       axis.text.y = element_text(size=16),
                       
                       legend.text = element_text(size=16),
                       legend.title = element_text(size=18))

# **********************************************************************************************
#### import and read the database
# **********************************************************************************************
# the github repo with the data set "Heart_failure_data_set" is available here: //github.com/azouri123/Own-Project-Submission_HRA_ECG-analysis-/blob/main/heart_failure_data_set.csv
# the file "Heart_failure_data_set.csv" provided in the github repo must be included in the working (project) directory for the code below to run

#Importing the data set: 
set.seed(1, sample.kind="Rounding")
heart_failure_dataset <- read.csv("heart_failure_data_set.csv")
#presenting the dataset:  
head(heart_failure_dataset)
### summarize the Database
summary(heart_failure_dataset)
typeof(heart_failure_dataset)
# **********************************************************************************************
#### 2. Data Analysis :(data wrangling, Data Visiualisation
# **********************************************************************************************

#####################################################################
#### 2.1 Data Wrangling ####

### Missing data review :

any(is.na(heart_failure_dataset))


### Harmonizing the variables (Factorization): 
heart_failure_dataset$Sex <- as.factor(heart_failure_dataset$Sex)
heart_failure_dataset$ChestPainType <- as.factor(heart_failure_dataset$ChestPainType)
heart_failure_dataset$FastingBS <- as.factor(heart_failure_dataset$FastingBS)
heart_failure_dataset$RestingECG <- as.factor(heart_failure_dataset$RestingECG)
heart_failure_dataset$ExerciseAngina <- as.factor(heart_failure_dataset$ExerciseAngina)
heart_failure_dataset$ST_Slope <- as.factor(heart_failure_dataset$ST_Slope)

#####################################################################
#### 2.1 Data Visiualisation ####

## Before visualising data, let's check if there is any ouliers 

### 2.1.1 Check for Outlier: 

p1 <- heart_failure_dataset %>% ggplot(aes(Age)) + geom_density(fill = "Red") + labs(x="",title = "Age") + theme_minimal()
p2 <- heart_failure_dataset %>% ggplot(aes(Sex)) + geom_bar(aes(fill = Sex)) + labs(x="",title = "Sex") + theme_minimal()
p3 <- heart_failure_dataset %>% ggplot(aes(ChestPainType)) + geom_bar(aes(fill = ChestPainType)) + labs(x="",title = "Chest Pain Type") + theme_minimal()
p4 <- heart_failure_dataset %>% ggplot(aes(RestingBP)) + geom_histogram(colour = "black",fill = "purple") + labs(x="",title = "Resting Blood Pressure") + theme_minimal()
p5 <- heart_failure_dataset %>% ggplot(aes(Cholesterol)) + geom_histogram(colour = "black",fill = "yellow") + labs(x="",title = "Cholesterol Level") + theme_minimal()
p6 <- heart_failure_dataset %>% ggplot(aes(FastingBS)) + geom_bar(fill = "maroon4") + labs(x="",title = "Blood Sugar after Fasting over 120") + theme_minimal()
p7 <- heart_failure_dataset %>% ggplot(aes(RestingECG)) + geom_bar(aes(fill = RestingECG)) + labs(x="",title = "Resting ECG Levels") + theme_minimal()
p8 <- heart_failure_dataset %>% ggplot(aes(MaxHR)) + geom_density(fill = "brown") + labs(x="",title = "Max Heart Rate") + theme_minimal()
p9 <- heart_failure_dataset %>% ggplot(aes(ExerciseAngina)) + geom_bar(aes(fill = ExerciseAngina)) + labs(x="",title = "Exercise-induced angina") + theme_minimal()
p10 <- heart_failure_dataset %>% ggplot(aes(Oldpeak)) + geom_histogram(color = "black", fill = "turquoise") + labs(x="",title = "Oldpeak") + theme_minimal()
p11 <- heart_failure_dataset %>% ggplot(aes(ST_Slope)) + geom_bar(aes(fill = ST_Slope)) + labs(x="",title = "ST Slope") + theme_minimal()
p12 <- heart_failure_dataset %>% ggplot(aes(HeartDisease)) + geom_bar(fill = "purple") + labs(x="",title = "Heart Disease (Y/N)") + theme_minimal()

(p1 | p2 | p3) 
(p4 | p5 | p6) 
(p7 | p8 | p9) 
(p10 | p11 | p12)


### it appears that the restingBp = 0 is an outlier and is not logical because if RestingBp = 0 so the patient is dead 
## So we gonna use data imputation on this outliers. Before that we gonna understand its related vaiables :
heart_failure_dataset %>% filter(RestingBP == 0)

## The Patient have a normal restingECG so definitely there was an error, different imputation method exist like Hot-deck, clod-deck mean substitution or Non-negative matrix factorization   
##in this time we gonna use the hot-deck imputation wchich was invented in the 1950s which consist simply of replacing every missing value with the last observed value in the same variable. 
## For that we ganna replace the RextingBp = 0 to RextingBp = NA and then use the hotdeck function; 
##Hot-deck in its vanilla form may break relations between variables, that is why we ganna  impute within domains (HeartDisease variable) 
heart_failure_dataset$RestingBP <- replace(heart_failure_dataset$RestingBP,heart_failure_dataset$RestingBP == 0,NA) 
## Verifying that there is no more 0 in the RestingBp  
heart_failure_dataset %>% filter(RestingBP == 0)
## Verifying that there is Na in the new dataset 
any(is.na(heart_failure_dataset))
## using the hotdeck function to replace the Na 
heart_failure_dataset <- hotdeck(heart_failure_dataset,domain_var = "HeartDisease")
##Verifying that we do not have any NA
any(is.na(heart_failure_dataset))
## The other non comprhensive value deducted from the the graphs is the cholesterol level = 0 
## For that we will use the mean substitution, but the problem is that method may attenuates any correlations involving the variable(s) that are imputed
### so before that we gonna try to look for the variable who the most predict the cholesterol level so we can then adjust the our mean substution to htat varibale: 
#### Cholesterol level analysis: 
ch1 <- heart_failure_dataset %>% filter(Cholesterol > 0) %>% ggplot(aes(x=Age,y=Cholesterol)) + geom_point() + geom_smooth()
ch2 <- heart_failure_dataset %>% filter(Cholesterol > 0) %>% ggplot(aes(x=Sex,y=Cholesterol)) + geom_boxplot(aes(fill = Sex)) + theme(legend.position='none')
ch3 <- heart_failure_dataset %>% filter(Cholesterol > 0) %>% ggplot(aes(x=ChestPainType,y=Cholesterol)) + geom_boxplot(aes(fill = ChestPainType)) + theme(legend.position='none')
ch4 <- heart_failure_dataset %>% filter(Cholesterol > 0) %>% ggplot(aes(x=FastingBS,y=Cholesterol)) + geom_boxplot(aes(fill = FastingBS)) + theme(legend.position='none')
ch5 <- heart_failure_dataset %>% filter(Cholesterol > 0) %>% ggplot(aes(x=RestingECG,y=Cholesterol)) + geom_boxplot(aes(fill = RestingECG)) + theme(legend.position='none')
ch6 <- heart_failure_dataset %>% filter(Cholesterol > 0) %>% ggplot(aes(x=MaxHR,y=Cholesterol)) + geom_point() + geom_smooth()
ch7 <- heart_failure_dataset %>% filter(Cholesterol > 0) %>% ggplot(aes(x=ExerciseAngina,y=Cholesterol)) + geom_boxplot(aes(fill = ExerciseAngina)) + theme(legend.position='none')
ch8 <- heart_failure_dataset %>% filter(Cholesterol > 0) %>% ggplot(aes(x=ST_Slope,y=Cholesterol)) + geom_boxplot(aes(fill = ST_Slope)) + theme(legend.position='none')
design <- "
AABC
DEFF
GGHH
"

ch1 + ch2 + ch3 + ch4 + ch5 + ch6 + ch7 + ch8 +
  plot_layout(design = design) + plot_annotation(title = "Cholesterol vs other variables")


#### The comparison of different variable against the cholesterol level shows that the sex is the variable that impact the most the cholesterol level.That is why we will replace the Cholesterol level = 0 with the median cholesterol level adjusted to sex :

m.Chol.Median <- heart_failure_dataset %>% filter(Sex == "M", Cholesterol > 0) %>% summarise(median(Cholesterol)) %>% as.numeric
f.Chol.Median <- heart_failure_dataset %>% filter(Sex == "F", Cholesterol > 0) %>% summarise(median(Cholesterol)) %>% as.numeric

heart_failure_dataset$Cholesterol[heart_failure_dataset$Sex == "M" & heart_failure_dataset$Cholesterol == 0] <- m.Chol.Median
heart_failure_dataset$Cholesterol[heart_failure_dataset$Sex == "F" & heart_failure_dataset$Cholesterol == 0] <- f.Chol.Median

#Checking the results: 
heart_failure_dataset %>% ggplot(aes(Cholesterol)) + geom_histogram(colour = "black",fill = "Red") + labs(x="",title = "Cholesterol Level") + theme_minimal()

### We have now a clean and tidy dataset, We can now compare the HeartDisease variable with all other variable to appraise if there is any correlation: 

heart_failure_dataset %>% corrgram(lower.panel = panel.pie ,upper.panel = panel.shade)

#### The correlogram indicates that the Oldpeak and the MaxHR has important correlation in determining if the person has high risk or not of heart disease
### to dive deep in this we goona analyse further the relation between the heart diseases and the other variables: 

hd1 <- heart_failure_dataset %>% ggplot(aes(Age)) + geom_histogram(aes(fill = factor(HeartDisease))) + theme(legend.position='none')
hd2 <- heart_failure_dataset %>% ggplot(aes(Sex)) + geom_bar(aes(fill = factor(HeartDisease))) + theme(legend.position='none')
hd3 <- heart_failure_dataset %>% ggplot(aes(ChestPainType)) + geom_bar(aes(fill = factor(HeartDisease))) + theme(legend.position='none')
hd4 <- heart_failure_dataset %>% ggplot(aes(RestingBP)) + geom_histogram(aes(fill = factor(HeartDisease))) + theme(legend.position='none')
hd5 <- heart_failure_dataset %>% ggplot(aes(Cholesterol)) + geom_histogram(aes(fill = factor(HeartDisease))) + theme(legend.position='none')
hd6 <- heart_failure_dataset %>% ggplot(aes(FastingBS)) + geom_bar(aes(fill = factor(HeartDisease))) + theme(legend.position='none')
hd7 <- heart_failure_dataset %>% ggplot(aes(RestingECG)) + geom_bar(aes(fill = factor(HeartDisease))) + theme(legend.position='none')
hd8 <- heart_failure_dataset %>% ggplot(aes(MaxHR)) + geom_histogram(aes(fill = factor(HeartDisease))) + theme(legend.position='none')
hd9 <- heart_failure_dataset %>% ggplot(aes(ExerciseAngina)) + geom_bar(aes(fill = factor(HeartDisease))) + theme(legend.position='none')
hd10 <- heart_failure_dataset %>% ggplot(aes(Oldpeak)) + geom_histogram(aes(fill = factor(HeartDisease))) + theme(legend.position='none')
hd11 <- heart_failure_dataset %>% ggplot(aes(ST_Slope)) + geom_bar(aes(fill = factor(HeartDisease))) + theme(legend.position='none')

(hd1 + (hd2 + hd3)) 
(hd4 + hd5 + hd6)
(hd7 + hd8 + hd9) 
(hd10 + hd11) +
  plot_annotation(title = "Heart Disease corrolation")


## After comparing HeartDisease with other variable, we can conlcude that: 

# the higher the Age the higher 
#The males have higher risk
#ChestPainType ASY have higher risk)
#FastingBS >120 increase the risk)
#MaxHR the lower induce more risk)
#ExerciseAngina (Y = high risk)
#Having a ST_Slope Down or Flat is correlated with high risk

## After cleaning then analysis our dataset we can build our Models: 

# 4.Creating models: 
## For this project we will use as recommended advanced methods of machine learning: 
# Logistic Regression,
#Random Forest,
#Support Vector Machine,
#K-nearest neighbour & Neural Nets

# Splitting train and HF_testset 

# we will create train set(80%) and HF_testset set (20%) : 
ind = createDataPartition(heart_failure_dataset$HeartDisease, times = 1, p = 0.8, list = FALSE)
HF_trainset <- heart_failure_dataset[ind,]
HF_testset <- heart_failure_dataset[-ind,]

#4 Logistic Regression Model
log.model <- glm(HeartDisease ~ ., data = HF_trainset, family = binomial(link = 'logit'))
summary(log.model)

# Logistic Regression Model heart_failure_dataseting
glm.prediction <- predict(log.model, newdata = HF_testset, type = "response")
glm.prediction <- ifelse(glm.prediction >= 0.5, 1, 0)

table(HF_testset$HeartDisease, glm.prediction)
sum(HF_testset$HeartDisease==glm.prediction) / nrow(HF_testset)

#Random Forest Model

rf.model <- randomForest(HeartDisease ~ ., data = HF_trainset)
importance(rf.model)

rf.prediction <- predict(rf.model, newdata = HF_testset)
rf.prediction <- ifelse(rf.prediction >= 0.5, 1, 0)

table(HF_testset$HeartDisease, rf.prediction)
sum(HF_testset$HeartDisease==rf.prediction) / nrow(HF_testset)

#Support Vector Machine (linear and radial kernel)

tune.svm.linear <- tune.svm(HeartDisease ~ ., data = HF_trainset, kernel = "linear" , cost=c(0.001,0.01,0.1), gamma=c(0.001,0.01,0.1))
tune.svm.radial <- tune.svm(HeartDisease ~ ., data = HF_trainset, kernel = "radial" , cost=c(0.01,0.1,0.5,1), gamma=c(0.1,0.5,1))
svm.linear <- svm(HeartDisease ~ ., data = HF_trainset, kernel = "linear", cost = 0.01, gamma = 0.001)
svm.radial <- svm(HeartDisease ~ ., data = HF_trainset, kernel = "radial", cost = 1, gamma = 0.1)

svm.linear.pred <- predict(svm.linear, newdata = HF_testset)
svm.linear.pred <- ifelse(svm.linear.pred >= 0.5, 1, 0)
table(HF_testset$HeartDisease,svm.linear.pred)
sum(HF_testset$HeartDisease == svm.linear.pred)/nrow(HF_testset)

svm.radial.pred <- predict(svm.radial, newdata = HF_testset)
svm.radial.pred <- ifelse(svm.radial.pred >= 0.5, 1, 0)
table(HF_testset$HeartDisease,svm.radial.pred)
sum(HF_testset$HeartDisease == svm.radial.pred)/nrow(HF_testset)

#Model Comparison

#Here is the list 3 models tested:

#Logistic Regression Model : 0.885
#Random Forest Model : 0.890
#Support Vector Machine (radial) Model : 0.852
#Support Vector Machine (linear) Model : 0.885

#CONCLUSION: 

#The Best model as described above is the Random Forest Model. 







