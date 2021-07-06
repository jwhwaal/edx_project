library(readxl)
library(tidyverse)
library(curl)
library(httr)
library(reshape2)
library(broom)
library(caret)
library(readr)
library(lubridate)
library(ggthemes)
library(formattable)
library(nnet)
library(rattle)


list.of.packages <- c("readxl", "tidyverse", "curl", "httr", "reshape2", "broom", "caret", "lubridate", 
                      "ggthemes", "formattable", "nnet", "knn", "rpart", "skimr", "rpart.plot", "UBL", "glmnet", 
                      "rf", "rda", "treebag", "svm", "rattle")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#************************************************************************************
#*                    CROWN ROT PREDICTION FOR ORGANIC BANANAS
#************************************************************************************
getwd()
setwd("C:/Users/hwwaal/projects/edx_project")
#READING QUALITY AND SHIPMENT DATA 
data <- read.csv("data.txt")

#data legend:
# ca    : dummy variable indicating if consignment is carried under controlled atmosphere
# pd    : pack date (a few days before date of departure of vessel)
# dd    : discharge data (date of opening the container in Europe)
# days  : the transit time, the time between pack date and date of discharge
# CR    : the percentage of crown rot (mild)
# CRC   : the percentage of crown rot (heavy)
# C     : the summed and weighted crown rot percentage (CR + 2 * CRC)


data <- data %>% select(-X) %>% mutate(dd = as.Date(dd), ca = as.factor(ca), pd = as.Date(pd)) 
#in the above, we make factors and create the dependent variable C, which is composed of 
#the incidence of light crown rot CR and heavy crown rot CRC. Heavy is penalised by a factor of 2.

# explore the crown rot incidence per pack date
data %>% ggplot(aes(pd, C*100, col=ca)) +
  geom_point() + 
  ggtitle(label  = "The time tendency of crown rot") +
  ylab('Crown rot %') + xlab('date') +
  theme(text = element_text(size=8),
        axis.text.x = element_text(angle=0, hjust=1)) +
  theme_few()

#READ WEATHER DATA FROM WEATHER STATION CLOSE TO PRODUCTION LOCATION.

#create a function for reading weather from meteostat API:

read_weather <- function(station){
  url <- paste("https://bulk.meteostat.net/hourly/",station,".csv.gz", sep= "")
  tempdir()
  tmp <- tempfile()
  curl_download(url, tmp)
  download.file(url, destfile = paste(station, ".wd", sep=""))
}

#define weather station of choice: Piura with ID 84401
station <- 84401


#read the weather from meteostat:84401
read_weather(station) #this function downloads the data in compressed form

#collect the data structure of weather report 
names <- read.csv("weather-structure.csv", header=FALSE, sep=";")
vars <- unlist(names$V2)
type <-unlist(names$V3)

weather <- read.csv(paste(station, ".wd", sep = "")) #read and decompress the weather file
names(weather) <- vars # attach understandable variable names to the columns

# select the variables to be used and mutate some new variables

W_PE <- weather %>% select(date, temp) %>%
  mutate(date = as.Date(date))

# create a function for calculation of a temperature sum over a specified time span
# define the time span for the accumulated degree days
span <- 10 #weeks

# make a dataframe with mean temperature per day
summ_PE <- W_PE %>% 
  group_by(date) %>%
  summarise(mean_temp = mean(temp, na.rm = TRUE)) # 
            
# make a graph of mean temperature against date
summ_PE %>% filter(date > "2016-01-01") %>%
  ggplot(aes(date, mean_temp)) +
  geom_line() +
  ggtitle(label  = "Average daily temperature Piura") +
  ylab('Temperature °C')+ xlab('date') +
  theme(text = element_text(size=8),
        axis.text.x = element_text(angle=0, hjust=1)) +
  theme_few()

#formula to calculate the cumulative sums based on span and cut-off of 13.5°C
summ_add_PE <- summ_PE %>% filter(date >=  (as.Date("2016-01-01")-span*7)) %>% 
  mutate(grow_temp = (mean_temp - 13.5), cdd = cumsum(grow_temp)) %>% #the grow temperature is the temperature above 13.5°C under which temperature bananas do not grow but die
  mutate(add = cdd - lag(cdd, span*7)) %>% # the accumulated degree.days or temperature sum is calculated for every day from the cumulative grow_temp
  na.exclude() #exclude rows with NaN

# for control, make a plot of accumulated degree days against date
summ_add_PE %>%
  ggplot(aes(date, add)) +
  geom_line() + 
  ylab('Accumulated degree.days °C.day')+ xlab('date') +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=0, hjust=1)) 



# join the quality data to the weather data to have a list of quality data with ADDs for every shipment.
data_PE <- data %>% left_join(summ_add_PE, by = c('pd' = 'date')) 

#************************************************************************************
#                             EXPLORATORY DATA ANLYSIS
#*********************************************************************************

#make a plot of average CR incidence per despatch month
data %>% mutate(week = strftime(pd, format = "%V"), month = strftime(pd, format = "%m")) %>% 
  ggplot(aes(month, C*100, col = ca)) +
  geom_boxplot() +
    scale_x_discrete(name = "month") +
    scale_y_continuous(name = "crown rot per month average %") +
    theme(axis.title.y = element_text(color = "grey"),
          axis.title.y.right = element_text(color = "blue")) +
    theme_few()

# make an overlay plot of ADD  and crown rot
ggplot() + 
  geom_line(data=summ_add_PE, aes(x=date, y=add), color='blue') + 
  geom_point(data=data_PE, aes(x=pd, y=C*2000), color='purple', alpha = 0.5)+
  scale_x_date(name = "date", labels = date) +
  scale_y_continuous(name = "temperature sum °C.day", 
                     sec.axis = sec_axis(~./100, name = "crown rot", 
                                         labels = function(b) { paste0(round(b, 0), "%")})) +  
  theme(axis.title.y = element_text(color = "grey"),
        axis.title.y.right = element_text(color = "blue")) +
  theme_few()

#make a heatmap of crown rot versus transit time (days) and ADD 
#define bins
lbl_days <- c("11-20", "21-30", "31-40", "41-50", "51-60", "61-70")

lbl_add <- c("401-500", "501-600", "601-700", "701-800", "801-900",
             "901-1000", "1001-1100","1101-1200")

#compute averages per bin
data_hm_PE <- data_PE %>% drop_na(add) %>% mutate(day_bins = cut(days, breaks = seq(10, 70, 10), 
                        include.lowest = FALSE, label = lbl_days), 
         add_bins = cut(add, breaks = seq(400,1200, 100), 
                        include.lowest = FALSE, label = lbl_add)) %>%
  select(day_bins, add_bins, C, days, ca) %>%
  group_by(day_bins, add_bins, ca) %>%
  dplyr::summarize(CR_avg = mean(C), n = n(), ca=ca)

#make geom_tile plot
p2 <- data_hm_PE %>%
  ggplot(aes(x = day_bins, y = add_bins)) +
  geom_tile(aes(fill = CR_avg*100)) + 
  scale_fill_gradient2(low = "white", high = "blue", 
                       midpoint = 1, 
                       breaks = seq(0, 20, 5), 
                       limits = c(0, 20)) +
  labs(title="crown rot versus ADD and transit time",
       x ="transit time (days)", y = "temperature sum (degree.days)", 
       subtitle = "figures indicate number of shipments") +
  labs(fill = "Avg crown rot %") +
  geom_text(aes(label = n), size = 2) +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=0, hjust=1)) + 
  facet_grid(. ~ ca)
p2 + theme(plot.subtitle=element_text(size=8, hjust=0, face="italic", color="black"))


#explore correlation between variables

#make a correlation matrix
source("corstars.R") #this retrieves the function created by 
data_PE %>% select(C, add, days, ca) %>% mutate(ca = as.numeric(ca)) %>%
cor(., method = "kendall", use = "complete.obs")

data_PE %>% select(C, add, days, ca) %>% mutate(CA = as.numeric(ca)) %>% select(-ca) %>%
  drop_na() %>% corstars(.) 



#******************************************************************************
#*                  PREDICTING CROWN ROT WITH VARIOUS ML ALGORITHMS
#******************************************************************************


data_PE %>% filter(C<0.2) %>% ggplot(aes(C)) +
  geom_histogram(binwidth  = 0.01 )


#Create a categorical variable for CR risk"(low/medium/high:
data_ML <- data_PE %>% select(ca, days, C, add, pd) %>% 
  mutate(C_risk = cut(C, breaks = c(0,0.007, 0.075, 1), 
                      include.lowest = TRUE,
                      labels = c("low", "medium",  "high")),
         ca = as.factor(ca)) %>%
  select(-C)


#make a descriptive statistics table to inspect the data
library(skimr)
skimmed <- skim(data_ML)
skimmed



#check for near-zero-variance predictors because dataset is unbalanced

nzv <- nearZeroVar(data_ML, saveMetrics= TRUE)
formattable(nzv)


#************************************************************************************
#* make train and test data sets
#* ********************************************************************************

#splitting the data in a train and a test set and a hold-out set for validation
validation <- data_ML %>% filter(pd >= "2021-01-01") %>% select(-pd) %>% mutate(ca = as.integer(ca))
traintest <- data_ML %>% filter(pd < "2021-01-01") %>% drop_na() %>% select(-pd) %>% mutate(ca = as.integer(ca))

#create a data partition
set.seed(1, sample.kind = "Rounding")
trainIndex <- createDataPartition(traintest$C_risk, p = .5, 
                                  list = FALSE, 
                                  times = 1)

train <- traintest[trainIndex,]
test <- traintest[-trainIndex,]

# train a decision tree for test
library(rpart)
set.seed(2, sample.kind = "Rounding")
model1 <- train(C_risk ~ ., data = train, 
                method = "rpart",
                preProcess = c("center", "scale"),
                tuneLength = 10,
                trControl = trainControl(method = "cv"))

y_hat <- predict(model1, newdata = test)
cfm_m1 <- confusionMatrix(y_hat, test$C_risk, dnn = c("Prediction", "Reference"))
cfm_m1$overall[1]

#visualize the decision tree
library(rpart.plot)
set.seed(3, sample.kind = "Rounding")
model.rpart1 <- rpart(C_risk ~ ., data = train, method = "class", control = rpart.control(cp = 0.01))
rpart.plot(model.rpart1)
prp(model.rpart1)
fancyRpartPlot(model.rpart1)

# train a simple k-nearest neighbour model 
set.seed(1, sample.kind = "Rounding")
model_knn1 <- train(C_risk ~ ., data = train, 
                   method = "knn",
                   preProcess = c("center", "scale"),
                   tuneLength = 10,
                   trControl = trainControl(method = "cv"))

y_hat_knn1 <- predict(model_knn1, newdata = test)
cfm_r <- confusionMatrix(y_hat_knn1, test$C_risk, dnn = c("Prediction", "Reference"))
cfm_r

# creating a synthetic balanced data set using synthetic sampling (SMOTE)

library(UBL)
train.smote <- SmoteClassif(C_risk ~ ., train, list("low"=0.5, "medium" = 2, "high"=7), k = 5, repl = FALSE,
             dist = "Euclidean")

#the table show the dataset is now balanced over the classes (using under and oversampling)
table(train.smote$C_risk)
skim(train.smote)


# test the smoted dataset on the decision tree:
model_rpart <- train(C_risk ~ ., data = train.smote, 
                method = "rpart",
                preProcess = c("center", "scale"),
                tuneLength = 10,
                trControl = trainControl(method = "cv"))

y_hat_rpart <- predict(model_rpart, newdata = test)
cfm_rpart <- confusionMatrix(y_hat, test$C_risk, dnn = c("Prediction", "Reference"))


#we can again visualize the decision tree
library(rpart.plot)
#we can use priors of the distribution to improve the model and define minimum splits to reduce detail
model.rpart2 <- rpart(C_risk ~ ., data = train.smote, method = "class", parms = list(split = "ca"),
                      control = rpart.control(minsplit = 50))
fancyRpartPlot(model.rpart2)

# train a  k-nearest neighbour model on smote dataset
set.seed(2, sample.kind = "Rounding")

model_knn <- train(C_risk ~ ., data = train.smote, 
                method = "knn",
                preProcess = c("center", "scale"),
                tuneLength = 10,
                trControl = trainControl(method = "cv"),
                tuneGrid = expand.grid(k = c(2, 5, 11)))
model_knn
y_hat_knn <- predict(model_knn, newdata = test)
cfm_knn <- confusionMatrix(y_hat_knn, test$C_risk, dnn = c("Prediction", "Reference"))

# train a multinomial generalized linear model/neural network
set.seed(1, sample.kind = "Rounding")
library(glmnet)
myControl <- trainControl(
  method = "cv", number = 10,
  classProbs = TRUE # Super important!
)

myGrid <- expand.grid(
  alpha = 0:1,
  lambda = seq(0.0001, 1, length = 20)
)
# Fit a model
set.seed(33)
model_glmnet <- train(C_risk ~ ., data = train.smote,  
                method = "glmnet", 
                tuneGrid = myGrid,
                trControl = myControl)

#Check the model
model_glmnet
y_hat_glmnet <- predict(model_glmnet, newdata = test)
cfm_glmnet <- confusionMatrix(y_hat_glmnet, test$C_risk, dnn = c("Prediction", "Reference"))

# train a support vector machine linear model

model_svm <- train(C_risk ~ ., data = train.smote, 
                method = "svmLinear",
                tuneGrid = expand.grid(C = seq(0, 2, length = 20)),
                preProcess = c("center", "scale"),
                trControl = trainControl(method = "cv"))

y_hat_svm <- predict(model_svm, newdata = test)
cfm_svm <- confusionMatrix(y_hat_svm, test$C_risk, dnn = c("Prediction", "Reference"))

# train a  neural network
set.seed(2, sample.kind = "Rounding")
model_nnet <- train(C_risk ~ ., data = train.smote, 
                method = "nnet",
                preProcess = c("center", "scale"),
                trControl = trainControl(method = "cv"))
model_nnet
y_hat_nnet <- predict(model_nnet, newdata = test)
cfm_nnet <- confusionMatrix(y_hat_nnet, test$C_risk, dnn = c("Prediction", "Reference"))

# train a  treebag
set.seed(2, sample.kind = "Rounding")
model_treebag <- train(C_risk ~ ., data = train.smote, 
                 method = "treebag",
                 preProcess = c("center", "scale"),
                 trControl = trainControl(method = "cv"))
model_treebag
y_hat_treebag <- predict(model_treebag, newdata = test)
cfm_treebag <- confusionMatrix(y_hat_treebag, test$C_risk, dnn = c("Prediction", "Reference"))

# train a random forest model
#10 folds repeat 10 times
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=10)
#Metric compare model is Accuracy
metric <- "Accuracy"
set.seed(123)

#Number random variable selected is mtry
mtry <- 2
tunegrid <- expand.grid(.mtry=mtry)
model_rf <- train(C_risk ~., 
                data=train.smote, 
                preProcess = c("center", "scale"),
                method='rf', 
                metric='Accuracy', 
                tuneLength  = 4, 
                trControl=control)
print(model_rf)

y_hat_rf <- predict(model_rf, newdata = test)
cfm_rf <- confusionMatrix(y_hat_rf, test$C_risk, dnn = c("Prediction", "Reference"))


# train an rda
set.seed(2, sample.kind = "Rounding")
model_rda <- train(C_risk ~ ., data = train.smote, 
                       method = "rda",
                       preProcess = c("center", "scale"),
                       trControl = trainControl(method = "cv"))
model_rda
y_hat_rda <- predict(model_rda, newdata = test)
cfm_rda <- confusionMatrix(y_hat_rda, test$C_risk, dnn = c("Prediction", "Reference"))

# train a multilayer perceptron
set.seed(2, sample.kind = "Rounding")
model_mlp <- train(C_risk ~ ., data = train.smote, 
                      method = "mlp",
                      preProcess = c("center", "scale"),
                      trControl = trainControl(method = "cv"))
model_mlp
y_hat_mlp <- predict(model_mlp, newdata = test)
cfm_mlp <- confusionMatrix(y_hat_mlp, test$C_risk, dnn = c("Prediction", "Reference"))

# make a table of the results of the models

results_models <- data.frame(cfm_knn$overall, cfm_mlp$overall, cfm_nnet$overall, 
                             cfm_rda$overall, cfm_rf$overall, cfm_rpart$overall,
                             cfm_svm$overall, cfm_treebag$overall, cfm_glmnet$overall)
algo_names <- c("k-nearest neighbors", "multilayer perceptron", "neural network", "regularized discriminant analysis", "random forest",
           "recursive partitioning & regression trees", "support vector machine", "bootstrap aggregated tree", "generalized linerar model")
results_models<- results_models[1:2,]
colnames(results_models) <- algo_names

df <- cbind(parameter = rownames(results_models), as_tibble(results_models)) %>% 
  pivot_longer(cols = -"parameter", names_to = c("algorithm"), values_to = "value")


p3 <- df %>% filter(parameter == "Accuracy") %>% 
  ggplot(aes(reorder(algorithm, value), value)) +
                geom_bar(stat = "identity", fill = "#FF6666") +
    scale_x_discrete(name = "algorithm") +
  scale_y_continuous(name = "value") +
  theme(axis.title.y = element_text(color = "grey"),
        axis.title.y.right = element_text(color = "blue")) +
  theme_few() 
p3 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + coord_flip() 

p4 <- df %>% filter(parameter == "Kappa") %>%
  ggplot(aes(reorder(algorithm, value), value)) +
  geom_bar(stat = "identity", fill = "#FF6666") +
  scale_x_discrete(name = "kappa") +
  scale_y_continuous(name = "value") +
  theme(axis.title.y = element_text(color = "grey"),
        axis.title.y.right = element_text(color = "blue")) +
  theme_few() 
p4 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + coord_flip() 

# make a table of the F1 values per class


F1_models <- data.frame(cfm_knn[["byClass"]][ , "F1"], 
                        cfm_mlp[["byClass"]][ , "F1"], 
                        cfm_nnet[["byClass"]][ , "F1"], 
                        cfm_rda[["byClass"]][ , "F1"], 
                        cfm_rf[["byClass"]][ , "F1"], 
                        cfm_rpart[["byClass"]][ , "F1"],
                        cfm_svm[["byClass"]][ , "F1"], 
                        cfm_treebag[["byClass"]][ , "F1"], 
                        cfm_glmnet[["byClass"]][ , "F1"])
colnames(F1_models) <- algo_names


df2 <- cbind(class = str_remove(rownames(F1_models), pattern = "Class: "), as_tibble(F1_models)) %>% 
  pivot_longer(cols = -"class", names_to = c("algorithm"), values_to = "value") 

p6 <- df2 %>%  arrange(class) %>%
  ggplot(aes(algorithm, value, fill = class)) +
  geom_col(position = "dodge") +
  scale_x_discrete(name = "F1-value") +
  scale_y_continuous(name = "value") +
  theme(axis.title.y = element_text(color = "grey"),
        axis.title.y.right = element_text(color = "blue")) +
  theme_few() 
p6 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + coord_flip() 


# final hold out test on validation sample with neural network
# 
y_hat_nnet_v <- predict(model_nnet, newdata = validation)
cfm_nnet_v <- confusionMatrix(y_hat_nnet_v, validation$C_risk, dnn = c("Prediction", "Reference"))
cfm_nnet_v
confusion_matrix <- as.data.frame(table(pred = y_hat_nnet_v, ref = validation$C_risk))


p5 <- ggplot(data = confusion_matrix, 
       mapping = aes(x = pred,
                     y = ref)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "light blue",
                      high = "purple") +
  scale_x_discrete(name = "Prediction") +
  scale_y_discrete(name = "Reference") +
  theme_few() 
p5

# train a neural network on the imbalanced set for comparison
set.seed(2, sample.kind = "Rounding")
model_nnet_t <- train(C_risk ~ ., data = train, 
                     method = "nnet",
                     preProcess = c("center", "scale"),
                     trControl = trainControl(method = "cv"))
model_nnet_t
y_hat_nnet_t_v <- predict(model_nnet_t, newdata = validation)
cfm_nnet_t <- confusionMatrix(y_hat_nnet_t_v, validation$C_risk, dnn = c("Prediction", "Reference"))
cfm_nnet_t


result_table <- data.frame(validation, prediction=y_hat_nnet_v)
