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
# days  : the transit time, the time between pack date and date of discharge
# CR    : the percentage of crown rot (mild)
# CRC   : the percentage of crown rot (heavy)
# bio   : dummy variable indicating the consignment is organic
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



# join the quality data to the weather data to have a list of qulaity data with ADDs for every shipment.
data_PE <- data %>% left_join(summ_add_PE, by = c('pd' = 'date')) 

#************************************************************************************
#                             EXPLORATORY DATA ANLYSIS
#*********************************************************************************

#make a plot of average CR incidence per despatch month
data %>% mutate(week = strftime(dd, format = "%V"), month = strftime(dd, format = "%m")) %>% 
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
  geom_point(data=data, aes(x=pd, y=C*2000), color='purple', alpha = 0.5)+
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
data_hm_PE <- data_PE %>% mutate(day_bins = cut(days, breaks = seq(10, 70, 10), include.lowest = FALSE, label = lbl_days), 
         add_bins = cut(add, breaks = seq(400,1200, 100), include.lowest = FALSE, label = lbl_add)) %>%
  group_by(day_bins, add_bins, ca) %>%
  summarize(CR_avg = mean(C), n = n())

#make geom_tile plot
p2 <- data_hm_PE %>%
  ggplot(aes(x = day_bins, y = add_bins)) +
  geom_tile(aes(fill = CR_avg*100)) + 
  scale_fill_gradient2(low = "white", high = "blue", 
                       midpoint = 1, 
                       breaks = seq(0, 20, 5), 
                       limits = c(0, 20)) +
  labs(title="crownrot versus ADD and transit time",
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
data_PE %>% select(C, add, grow_temp, days, ca) %>%
  drop_na() %>% corstars(.) 

#******************************************************************************
#*                  PREDICTING CROWN ROT WITH VARIOUS ML ALGORITHMS
#******************************************************************************




#Create a categorical variable for CR risk"(low/medium/high:
summary(data_PE$C)

data_ML <- data_PE %>% select(ca, days, C, add, pd) %>% 
  mutate(C_risk = cut(C, breaks = c(0,0.01,0.05,1), 
                      include.lowest = TRUE,
                      labels = c("low", "medium", "high")),
         ca = as.factor(ca)) %>%
  select(-C)


#make a descriptive statistics table
library(skimr)
skimmed <- skim(data_ML)
skimmed

#check for near-zero-variance predictors because dataset is unbalanced

nzv <- nearZeroVar(data_ML, saveMetrics= TRUE)
formattable(nzv)


#splitting the data in a train and a test set and a hold-out set for validation
validation <- data_ML %>% filter(pd >= "2021-01-01")
traintest <- data_ML %>% filter(pd < "2021-01-01") %>% drop_na()

#create a data partition
set.seed(3456)
trainIndex <- createDataPartition(traintest$C_risk, p = .5, 
                                  list = FALSE, 
                                  times = 1)

train <- traintest[trainIndex,]
test <- traintest[-trainIndex,]

# try a boosted tree model
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)


set.seed(825)
gbmFit1 <- train(C_risk ~ ., data = train, 
                 method = "gbm",
                 preProcess = c("center", "scale"),
                 trControl = fitControl,
                 verbose = FALSE)
gbmFit1

y_hat <- predict(gbmFit1, newdata = test)
confusionMatrix(y_hat, test$C_risk, dnn = c("Prediction", "Reference"))


gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)
gbmFit2 <- train(C_risk ~ ., data = train, 
                 method = "gbm",
                 preProcess = c("center", "scale"),
                 tuneGrid = gbmGrid,
                 verbose = FALSE)
gbmFit2
y_hat <- predict(gbmFit2, newdata = test)
confusionMatrix(y_hat, test$C_risk, dnn = c("Prediction", "Reference"))



## now using ROSE to create a more balanced dataset
library(ROSE)

genData_2 = SMOTE(train[,-4],train[,4],K=3)
table(newData$Species)

###################################
####################################
##################################



# try a k-nearest neighbour model
knnFit1 <- train(C_risk ~ ., data = train, 
                 method = "knn",
                 preProcess = c("center", "scale"),
                 tuneLength = 10,
                 trControl = trainControl(method = "cv"))

y_hat <- predict(knnFit1, newdata = test)
confusionMatrix(y_hat, test$C_risk, dnn = c("Prediction", "Reference"))



#data preprocessing (centering and scaling)
preProcess(data_ML, method=c('center','scale'))


# do a normal linear regression
library(broom)
model3 <- lm(C ~ add + rfs + days + ca, data = data_PE)
tidy(model3)




summary(data$C)
#do a fractional response regression
model4 <- glm(C ~ add + rfs + days + ca, 
              family = quasipoisson(link = "log"),
              data = data_PE)
summary(model4)
library(lmtest)
library(sandwich)

se_glm_robust = coeftest(model4, vcov = vcovHC(model4, type="HC"))
se_glm_robust

# make a plot of average weekly temperature since start of measurements
tw <- summ_PE %>% mutate(wk = week(date)) %>% group_by(wk) %>%
  summarize(temp_wk = mean(mean_temp, na.rm=TRUE))
plot(tw$wk, tw$temp_wk)


#make a plot of crown rot versus ADD by CA
data_PE %>% filter(C > 0.01) %>% ggplot(aes(add, C, col=ca)) + geom_point()

#make a plot of crown rot versus RFS by CA
data_PE %>% filter(C > 0.01) %>% ggplot(aes(rfs, C, col=ca)) + geom_point()

#make a boxplot of crown rot by CA when add >1000 (below not much difference)
data_PE %>% filter(add > 1000) %>% ggplot(aes(C, col=ca)) + geom_boxplot()

#make a heatmap of add, days and C
#first create bins

data_PE$days <- as.integer(data$days)
data_PE$add <- as.integer(data$add)

lbl_days <- c("11-15", "16-20", "21-25", "26-30", "31-35", "36-40", "41-45", "46-50", "51-55", "56-60", "61-65", "66-70")

lbl_add <- c("401-450", "451-500", "501-550", "551-600", "601-650",
             "651-700", "701-750","751-800", "801-850", "851-900","901-950",
             "951-1000", "1001-1050", "1051-1100", "1101-1150", "1151-1200")

lbl_days <- c("11-20", "21-30", "31-40", "41-50", "51-60", "61-70")

lbl_add <- c("401-500", "501-600", "601-700", "701-800", "801-900",
             "901-1000", "1001-1100","1101-1200")


data_hm_PE <- data %>% select(add, days, C, ca, lot) %>%
  mutate(day_bins = cut(days, breaks = seq(10, 70, 10), include.lowest = FALSE, label = lbl_days), 
                add_bins = cut(add, breaks = seq(400,1200, 100), include.lowest = FALSE, label = lbl_add)) %>%
  group_by(day_bins, add_bins, ca) %>%
  summarize(CR_avg = mean(C), n = n())

summary(data_hm_PE$CR_avg)

p2 <- data_hm_PE %>%
  ggplot(aes(x = day_bins, y = add_bins)) +
  geom_tile(aes(fill = CR_avg)) + 
  scale_fill_gradient2(low = "white", high = "blue", 
                       midpoint = 0.1, 
                       breaks = seq(0, 0.2, 0.05), 
                       limits = c(0, 0.2)) +
  labs(title="crownrot Peru by temperature sum and transit ~ CA",
       x ="transit time (days)", y = "temperature sum (degree.days)") +
  labs(fill = "Avg crown rot ratio") +
  geom_text(aes(label = n), size = 2) +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=0, hjust=1)) + 
  facet_grid(. ~ ca)
p2
 
#Kruskall Wallis test for difference of the means
data_PE %>% filter(add > 1000) %>%
group_by(ca) %>%
  summarise(
    count = n(),
    mean = mean(C, na.rm = TRUE),
    sd = sd(C, na.rm = TRUE),
    median = median(C, na.rm = TRUE),
    IQR = IQR(C, na.rm = TRUE)) 

data_PE %>% filter(add > 1000) %>% kruskal.test(C ~ ca, data = .)
            
model <- lm(C ~ add + rfs + days + ca + supplier, data = data_PE)
summary(model)

plot(data_PE$pd, data_PE$rfs)
res <- data_PE %>% mutate(ca = as.numeric(ca)) %>% 
  select(-pd, -supplier, -cdd, -CR, -CRC, -lot, -med_temp, -mean_temp, 
         -grow_temp, -crf, -mean_rainfall, -mean_humidity) %>% 
  cor(., use = "complete.obs")
res

corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
library("PerformanceAnalytics")
data_PE %>% mutate(ca = as.numeric(ca)) %>% 
  select(-pd, -supplier, -cdd, -CR, -CRC, -lot, -med_temp, -mean_temp, 
         -grow_temp, -crf, -mean_rainfall, -mean_humidity) %>% 
  chart.Correlation(., histogram=TRUE, pch=19)





#************************************************************************************
#*                                            ECUADOR
#************************************************************************************
write_excel_csv(W84370, "W84370.xls") #Tumbes, while Santa Rosa is not available. Rainfall
# in Tumbes is lower (178 versus 418 mm annually).
getwd()
#*******************************************************************************
#read the weather from meteostat:84370
url <- "https://bulk.meteostat.net/hourly/84370.csv.gz"
tempdir()
tmp <- tempfile()
curl_download(url, tmp)
download.file(url, destfile = "84370")




sapply(84370, read_weather)
W84370 <- read.csv("W84370.wd")
names(W84370) <- vars


#* read the weather data of Tumbes from disk 

W_EC <- W84370 %>% mutate(wk = strftime(date, format = "%V"), 
                          wk_yr =strftime(date, format = "%V-%Y"),
                          week = week(date),
                          week_date = as.Date(cut(as.Date(date), "week")),
                          date = as.Date(date))

#create a function for calculation of temperature sum
# define the time span for the accumulated degree days
span <- 10 #weeks

# make a dataframe with mean and median weather data per day
summ_EC <- W_EC %>% 
  group_by(date) %>%
  summarise(med_temp = median(temp, na.rm = TRUE),
            mean_temp = mean(temp, na.rm = TRUE), # you can also calc mean if you want
            mean_humidity = mean(rhum, na.rm = TRUE),
            sum_rainfall = sum(prcp, na.rm = TRUE))



# graph of mean temperature against date
summ_EC %>% filter(date > "2015-01-01") %>%
  ggplot(aes(date, mean_temp)) +
  geom_line()

# graph of rainfall against date : only available after 01-01-2021
summ_EC %>% filter(date > "2015-01-01") %>%
  ggplot(aes(date, sum_rainfall)) +
  geom_line()


#formula to calculate the cumulative sums based on span and cut-off of 13.5
summ_add_EC <- summ_EC %>% filter(date >=  (as.Date("2015-01-01")-span*7)) %>% 
  mutate(grow_temp = (mean_temp - 13.5), cdd = cumsum(grow_temp)) %>%
  mutate(add = cdd - lag(cdd, span*7)) %>%
  mutate(crf = cumsum(sum_rainfall)) %>% #calculate cumulative rainfall
  mutate(rfs = crf - lag(crf, span*7)) #calculate rainfallsum


# taking out NAs from data
d5 <- summ_add_EC %>% na.exclude() 

#plot accumulated degree days against date
d5 %>%
  ggplot(aes(date, add)) +
  geom_line() + 
  ylab('Accumulated Degree-Days')+ xlab('date') +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=0, hjust=1)) 

# make a dataframe with summed crown rot incidence by pack date
#d6 <- df_cl %>% filter(origin == 'Peru') %>%
#  select(pd, CR, CRC, supplier, days, ca, lot) %>%
#  mutate(supplier = as.factor(supplier), ca = as.factor(ca)) %>%
#  group_by(pd) %>%
#  summarize(C = sum(CR+CRC), supplier = supplier, days = days, ca = ca, lot=lot)

d6 <- df_cl %>% filter(origin == 'Ecuador' & bio == 1) %>%
  select(pd, CR, CRC, supplier, days, ca, bio, lot) %>%
  mutate(supplier = as.factor(supplier), ca = as.factor(ca), C = CR+2*CRC) 


# overlay add, cumulative rainfall and crown rot: rainfall makes no sense as data only from 01-01-2021
ggplot() + 
  geom_line(data=d5, aes(x=date, y=add), color='green') + 
  geom_smooth(data=d6, aes(x=pd, y=C*1E4), color='red',  method = 'loess', span = 0.05)+
  scale_x_date(name = "date", labels = date) +
  scale_y_continuous(name = "temperature sum °C.d (13.5)", 
                     sec.axis = sec_axis(~./1E3, name = "crown rot", 
                                         labels = function(b) { paste0(round(b, 0), "%")})) +  
  theme(axis.title.y = element_text(color = "grey"),
        axis.title.y.right = element_text(color = "blue"))

# join the crown rot data on the weather data to prepare for a linear regression
data_EC <- d6 %>% left_join(d5, by = c('pd' = 'date')) %>% filter(pd > "2015-01-01") %>% ungroup()

# do a normal linear regression
library(broom)
model5 <- data_EC %>% filter(rfs >= 0) %>% do(tidy(lm(C ~ add +  days + ca, data = .)))
model5
summary(model5)



#do a fractional response regression
model6 <- glm(C ~ add + rfs + days + ca, 
              family = gaussian,
              data = data_EC)
summary(model6)
library(lmtest)
library(sandwich)

se_glm_robust = coeftest(model6, vcov = vcovHC(model6, type="HC"))
se_glm_robust

# make a plot of average weekly temperature since start of measurements
tw <- summ_EC %>% mutate(wk = week(date)) %>% group_by(wk) %>%
  summarize(temp_wk = mean(mean_temp, na.rm=TRUE))
plot(tw$wk, tw$temp_wk)


#make a plot of crown rot versus ADD by CA
data_EC %>% filter(C > 0.01) %>% ggplot(aes(add, C, col=ca)) + geom_point()

#make a plot of crown rot versus RFS by CA
data_EC %>% filter(C > 0.01) %>% ggplot(aes(rfs, C, col=ca)) + geom_point()

#make a boxplot of crown rot by CA when add >1000 (below not much difference)
data_EC %>% filter(add > 1000) %>% ggplot(aes(C, col=ca)) + geom_boxplot()

#make a heatmap of add, days and C
#first create bins

data$days <- as.integer(data_EC$days)
data$add <- as.integer(data_EC$add)

lbl_days <- c("11-15", "16-20", "21-25", "26-30", "31-35", "36-40", "41-45", "46-50", "51-55", "56-60", "61-65", "66-70")

lbl_add <- c("401-450", "451-500", "501-550", "551-600", "601-650",
             "651-700", "701-750","751-800", "801-850", "851-900","901-950",
             "951-1000", "1001-1050", "1051-1100", "1101-1150", "1151-1200")

lbl_days <- c("11-20", "21-30", "31-40", "41-50", "51-60", "61-70")

lbl_add <- c("401-500", "501-600", "601-700", "701-800", "801-900",
             "901-1000", "1001-1100","1101-1200")


data_hm_EC <- data_EC %>% select(add, days, C, ca, lot) %>%
  mutate(day_bins = cut(days, breaks = seq(10, 70, 10), include.lowest = FALSE, label = lbl_days), 
         add_bins = cut(add, breaks = seq(400,1200, 100), include.lowest = FALSE, label = lbl_add)) %>%
  group_by(day_bins, add_bins, ca) %>%
  summarize(CR_avg = mean(C), n = n())
summary(data_hm_EC$CR_avg)

p3 <- data_hm_EC %>% drop_na(ca) %>%
  ggplot(aes(x = day_bins, y = add_bins)) +
  geom_tile(aes(fill = CR_avg)) + 
  scale_fill_gradient2(low = "white", high = "purple", 
                       midpoint = 0.025, 
                       breaks = seq(0, 0.15, 0.05), 
                       limits = c(0, 0.2)) +
  labs(title="crown rot EC bio by temperature sum and transit time with and without CA",
       x ="transit time (days)", y = "temperature sum (degree.days)") +
  labs(fill = "Avg crown rot ratio") +
  geom_text(aes(label = n), size = 2) +
  facet_grid(. ~ ca)
p3


data_hm_EC %>% filter(add_bins == c("901-950") & day_bins == c("36-40") & ca == "CA")


#Kruskall Wallis test for difference of the means
data_EC %>% filter(add > 1000) %>%
  group_by(ca) %>%
  summarise(
    count = n(),
    mean = mean(C, na.rm = TRUE),
    sd = sd(C, na.rm = TRUE),
    median = median(C, na.rm = TRUE),
    IQR = IQR(C, na.rm = TRUE)) 

data_EC %>% filter(add > 1000) %>% kruskal.test(C ~ ca, data = .)



model5










     
