# read data
train <- read.csv('pml-training.csv', h = T, row.names = 1)
test <- read.csv('pml-testing.csv', h = T, row.names = 1)
# look into data
summary(train)
summary(test)
# select variables
library(dplyr)

train1 <- train %>% 
  select(-kurtosis_roll_belt,-kurtosis_picth_belt,-kurtosis_yaw_belt,-skewness_roll_belt, -skewness_roll_belt.1,
         -skewness_yaw_belt,-max_roll_belt,-max_picth_belt,-max_yaw_belt,-min_roll_belt, -min_pitch_belt,
         -min_yaw_belt,-amplitude_roll_belt,-amplitude_pitch_belt,-amplitude_yaw_belt,-var_total_accel_belt,
         -avg_roll_belt,-stddev_roll_belt,-var_roll_belt,-avg_pitch_belt,-stddev_pitch_belt,-var_pitch_belt,
         -avg_yaw_belt,-stddev_yaw_belt,-var_yaw_belt,-var_accel_arm,-avg_roll_arm,-stddev_roll_arm,
         -var_roll_arm,-avg_pitch_arm,-stddev_pitch_arm,-var_pitch_arm,-avg_yaw_arm,-stddev_yaw_arm,     
         -var_yaw_arm,-kurtosis_roll_arm,-kurtosis_picth_arm,-kurtosis_yaw_arm,-skewness_roll_arm,
         -skewness_pitch_arm,-skewness_yaw_arm,-max_roll_arm,-max_picth_arm,-max_yaw_arm,-min_roll_arm,
         -min_pitch_arm,-min_yaw_arm,-amplitude_roll_arm,-amplitude_pitch_arm,-amplitude_yaw_arm,
         -kurtosis_roll_dumbbell,-kurtosis_picth_dumbbell,-kurtosis_yaw_dumbbell,-skewness_roll_dumbbell,
         -skewness_pitch_dumbbell,-skewness_yaw_dumbbell,-max_roll_dumbbell,-max_picth_dumbbell,
         -max_yaw_dumbbell,-min_roll_dumbbell,-min_pitch_dumbbell,-min_yaw_dumbbell,-amplitude_roll_dumbbell,
         -amplitude_pitch_dumbbell,-amplitude_yaw_dumbbell,-var_accel_dumbbell,-avg_roll_dumbbell,
         -stddev_roll_dumbbell,-var_roll_dumbbell,-avg_pitch_dumbbell,-stddev_pitch_dumbbell,
         -var_pitch_dumbbell,-avg_yaw_dumbbell,-stddev_yaw_dumbbell,-var_yaw_dumbbell,-kurtosis_roll_forearm,
         -kurtosis_picth_forearm,-kurtosis_yaw_forearm,-skewness_roll_forearm,-skewness_pitch_forearm,
         -skewness_yaw_forearm,-max_roll_forearm,-max_picth_forearm,-max_yaw_forearm,-min_roll_forearm,
         -min_pitch_forearm,-min_yaw_forearm,-amplitude_roll_forearm,-amplitude_pitch_forearm,
         -amplitude_yaw_forearm,-var_accel_forearm,-avg_roll_forearm,-stddev_roll_forearm,-var_roll_forearm,
         -avg_pitch_forearm,-stddev_pitch_forearm,-var_pitch_forearm,-avg_yaw_forearm,-stddev_yaw_forearm,
         -var_yaw_forearm,-cvtd_timestamp)

test1 <- test %>% 
  select(-kurtosis_roll_belt,-kurtosis_picth_belt,-kurtosis_yaw_belt,-skewness_roll_belt, -skewness_roll_belt.1,
         -skewness_yaw_belt,-max_roll_belt,-max_picth_belt,-max_yaw_belt,-min_roll_belt, -min_pitch_belt,
         -min_yaw_belt,-amplitude_roll_belt,-amplitude_pitch_belt,-amplitude_yaw_belt,-var_total_accel_belt,
         -avg_roll_belt,-stddev_roll_belt,-var_roll_belt,-avg_pitch_belt,-stddev_pitch_belt,-var_pitch_belt,
         -avg_yaw_belt,-stddev_yaw_belt,-var_yaw_belt,-var_accel_arm,-avg_roll_arm,-stddev_roll_arm,
         -var_roll_arm,-avg_pitch_arm,-stddev_pitch_arm,-var_pitch_arm,-avg_yaw_arm,-stddev_yaw_arm,     
         -var_yaw_arm,-kurtosis_roll_arm,-kurtosis_picth_arm,-kurtosis_yaw_arm,-skewness_roll_arm,
         -skewness_pitch_arm,-skewness_yaw_arm,-max_roll_arm,-max_picth_arm,-max_yaw_arm,-min_roll_arm,
         -min_pitch_arm,-min_yaw_arm,-amplitude_roll_arm,-amplitude_pitch_arm,-amplitude_yaw_arm,
         -kurtosis_roll_dumbbell,-kurtosis_picth_dumbbell,-kurtosis_yaw_dumbbell,-skewness_roll_dumbbell,
         -skewness_pitch_dumbbell,-skewness_yaw_dumbbell,-max_roll_dumbbell,-max_picth_dumbbell,
         -max_yaw_dumbbell,-min_roll_dumbbell,-min_pitch_dumbbell,-min_yaw_dumbbell,-amplitude_roll_dumbbell,
         -amplitude_pitch_dumbbell,-amplitude_yaw_dumbbell,-var_accel_dumbbell,-avg_roll_dumbbell,
         -stddev_roll_dumbbell,-var_roll_dumbbell,-avg_pitch_dumbbell,-stddev_pitch_dumbbell,
         -var_pitch_dumbbell,-avg_yaw_dumbbell,-stddev_yaw_dumbbell,-var_yaw_dumbbell,-kurtosis_roll_forearm,
         -kurtosis_picth_forearm,-kurtosis_yaw_forearm,-skewness_roll_forearm,-skewness_pitch_forearm,
         -skewness_yaw_forearm,-max_roll_forearm,-max_picth_forearm,-max_yaw_forearm,-min_roll_forearm,
         -min_pitch_forearm,-min_yaw_forearm,-amplitude_roll_forearm,-amplitude_pitch_forearm,
         -amplitude_yaw_forearm,-var_accel_forearm,-avg_roll_forearm,-stddev_roll_forearm,-var_roll_forearm,
         -avg_pitch_forearm,-stddev_pitch_forearm,-var_pitch_forearm,-avg_yaw_forearm,-stddev_yaw_forearm,
         -var_yaw_forearm,-cvtd_timestamp)

#check the similarity between the numeric variables
M <- abs(cor(train1[, -c(1,4,58)]))
diag(M) <- 0
xg <- which(M > 0.8, arr.ind = T)
#####################################process the train data#################################################
#PCA
p <- train1[, unique(dimnames(xg)[[1]])]
pr <- princomp(p)
library(psych)
fa.parallel(p, fa = "pc", n.iter = 100,
            show.legend = F, main = "Scree plot with parallel analysis")
#select 6 factors
pc<-principal(p, nfactors = 6, score = T, rotate = "varimax")
s <- pc$score
#change factor to number
user_name <- factor(train1$user_name,labels=c(1:6))
new_window <- factor(train1$new_window, labels=c(1:2))
#standardize the  remain variables
sy <- scale(train1[,  !names(train1) %in% c(unique(dimnames(xg)[[1]]), 'user_name', 'new_window', 'classe')])
#combine all the variables
training <- data.frame(s, user_name = user_name, new_window = new_window, sy, classes = train1$classe)

#####################################process the test data same as train data#######################################

p <- test1[, unique(dimnames(xg)[[1]])]

pc<-principal(p, nfactors = 6, score = T, rotate = "varimax")
s <- pc$score

user_name <- factor(test1$user_name,labels=c(1:6))
new_window <- factor(test1$new_window, labels=c(1))

sy <- scale(test1[,  !names(test1) %in% c(unique(dimnames(xg)[[1]]), 'user_name', 'new_window', 'classe')])

testing <- data.frame(s, user_name = user_name, new_window = new_window, sy, classe = test1$problem_id)
######################################Train Model use Radial SVM method############################################
library(caret)
set.seed(1)
fitControl <- trainControl(method = "repeatedcv",
                           number = 3,
                           repeats = 1,
                           verboseIter=TRUE)
modelFit <- train(classes ~.,data=training, method="svmRadial", trControl = fitControl, verbose=FALSE)

modelFit$results

prediction <- predict(modelFit, testing)

#######################################Submit the answer########################################################
answers <- as.character(prediction)
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(answers)