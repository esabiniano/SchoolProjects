#### STAT 208 PROJECT #####

library(data.table)
library(caret)
library(MASS)
library(class)
library(e1071)
library(rpart)
library(rpart.plot)
library(rattle)
library(stringr)
library(Hmisc)
library(gmodels)

# set location
proj.loc <- 'C:/UCR/STAT 208/Project'

# load data
setwd(proj.loc)
db_proj <- fread('drug_consumption.data')
nscore <- fread('NScoreMap.csv') # neuroticism
escore <- fread('EScoreMap.csv') # extravertedness
oscore <- fread('OScoreMap.csv') # openness to experience
ascore <- fread('AScoreMap.csv') # agreeableness
cscore <- fread('cscoremap.csv') # conscentiousness

db_names <- c('ID', 'age', 'gender', 'education', 'country', 'ethnicity'
              , 'neuroticism', 'extraversion', 'openness', 'agreeableness'
              , 'conscientiousness', 'impulsiveness', 'sensation'
              , 'alcohol', 'amphet', 'amyl', 'benzos', 'caff', 'cannabis'
              , 'choc', 'coke', 'crack', 'ecstasy', 'heroin', 'ketamine'
              , 'legalh', 'lsd', 'meth', 'mushroom', 'nicotine', 'semer'
              , 'vsa')

setnames(db_proj, names(db_proj), db_names)

# convert to binary classification on a yearly basis per doc desc: user/non-user
drug_lst <- db_names[14:32]

db_proj[, 14:32 := lapply(.SD, function(x) ifelse(x %in% c('CL0', 'CL1')
                                             , 'Non-User', 'User'))
        , .SDcols = 14:32]

# only focusing on methadone, a type of opioid
db_methadone <- db_proj[, c(1:13, 28)]
db_methadone[, methadone_user := ifelse(meth == 'User', 1, 0)]

# Let's look at the personality classes for each of the user/non-user group ----
# create a copy of the non-transformed data
db_meth_graph <- copy(db_methadone)

# demographics ----
#age
db_meth_graph[age==-0.95197, age_grp := '18-24']
db_meth_graph[age==-0.07854, age_grp := '25-34']
db_meth_graph[age==0.49788, age_grp := '35-44']
db_meth_graph[age==1.09449, age_grp := '45-54']
db_meth_graph[age==1.82213, age_grp := '55-64']
db_meth_graph[age==2.59171, age_grp := '65+']

# gender
db_meth_graph[gender == 0.48246, gender_grp := 'Female']
db_meth_graph[gender == -0.48246, gender_grp := 'Male']

# education
db_meth_graph[education == -2.43591
              , edu_grp := 'Left school before 16 years']
db_meth_graph[education == -1.73790
              , edu_grp := ' Left school at 16 years']
db_meth_graph[education == -1.43719
              , edu_grp := 'Left school at 17 years']
db_meth_graph[education == -1.22751
              , edu_grp := 'Left school at 18 years']
db_meth_graph[education == -0.61113 
              , edu_grp := 'Some college or university, no certificate or degree']
db_meth_graph[education == -0.05921 
              , edu_grp := 'Professional certificate/ diploma']
db_meth_graph[education == 0.45468 
              , edu_grp := 'University degree']
db_meth_graph[education == 1.16365 
              , edu_grp := 'Masters degree']
db_meth_graph[education == 1.98437 
              , edu_grp := 'Doctorate degree']

# country
db_meth_graph[country == -0.09765
              , country_grp := 'Australia']
db_meth_graph[country == 0.24923
              , country_grp := 'Canada']
db_meth_graph[country == -0.46841
              , country_grp := 'New Zealand']
db_meth_graph[country == -0.28519
              , country_grp := 'Other']
db_meth_graph[country == 0.21128
              , country_grp := 'Republic of Ireland']
db_meth_graph[country == 0.96082
              , country_grp := 'UK']
db_meth_graph[country == -0.57009
              , country_grp := 'USA']

# ethnicity
db_meth_graph[ethnicity == -0.50212, ethnic_grp := 'Asian']
db_meth_graph[ethnicity == -1.10702, ethnic_grp := 'Black']
db_meth_graph[ethnicity == 1.90725, ethnic_grp := 'Mixed-Black/Asian']
db_meth_graph[ethnicity == 0.12600, ethnic_grp := 'Mixed-White/Asian']
db_meth_graph[ethnicity == -0.22166, ethnic_grp := 'Mixed-White/Black']
db_meth_graph[ethnicity == 0.11440, ethnic_grp := 'Other']
db_meth_graph[ethnicity == -0.31685, ethnic_grp := 'White']

plot_age <-
ggplot(db_meth_graph, aes(age_grp, ..count../sum(..count..))) + 
  geom_bar(aes(fill = meth), position = "dodge") + 
  scale_y_continuous(labels = scales::percent) +
  labs( #title = "Age Group Distribution Across Methadone Users and Non-Users",
        y = "Percent", x = "Age Group"
       , fill = 'Methadone User Classification') +
  theme(panel.grid.major = element_line(), 
        panel.grid.minor = element_line(),
        panel.background = element_rect(colour = "black", size=1)
        , legend.position = 'none')
# plot_age
# ggsave('Age_Demographics_Methadone.png')

plot_gender <-
ggplot(db_meth_graph, aes(gender_grp, ..count../sum(..count..))) + 
  geom_bar(aes(fill = meth), position = "dodge") + 
  scale_y_continuous(labels = scales::percent) +
  labs(#title = "Gender Distribution Across Methadone Users and Non-Users",
        y = "Percent", x = "Gender"
       , fill = 'Methadone User Classification') +
  theme(panel.grid.major = element_line(), 
        panel.grid.minor = element_line(),
        panel.background = element_rect(colour = "black", size=1)
        , legend.position = 'none')

plot_edu <-
ggplot(db_meth_graph, aes(edu_grp, ..count../sum(..count..))) + 
  geom_bar(aes(fill = meth), position = "dodge") + 
  scale_y_continuous(labels = scales::percent) +
  labs(#title = "Education Level Distribution Across Methadone Users and Non-Users",
        y = "Percent", x = "Level of Education"
       , fill = 'Methadone User Classification')+
  coord_flip() +
  theme(panel.grid.major = element_line(), 
        panel.grid.minor = element_line(),
        panel.background = element_rect(colour = "black", size=1)
        )

plot_country <-
ggplot(db_meth_graph, aes(country_grp, ..count../sum(..count..))) + 
  geom_bar(aes(fill = meth), position = "dodge") + 
  scale_y_continuous(labels = scales::percent) +
  labs(#title = "Country Distribution Across Methadone Users and Non-Users",
        y = "Percent", x = "Country"
       , fill = 'Methadone User Classification') +
  theme(panel.grid.major = element_line(), 
        panel.grid.minor = element_line(),
        panel.background = element_rect(colour = "black", size=1)
        , legend.position = 'none'
        , axis.text.x = element_text(angle=90, vjust=0.5))

plot_eth <-
ggplot(db_meth_graph, aes(ethnic_grp, ..count../sum(..count..))) + 
  geom_bar(aes(fill = meth), position = "dodge") + 
  scale_y_continuous(labels = scales::percent) +
  labs(#title = "Ethnic Group Distribution Across Methadone Users and Non-Users",
       y = "Percent", x = "Ethnic Group"
       , fill = 'Methadone User Classification') +
  theme(panel.grid.major = element_line(), 
        panel.grid.minor = element_line(),
        panel.background = element_rect(colour = "black", size=1)
        , legend.position = 'none'
        , axis.text.x = element_text(angle=90, vjust=0.5))

require(cowplot)
plot_grid(
  # row 1
  plot_grid(plot_age, plot_gender, nrow = 1, labels = c('A', 'B')) +
    theme(plot.background = element_rect(color = "black")),
  
  # row 2
  plot_grid(plot_country, plot_eth, nrow = 1, labels = c('C', 'D')) +
    theme(plot.background = element_rect(color = "black")), 
  
  # row 2
  plot_grid(plot_edu, nrow = 1, labels = c('E')) +
    theme(plot.background = element_rect(color = "black")),
  
  nrow = 3)

# personality ----
setkey(nscore, Value)
setkey(db_meth_graph, neuroticism)
db_meth_graph <- nscore[, .(Nscore, Value)][db_meth_graph]
setnames(db_meth_graph, 'Value', 't_nscore')

setkey(escore, Value)
setkey(db_meth_graph, extraversion)
db_meth_graph <- escore[, .(Escore, Value)][db_meth_graph]
setnames(db_meth_graph, 'Value', 't_escore')

setkey(oscore, Value)
setkey(db_meth_graph, openness)
db_meth_graph <- oscore[, .(Oscore, Value)][db_meth_graph]
setnames(db_meth_graph, 'Value', 't_oscore')

setkey(ascore, Value)
setkey(db_meth_graph, agreeableness)
db_meth_graph <- ascore[, .(Ascore, Value)][db_meth_graph]
setnames(db_meth_graph, 'Value', 't_ascore')

setkey(cscore, Value)
setkey(db_meth_graph, conscientiousness)
db_meth_graph <- cscore[, .(Cscore, Value)][db_meth_graph]
setnames(db_meth_graph, 'Value', 't_cscore')

db_grp_sum <- db_meth_graph[, .(Nscore = sum(Nscore), Escore = sum(Escore)
                                , Oscore = sum(Oscore), Ascore = sum(Ascore)
                                , Cscore = sum(Cscore))
                            , .(meth)]
db_grp_sum <- db_grp_sum[, `:=`(Neuroticism = Nscore/sum(Nscore)
                                , Extravertedness = Escore/sum(Escore)
                                , Openness = Oscore/sum(Oscore)
                                , Agreeableness = Ascore/sum(Ascore)
                                , Conscientiousness = Cscore/sum(Cscore))]
db_grp_sum[, c('Nscore', 'Escore', 'Oscore', 'Ascore', 'Cscore') := NULL]

db_grp_sum <- melt(db_grp_sum, id.vars = 'meth')

# distribution
plot_per1 <-
ggplot(db_grp_sum, aes(variable, value)) +   
  geom_bar(aes(fill = meth), position = "dodge", stat="identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Personality Traits"
       , y = "Percent", x = "Personality Traits"
       , fill = 'Methadone User Classification') +
  theme(axis.text.x = element_text(angle=70, vjust=0.5)
        , legend.position = 'none')

db_grp_sum <- db_meth_graph[, .(Impulsiveness = mean(impulsiveness)
                                , Sensation_Seeking = mean(sensation))
                            , .(meth)]

db_grp_sum <- melt(db_grp_sum, id.vars = 'meth')

plot_per2 <-
ggplot(db_grp_sum, aes(variable, value)) +   
  geom_bar(aes(fill = meth), position = "dodge", stat="identity") +
  labs(title = "Impulsiveness and Sensation Seeking"
       , y = "Average Score", x = "Personality Traits"
       , fill = 'Methadone User Classification') +
  theme(
    panel.grid.major = element_line(), 
    panel.grid.minor = element_line(),
    panel.background = element_rect(colour = "black", size=1)
  )

plot_grid(
  # row 1
  plot_grid(plot_per1, plot_per2, nrow = 1, labels = c('A', 'B')) +
    theme(plot.background = element_rect(color = "black")),
  
  nrow = 1)

######### Building Preliminary Models Using Given Standardized Values #########
# Classification attempt 1 - knn ----------------------------------------------
# select only relevant variables, dropping ID and character var meth
knn_train <- db_methadone[1:1320, 2:13] # approximately 70%
knn_test <- db_methadone[1321:1885, 2:13] # approximately 30% 

# get labels
knn_train_labels <- db_methadone[1:1320, methadone_user]
knn_test_labels <- db_methadone[1321:1885, methadone_user]

# fit the knn model

knn_test_pred <- knn(train = knn_train, test = knn_test
                      , cl = knn_train_labels, k = 4)
# check performance - various ways of calculating confusion matrix
checktbl <- CrossTable(knn_test_labels, knn_test_pred, prop.chisq = F)

knn_cm <- confusionMatrix(knn_test_pred, as.factor(knn_test_labels))

# Display confusion matrix
confusion <- checktbl$t
colnames(confusion) <- c("Predicted = 0","Predicted = 1")
rownames(confusion) <- c("True = 0","True = 1")
print(as.table(confusion))

# FPR = FP/(FP+TN)
FPR = confusion[1,2]/sum(confusion[1,1:2])
print(FPR)
# FNR = FN/(FN+TP)
FNR = confusion[2,1]/sum(confusion[2,1:2])
print(FNR)
# OER
Mis_Er = (confusion[1, 2] + confusion[2, 1])/sum(confusion)
print(Mis_Er)
# accuracy
Acc = (confusion[1, 1] + confusion[2, 2])/sum(confusion)
print(Acc)

# k fold cross validation
require(caret)
set.seed(1234)
trControl <- trainControl(method  = "cv", number = 10)
knn_tst_fit <- train(as.factor(methadone_user) ~ .,
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:10),
             trControl  = trControl,
             metric     = "Accuracy",
             data       = cbind(knn_train, methadone_user = knn_train_labels))

knn_test_pred2 <- predict(knn_tst_fit, knn_test)
confusionMatrix(knn_test_pred2, as.factor(knn_test_labels))

# Classification Attempt 2 - Decision Tree ------------------------------------
tree_train <- db_methadone[1:1320, c(2:13,15)]
tree_test <- db_methadone[1321:1885, c(2:13, 15)]
tree_train[, methadone_user := as.factor(methadone_user)]
tree_test[, methadone_user := as.factor(methadone_user)]

fittree <- rpart(as.factor(methadone_user) ~., tree_train, method="class")
rpart.plot(fittree)
summary(fittree)

rpartpred <- predict(fittree, tree_test, type="class")
confusionMatrix(rpartpred,tree_test$methadone_user)

prp(fittree, faclen = 0, cex = 0.8, extra = 1)

# total count
tot_count <- function(x, labs, digits, varlen)
{paste(labs, "\n\nn =", x$frame$n)}
## Decision Tree
prp(fittree, faclen = 0, cex = 0.8, node.fun=tot_count)

# prune the tree
bestcp <- fittree$cptable[which.min(fittree$cptable[,"xerror"]),"CP"]

#Pruning & classification matrix of Pruning
pruned <- prune(fittree, cp = bestcp)
prp(pruned, faclen = 0, cex = 0.8, extra = 1)

predictions <- predict(pruned, tree_test, type="class")
tree_cm <- confusionMatrix(predictions,tree_test$methadone_user)

trControl <- trainControl(method  = "cv", number = 10)
dt_tst_fit <- train(as.factor(methadone_user) ~ .,
             method     = "rpart",
             trControl  = trControl,
             metric     = "Accuracy",
             data       = tree_train)
dt_test_pred2 <- predict(dt_tst_fit, tree_test)
confusionMatrix(dt_test_pred2, tree_test$methadone_user)


# Classification Attemp 3 - Naive Bayes ---------------------------------------
nb_train <- db_methadone[1:1320, c(2:13, 15)] # approximately 70%
nb_test <- db_methadone[1321:1885, c(2:13, 15)] # approximately 30% 
nb_test[, methadone_user := as.factor(methadone_user)]

nb_fit1 <- naiveBayes(as.factor(methadone_user) ~., data = nb_train)
nb_pred1 <- predict(nb_fit, nb_test[,-13])
nb1 <- confusionMatrix(nb_pred, nb_test$methadone_user)

trControl <- trainControl(method  = "cv", number = 10)
nb_tst_fit <- train(as.factor(methadone_user) ~ .,
             method     = "nb",
             trControl  = trControl,
             metric     = "Accuracy",
             data       = nb_train)


nb_test_pred2 <- predict(nb_tst_fit, nb_test)
confusionMatrix(nb_test_pred2, nb_test$methadone_user)

plot_nb_db <- cbind(nb_test, pred_val = nb_test_pred2)

plot_nb <-
ggplot(plot_nb_db, aes(x = neuroticism, y = conscientiousness)) +
  theme_classic() +
  geom_point(
    mapping = aes(colour = methadone_user),
    shape = 1,
    size = 3,
    stroke = 2,
    alpha = 5 / 6
  ) +
  geom_point(
    mapping = aes(fill = pred_val, colour = NA),
    size = 2.88,
    alpha = 5 /6,
    shape = 21
  ) +
  labs(title = "Methadone User Classification Prediction from Naive Bayes Model"
       , subtitle = "Limited to Neuroticism and Conscientiousness Scores for Graphical Display"
       , y = "Conscientiousness Score (scaled)", x = "Neuroticism Score (scaled)"
       , fill = c('Predicted Methadone\nUser Classification'
                  , 'Actual Methadone User Classification')) +
  scale_fill_manual(aesthetics = c('Predicted Methadone\nUser Classification'
                      , 'Actual Methadone User Classification')) +
  theme(
    panel.grid.major = element_line(), 
    panel.grid.minor = element_line(),
    panel.background = element_rect(colour = "black", size=1)
  )

# Classification Attemp 4 - Logistic Model ------------------------------------
log_train <- db_methadone[1:1320, c(2:13, 15)] # approximately 70%
log_test <- db_methadone[1321:1885, c(2:13, 15)] # approximately 30% 
log_test[, methadone_user := as.factor(methadone_user)]

logitmodel <- glm(as.factor(methadone_user)~., data = log_train
                  , family="binomial")
print(summary(logitmodel))

logit_pred <- predict(logitmodel, type = 'response', newdata = log_test)
logit_confusion <- table(log_test$methadone_user, logit_pred > 0.3)
colnames(logit_confusion) <- c("Predicted = 0","Predicted = 1")
rownames(logit_confusion) <- c("True = 0","True = 1")
print(as.table(logit_confusion))

# FPR = FP/(FP+TN)
FPR = logit_confusion[1,2]/sum(logit_confusion[1,1:2])
print(FPR)
# FNR = FN/(FN+TP)
FNR = logit_confusion[2,1]/sum(logit_confusion[2,1:2])
print(FNR)

# OER
logit_Mis_Er = (logit_confusion[1, 2] + logit_confusion[2, 1])/sum(logit_confusion)
print(Mis_Er)
# accuracy
logit_Acc = (logit_confusion[1, 1] + logit_confusion[2, 2])/sum(logit_confusion)
print(Acc)

trControl <- trainControl(method  = "cv", number = 10)
lr_tst_fit <- train(as.factor(methadone_user) ~ .,
                    method     = "bayesglm",
                    trControl  = trControl,
                    metric     = "Accuracy",
                    data       = log_train)

log_test_pred2 <- predict(lr_tst_fit, log_test)
confusionMatrix(log_test_pred2, log_test$methadone_user)

# Classification Attemp 5 - SVM -----------------------------------------------
svm_train <- db_methadone[1:1320, c(2:13, 15)] # approximately 70%
svm_test <- db_methadone[1321:1885, c(2:13, 15)] # approximately 30% 
svm_test[, methadone_user := as.factor(methadone_user)]

tc = tune.control(cross = 10)
svm_model <- tune.svm(as.factor(methadone_user)~., data=svm_train
                 , kernel="polynomial", scale=F, gamma=1, coef0=1
                 , cost=1, tunecontrol = tc)
bhat<-c(-svm_model$best.model$rho,t(svm_model$best.model$coefs)%*%svm_model$best.model$SV)
print(bhat)

svm_pred <- predict(svm_model$best.model, svm_test)

svm_poly <- confusionMatrix(svm_pred, svm_test$methadone_user)

svm_model <- tune.svm(as.factor(methadone_user)~., data=svm_train
                 , kernel="radial", scale=F, gamma=1, coef0=1, cost=1
                 , tunecontrol = tc)
bhat<-c(-svm_model$best.model$rho,t(svm_model$best.model$coefs)%*%svm_model$best.model$SV)
print(bhat)

svm_pred <- predict(svm_model$best.model, svm_test)

svm_radial <- confusionMatrix(svm_pred, svm_test$methadone_user)


############# End of Preliminary Models Using Standardized Values #############
# Dummy variable transformation for categorical variables ---------------------
db_meth_dum <- db_meth_graph[, .(methadone_user
                                 # categorical variables to be dummified
                                 , age_grp, gender_grp, edu_grp, country_grp
                                 , ethnic_grp
                                 # scaled personality/behavior score variables
                                 , Neuroticism = t_nscore
                                 , Extravertedness = t_escore
                                 , Openness = t_oscore
                                 , Agreeableness = t_ascore
                                 , Concientiousness = t_cscore
                                 , Impulsiveness = impulsiveness
                                 , Sensation = sensation)]
# create dummies for the categories, one column each category
db_meth_dum <-fastDummies::dummy_cols(db_meth_dum
                                      , select_columns = c('age_grp'
                                                           , 'gender_grp'
                                                           , 'edu_grp'
                                                           , 'country_grp'
                                                           , 'ethnic_grp')) 
# drop the categorical variables and keep respective dummy versions
db_meth_dum[, c('age_grp', 'gender_grp', 'edu_grp', 'country_grp'
                , 'ethnic_grp') := NULL]
######### Building Preliminary Models Using Given Standardized Values #########
partition <- createDataPartition(db_methadone$methadone_user, p =.7, list = F)
# Classification attempt 1 - knn ----------------------------------------------
# select only relevant variables, dropping ID and character var meth
knn_train <- db_meth_dum[partition, 2:39] # approximately 70%
knn_test <- db_meth_dum[-partition, 2:39] # approximately 30% 

# get labels
knn_train_labels <- db_meth_dum[partition, methadone_user]
knn_test_labels <- db_meth_dum[-partition, methadone_user]

# fit the knn model

knn_test_pred <- knn(train = knn_train, test = knn_test
                     , cl = knn_train_labels, k = 4)
# check performance - various ways of calculating confusion matrix
knn_cm <- confusionMatrix(knn_test_pred, as.factor(knn_test_labels))

# k fold cross validation
set.seed(1234)
trControl <- trainControl(method  = "cv", number = 10)
knn_tst_fit <- train(as.factor(methadone_user) ~ .,
                     method     = "knn",
                     tuneGrid   = expand.grid(k = 1:10),
                     trControl  = trControl,
                     metric     = "Accuracy",
                     data       = cbind(knn_train, methadone_user = knn_train_labels))

knn_test_pred2 <- predict(knn_tst_fit, knn_test)
confusionMatrix(knn_test_pred2, as.factor(knn_test_labels))

# Classification Attempt 2 - Decision Tree ------------------------------------
colnames(db_meth_dum) <- make.names(colnames(db_meth_dum))
tree_train <- db_meth_dum[partition]
tree_test <- db_meth_dum[-partition]
tree_train[, methadone_user := as.factor(methadone_user)]
tree_test[, methadone_user := as.factor(methadone_user)]

# clean names for the punctuations
setnames(tree_train, names(tree_train)
         , stringr::str_replace_all(names(tree_train), '[\ / - +]', ''))
setnames(tree_test, names(tree_test)
         , stringr::str_replace_all(names(tree_test), '[\ / - +]', ''))

fittree <- rpart(methadone_user~., tree_train, method="class")
rpart.plot(fittree)
summary(fittree)

rpartpred <- predict(fittree, tree_test, type="class")
confusionMatrix(rpartpred,tree_test$methadone_user)

prp(fittree, faclen = 0, cex = 0.8, extra = 1)

# total count
tot_count <- function(x, labs, digits, varlen)
{paste(labs, "\n\nn =", x$frame$n)}
## Decision Tree
prp(fittree, faclen = 0, cex = 0.8, node.fun=tot_count)

# prune the tree
bestcp <- fittree$cptable[which.min(fittree$cptable[,"xerror"]),"CP"]

#Pruning & classification matrix of Pruning
pruned <- prune(fittree, cp = bestcp)
prp(pruned, faclen = 0, cex = 0.8, extra = 1)

predictions <- predict(pruned, tree_test, type="class")
tree_cm <- confusionMatrix(predictions,tree_test$methadone_user)

trControl <- trainControl(method  = "cv", number = 10)
set.seed(1234)
dt_tst_fit <- train(as.factor(methadone_user) ~ .,
                    method     = "rpart",
                    trControl  = trControl,
                    metric     = "Accuracy",
                    data       = tree_train)
dt_test_pred2 <- predict(dt_tst_fit, tree_test)
confusionMatrix(dt_test_pred2, tree_test$methadone_user)

# plot the results using impulsiveness and sensation seeking
plot_tree_db <- cbind(tree_test, pred_val = dt_test_pred2)

plotsymbol <- ifelse(plot_tree_db$methadone_user==1,"o","x")
plotsymbol2 <- ifelse(plot_tree_db$pred_val==1,"o","x")
plot(plot_tree_db$impulsiveness, plot_tree_db$sensation
     , pch = plotsymbol, cex=1.5, xlab="Impulsiveness Score"
     , ylab="Sensation Score")
points(plot_tree_db$impulsiveness, plot_tree_db$sensation
     , pch = plotsymbol2, cex=1.5, col = 'red')

plot(plot_tree_db$neuroticism, plot_tree_db$conscientiousness
     , pch = plotsymbol, cex=1.5, xlab="Neuroticism Score"
     , ylab="Conscientiousness Score")
points(plot_tree_db$neuroticism, plot_tree_db$conscientiousness
       , pch = plotsymbol2, cex=1.5, col = 'red')

plot_tree <-
ggplot(plot_tree_db, aes(x = neuroticism, y = conscientiousness)) +
  theme_classic() +
  geom_point(
    mapping = aes(colour = methadone_user),
    shape = 1,
    size = 3,
    stroke = 2,
    alpha = 5 / 6
  ) +
  geom_point(
    mapping = aes(fill = pred_val, colour = NA),
    size = 2.88,
    alpha = 5 /6,
    shape = 21
  ) +
  labs(title = "Methadone User Classification Prediction from Decision Tree Model"
       , subtitle = "Limited to Neuroticism and Conscientiousness Scores for Graphical Display"
       , y = "Conscientiousness Score (scaled)", x = "Neuroticism Score (scaled)"
       , fill = c('Predicted Methadone\nUser Classification'
                  , 'Actual Methadone User Classification')) +
  theme(
    panel.grid.major = element_line(), 
    panel.grid.minor = element_line(),
    panel.background = element_rect(colour = "black", size=1)
    # , legend.position = 'none'
  )

plot_grid(
  # row 1
  plot_grid(plot_tree, plot_nb, nrow = 1, labels = c('A', 'B')) +
    theme(plot.background = element_rect(color = "black")),
 
  nrow = 1)

# Classification Attemp 3 - Naive Bayes ---------------------------------------
nb_train <- db_meth_dum[partition] # approximately 70%
nb_test <- db_meth_dum[-partition] # approximately 30% 
nb_test[, methadone_user := as.factor(methadone_user)]
nb_train[, methadone_user := as.factor(methadone_user)]

nb_fit <- naiveBayes(as.factor(methadone_user) ~., data = nb_train)
nb_pred <- predict(nb_fit, nb_test[,-1])
nb <- confusionMatrix(nb_pred, nb_test$methadone_user)
set.seed(1234)
trControl <- trainControl(method  = "cv", number = 10)
nb_tst_fit <- train(x = nb_train[, 2:39], y = nb_train$methadone_user,
                    method     = "nb",
                    trControl  = trControl,
                    metric     = "Accuracy",
                    data       = nb_train)

nb_test_pred2 <- predict(nb_tst_fit, nb_test)
confusionMatrix(nb_test_pred2, nb_test$methadone_user)

# Classification Attemp 4 - Logistic Model ------------------------------------
log_train <- db_meth_dum[partition] # approximately 70%
log_test <- db_meth_dum[-partition] # approximately 30% 
log_test[, methadone_user := as.factor(methadone_user)]

logitmodel <- glm(as.factor(methadone_user)~., data = log_train
                  , family="binomial")
print(summary(logitmodel))

logit_pred <- predict(logitmodel, type = 'response', newdata = log_test)
logit_confusion <- table(log_test$methadone_user, logit_pred > 0.3)
colnames(logit_confusion) <- c("Predicted = 0","Predicted = 1")
rownames(logit_confusion) <- c("True = 0","True = 1")
print(as.table(logit_confusion))

# FPR = FP/(FP+TN)
FPR = logit_confusion[1,2]/sum(logit_confusion[1,1:2])
print(FPR)
# FNR = FN/(FN+TP)
FNR = logit_confusion[2,1]/sum(logit_confusion[2,1:2])
print(FNR)

# OER
logit_Mis_Er = (logit_confusion[1, 2] + logit_confusion[2, 1])/sum(logit_confusion)
print(Mis_Er)
# accuracy
logit_Acc = (logit_confusion[1, 1] + logit_confusion[2, 2])/sum(logit_confusion)
print(Acc)
set.seed(1234)
trControl <- trainControl(method  = "cv", number = 10)
lr_tst_fit <- train(as.factor(methadone_user) ~ .,
                    method     = "bayesglm",
                    trControl  = trControl,
                    metric     = "Accuracy",
                    data       = log_train)

log_test_pred2 <- predict(lr_tst_fit, log_test)
confusionMatrix(log_test_pred2, log_test$methadone_user)

# Classification Attemp 5 - SVM -----------------------------------------------
svm_train <- db_meth_dum[partition] # approximately 70%
svm_test <- db_meth_dum[-partition] # approximately 30% 
svm_test[, methadone_user := as.factor(methadone_user)]

svm_model <- svm(as.factor(methadone_user)~., data=svm_train
                 , kernel="polynomial", scale=F, d=2, gamma=1, coef0=1, cost=1)
bhat<-c(-svm_model$rho,t(svm_model$coefs)%*%svm_model$SV)
print(bhat)

svm_pred <- predict(svm_model, svm_test)

svm_poly <- confusionMatrix(svm_pred, svm_test$methadone_user)

svm_model <- svm(as.factor(methadone_user)~., data=svm_train
                 , kernel="radial", scale=F, d=2, gamma=1, coef0=1, cost=1)
bhat<-c(-svm_model$rho,t(svm_model$coefs)%*%svm_model$SV)
print(bhat)

svm_pred <- predict(svm_model, svm_test)

svm_radial <- confusionMatrix(svm_pred, svm_test$methadone_user)
set.seed(1234)
# validation
svm_tst_fit <- train(as.factor(methadone_user) ~ .,
                     method     = "svmPoly",
                     trControl  = trControl,
                     metric     = "Accuracy",
                     data       = svm_train)
svm_test_pred2 <- predict(svm_tst_fit, svm_test)
confusionMatrix(svm_test_pred2, svm_test$methadone_user)

#radial
svm_tst_fit <- train(as.factor(methadone_user) ~ .,
                     method     = "svmRadial",
                     trControl  = trControl,
                     metric     = "Accuracy",
                     data       = svm_train)
svm_test_pred2 <- predict(svm_tst_fit, svm_test)
confusionMatrix(svm_test_pred2, svm_test$methadone_user)

########################### End of Classification #############################
