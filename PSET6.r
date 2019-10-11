#rm(list = ls())
#dev.off()
#shell("cls")

library("tidyverse")
library("plotROC")
library("ROSE")

setwd("C:/Users/Elmer/Documents/R/Statistical Modeling/PSET6")

set.seed(1861)
options(scipen = 50)

#a

options(scipen = 50)
set.seed(1861)
movies <- read.csv("data/movie_metadata.csv")
movies <- movies %>% filter(budget < 400000000) %>%
  filter(content_rating != "",
         content_rating != "Not Rated",
         !is.na(gross))
movies <- movies %>%
  mutate(genre_main = unlist(map(strsplit(as.character(movies$genres),"\\|"),1)),
         grossM = gross / 1000000,
         budgetM = budget / 1000000,
         profitM = grossM - budgetM,
         blockbuster = ifelse(grossM > 200,1,0))
movies <- movies %>% mutate(genre_main = fct_lump(genre_main,5),
                            content_rating = fct_lump(content_rating,3),
                            country = fct_lump(country,2),
                            cast_total_facebook_likes000s =
                              cast_total_facebook_likes / 1000,) %>%
  drop_na()
top_director <- movies %>%
  group_by(director_name) %>%
  summarize(num_films = n()) %>%
  top_frac(.1) %>%
  mutate(top_director = 1) %>%
  select(-num_films)
movies <- movies %>%
  left_join(top_director, by = "director_name") %>%
  mutate(top_director = replace_na(top_director,0))
train_idx <- sample(1:nrow(movies),size = floor(0.75*nrow(movies)))
movies_train <- movies %>% slice(train_idx)
movies_test <- movies %>% slice(-train_idx)


#b
summary(movies_train)

mean_train <- mean(movies_train$blockbuster)
mean_test <- mean(movies_test$blockbuster)
mean_difference <- abs(mean_train - mean_test)


p_value <-  t.test(x = movies_train$blockbuster, y = movies_test$blockbuster, mu = mean_difference)

p_value


# The low p-value means we are confident that the observed value did not happen by chance. We reject the null hypotehesis

#c

logit_train <- glm(blockbuster ~ budgetM + top_director + cast_total_facebook_likes000s + content_rating + genre_main,
                   family = binomial,
                   data = movies_train)

# logit_test <- glm(blockbuster ~ budgetM + top_director + cast_total_facebook_likes000s + content_rating + genre_main,
#                    family = binomial,
#                    data = movies_test)
#d
summary(logit_train)

train_exp_coef <- exp(logit_train$coefficients)
train_exp_coef
# 
# test_exp_coef <- exp(logit_test$coefficients)
# test_exp_coef


# d (interpret coeffcs)
# rated r [the exp of r says that movies rated R are 85% less likely to be a 'blockbuster']
exp(-1.918355)


# genremain (the coefficient says adventure movies are 52% more likely to be a 'blockbuster' )
exp(0.419475)

#  top_director (if a movie has a 'top director' it is roughly 83% more likely to be a )
exp(0.607554)

#e
mods_LOOCV <- list()
preds_LOOCV <- NULL

#this for loop takes a minute, do not stop
for(i in 1:nrow(movies_train)){
  mod = glm(blockbuster ~ budgetM + top_director + cast_total_facebook_likes000s + content_rating + genre_main,
           data = movies_train %>% slice(-i),family = binomial)
  preds_LOOCV[i] <- predict(mod, newdata =
                              slice(movies_train,i))
  mods_LOOCV[[i]] <- mod
}

head(preds_LOOCV)

preds_LOOCV_DF <- data.frame(
  scores_LOOCV_train = preds_LOOCV,
  movies_train
)

#f

preds_train_DF <- data.frame(
  scores_train = predict(logit_train, 
                         newdata = movies_train,
                         type = "response"),
  movies_train
)

preds_test_DF <- data.frame(
  scores_test = predict(logit_train, 
                        newdata = movies_test,
                         type = "response"),
  movies_test
)


#g
trainROC <- ggplot(data = preds_train_DF,
                   aes(m = scores_train,
                       d = blockbuster)) +
  geom_roc(labelsize = 3.5,
           cutoffs.at = c(.99,.9,.7,.5,.3,.1,0))

testROC <- ggplot(data = preds_test_DF,
                  aes(m = scores_test,
                      d = blockbuster)) +
  geom_roc(labelsize = 3.5,
           cutoffs.at = c(.99,.9,.7,.5,.3,.1,0))



LOOCV_ROC <- ggplot(data = preds_LOOCV_DF,
                  aes(m = scores_LOOCV_train,
                      d = blockbuster)) +
  geom_roc(labelsize = 3.5,
           cutoffs.at = c(.99,.9,.7,.5,.3,.1,0))

plot(trainROC)
plot(testROC)
plot(LOOCV_ROC)


#g describe the three ROC curves in relatipon
# The three ROC curves show how each model performed in terms of the true to false
# positive tradeoff. Comparing them all at the same cutoff (say 0.1) we can compare each model's relative accuracy 
# in terms of the true and false positive tradeoffs. The training model (graph 1) shows in getting almost 75% of the
# true positives we have an approximately 8% false positive rate. Our test model (graph 2) only gets us about 62% of the
# true positives with roughly the same false positive rate. The LOOCV model at a cutoff of 0.1 indicates that although we
# are predicting close to 0 false positives we are only able to predict roughly 25% of true positives.





#h

calc_auc(trainROC)
calc_auc(LOOCV_ROC)
calc_auc(testROC)
# We suppose the models are ordered the way they are becuase of possible model overfitting from the training data
# or having picked a poor sample for the test set. LOOCV_ROC being the second highest seems weird considering the weird cutoff
# situation before. There may be an error in the code somewhere or something else at play.

#i

rose_down <- ROSE(blockbuster ~budgetM + top_director + cast_total_facebook_likes000s + content_rating + genre_main,
                  data = movies_train,
                  N = 5000, p = 1/2)
rose_up <- ROSE(blockbuster ~budgetM + top_director + cast_total_facebook_likes000s + content_rating + genre_main,
                  data = movies_train,
                  N = 220, p = 1/2)


logit_down <- glm(blockbuster ~ budgetM + top_director + cast_total_facebook_likes000s + content_rating + genre_main,
                  data =rose_down$data,
                  family = "binomial")
summary(logit_down)


logit_up <- glm(blockbuster ~ budgetM + top_director + cast_total_facebook_likes000s + content_rating + genre_main,
                  data =rose_up$data,
                  family = "binomial")
summary(logit_up)


#j

library(tidyverse)
library(magrittr)

J_DF_down <- data.frame(
  scores_J = predict(logit_down,newdata= movies_test,
                     type = "response"), movies_test)
J_DF_up <- data.frame(
  scores_J1 = predict(logit_up,newdata=movies_test,
                      type = "response"), movies_test)
J_DF_c <- data.frame(
  scores_J2 = predict(logit_train,newdata=movies_test,
                      type = "response"), movies_test)

J_DF_down %<>% mutate(class_pred05=ifelse(scores_J>0.5,1,0))
J_DF_up %<>% mutate(class_pred06=ifelse(scores_J1>0.5,1,0))
J_DF_c %<>% mutate(class_pred07=ifelse(scores_J2>0.5,1,0))

table(movies_test$blockbuster,J_DF_down$class_pred05)
table(movies_test$blockbuster,J_DF_up$class_pred06)
table(movies_test$blockbuster,J_DF_c$class_pred07)
