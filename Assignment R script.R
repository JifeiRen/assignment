#Set Directory as appropriate
setwd("C:/Users/Ren/OneDrive - University of Warwick/ec349/assigment/yelp_dataset")

#Install package 
install.packages("glmnet")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("tree")
install.packages("rpart")
install.packages("rpart.plot")
install.packages(c("tidytext", "sentimentr"))
install.packages("tm")
install.packages("content mining")
install.packages("dplyr")
install.packages("tm")

#Load Libraries
library(glmnet)
library(ggplot2)
library(tidyverse)
library(tree)
library(rpart)
library(rpart.plot)
library(jsonlite)
library(tidytext)
library(glue)
library(stringr)
library(sentimentr)
library(dplyr)
library(tm)

#Load Different Data
business_data <- stream_in(file("yelp_academic_dataset_business.json")) #note that stream_in reads the json lines (as the files are json lines, not json)
checkin_data  <- stream_in(file("yelp_academic_dataset_checkin.json")) #note that stream_in reads the json lines (as the files are json lines, not json)
tip_data  <- stream_in(file("yelp_academic_dataset_tip.json")) #note that stream_in reads the json lines (as the files are json lines, not json)
load("yelp_review_small.Rda")
load("yelp_user_small.Rda")


#Merge data sets
df <- merge(user_data_small,review_data_small, by = "user_id", all = FALSE)
df2 <- merge(df,business_data, by="business_id", all=FALSE)
df2$stars.x <- factor(df2$stars.x, levels = c(1, 2, 3, 4, 5), labels = c("1", "2", "3", "4", "5"))

#split training data set and test data set
set.seed(1)
n <- nrow(df2)
train_index <- sample(1:n, size = 0.7 * n, replace = FALSE)
training_data <- df2[train_index, ]
test_data <- df2[-train_index, ]

#Plot the tree]
tree1 <- rpart(stars.x ~ review_count.x + useful.x + funny.x + cool.x + fans + average_stars + compliment_hot + compliment_more + 
                 compliment_profile + compliment_cute + compliment_list + compliment_note + compliment_plain + compliment_cool +
                 compliment_funny + compliment_writer + compliment_photos + useful.y + funny.y + cool.y + latitude + longitude +
                 stars.y + review_count.y + is_open
               , data = training_data, method = "class")
rpart.plot(tree1)

#Make stars.x a continuous variable
df2$stars.x<- as.numeric(as.character(df2$stars.x))

#split training data set and test data set
set.seed(1)
n <- nrow(df2)
train_index <- sample(1:n, size = 0.7 * n, replace = FALSE)
training_data <- df2[train_index, ]
test_data <- df2[-train_index, ]

#Plot the tree]
tree1 <- rpart(stars.x ~ review_count.x + useful.x + funny.x + cool.x + fans + average_stars + compliment_hot + compliment_more + 
                 compliment_profile + compliment_cute + compliment_list + compliment_note + compliment_plain + compliment_cool +
                 compliment_funny + compliment_writer + compliment_photos + useful.y + funny.y + cool.y + latitude + longitude +
                 stars.y + review_count.y + is_open
               , data = training_data)
rpart.plot(tree1)

#Make prediction
predictions<-predict(tree1,newdata = test_data)
predictions <- factor(predictions)
conf_matrix <- table(rounded_predictions, test_data$stars.x)
print(conf_matrix)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", accuracy))

predictions<-predict(tree1,newdata = training_data)
rounded_predictions <- round(predictions)
conf_matrix <- table(rounded_predictions, training_data$stars.x)
print(conf_matrix)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", accuracy))

# Sentiment Analysis
dict <- lexicon::hash_sentiment_jockers_rinker
sentiment_score<-sentiment(df2$text)
sum_sentiment<-sentiment_score %>%
  group_by(element_id) %>%
  summarise(sentiment = sum(sentiment))
df2<-df2 %>%
  mutate(element_id = row_number())
DATA<-merge(df2,sum_sentiment,by="element_id",all=TRUE)

#splite the new data
training_data2 <- DATA[train_index, ]
test_data2 <- DATA[-train_index, ]


#Plot the tree]
tree2 <- rpart(stars.x ~ review_count.x + useful.x + funny.x + cool.x + fans + average_stars + compliment_hot + compliment_more + 
                 compliment_profile + compliment_cute + compliment_list + compliment_note + compliment_plain + compliment_cool +
                 compliment_funny + compliment_writer + compliment_photos + useful.y + funny.y + cool.y+stars.y + review_count.y + is_open+sentiment
               , data = training_data2)
rpart.plot(tree2)

#Make prediction
predictions2<-predict(tree2,newdata = training_data2)
rounded_predictions2 <- round(predictions2)
conf_matrix <- table(rounded_predictions2, training_data2$stars.x)
print(conf_matrix)
accuracy2 <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", accuracy2))

predictions2<-predict(tree2,newdata = test_data2)
rounded_predictions2 <- round(predictions)
conf_matrix <- table(rounded_predictions2, test_data2$stars.x)
print(conf_matrix)
accuracy2 <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", accuracy2))





