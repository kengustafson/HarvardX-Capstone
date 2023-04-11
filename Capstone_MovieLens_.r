library(tidyverse)
library(caret)
library(data.table)
library(stringr)
library(dplyr)
library(tidyr)
library(lubridate)
library(tinytex)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

#rm(dl, ratings, movies, test_index, temp, movielens, removed)
val <- validation


## Exploring the Data



# Computes number of unique users and movies
length(unique(edx$userId))
length(unique(edx$movieId))





# Histogram for Number of Reviews

edx %>% ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.25, color = "black",fill="white") +
  scale_x_discrete(limits = c(seq(0.5,5,0.5)))  +
  ggtitle("Rating distribution") +
  labs(title = "Number of Reviews by Rating", x="Rating",y = "Number of Reviews")





# Histogram for Average Movie Rating
meanmovie <- edx %>% group_by(movieId) %>% summarize(meanm = mean(rating))
meanmovie$movieId <- as.integer(meanmovie$movieId)
p1 <- ggplot(meanmovie, aes(x=meanm)) + geom_histogram(binwidth = .25,color = "black", fill = "white") + labs(title="Histogram of Average Movie Ratings",x="Rating", y = "Number of Movies")
p1



meanuser <- edx %>% group_by(userId) %>% summarize(meanu = mean(rating))
meanuser$userId <- as.integer(meanuser$userId)
p2 <- ggplot(meanuser, aes(x=meanu)) + geom_histogram(binwidth = .25,color = "black", fill = "white") + labs(title="Histogram of Average User Ratings",x="Rating", y = "Number of Users")
p2





## Model 1


# Function to Compute RMSE
rmse <- function(realrating, predictedrating){
  dif <- sqrt(mean((realrating - predictedrating)^2))
  return(dif)
}



# Create Test and Validation Set for Model 1
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx1 <- edx[-test_index,]
temp <- edx[test_index,]

validation1 <- temp %>% 
  semi_join(edx1, by = "movieId") %>%
  semi_join(edx1, by = "userId")

removed <- anti_join(temp, validation1)
edx1 <- rbind(edx1, removed)

# Model 1
mu <- mean(edx1$rating)

# Model 1 Cross Validation Statistic
model1cvrmse <- rmse(validation1$rating,mu)
model1cvrmse


# Model 1 Final Validation RMSE

predictedrating <- mu 

val2 <- validation
val2 <- left_join(val2,meanmovie,by="movieId")
val2 <- left_join(val2,meanuser,by="userId")

predictedrating <- mu
RMSE(predictedrating, validation$rating)





## Model 2

# Model 2
meanmovie <- edx1 %>% group_by(movieId) %>% summarize(meanm = mean(rating))
meanmovie$movieId <- as.integer(meanmovie$movieId)

meanmovie$mi <- meanmovie$meanm - mu

val2 <- validation1
val2 <- left_join(val2, meanmovie, by="movieId")


# Model 2 Cross Validation Statistic
model2rmse <- rmse(validation1$rating, val2$meanm)
model2rmse


# Model 2 Final Validation RMSE

predictedrating <- mu + val2$mi

meanmovie <- edx %>% group_by(movieId) %>% summarize(meanm = mean(rating))
meanmovie$movieId <- as.integer(meanmovie$movieId)

meanmovie$mi <- meanmovie$meanm - mu


val2 <- validation
val2 <- left_join(val2,meanmovie,by="movieId")
val2 <- left_join(val2,meanuser,by="userId")

predictedrating <- mu + val2$mi
RMSE(predictedrating, validation$rating)

## Model 3

# Model 3

meanuser <- edx1 %>% group_by(userId) %>% summarize(meanu = mean(rating))
meanuser$userId <- as.integer(meanuser$userId)

meanuser$ui <- meanuser$meanu - mu

val3 <- validation1
val3 <- left_join(val3, meanuser, by="userId")

# Model 3 Cross Validation Statistic
model2rmse <- rmse(validation1$rating, val3$meanu)
model2rmse


# Model 3 Final Validation RMSE

predictedrating <- mu + val3$ui

meanmovie <- edx %>% group_by(movieId) %>% summarize(meanm = mean(rating))
meanmovie$movieId <- as.integer(meanmovie$movieId)

meanuser <- edx %>% left_join(meanmovie, by="movieId") %>% group_by(userId) %>% summarize(ui=mean(rating-mu))
meanuser$userId <- as.integer(meanuser$userId)

val2 <- validation
val2 <- left_join(val2,meanmovie,by="movieId")
val2 <- left_join(val2,meanuser,by="userId")

predictedrating <- mu + val2$ui
RMSE(predictedrating, validation$rating)


## Model 4


# Model 4

meanmovie <- edx1 %>% group_by(movieId) %>% summarize(meanm = mean(rating))
meanmovie$movieId <- as.integer(meanmovie$movieId)

meanmovie$mi <- meanmovie$meanm - mu

meanuser <- edx1 %>% left_join(meanmovie, by="movieId") %>% group_by(userId) %>% summarize(ui=mean(rating-mu-mi))
meanuser$userId <- as.integer(meanuser$userId)

val4 <- validation1
val4 <- left_join(val4,meanmovie,by="movieId")
val4 <- left_join(val4,meanuser,by="userId")


# Model 4 Cross Validation Statistic
predictedrating <- mu + val4$mi + val4$ui
RMSE(predictedrating, validation1$rating)

# Model 4 Final Validation RMSE

predictedrating <- mu + val4$mi + val4$ui

meanmovie <- edx %>% group_by(movieId) %>% summarize(meanm = mean(rating))
meanmovie$movieId <- as.integer(meanmovie$movieId)

meanmovie$mi <- meanmovie$meanm - mu

meanuser <- edx %>% left_join(meanmovie, by="movieId") %>% group_by(userId) %>% summarize(ui=mean(rating-mu-mi))
meanuser$userId <- as.integer(meanuser$userId)

val2 <- validation
val2 <- left_join(val2,meanmovie,by="movieId")
val2 <- left_join(val2,meanuser,by="userId")

predictedrating <- mu + val2$mi + val2$ui
RMSE(predictedrating, validation$rating)



## Model 5

testlambdas <- seq(0,10,1)
testrmse <- sapply(testlambdas, function(x){
  mu <- mean(edx1$rating)
  
  meanmovie <- edx1 %>%
    group_by(movieId) %>%
    summarize(meanmovie = sum(rating - mu)/(n() + x))
  
  meanuser <- edx1 %>%
    left_join(meanmovie, by='movieId') %>% 
    group_by(userId) %>%
    summarize(meanuser = sum(rating - meanmovie - mu)/(n() +x))
  
  predicted_ratings <- validation1 %>%
    left_join(meanmovie, by = "movieId") %>%
    left_join(meanuser, by = "userId") %>%
    mutate(pred = mu + meanmovie +  meanuser) %>% .$pred
  
return(RMSE(predicted_ratings, validation1$rating))
})


optimal_lambda <- testlambdas[which.min(testrmse)]
optimal_lambda
min(testrmse)

# Model 5 Cross Validation
functionrmse <- function(x){
  mu <- mean(edx1$rating)
  
  meanmovie <- edx1 %>%
    group_by(movieId) %>%
    summarize(meanmovie = sum(rating - mu)/(n() + x))
  
  meanuser <- edx1 %>%
    left_join(meanmovie, by='movieId') %>% 
    group_by(userId) %>%
    summarize(meanuser = sum(rating - meanmovie - mu)/(n() +x))
  
  predicted_ratings <- validation1 %>%
    left_join(meanmovie, by = "movieId") %>%
    left_join(meanuser, by = "userId") %>%
    mutate(pred = mu + meanmovie +  meanuser) %>% .$pred
  
return(RMSE(predicted_ratings, validation1$rating))
}

functionrmse(5)

# Model 5 Final Validation RMSE

functionrmse <- function(x){
  mu <- mean(edx$rating)
  
  meanmovie <- edx %>%
    group_by(movieId) %>%
    summarize(meanmovie = sum(rating - mu)/(n() + x))
  
  meanuser <- edx %>%
    left_join(meanmovie, by='movieId') %>% 
    group_by(userId) %>%
    summarize(meanuser = sum(rating - meanmovie - mu)/(n() +x))
  
  predicted_ratings <- validation %>%
    left_join(meanmovie, by = "movieId") %>%
    left_join(meanuser, by = "userId") %>%
    mutate(pred = mu + meanmovie +  meanuser) %>% .$pred
  
return(RMSE(predicted_ratings, validation$rating))
}

functionrmse(5)

