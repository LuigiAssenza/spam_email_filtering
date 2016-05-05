# Simple Algorithm for detecting spam emails
library(tm)
library(plyr)
library(class)
library(caret)

# Setting options stringAsFactors equals to FALSE
options(stringAsFactors = FALSE)

# Creating two folders to classify "ham" and "spam" emails
emails <- c("ham", "spam")

# Folders pathname
pathname <- "C:/Users/Luigi/Desktop/emails"

# Using tm_map function to clean the emails text. Please add any other
# transformation function you require
cleanText <- function(text) {
  email_text <- tm_map(text, removePunctuation)
  email_text <- tm_map(email_text, PlainTextDocument)
  email_text <- tm_map(email_text, stripWhitespace)
  email_text <- tm_map(email_text, removeWords, stopwords("english"))
  return(email_text)
}

# Build a Document Term Matrix (DTM)
createDTM <- function(email, path) {
  email_dir <- sprintf("%s/%s", path, email) # create a directory
  email_corpus <- Corpus(DirSource(directory = email_dir))
  email_clean_corpus <- cleanText(email_corpus)
  email_DTM <- DocumentTermMatrix(email_clean_corpus)
  email_final <- list(name = email, dtm = email_DTM)
}

# Creating Document Term Matrix
dtm <- lapply(emails, createDTM, path = pathname)

# Adding the "EmailType" column (such as ham or spam)
addEmailType <- function(dtm) {
  email_num_matrix <- data.matrix(dtm[["dtm"]])# Converting DMT to numeric matrix
  email_data_frame <- as.data.frame(email_num_matrix, stringsAsFactors = FALSE) # converting numeric matrix to data frame
  email_data_frame <- cbind(email_data_frame, rep(dtm[["name"]], nrow(email_data_frame))) # # adding a last column to indentify the email type ham/spam
  colnames(email_data_frame)[ncol(email_data_frame)] <- "EmailType"
  return(email_data_frame)
}

# Applying singleDoc function to emails
emailDTM <- lapply(dtm, addEmailType)

# Merging the two documents together
dtm_merge <- do.call(rbind.fill, emailDTM)
dtm_merge[is.na(dtm_merge)] <- 0

# Creating training and test data sets
train_index <- sample(1:nrow(dtm_merge),ceiling(nrow(dtm_merge)*0.5))
test_index <- (1:nrow(dtm_merge))[-train_index]
dtm_email_set <- dtm_merge[,!colnames(dtm_merge) %in% "EmailType"]
#dtm_test_set <- dtm_email_target[test_index,]
#dtm_train_set <- dtm_email_target[train_index,]  # creating a train index
dtm_merge_target <- dtm_merge[,"EmailType"] # creating factor of true classifications of training set

# Applying k-Nearest Neighbour Classification
knn.pred <- knn(dtm_email_set[train_index,], dtm_eamil_set[test_index], dtm_merge_target[train_index])

# calculating the accuracy of the model
conf.matrix <- table("Predictions" = knn.pred, "Actuals" = dtm_merge_target[test_index])

conf.matrix

confusionMatrix(conf.matrix) # confusion matrix from the Caret package
