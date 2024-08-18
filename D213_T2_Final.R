install.packages(c('neuralnet', 'keras', 'tensorflow'), dependencies = T)
install.packages('reticulate')

install_tensorflow(
  method = "conda", 
  version = "2.5", 
  conda = "C:/Users/nahur/anaconda3/Scripts/conda.exe", 
  envname = "py3.6",
  conda_python_version = "3.6", 
  extra_packages = c("matplotlib", "numpy", "pandas", "scikit-learn")
)

install_miniconda()

use_condaenv("py3.6", required = TRUE)

library(magrittr)
library(dplyr)
library(tidytext)
library(neuralnet)
library(keras)
library(tensorflow)
library(tidyverse)
library(reticulate)
library(ggplot2)
library(purrr)
library(caret)
library(tm)
library(reshape2)

imdb <- read.csv("C:/Users/nahur/Desktop/WGU/D213 Advanced Data Analytics/Task 2/Datasets/sentiment labelled sentences/imdb_labelled.txt", quote = "", header = F, sep = "\t")
amazon <- read.csv("C:/Users/nahur/Desktop/WGU/D213 Advanced Data Analytics/Task 2/Datasets/sentiment labelled sentences/amazon_cells_labelled.txt", quote = "", header = F, sep = "\t")
yelp <- read.csv("C:/Users/nahur/Desktop/WGU/D213 Advanced Data Analytics/Task 2/Datasets/sentiment labelled sentences/yelp_labelled.txt", quote = "", header = F, sep = "\t")  

imdb <- imdb %>% 
  rename(review = V1, score = V2)

amazon <- amazon %>% 
  rename(review = V1, score = V2)

yelp <- yelp %>% 
  rename(review = V1, score = V2)

combined_data <- rbind(yelp, amazon)
combined_data <- rbind(combined_data, imdb)

# Part II: Data Preparation 
# Part B

# B.1.a 

summary(combined_data)
sum(duplicated(combined_data)) 
combined_data <- combined_data[!duplicated(combined_data, fromLast = TRUE), ] # Keep first occurrence 
sum(duplicated(combined_data))
colSums(is.na(combined_data))
deltat(combined_data) 
str(combined_data)
head(combined_data)
is.null(combined_data)

# Unusual characters and emjois 
# Remove non-alphanumeric characters from strings
strsplit(combined_data$review, "[^a-zA-Z0-9]+")

# Make everything lowercase
combined_data$review <- tolower(combined_data$review)
head(combined_data$review)

# Remove punctuation 
combined_data$review <- gsub("[[:punct:]]", "", combined_data$review)
head(combined_data$review)

# Adress Emojis 
row_with_emojis <- grepl("[\U{1F600}-\U{1F64F}\U{2702}-\U{27B0}\U{1F680}-\U{1F6C0}\U{1F170}-\U{1F251}\U{1F004}\U{1F0CF}\U{1F004}\U{1F004}]", combined_data$review, perl = TRUE)
sum(row_with_emojis)

# B.1.b
# Vocab size
# Character/word count
combined_data$review %>%
  strsplit(" ") %>%
  sapply(length) %>%
  summary()

tidy_combined_data <- combined_data %>%
  unnest_tokens(word, review)

vocab_size <- tidy_combined_data %>% 
  count(word) %>%
  nrow()
cat("Vocabulary Size:", vocab_size)

# B.1.c 
# Sentence Lengths
sent_length <- sapply(strsplit(combined_data$review, "\\s+"), length)
summary(sent_length)
# B.1.c - Word embedding length
max_seq_embed <- as.integer(round(sqrt(sqrt(vocab_size)), 0))
max_seq_length <- max(sent_length)
med_seq_length <- median(sent_length)
min_seq_length <- min(sent_length)
cat("Estimated Embedding Length:", max_seq_embed, 
    "\nMaximum Sequence Length:", max_seq_length, 
    "\nMedian Sequence Length:", med_seq_length, 
    "\nMinimum Sequence Length:", min_seq_length)

# B.1.d   Statistical justification for chosen max seq length 
# Create histogram
hist(sent_length, 
     main = "Histogram of Sentence Lengths", 
     xlab = "Sentence Length", 
     ylab = "Frequency",
     col = "skyblue",
     border = "black")

# B2 - Tokenization 
tidy_combined_data <- combined_data %>% # EXECUTED ABOVE IN B1b
  unnest_tokens(word, review)

tidy_combined_data <- tidy_combined_data %>%
  mutate(word = tolower(word),
         word = removePunctuation(word),
         word = stemDocument(word))
head(tidy_combined_data)

tokenizer <- text_tokenizer()
tokenizer$fit_on_texts(tidy_combined_data$word)
word_indicies <- tokenizer$texts_to_sequences(tidy_combined_data$word)

# B.3.a - Padding Process 
padded_seq <- pad_sequences(word_indicies, maxlen = max_seq_length, padding = "post", truncating = "post")

# B.3.b - Single padded sequence 
single_pad_seq_index <- 8
single_pad_seq <- padded_seq[single_pad_seq_index,]
single_pad_seq

# B.4 - How many categories of sentiment and activation function 

text_vectorization <- layer_text_vectorization(
  max_tokens = vocab_size, 
  output_sequence_length = max_seq_length,
)

text_vectorization %>% adapt(combined_data$review)

text_vectorization(matrix(combined_data$review[1], ncol = 1))
text_vectorization(matrix(combined_data$review))

input_layer <- layer_input(shape = c(1), dtype = "string")
output <- input_layer %>% 
  text_vectorization() %>%
  layer_embedding(input_dim = vocab_size + 1, output_dim = 16) %>% 
  layer_global_average_pooling_1d() %>% 
  layer_dense(units = 16, activation = "relu") %>%
  layer_dropout(0.5) %>% 
  layer_dense(units = 1, activation = "sigmoid")

# Split data 
set.seed(123)

  # Define proportions for train, validation, and test sets
train_split <- 0.6
validation_split <- 0.2
test_split <- 0.2

  # Determine the number of samples for each split
num_samples <- nrow(tidy_combined_data)
num_train <- round(train_split * num_samples)
num_validation <- round(validation_split * num_samples)
num_test <- num_samples - num_train - num_validation

  # Split the data into train, validation, and test sets
train_data <- tidy_combined_data[1:num_train, ]
validation_data <- tidy_combined_data[(num_train + 1):(num_train + num_validation), ]
test_data <- tidy_combined_data[(num_train + num_validation + 1):(num_train + num_validation + num_test), ]

# B.6 - Copy of prepared data set
# B6 ************************* CHANGE TEST AND TRAIN TO TEST_DATA OR WHATEVER *******************
# Export the data 
# write.csv(train, "C:\\Users\\nahur\\Desktop\\WGU\D213 Advanced Data Analytics\\Task 2\\D213_train.csv", row.names=FALSE)
# write.csv(test, "C:\\Users\\nahur\\Desktop\\WGU\D213 Advanced Data Analytics\\Task 2\\D213_test.csv", row.names=FALSE)

# Part III: Network Architecture 

# C.1 
model <- keras_model(input_layer, output)
summary(model)

# C.2
model %>% compile(
  loss = "binary_crossentropy",
  optimizer = "adam",
  metrics = list('accuracy')
)

history <- model %>% fit(
  train_data$word, 
  as.numeric(train_data$score == "1"), 
  epochs = 15, 
  batch_size = 32, 
  validation_split = 0.2, 
  callbacks = c(
    callback_early_stopping(monitor = "val_loss", 
                            patience = 3, 
                            mode = "auto", 
                            restore_best_weights = TRUE)),
  verbose = 2
)

# Number of parameters 
output_dim <- 16
dense_units <- 16

params_embedding <- (vocab_size + 1) * output_dim # Embedding Layer
params_dense <- (output_dim + 1) * dense_units # Dense Layer
params_output <- dense_units + 1 # Output Layer

total_params <- params_embedding + params_dense + params_output
print(total_params)

# Test Results
metrics <- model %>% evaluate(
  test_data$word, 
  as.numeric(test_data$score == "1")
)

validation_metrics <- model %>% evaluate(
  validation_data$word, 
  as.numeric(validation_data$score == "1")
)

# Access the final epoch's metrics
final_epoch <- length(history$metrics[[1]])
final_loss <- history$metrics[[1]][final_epoch]
final_accuracy <- history$metrics[[2]][final_epoch]

print(paste("Final Training Loss:", final_loss))
print(paste("Final Training Accuracy:", final_accuracy))

plot(history)

# Extract training and validation metrics
train_metrics <- history$metrics$accuracy
val_metrics <- history$metrics$val_accuracy
loss <- history$metrics$loss
val_loss <- history$metrics$val_loss

metrics_df <- data.frame(
  epoch = 1:length(train_metrics),
  train_accuracy = train_metrics,
  val_accuracy = val_metrics,
  loss = loss,
  val_loss = val_loss
)
# Melt the data frame for easier plotting
metrics_df <- melt(metrics_df, id.vars = "epoch")
# Plot accuracy & loss
accuracy_plot <- ggplot(metrics_df, aes(x = epoch, y = value, color = variable)) +
  geom_line() +
  labs(x = "Epoch", y = "Accuracy/Loss Value", color = "Metrics") +
  ggtitle("Training and Validation Accuracy and Loss Over Epochs") +
  theme_minimal()
# Print plot
print(accuracy_plot)
