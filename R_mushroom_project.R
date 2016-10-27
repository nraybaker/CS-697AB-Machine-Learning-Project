rm(list = ls())
require(readr)
require(randomForest)
require(dplyr)
require(tidyr)
require(magrittr)
require(caret)
require(Matrix)
require(data.table)
source("~/R/Functions/naCount.R")

char_var_preprocessing <- function(data){
  SUPPORT_MIN <- max(5, nrow(data)/1000)
  data <- data.table(data)
  
  char_cols <- names(which(sapply(data, FUN = is.character) | sapply(data, FUN = is.factor)))
  uniques <- sapply(data, function(x) length(unique(x)))
  remove <- names(uniques)[uniques <= 1]
  char_cols <- setdiff(char_cols, remove)
  
  freq_tables <- lapply(char_cols, function(x){
    freq <- table(data[[x]])
    freq <- sort(freq, decreasing = TRUE)
    return(freq)
  })
  names(freq_tables) <- char_cols
  
  for(col in char_cols){
    freq <- freq_tables[[col]]
    x <- factor(data[[col]], levels = names(freq))
    x <- addNA(x, ifany = FALSE)
    set(data, j = col, value = x)
  }
  
  col_levels <- sapply(data[, char_cols, with = FALSE], FUN = nlevels)
  char_cols <- names(which((col_levels > 1) & (col_levels < (0.95 * nrow(data)))))
  
  factor_levels <- lapply(data[, char_cols, with = FALSE], levels)
  
  data2 <- sparse.model.matrix(~. - 1, data = data[, char_cols, with = FALSE])
  data2 <- as.data.frame(as.matrix(data2))
  
  return(data2)
}

column_names <- c('edibility', 'cap_shape', 'cap_surface', 'cap_color', 'bruises', 'odor', 'gill_attachment',
                  'gill_spacing', 'gill_size', 'gill_color', 'stalk_shape', 'stalk_root', 'stalk_surface_above_ring',
                  'stalk_surface_below_ring', 'stalk_color_above_ring', 'stalk_color_below_ring', 'veil_type',
                  'veil_color', 'ring_number', 'ring_type', 'spore_print_color', 'population', 'habitat')
data <- read_csv("http://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data",
                 col_names = column_names,
                 na = "?")

# -----------------------------------------------------------------------------------------------------------------------
# MISSING DATA IMPUTATION WITH BREIMAN CUTLER RANDOM FOREST -------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------
data_no_missing <- data[!(data$stalk_root %>% is.na), ] %>% select(-stalk_root)
data_missing <- data[data$stalk_root %>% is.na, 2:ncol(data)] %>% select(-stalk_root)

response_no_missing <- data[!(data$stalk_root %>% is.na), ] %>% select(stalk_root)
response_missing <- data[data$stalk_root %>% is.na, ] %>% select(stalk_root)

data_no_missing <- char_var_preprocessing(data_no_missing)
data_missing <- char_var_preprocessing(data_missing)

data_no_missing$stalk_root <- NA
data_no_missing$stalk_root <- as.matrix(response_no_missing)
data_no_missing$stalk_root %<>% factor
data_missing$stalk_root <- NA
data_missing$stalk_root <- as.matrix(response_missing)

holdout_index <- createDataPartition(data_no_missing$stalk_root, p = .8, list = FALSE, times = 1)
train <- data_no_missing[holdout_index, ]
holdout <- data_no_missing[-holdout_index, ]

cv_index <- createDataPartition(train$stalk_root, p = .8, list = FALSE, times = 5)

val1 <- train[cv_index[, 1], ]
cross1 <- train[-cv_index[, 1], ]

val2 <- train[cv_index[, 2], ]
cross2 <- train[-cv_index[, 2], ]

val3 <- train[cv_index[, 3], ]
cross3 <- train[-cv_index[, 3], ]

val4 <- train[cv_index[, 4], ]
cross4 <- train[-cv_index[, 4], ]

val5 <- train[cv_index[, 5], ]
cross5 <- train[-cv_index[, 5], ]

validation <- list(val1, val2, val3, val4, val5)
cross <- list(cross1, cross2, cross3, cross4, cross5)
cross_class <- list(cross1$stalk_root, cross2$stalk_root, cross3$stalk_root, cross4$stalk_root, cross5$stalk_root)
predictions <- list()
models = list()

for(i in 1:5){
  models[[i]] <- randomForest(stalk_root ~ .,
                              data = validation[[i]],
                              #xtest = select(cross[[i]], -stalk_root),
                              #ytest = factor(as.matrix(select(cross[[i]], stalk_root))),
                              ntree = 250,
                              nodesize = 2,
                              importance = TRUE
  )
  predictions[[i]] <- predict(models[[i]], newdata = cross[[i]], type = "response")
}

correct <- list()
for(i in 1:5){
  correct[[i]] <- sum(predictions[[i]] == cross_class[[i]])
}

fit <- randomForest(stalk_root ~ .,
                    data = train,
                    ntree = 250,
                    nodesize = 2,
                    importance = TRUE
)
holdout_predictions <- predict(fit, newdata = holdout, type = "response")
                              
                              
                              
                              