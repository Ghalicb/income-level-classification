library(GGally)

## DATA LOADING
setwd(dir="~/Desktop/Data Science - MA4/Applied Biostatistics/Project/Individual Project/")
train.datafile.path <- "data/adult.data"

train.data.raw <- read.csv(train.datafile.path, header=FALSE)

## DATA PRE-PROCESSING
# Remove the row with unknown values
is.na(train.data.raw) <- train.data.raw == ' ?'
train.data <- na.omit(train.data.raw)

# Rename the variables with relevant names
colnames(train.data) <- c("age", "workclass", "demographic-weight", "education",
                          "education-num", "marital-status", "occupation",
                          "relationship", "race", "sex", "capital-gain",
                          "capital-loss", "hours-per-week", "native-country",
                          ">50K")

# Convert the target variable into binary
train.data$`>50K` <- ifelse(train.data$`>50K`==" >50K", 1, 0)
train.data$`>50K` <- as.logical(train.data$`>50K`)

## DATA EXPLORATION
# Amount of rows with unknown values (absolute & %)
train.data.lost <- nrow(train.data.raw) - nrow(train.data)
train.data.lost.rel <- 100 * train.data.lost / nrow(train.data)

# Distribution of data between the two target class
train.data.upper <- sum(train.data$`>50K`)
train.data.upper.rel <- train.data.upper / nrow(train.data) 

train.data.lower <- nrow(train.data) - train.data.upper
train.data.lower.rel <- train.data.lower / nrow(train.data)

# Univariate & Bivariate analysis for main variables
main_cols <- c("sex", "age", "education-num", "hours-per-week", ">50K") 

my_bin <- function(data, mapping, ..., low = "#100000", high = "#CC1E0A") {
  ggplot(data = data, mapping = mapping) +
    geom_bin2d(...) +
    scale_fill_gradient(low = low, high = high)
}

ggpairs(train.data, columns= main_cols,
        upper=list(combo="blank", discrete="blank"),
        diag=list(continuous="densityDiag"), 
        lower=list(combo="box", continuous=my_bin))
      
# Univariate numerical statistics for capital
summary(train.data$`Capital gain`)
quantile(train.data$`Capital gain`, probs=0.95)
summary(train.data$`Capital loss`)
quantile(train.data$`Capital loss`, probs=0.95)

