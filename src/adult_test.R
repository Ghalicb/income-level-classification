
## DATA LOADING
setwd(dir="~/Desktop/Data Science - MA4/Applied Biostatistics/Project/Individual Project/")
train.datafile.path <- "data/adult.data"
test.datafile.path <- "data/adult.test"

train.data.raw <- read.csv(train.datafile.path, header=FALSE)
test.data.raw <- read.csv(test.datafile.path, header=FALSE)

## DATA PRE-PROCESSING
# Remove the row with unknown values
is.na(train.data.raw) <- train.data.raw == ' ?'
train.data <- na.omit(train.data.raw)

is.na(test.data.raw) <- test.data.raw == ' ?'
test.data <- na.omit(test.data.raw)

# Rename the variables with relevant names
colnames(train.data) <- c("Age", "Workclass", "Demographic weight", "Education",
                          "Education number", "Marital status", "Occupation",
                          "Relationship", "Race", "Sex", "Capital gain",
                          "Capital loss", "Hours per week", "Native country",
                          ">50K")

colnames(test.data) <- c("Age", "Workclass", "Demographic weight", "Education",
                         "Education number", "Marital status", "Occupation",
                         "Relationship", "Race", "Sex", "Capital gain",
                         "Capital loss", "Hours per week", "Native country",
                         ">50K")

# Convert the target variable into binary
train.data$`>50K` <- ifelse(train.data$`>50K`==" >50K", 1, 0)
test.data$`>50K` <- ifelse(test.data$`>50K`==" >50K.", 1, 0)


test.data$Age
