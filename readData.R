readData<-function(path.name,file.name,column.types,missing.types){
  read.csv(url(paste(path.name,file.name,sep="")),colClasses = column.types,na.strings = missing.types)
}
Titanic.path <- "https://raw.github.com/wehrley/Kaggle_Titanic/master/"
train.data.file <- "train.csv"
test.data.file <- "test.csv"
missing.types <- c("NA", "")
train.column.types <- c('integer',   # PassengerId
                        'factor',    # Survived 
                        'factor',    # Pclass
                        'character', # Name
                        'factor',    # Sex
                        'numeric',   # Age
                        'integer',   # SibSp
                        'integer',   # Parch
                        'character', # Ticket
                        'numeric',   # Fare
                        'character', # Cabin
                        'factor'     # Embarked
)
test.column.types <- train.column.types[-2] 
train.raw<-readData(Titanic.path,train.data.file,train.column.types,missing.types)
test.raw<-readData(Titanic.path,test.data.file,test.column.types,missing.types)
df.train <- train.raw
df.infer <- test.raw   
## map missing data by provided feature
require(Amelia)
missmap(df.train, main="Titanic Training Data - Missings Map", 
        col=c("yellow", "black"), legend=FALSE)
