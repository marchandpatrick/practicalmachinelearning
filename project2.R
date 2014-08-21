## @knitr initiation
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
opts_chunk$set(cach.path = 'cache/report-one-')
knit_hooks$set(test = function(before, options, envir) {
  if (!before) {
    paste("Objects available in the above chunk:", paste("`", ls(envir), 
                                                         "`", sep = "", collapse = ", "))
  }
})
## @knitr test
ls(pattern="rf")
# rfFitfunc
## @knitr cleansing
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
usePackage("RCurl")
training <- read.csv(file = "pml-training.csv",head=TRUE,sep=",",na.strings=c("#DIV/0!"))
testing <- read.csv(file = "pml-testing.csv",head=TRUE,sep=",",na.strings=c("#DIV/0!"))
summary(training)
head(training)
names(training)
dim(training)
## @knitr z2
# remove near zero variance covariates 
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
# suppress 8 first columns, used for data management 
training <- training[, -seq(from = 1, to = 8, by = 1)]
testing <- testing[, -seq(from = 1, to = 8, by = 1)]

options(repos = list(CRAN = "http://cran.at.r-project.org/"))
usePackage("caret") 
nzv <- nearZeroVar(training, saveMetrics = T) 
training <- training[, !nzv$nzv]
testing <-testing[, !nzv$nzv]
dim(training)
# remove variables with more than 90% missing values 
nav <- sapply(colnames(training), function(x) if(sum(is.na(training[, x])) > 0.9*nrow(training)){return(T)}else{return(F)}) 
training <- training[, !nav] 
trainingsetindex <- createDataPartition(y = training$classe, p = 0.7, list = FALSE)
trainingset<- training[trainingsetindex, ]

validatingset <- training[-trainingsetindex, ]

## @knitr train
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
usePackage("caret")
usePackage("randomForest")
usePackage("class")
usePackage("e1071")
set.seed(33800)
rfFitfunc <- train(classe ~ ., method = "rf", data = trainingset, preProcess=c("center", "scale"), importance = T, trControl = trainControl(method = "cv", number = 10)) 
## ---- influential ----
load("rfFitfunc.RData")

imp <- varImp(rfFitfunc)$importance  
imp$max <- apply(imp, 1, max) 
imp <- imp[order(imp$max, decreasing = T), ] 
## @knitr plotaccuracy
plot(rfFitfunc, ylim = c(0.98, 1)) 
rfFitfunc  
## @knitr confusion
# final model 
confusion<-rfFitfunc$finalModel 
confusion
## @knitr prediction
# prediction 
prediction <- as.character(predict(rfFitfunc, newdata=testing)) 
## @knitr plotimpvar
#Plot of important variables
plot(varImp(rfFitfunc, scale=FALSE), top=20)
## @knitr Ploterrorrates
#Plot error rates
plot(rfFitfunc$finalModel, log="y", main="Error rates vs number of trees") 
## @knitrplotoutsampleerror
# out of sample error calculation
predictionvalidateset <- as.character(predict(rfFitfunc, newdata=validatingset)) 
outOfSampleError <- sum(predictionvalidateset != as.character(validatingset$classe)) * 100 / nrow(validatingset) 
## @knitr onlineeval
# Prepare results for online automated evaluation
pml_write_files = function(x){ 
  n = length(x) 
  for(i in 1:n){ 
    filename = paste0("problem_id_", i, ".txt") 
    write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, col.names = FALSE) 
  } 
} 
pml_write_files(prediction) 
## @knitr prediction
#predictions:
prediction

