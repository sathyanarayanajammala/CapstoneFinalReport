### Define  required fuctions

### List of functions defined
##
## 1 displaySplitCount
## 2 printggPlot2
## 3 chisqareTest
## 4 eda_functions
## 5 perf_measures_cart
## 6 logistic
## 7 perf_measures_logistic
## 
### --------------------------------

### Function name: displaySplitCount
###
### Purpose: Display the count of observations in the specified columns of the data frame
###
### Arguments:
###
#@ dd  : Name of the data frame with target variable NCD set to true
#@ dn  : Name of the data frame with target variable NCD set to false
#@ strTarget:String value of target
#@ dnTarget: target variable for data frame dn
#@ ddTarget: target variable for data frame dd


displaySplitCount<-function(dd,number,strTarget){
  
  cat(paste("\n Dataset file",number,strTarget,"  Count : ", nrow(dd), sep = " "))
  cat("\n\n")
  table(dd)
  
  #cat(paste("\n Dataset file 2",strTarget,"  Count : ",nrow(dn),sep = " "))
  #cat("\n\n")
  # table(dn)
}
###
### End of function
###

### Function name: printggPlot2
###
### Purpose: Plots the stacked bar chart as per the specified columns of the data frame
###
### Arguments:
###
#@ data  :is the data set we are using to plot
#@ column: comparision column with target variable
#@ str col: String value of heading
#@ strTarget:String value of target

printggPlot2 <-function(data,column,target,strTarget,strCol){
  
  title<-paste(strTarget," Vs ",strCol,sep=" ")
  
  ggplot(data, aes(x = as.factor(column),fill=factor(target))) +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), 
              stat = "count", position=position_stack(vjust=0.5)) +
    scale_y_continuous(labels = percent) +
    labs(title = title, y = strTarget, x = strCol)
  
}

###
### End of function
###
### Function name: minCellValues
###
### Purpose: returns the minimum value in the frequency table
###
###
### Arguments:
###
#@ tab1: Frequency table

minCellValues <- function(tab1) {
  a1 <- tab1[1,2]
  a0 <- tab1[1,1]
  b1 <- tab1[2,1]
  b0 <- tab1[2,2]
  minVal <- min(a0,a1,b0,b1)
  cat("\n Minimum value ",minVal,"\n" )
  return(minVal)
}
###
### End of function
###

### Function name: chisqareTest
###
### Purpose: Performs Chi-square test of independence for the given target and the predictor variable 
###          using the frequency table
###
### Arguments:
###
#@ strTarget:String value of target
#@ strColName: String value of column heading name
#@ tab1: Frequency table

chisqareTest<-function(strTarget,strCOlName,tab1){
  
  
  h0 <- paste("The relative proportions of", strTarget ,"are independent of", strCOlName)
  hA <- paste("The relative proportions of", strTarget ,"are dependent of", strCOlName)
  
  cat("\n\n Step 1: State the hypothesis \n\n")
  cat("\n Null hypothesis : ",h0)
  cat("\n\n")
  cat("\n Alternative hypothesis : ",hA)
  
  cat("\n\n Step 2: Formulate Analysis Plan \n\n")
  #
  cat("\n The analysis plan describes how to use sample data to accept or reject the null hypothesis.\n")
  cat("Plan should have these elements:")
  cat("\nSignificance level: We can use the standard 0.05 or 5% level")
  cat("\nTest method: Use Chi Square test of independence")
  
  cat("\n Significance level: We can use the standard 0.05 or 5% level")
  cat("\n Test method: Chi Square test of independence")
  cat("\n In our contingency table, we assume the expected frequency count for each cell of the table is at least 5.")
  
  cat("\n\n Step 3: Analyze Sample data \n\n")
  print(tab1)
  print(summary(tab1))
  
  cat("\n\n If the counts are not large enough to use a Chi-square distribution, 
      we shall use the p-value based on simulation instead, including the argument simulate.p.value = TRUE.")
  
  cat("\n In our contingency table, if we do not have the expected frequency count for each cell of the table is at least 5,")
  cat("\n we shall use the p-value based on simulation instead, including the argument simulate.p.value = TRUE")
  
  minCellValue <- minCellValues(tab1)
  
  if (minCellValue < 5) {
    
    with = "with"
    
    ### (1) Chi square with simulation for p value")
    
    cat("\n<------   Chi square with simulation for p value ---------> ")
    c <- chisq.test(tab1,simulate.p.value = TRUE)
    print(c)
  }
  else {
    
    with = "without"
    
    ### (2) Chi square without simulation for p value")
    
    cat("\n<------   Chi square without simulation for p value ---------> ")
    
    c <- chisq.test(tab1,simulate.p.value = FALSE)
    print(c)
  }
  
  cat("\n\n Step 4: Interpret the results \n\n")
  
  pvalue <- c$p.value
  
  cat("\n\n ### ---------------------------------------------------------")
  
  ######
  cat( "\n P value obtained ", with," simulation for p-value : ",pvalue,"\n")
  
  decision <- ""
  
  ifelse(pvalue < 0.05, decision <- hA, decision <- h0) 
  
  ###
  
  cat("\n\n <-- Decision: -->")
  cat("\n ",decision, "at 5% level of significance \n\n")
  
  ### ------------------------------------------------------------------------------------------------------------------------------------
  
  cat("\n This p-value is the probability of observing a sample statistic as extreme as the test statistic. \n") 
  cat("\n We compare the p-value to the significance level")
  cat(" and reject the null hypothesis when the p-value is less than the significance level else accept.\n")
  
  ###
  
}

###
### End of function
###

### Function name: eda_functions
###
### Purpose: Performs xtab and chi squared test for the specified pair of columns of the data frame
###
### Arguments:
###
#@ NCD        : Name of NCD to analyze
#@ data1       : Data frame containing the columns for xtab
#@ target     : Target variable for xtab
#@ column     : Predictor variable used for xtab 
#@ title      : varaiabke against which NCD is analyzed

eda_functions <- function(NCD,data1,target,column,title){
  
  cat("\n\n Performing mlbenchxtab ")
  
  cat(paste("\n\n",NCD, " Vs ",title," \n\n", sep=""))
  
  tab1 <- xtabs(~ (target + column),data=data1)
  print(ftable(tab1))
  
  ###
  
  cat(paste("\n\n Within each level of predictor variable values, % of " ,NCD, " patients  \n\n"))
  print(prop.table(tab1,1)) # Row %
  
  ###
  
  cat(paste("\n\n", NCD, " Vs predictor variable values \n\n"))
  print(prop.table(tab1,2)) # Column %
  
  
  cat(paste("\n\n Performing Chi Square Test for ",NCD, " and", title ))
  
  chisqareTest(NCD,title,tab1)
  #printggPlot2(d_train,d_train[,e],d_train[,"Diabetes"],"Diabetes",title)
}



###
### End of function
###

### Function name: perf_measures_cart
###
### Purpose: Computes performance measures for evaluating model performance for classification type 
###
### Arguments:
###
#@ df       : Data frame containing the columns for calculating model performance
#@ pfit     : model to be evaluated
#@ Target   : Target variable
#@ Title    : Title for ROC curve
perf_measures_cart <- function(df,pfit,target,title) {
  
  ### Using training data
  
  df$predict.class <- predict(pfit, df, type= "class")
  df$predict.score <- predict(pfit, df)
  
  pred <- prediction(df$predict.score[,2], target)
  perf <- performance(pred, "tpr", "fpr")
  
  ### Plot 
  
  mainlab = paste("ROC using ",title)
  
  plot(perf, main = mainlab)
  abline(a=0,b=1)
  ### Performance measures using training dataset
  
  KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
  auc <- performance(pred,"auc"); 
  auc <- as.numeric(auc@y.values)
  gini = ineq(df$predict.score[,2], type="Gini")
  
  cm <- with(df, table(target, predict.class))
  
  OA = ( cm[1,1] + cm[2,2] ) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])  
  
  performance_measures <- data.frame(Dataset = title, KS, auc, gini , OA)
  
  return (performance_measures)
}
###
### End of function
###



### Function name: cart_fn
###
### Purpose: Decision tree algorithm
###
### Arguments:
###
#@ d_train       : Training data
#@ formula       : formula for building the model
#@ title         : title for algorithm



cart_fn <- function(df, formula, title) {
  
  library(rpart)
  
  # grow tree 
  
  d_train = df
  
  fit <- rpart(formula,method="class", data=d_train)
  
  printcp(fit) # display the results 
  plotcp(fit) # visualize cross-validation results 
  summary(fit) # detailed summary of splits
  title<-paste("Classification Tree for",title,sep = " ")
  # plot tree 
  plot(fit, uniform=TRUE, 
       main=title)
  text(fit, use.n=TRUE, all=TRUE, cex=.8)
  # prune the tree 
  pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
  ptitle<-paste("Pruned Classification Tree for",title,sep = " ")
  # plot the pruned tree 
  plot(pfit, uniform=TRUE, 
       main=ptitle)
  text(pfit, use.n=TRUE, all=TRUE, cex=.8)
  
  fit <<- train(formula,
               method="rpart", data=d_train,
               control=rpart.control(minsplit=2),
               trControl = trainControl(method = "cv", number = 10),
               tuneLength=5)
  
  
  
  return(pfit)
}
###
### End of function
###




### Function name: logistic
###
### Purpose: Decision tree algorithm
###
### Arguments:
###
#@ formula       : formula for building the model
#@ d_train       : Training data 

logistic<-function(formula,d_train){
  
  lfit <- glm(formula,
              family="binomial", data=d_train)
  summary(lfit)
  
  return(lfit)
}
###
### End of function
###



### Function name: perf_measures_logistic
###
### Purpose: Computes performance measures for evaluating model performance for classification type(Logisitic Regression) 
###
### Arguments:
###
#@ df       : Data frame containing the columns for calculating model performance
#@ pfit     : model to be evaluated
#@ Target   : Target variable
#@ Title    : Title for ROC curve
perf_measures_logistic <- function(df,pfit,target,title) {
  
  ### Using training data
  
  
  df$predict.score <- predict(pfit, df)
  
  pred <- prediction(df$predict.score, target)
  perf <- performance(pred, "tpr", "fpr")
  
  ### Plot 
  
  mainlab = paste("ROC using ",title)
  
  plot(perf, main = mainlab)
  abline(a=0,b=1)
  ### Performance measures using training dataset
  
  
  auc <- performance(pred,"auc"); 
  auc <- as.numeric(auc@y.values)
  
  
  cm <- with(df, table(target, predict.score))
  
  OA = ( cm[1,1] + cm[2,2] ) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])  
  McFadden = 0.1732045
  performance_measures <- data.frame(Dataset = title, auc,McFadden,OA)
 
  
  return (performance_measures)
}
###
### End of function
###

