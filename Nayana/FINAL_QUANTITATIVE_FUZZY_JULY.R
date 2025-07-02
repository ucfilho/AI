rm(list=ls());


library(frbs)
library(readxl)
library("writexl")
library(Metrics) 

# read the dataset
path = "C:/Users/User/Documents/Atividades_andamento/Nayana/fuzzy anfis"
path = "C:/Users/User/Documents/Atividades_andamento/Nayana"
setwd(path)

fileName = 'custos_versao_mar_14B_2024.xlsx'
print(getwd())
df = read_excel(fileName)
#df$Dams = log10(df$Dams)
m = mean(df$Dams)
sd = sd(df$Dams)
df$Dams = (df$Dams-m)/sd
# head(df,3)
columnsNames = c( 'Volume', 'Area', 'Height')
columnSelect = which(names(df) %in% columnsNames )
data = df[, columnSelect]
data$y = df[,'Dams']



find_range <- function(data) {
  ranges <- apply(data, 2, function(col) c(min(col), max(col)))
  return(matrix(ranges, nrow = 2))
}

## Set the method and its parameters,
## for example, we use Wang and Mendel's algorithm
method.type <- "WM"

control_WM_TRIANGLE <- list(num.labels = 5, max.iter=200, type.mf = "TRIANGLE", type.defuz = "WAM",
                             type.tnorm = "MIN", type.snorm = "MAX", type.implication.func = "ZADEH",
                             name = "sim-0")

control_WM_TRAPEZOID <- list(num.labels = 5, max.iter=200, type.mf = "TRAPEZOID", type.defuz = "WAM",
                type.tnorm = "MIN", type.snorm = "MAX", type.implication.func = "ZADEH",
                name = "sim-0")

control_WM_GAUSSIAN <- list(num.labels = 5, max.iter=200, type.mf = "GAUSSIAN", type.defuz = "WAM",
                type.tnorm = "MIN", type.snorm = "MAX", type.implication.func = "ZADEH",
                name = "sim-0")

control_WM_SIGMOID <- list(num.labels = 5, max.iter=200, type.mf = "SIGMOID", type.defuz = "WAM",
                type.tnorm = "MIN", type.snorm = "MAX", type.implication.func = "ZADEH",
                name = "sim-0")

control_ANFIS <-list(num.labels=5, max.iter=50, step.size = 0.01,
                     type.tnorm = "MIN", type.snorm = "MAX",
                     type.implication.func = "ZADEH")

control_HYFIS <-list(num.labels=5, max.iter=50, step.size = 0.01,
                     type.tnorm = "MIN", type.snorm = "MAX",
                     type.implication.func = "ZADEH")

control_TSK = list(num.labels = 5, max.iter = 50, step.size = 0.01, 
                   alpha.heuristic = 1, type.tnorm = "MIN",  
                   type.implication.func = "ZADEH", name = "Sim-0") 



method.type = "WM" # "WM" ,"ANFIS", "HYFIS", "FS.HGD" (para TSK)
control = control_WM_SIGMOID #control_WM_TRIANGLE   # control_ANFIS control_TSK
file_name = "df_rules_control_WM_TRIANGLE.xlsx"

##  WM TRAPEZOID cross validation   cross validation RSSE= 7528137
##  WM_GAUSSIAN cross validation     RSSE= 5342168
##  WM_SIGMOID  cross validation     RSSE= 8137168
##  WM_TRIANGLE  cross validation    RSSE= 13259806
##  hyfis cross validation           RSSE= 5353482 
##  Anfis cross validation          RSSE= 7169801
##  TSK cross validation    RSSE=    NAO RODOU
# Use the function to find the range for the dataframe
range_matrix <- find_range(data)

num = nrow(df)
resp = c()
for (idx in 1:num)
  {
    
    X_train = df[-idx, columnSelect]
    y_train <- df[-idx,'Dams']
    X_test = df[idx,columnSelect]
    y_test = df[idx,'Dams']
    data.train = X_train
    data.train$y= y_train
    ## Learning step: Generate an FRBS model
    frbs_model <- frbs.learn(data.train, range.data = range_matrix ,
                             method.type = method.type, control = control)
    # Make predictions
    features = as.matrix(X_test)
    predictions = predict(frbs_model, newdata = features)
    resp = append(resp,predictions)
}

y_pred = log10(resp*sd+m)
y_true = log10(df$Dams*sd+m)

cat(' cross validation  R^2=', cor(y_true, y_pred))
cat(' cross validation RSSE=', rmse(y_true, y_pred))
cat(' cross validation MAE=', mae(y_true, y_pred))
#df_rules=data.frame(frbs_model$rule)
#write_xlsx(df_rules,file_name)
