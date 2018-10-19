#df=read.csv(file="C:/Users/lengada1/NCSU/periodic.csv",sep=",", header=TRUE)
df=read.csv(file="~/Desktop/NCSU/topNSKUs.csv",sep=",", header=TRUE)

dft_train=df[c(1:200),c(2:ncol(df))]
#dft_test=ts(df[c(201:nrow(df)),c(2:ncol(df))],frequency=7)

######

df1 = ts(dft_train[,1:1], frequency=7)

bag <- baggedModel(df1)
forecasts <- forecast(bag, h=nrow(df)-nrow(dft_train))

pred <- forecasts$mean

# Initialize data.frame
predictions <- data.frame(c(1:163))

for(col in names(dft_train)){
  
  X <- ts(dft_train[,col], frequency=7)

  # Forecasting using bagged model
  bag <- baggedModel(X)
  forecasts <- forecast(bag, h=nrow(df)-nrow(dft_train))
  pred <- forecasts$mean
  
  predictions[col] <- pred

  print(predictions)
}

# Drop helper column
predictions <- predictions[,2:ncol(predictions)]

# Write data.frame as CSV in local directory
# TODO: Set working directory (setwd)
write.csv(predictions, "BaggedModel_pred.csv", row.names = FALSE)