library(forecast)
library(fpp)
library(smooth)
library(readxl)
library(xlsx)

cocacola <- read_xlsx(file.choose())
  #read.table(file = "clipboard",sep = "\t" , header = TRUE)
View(cocacola)
dev.off()
plot(cocacola$Sales, type = "o")

Q1 <- ifelse(grepl("Q1",cocacola$Quarter),'1','0')
Q2 <- ifelse(grepl("Q2",cocacola$Quarter),'1','0')
Q3 <- ifelse(grepl("Q3",cocacola$Quarter),'1','0')
Q4 <- ifelse(grepl("Q4",cocacola$Quarter),'1','0')
?grepl


#So creating 12 dummy variables 

CocacolaData <- cbind(cocacola,Q1,Q2,Q3,Q4)
View(CocacolaData)
colnames(CocacolaData)

CocacolaData["t"] <- 1:42
View(CocacolaData)

CocacolaData["larg_sale"] <- log(CocacolaData["Sales"])
CocacolaData["t_square"] <- CocacolaData["t"]*CocacolaData["t"]
attach(CocacolaData)


train <- CocacolaData[1:36,]

test <- CocacolaData[36:42,]

#LINEAR MODEL

linear_model <- lm(Sales ~ t, data = train)
summary(linear_model)

linear_pred <- data.frame(predict(linear_model,interval='predict',newdata=test))
View(linear_pred)

rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear#620.7862


#Exponential


expo_model <- lm(larg_sale~t, data = train)
summary(expo_model)


expo_pred <- data.frame(predict(expo_model,interval = 'predict', newdata=test))
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo 



#Quadratic

Quad_model <- lm(Sales~t+t_square,data = train)
summary(Quad_model)


Quad_pred <- data.frame(predict(Quad_model,interval = 'predict', newdata = test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 458.8969 and Adjusted R2 - 85.96%

#add seasonality

add_sea_model <- lm(Sales~Q1+Q2+Q3+Q4, data = train)
summary(add_sea_model)

add_sea_pred <- data.frame(predict(add_sea_model,newdata=test,
                                   interval ='predict'))
rmse_add_sea <- sqrt(mean((test$Sales- add_sea_pred$fit)^2,na.rm=T))

rmse_add_sea


#add seasonality with Linear

add_sea_lm <- lm(Sales~t+Q1+Q2+Q3+Q4, data = train)
summary(add_sea_lm)
add_sea_lm_pred <- data.frame(predict(add_sea_lm,interval='predict',newdata=test))
rmse_add_lm <- sqrt(mean((test$Sales-add_sea_lm_pred$fit)^2,na.rm=T))
rmse_add_lm

#add seasonality with Quadratic

add_sea_Quad <- lm(Sales~t+Q1+Q2+Q3+Q4, data = train)
summary(add_sea_Quad)

add_sea_Quad_pred <- data.frame(predict(add_sea_Quad,interval='predict',newdata=test))
rmse_add_Quad <- sqrt(mean((test$Sales-add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_add_Quad


# Multiplicative Seasonality #########################

multi_sea_model<-lm(larg_sale~Q1+Q2+Q3+Q4,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi <- sqrt(mean((test$Sales-multi_sea_pred$fit)^2,na.rm=T))
rmse_multi


# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_add_sea","rmse_add_lm","rmse_add_Quad","rmse_multi"),
                       c(rmse_linear,rmse_expo,rmse_Quad,rmse_add_sea,rmse_add_lm,rmse_add_Quad,rmse_multi))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Additive Seasonality with Quadratic trend  has least RMSE value

new_model<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=CocacolaData)
new_model_pred<-data.frame(predict(new_model,newdata=CocacolaData,interval='predict'))

new_model_fin <- new_model$fitted.values
View(new_model_fin)


Quarter <- as.data.frame(CocacolaData$Quarter)
final <- as.data.frame(cbind(Quarter,CocacolaData$Sales,new_model_fin))

colnames(final)

colnames(final) <- c("Quarter","Sales","New_pred_value")

plot(final$Sales, col.axis="blue")

plot(final$Sales,main="ActualGraph",xlab="Sales(Actual",
     ylab = "Quarter",
     col.axis="blue",type="o")

plot(final$New_pred_value, main = "PredictedGraph", xlab = "Sales(predicted)",ylab = "Quarter",
     col.axis="Red",type="s")

View(final)
     