library(forecast)
library(fpp)
library(rmarkdown)
library(smooth)
library(readxl)


plastics <- read.csv(file.choose())
View(plastics)
head(plastics)
windows()
plot(plastics$Sales,type='o')


#dummy var

dumy <- data.frame(outer(rep(month.abb,length=60),month.abb,"==")+0)
View(dumy)

colnames(dumy) <- month.abb
plasticssales <- cbind(plastics,dumy)
View(plasticssales)
colnames(plasticssales)

plasticssales["t"] <- 1:60
View(plasticssales)

#log
plasticssales["log_sales"] <- log(plasticssales["Sales"])
plasticssales["t_squ"] <- plasticssales["t"]*plasticssales["t"]
attach(plasticssales)

#train test

train <- plasticssales[1:40,]
test <- plasticssales[41:60,]


#linear model

plastic_lm <- lm(Sales~t,data = train)
summary(plastic_lm) #p-value: 0.02775, R-squared:  0.09802

lm_pred <-data.frame(predict(plastic_lm, interval = 'predict',newdata = test))
View(lm_pred)

rmse_lm <- sqrt(mean((test$Sales-lm_pred$fit)^2,na.rm = T))
rmse_lm  #248.924

plot(lm_pred$fit)

#Exponential

plastic_expo <- lm(Sales~t, data = train)
summary(plastic_expo) #Adjusted R-squared:  0.09802 # p-value: 0.02775

expo_pred <- data.frame(predict(plastic_expo, interval = 'predict', newdata = test))
View(expo_pred) 

rmse_expo <- sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo


#Quadratic

plastic_Quad <- lm(Sales~t+t_squ, data = train)
summary(plastic_Quad) #R-squared:  0.1253, p-value: 0.03173

Quad_pred  <- data.frame(predict(plastic_Quad, interval='predict', newdata=test))
View(Quad_pred)

rmse_Quad <- sqrt(mean((test$Sales-Quad_pred$fit)^2, na.rm =T))
rmse_Quad   #495.4669


#Additive seasonality

sea_add_model <- lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec , data=train)
summary(sea_add_model)  #p-value: 2.036e-08   R-squared:   0.7778 

sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
View(sea_add_pred)

rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add #263.2362


# Additive Seasonality with Linear

Add_sea_Linear_model <- lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,
                           data = train)
summary(Add_sea_Linear_model)

Add_sea_Linear_pred <- data.frame(predict(Add_sea_Linear_model,interval='predict',
                                          newdata = test))
View(Add_sea_Linear_pred)

rmse_Add_sea_Linear <- sqrt(mean((test$Passengers-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear 


# Additive Seasonality with Quadratic 

Add_sea_Quad_model <- lm(Sales~t+t_squ+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,
                         data = train)
summary(Add_sea_Quad_model) #Adjusted R-squared:  0.9524

Add_sea_Quad_pred <- data.frame(predict(Add_sea_Quad_model,interval='predict',
                                        newdata=test))
View(Add_sea_Quad_pred)

rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad #118.2369

# Multiplicative Seasonality 

multi_sea_model <- lm(log_sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,
                      data = train)
summary(multi_sea_model)#Adjusted R-squared:  0.7784 

multi_sea_pred <- data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
View(multi_sea_pred)

rmse_multi_sea <- sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea  #266.6198


# Multiplicative Seasonality Linear trend 

multi_add_sea_model <- lm(log_sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,
                        data = train)
summary(multi_add_sea_model)

multi_add_sea_pred <- data.frame(predict(multi_add_sea_model,newdata=test,
                                       interval='predict'))
View(multi_add_sea_pred)

rmse_multi_add_sea <- sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea #117.115


# Preparing table on model and it's RMSE values 

table_rmse <- data.frame(c("rmse_lm","rmse_expo","rmse_Quad","rmse_sea_add",
                           "rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),
                         c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,
                           rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)

colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Multiplicative Seasonality Linear trend  has least RMSE value

new_model<-lm(log_sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,
              data = plasticssales)
summary(new_model)

new_model_pred<-data.frame(predict(new_model,newdata = plasticssales,interval='predict'))
View(new_model_pred)

new_model_fin <- exp(new_model$fitted.values)
View(new_model_fin)

pred_res<- predict(arima(log_sales,order=c(1,0,0)),n.ahead = 12)
Month <- as.data.frame(plastics$Month)

Final <- as.data.frame(cbind(Month,plasticssales$Sales,new_model_fin))
colnames(Final) <-c("Month","Sales","New_Pred_Value")
Final <- as.data.frame(Final)
View(Final)

plot(Final$New_Pred_Value,type="s")
plot(Final$New_Pred_Value,typt="o")

plot(Final$Sales,main = "ActualGraph", xlab="Sales(Actual)", ylab="Months",
     col.axis="blue",type="o") 

plot(Final$New_Pred_Value, main = "PredictedGraph", xlab="Sales(Predicted)", ylab="Months",
     col.axis="Green",type="s")
