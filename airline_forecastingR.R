library(rmarkdown)
library(forecast)
library(fpp)
library(smooth)
library(readxl)



airline <- read_excel(file.choose())

View(airline)
head(airline)
windows()
plot(airline$Passengers, type = "o")


#dummy value

x <- data.frame(outer(rep(month.abb,length = 96),month.abb,"==")+0)
View(x)

colnames(x) <- month.abb
airlinedata <- cbind(airline,x)
View(airlinedata)
colnames(airlinedata)

airlinedata["t"] <- 1:96
View(airlinedata)

airlinedata["log_passenger"] <- log(airlinedata["Passengers"])
airlinedata["t_square"] <- airlinedata["t"]*airlinedata["t"]
attach(airlinedata)


train <- airlinedata[1:84,]
test <- airlinedata[85:96,]


#LINEAR MODEL

airline_lm <- lm(Passengers~t, data = train)
summary(airline_lm)

airline_pred <- data.frame(predict(airline_lm, interval='predict',newdata=test))
View(airline_pred)

rmse_lm <- sqrt(mean((test$Passengers-airline_pred$fit)^2,na.rm = T))
rmse_lm   #53.19924

#Exponential

airline_expo <- lm(log_passenger~t,data=train)
summary(airline_expo)

airline_expo_pred <- data.frame(predict(airline_expo,interval='predict', newdata=test))
View(airline_expo_pred)

rmse_expo <- sqrt(mean((test$Passengers-exp(airline_expo_pred$fit))^2,na.rm = T))
rmse_expo   #46.05736

#Quadratic

airline_Quad <- lm(Passengers~t+t_square, data = train)
summary(airline_Quad)

airline_Quad_pred  <- data.frame(predict(airline_Quad,interval='predict',newdata = test))
View(airline_Quad_pred)

rmse_Quad <- sqrt(mean((test$Passengers-airline_Quad_pred$fit)^2, na.rm =T))
rmse_Quad   #48.05189

#Additive seasonality

sea_add_model <- lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec , data=train)
summary(sea_add_model)  #p-value: 0.2337   R-squared:  0.04015
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Passengers-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add #132.8198

# Additive Seasonality with Linear

Add_sea_Linear_model<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(Add_sea_Linear_model)

Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Passengers-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 35.34896


# Additive Seasonality with Quadratic 

Add_sea_Quad_model<-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model) #Adjusted R-squared:  0.9524

Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Passengers-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad #26.36082

# Multiplicative Seasonality 

multi_sea_model<-lm(log_passenger~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)#Adjusted R-squared:  0.05006 
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea  #140.0632

# Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Passengers-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea #10.51917


# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_lm","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Multiplicative Seasonality Linear trend  has least RMSE value

new_model<-lm(log_passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec, data = airlinedata)
new_model_pred<-data.frame(predict(new_model,newdata=airlinedata,interval='predict'))
new_model_fin <- exp(new_model$fitted.values)

View(new_model_fin)

pred_res<- predict(arima(log_passenger,order=c(1,0,0)),n.ahead = 12)
Month <- as.data.frame(airline$Month)

Final <- as.data.frame(cbind(Month,airlinedata$Passengers,new_model_fin))
colnames(Final) <-c("Month","Passengers","New_Pred_Value")
Final <- as.data.frame(Final)
View(Final)

plot(Final$New_Pred_Value,type="s")
plot(Final$New_Pred_Value,type="o")
