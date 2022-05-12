setwd("/prm-mnt/morrisons/raw_data")

options(scipen=999) #options(scipen=0) to revert
########### read data ####################### 
Depot_detail = read.csv(file="depot_detail.csv", header=TRUE, sep=",")
Full_forecast = read.csv(file="full_forecast.csv", header=TRUE, sep=",")
Product_meta = read.csv(file="product_meta_master.csv", header=TRUE, sep=",")
Store_detail = read.csv(file="store_detail.csv", header=TRUE, sep=",")
Store_mileage = read.csv(file="store_mileage.csv", header=TRUE, sep=",")

Store_detail$store_forecast[is.na(Store_detail$store_forecast)] = 0
Full_forecast$forecast[is.na(Full_forecast$forecast)] = 0


####### complete cases and correlations ####
#store_prod = merge(Store_detail,Product_meta,by="product_id",all.x=T)

store_no_zero = Store_detail[!(Store_detail$start_of_day_stock == 0 & Store_detail$store_forecast !=0),]
dim(store_no_zero)[1]/dim(Store_detail)[1]
library(corrplot)

#prod_cor = aggregate(Store_detail$store_sales, by=list(product_id=Store_detail$product_id,day=Store_detail$day), FUN=sum)
prod_cor = Store_detail
prod_cor = prod_cor[order(prod_cor$product_id),]
prod_cor$index = rep(1:(63*3),55)
prod_cor = prod_cor[,c("product_id","store_forecast","index")]


head(prod_cor)
library(reshape)
library(tidyr)
prod_cor_n = spread(prod_cor, key = product_id, value = store_forecast)
#prod_cor_n = reshape(prod_cor, idvar = "index", timevar = "product_id", direction = "wide")
head(prod_cor_n)

M<-cor(prod_cor_n[,2:56])

jpeg("store_data_correlations_no_zero_stock.jpg")
corrplot(M, method="number")
corrplot(M)
dev.off()

#### waste ####

store_waste = Store_detail

store_waste$waste_sale = store_waste$waste_at_store / (store_waste$store_sales +.1)
store_waste$waste_forecast = store_waste$waste_at_store / (store_waste$store_forecast +.1)
store_waste$store_id = as.factor(store_waste$store_id)

store_wast_agg = aggregate(.~ store_id + product_id, data = store_waste, sum)

store_wast_agg$waste_sale = store_wast_agg$waste_at_store / (store_wast_agg$store_sales +15)
store_wast_agg$waste_forecast = store_wast_agg$waste_at_store / (store_wast_agg$store_forecast)
store_wast_agg$waste_sale_del = store_wast_agg$waste_at_store / (store_wast_agg$store_sales + store_wast_agg$delivery_from_depot)
store_wast_agg$waste_for_del = store_wast_agg$waste_at_store / (store_wast_agg$store_forecast + store_wast_agg$delivery_from_depot)
store_wast_agg$waste_delivery = store_wast_agg$waste_at_store / (store_wast_agg$delivery_from_depot)

library(ggplot2)
jpeg("Total waste to delivery ratio for each product and each store.jpeg")
ggplot(store_wast_agg, aes(fill=store_id, y=waste_delivery, x=product_id)) + 
  geom_bar(position="stack", stat="identity")+
  scale_x_continuous(breaks=seq(1,55,1)) +
  coord_cartesian(xlim=c(1,55)) +theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank(),legend.position="top",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title= "Total waste divided by total delivery",x = "Product ID", y = "Waste / Delivery") +
  scale_fill_discrete(name="",breaks=c("1", "2", "3"),
                        labels=c("Store 1", "Store 2","Store 3"))
dev.off()

ggplot(Store_detail, aes(x=store_sales,y=waste_at_store))+
  geom_point()+
  geom_smooth(method = "lm")
  
cor.test(Store_detail$store_sales,Store_detail$waste_at_store)

#"product_id"          "store_id"            "day"        "start_of_day_stock"  "store_sales"
#"store_forecast"      "waste_at_store"     
# "delivery_from_depot" "waste_sale"          "waste_forecast"     

####### PCA ####

store_f = Store_detail
store_f[,c("product_id","store_id")] = lapply(store_f[,c("product_id","store_id")],factor)

store_pca = Store_detail[,4:8]
head(store_pca)

s_pca = prcomp(store_pca, scale = TRUE)
s_pca
summary(s_pca)

jpeg("PCA_variance_left_after_including_each_component.jpg")
plot(s_pca, type='l') #how much variance is left after including PCi
dev.off()

#library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
jpeg("store_data_PCs_and_vars_vectors_only.jpg")
ggbiplot(s_pca,scale = 0,alpha=0.2)
dev.off()


str(s_pca)
head(s_pca$x)
store_pca2 = cbind(store_f, s_pca$x[,1:2])
head(store_pca2)

### PCA plot ####

library("ggplot2")

jpeg("store_data_PCA_product_id.jpg")
ggplot(store_pca2, aes(PC1, PC2, col = product_id, fill = product_id))+
  stat_ellipse(geom = "polygon", col = "black", alpha = .5)+ ##95%conf ellipse
  geom_point(shape =21, col = "black")
dev.off()

cor(Store_detail[,4:8], s_pca$x[,1:3])

#### XGBoost ####

#store_xg = Store_detail[,c(1:3,6,8)]
#head(store_xg)

df = Full_forecast
df$day_w = rep(1:7,13*3*55)
df <- df[order(df$day),]
st = Store_detail[order(Store_detail$day),]
df$delivery_from_depot = c(st$delivery_from_depot, rep(NA, nrow(df)-length(st$delivery_from_depot)))
df$forecast = scale(df$forecast, center = TRUE, scale = TRUE)
head(df)

df_prod = merge(df,Product_meta[,c(1:2,6:9)],by="product_id",all.x=T)
df_prod <- df_prod[order(df_prod$day),]
df_prod[,c("pack_size","weight","cost_price","item_allocation_shelf_life","minimum_order_quantity")] = scale(df_prod[,c("pack_size","weight","cost_price","item_allocation_shelf_life","minimum_order_quantity")], center = TRUE, scale = TRUE)
head(df_prod)
df_prod_temp = df_prod
#install.packages("plyr")
#library(plyr)
#df = rbind.fill(store_xg,Full_forecast, fill=TRUE)



table(Product_meta$item_allocation_shelf_life)
Product_meta %>%
  select( item_allocation_shelf_life ) %>%
  ggplot( aes(x=item_allocation_shelf_life)) +
  geom_histogram(aes(y=..density..), position="identity", binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  #ggtitle("Bin size = 3") +
  theme_bw()

detach(package:ggbiplot)
detach(package:plyr)
library(dplyr)
library(RcppRoll)
df_prod <- df_prod %>%
  group_by(store_id, product_id) %>%
  mutate(lag_1 = lag(forecast, 1)
         , avg_7 = lag(roll_meanr(forecast, 7), 1)
         #, avg_5 = lag(roll_meanr(forecast, 5), 1)
         , avg_4 = lag(roll_meanr(forecast, 4), 1)
         , avg_3 = lag(roll_meanr(forecast, 3), 1)
  )

View(df_prod[df_prod$store_id==2&df_prod$product_id==51,])

df_prod_lead <- df_prod_temp %>%
  group_by(store_id, product_id) %>%
  mutate(lead_1 = lead(forecast, 1)
         , avg_7 = roll_meanl(forecast, 7)
         #, avg_5 = lag(roll_meanr(forecast, 5), 1)
         , avg_4 = roll_meanl(forecast, 4)
         , avg_3 = roll_meanl(forecast, 3)
  )

View(df_prod_lead[df_prod_lead$store_id==2&df_prod_lead$product_id==51,])



p=aggregate(Store_detail$delivery_from_depot, by=list(Day=Store_detail$day), FUN=sum)
plot(p$Day,p$x,type='l',xaxt="n")
axis(1, at = seq(0, 63, by = 1), las=2)
#consider avg_3 and avg_4

library(xgboost)

#remove rows with NAs
df_n <- df[!is.na(df$avg_7),]
df_n$day_w = as.factor(df_n$day_w)

df_prod_n = df_prod[!is.na(df_prod$avg_7),]
df_prod_n$day_w = as.factor(df_prod_n$day_w)
head(df_prod_n)
str(df_prod_n)

df_prod_lead_n = df_prod_lead[!is.na(df_prod_lead$avg_7),]
df_prod_lead_n$day_w = as.factor(df_prod_lead_n$day_w)
head(df_prod_lead_n)
tail(df_prod_lead_n)
str(df_prod_lead_n)

#colnames(store_xg_n)

train = subset(df_n, day < 63)
test = subset(df_n, day >= 63)

train_p = subset(df_prod_n, day < 63)
train_p_3 = train_p[train_p$store_id==3,]
train_p_1_2 = train_p[train_p$store_id!=3,]
test_p = subset(df_prod_n, day >= 63)

train_l = subset(df_prod_lead_n, day < 63 & day>=7) # & day>=7
train_l_3 = train_l[train_l$store_id==3,]
train_l_1_2 = train_l[train_l$store_id!=3,]
test_l = subset(df_prod_lead_n, day >= 63)

#train_p$day_w = as.factor(train_p$day_w)

label <- train$delivery_from_depot
label_p <- train_p$delivery_from_depot
label_p_3 <- train_p_3$delivery_from_depot
label_p_1_2 <- train_p_1_2$delivery_from_depot

label_l <- train_l$delivery_from_depot
label_l_3 = train_l_3$delivery_from_depot
label_l_1_2 = train_l_1_2$delivery_from_depot

#Returns object unchanged if there are NA values
previous_na_action<- options('na.action')
options(na.action='na.pass')

#Build matrix input for the model
library(Matrix)

trainMatrix <- sparse.model.matrix(~ day_w + forecast + lag_1 + avg_3 + avg_4 + avg_7 + pack_size + weight + cost_price + item_allocation_shelf_life + minimum_order_quantity
                                   , data = train_p_3
                                   , contrasts.arg = c('day_w')
                                   , sparse = FALSE, sci = FALSE)


options(na.action = previous_na_action$na.action)

#Create input for xgboost
trainDMatrix <- xgb.DMatrix(data = trainMatrix, label = label_p_3)

#getinfo(trainDMatrix, "label")

#Set parameters of model
params <- list(booster = "gbtree"
               , objective = "reg:squarederror"
               , eta=0.4
               , gamma=0
)

#Cross-validation
xgb.tab <- xgb.cv(data = trainDMatrix
                  , param = params
                  , maximize = FALSE, evaluation = "rmse", nrounds = 200
                  , nfold = 5, early_stopping_round = 15)

#Number of rounds
num_iterations = xgb.tab$best_iteration

model <- xgb.train(data = trainDMatrix
                   , param = params
                   , maximize = FALSE, eval_metric = 'rmse', nrounds = num_iterations)

importance <- xgb.importance(feature_names = colnames(trainMatrix), model = model)
importance
png("XGBoost_feature_importance_lag_s3_new.png",width=1500, height=1400, res=300)
xgb.ggplot.importance(importance_matrix = importance)
dev.off()
#xgb.dump(model, with_stats = TRUE)

#xgb.plot.tree(model = model)

# previous_na_action<- options('na.action')
# options(na.action='na.pass')
# 
# #Build matrix input for the model
# #library(Matrix)
# 
# testMatrix <- sparse.model.matrix(~ day_w + forecast + lag_1 + avg_3 + avg_4 + avg_7 
#                                    , data = test
#                                    , contrasts.arg = c('day_w')
#                                    , sparse = FALSE, sci = FALSE)
# 
# options(na.action = previous_na_action$na.action)
# 
# 
# pred <- predict(model, testMatrix)
# 
# test_p = test
# test_p$delivery_from_depot = pred

previous_na_action<- options('na.action')
options(na.action='na.pass')

#Build matrix input for the model
#library(Matrix)

testMatrix <- sparse.model.matrix(~ day_w + forecast +lag_1 + avg_3 + avg_4 + avg_7 + pack_size + weight + cost_price + item_allocation_shelf_life + minimum_order_quantity 
                                  , data = df_prod_n[df_prod_n$store_id!=3,]
                                  , contrasts.arg = c('day_w')
                                  , sparse = FALSE, sci = FALSE)

options(na.action = previous_na_action$na.action)

pred <- predict(model, testMatrix)
d = df_prod_n[df_prod_n$store_id!=3,]
d$delivery_pred = pred



library(reshape2)
library(ggplot2)

plot_xg = function(store,product,agg){
    if (agg == 1){
      p=aggregate(d$delivery_pred, by=list(day=d$day), FUN=sum)
      colnames(p) <- c("Day","Delivery_pred")
      s=Store_detail[Store_detail$store_id!=3,]
      p1=aggregate(s$delivery_from_depot, by=list(day=s$day), FUN=sum)
      colnames(p1) <- c("Day","Delivery_actual")
      
      new = data.frame(Day = 0:6, Delivery_pred = rep(NA, 7))
      p_new = rbind(new,p)
      
      #plot(p$Day,p$x,type='l',xaxt="n")
      new1 = data.frame(Day = 63:90, Delivery_actual = rep(NA, 28))
      p1_new = rbind(p1,new1)
      
      all = cbind(p1_new,p_new$Delivery_pred)
      colnames(all) <- c("Day","Delivery_actual","Delivery_pred")
      all.melt<-melt(all, id="Day")
      #print(all.melt)
      
      ggplot(all.melt, aes(x=Day,  y=value, colour=variable)) +
        geom_line() +
        scale_x_continuous(breaks=seq(0,91,7)) +
        coord_cartesian(xlim=c(0,91)) +
        theme_bw() + theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank(),legend.position="top") +
        labs(title= "Cumulative delivery in units",x = "Day", y = "Delivery (units)") +
        scale_colour_discrete(name="",breaks=c("Delivery_actual", "Delivery_pred"),
                            labels=c("Delivery actual", "Delivery predicted"))
      
      #plot(p1_new$Day,p1_new$Delivery_actual,type='l',xaxt="n",col="red")
      #lines(p_new$Day,p_new$Delivery_pred,col="green")
      #axis(1, at = seq(0, 92, by = 1), las=2)
      
    } else {
      p=d[d$store_id==store&d$product_id==product,][,c("day","delivery_pred")]
      colnames(p) <- c("Day","Delivery_pred")
      p1=Store_detail[Store_detail$store_id==store&Store_detail$product_id==product,][,c("day","delivery_from_depot")]
      colnames(p1) <- c("Day","Delivery_actual")
      
      new = data.frame(Day = 0:6, Delivery_pred = rep(NA, 7))
      p_new = rbind(new,p)
      
      #plot(p$Day,p$x,type='l',xaxt="n")
      new1 = data.frame(Day = 63:90, Delivery_actual = rep(NA, 28))
      p1_new = rbind(p1,new1)
      
      all = cbind(p1_new,p_new$Delivery_pred)
      colnames(all) <- c("Day","Delivery_actual","Delivery_pred")
      all.melt<-melt(all, id="Day")
      
      t = paste("Delivery in units for store",store,"and product",product)
      
      ggplot(all.melt, aes(x=Day,  y=value, colour=variable)) +
        geom_line() +
        scale_x_continuous(breaks=seq(0,91,7)) +
        coord_cartesian(xlim=c(0,91)) +
        theme_bw() + theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank(),legend.position="top") +
        labs(title= t, x = "Day", y = "Delivery (units)") +
        scale_colour_discrete(name="",breaks=c("Delivery_actual", "Delivery_pred"),
                              labels=c("Delivery actual", "Delivery predicted"))
      
      #plot(p1_new$Day,p1_new$Delivery_actual,type='l',xaxt="n",col="red")
      #lines(p_new$Day,p_new$Delivery_pred,col="green")
      #axis(1, at = seq(0, 92, by = 1), las=2)
      
    }
}

png("Delivery_actual_predicted_cumulative_lag_s3_new.png",width=1500, height=1300, res=300)
plot_xg(agg = 1,store = 2,product = 6)
dev.off()

plot_xg_l = function(store,product,agg){
  if (agg == 1){
    p=aggregate(d$delivery_pred, by=list(day=d$day), FUN=sum)
    colnames(p) <- c("Day","Delivery_pred")
    s=Store_detail
    p1=aggregate(s$delivery_from_depot, by=list(day=s$day), FUN=sum)
    colnames(p1) <- c("Day","Delivery_actual")
    
    new = data.frame(Day = 85:90, Delivery_pred = rep(NA, 6))
    new_7 = data.frame(Day = 0:6, Delivery_pred = rep(NA, 7))
    p_new = rbind(new_7,p,new)
    
    #plot(p$Day,p$x,type='l',xaxt="n")
    new1 = data.frame(Day = 63:90, Delivery_actual = rep(NA, 28))
    p1_new = rbind(p1,new1)
    
    all = cbind(p1_new,p_new$Delivery_pred)
    colnames(all) <- c("Day","Delivery_actual","Delivery_pred")
    all.melt<-melt(all, id="Day")
    #print(all.melt)
    
    ggplot(all.melt, aes(x=Day,  y=value, colour=variable)) +
      geom_line() +
      scale_x_continuous(breaks=seq(0,91,7)) +
      coord_cartesian(xlim=c(0,91)) +
      theme_bw() + theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank(),legend.position="top") +
      labs(title= "Cumulative delivery in units",x = "Day", y = "Delivery (units)") +
      scale_colour_discrete(name="",breaks=c("Delivery_actual", "Delivery_pred"),
                            labels=c("Delivery actual", "Delivery predicted"))
    
    #plot(p1_new$Day,p1_new$Delivery_actual,type='l',xaxt="n",col="red")
    #lines(p_new$Day,p_new$Delivery_pred,col="green")
    #axis(1, at = seq(0, 92, by = 1), las=2)
    
  } else {
    p=d[d$store_id==store&d$product_id==product,][,c("day","delivery_pred")]
    colnames(p) <- c("Day","Delivery_pred")
    p1=Store_detail[Store_detail$store_id==store&Store_detail$product_id==product,][,c("day","delivery_from_depot")]
    colnames(p1) <- c("Day","Delivery_actual")
    
    new = data.frame(Day = 0:6, Delivery_pred = rep(NA, 7))
    p_new = rbind(new,p)
    
    #plot(p$Day,p$x,type='l',xaxt="n")
    new1 = data.frame(Day = 63:90, Delivery_actual = rep(NA, 28))
    p1_new = rbind(p1,new1)
    
    all = cbind(p1_new,p_new$Delivery_pred)
    colnames(all) <- c("Day","Delivery_actual","Delivery_pred")
    all.melt<-melt(all, id="Day")
    
    t = paste("Delivery in units for store",store,"and product",product)
    
    ggplot(all.melt, aes(x=Day,  y=value, colour=variable)) +
      geom_line() +
      scale_x_continuous(breaks=seq(0,91,7)) +
      coord_cartesian(xlim=c(0,91)) +
      theme_bw() + theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank(),legend.position="top") +
      labs(title= t, x = "Day", y = "Delivery (units)") +
      scale_colour_discrete(name="",breaks=c("Delivery_actual", "Delivery_pred"),
                            labels=c("Delivery actual", "Delivery predicted"))
    
    #plot(p1_new$Day,p1_new$Delivery_actual,type='l',xaxt="n",col="red")
    #lines(p_new$Day,p_new$Delivery_pred,col="green")
    #axis(1, at = seq(0, 92, by = 1), las=2)
    
  }
}

png("Delivery_actual_predicted_cumulative_lead_7_new.png",width=1500, height=1300, res=300)
plot_xg_l(agg = 1,store = 2,product = 6)
dev.off()

library(Metrics)
p=aggregate(d$delivery_pred, by=list(day=d$day), FUN=sum)

p1=aggregate(Store_detail$delivery_from_depot, by=list(day=Store_detail$day), FUN=sum)

pp = p[1:56,]
pp1 = p1[8:63,]
print(rmse(pp1$x,pp$x)) #249.2

p=aggregate(d$delivery_pred, by=list(day=d$day), FUN=sum)
s=Store_detail[Store_detail$store_id==3,]
p1=aggregate(s$delivery_from_depot, by=list(day=s$day), FUN=sum)

pp = p[1:56,]
pp1 = p1[8:63,]
print(rmse(pp1$x,pp$x)) #store 3: 67.4 stores 1 and 2: 254.1

pp = p[1:56,]
pp1 = p1[8:63,]
print(rmse(pp1$x,pp$x)) #568.3 #261.3 without first 7 days

p=aggregate(d$delivery_pred, by=list(day=d$day), FUN=sum)
s=Store_detail[Store_detail$store_id!=3,]
p1=aggregate(s$delivery_from_depot, by=list(day=s$day), FUN=sum)

pp = p[7:63,]
pp1 = p1[7:63,]
print(rmse(pp1$x,pp$x)) #568.3 #261.3 without first 7 days; 103.9 for store 3; 254.6 for stores 1 and 2

#simpsons paradox

store_sim = Store_detail
store_sim$product_id = as.factor(store_sim$product_id)
store_sim$store_id = as.factor(store_sim$store_id)

ggplot(store_sim,aes(x=delivery_from_depot,y=waste_at_store))+ #ROE_m Returns_m
  #geom_point(aes(color = store_id),alpha = 8/10)+
  #geom_point()+
  #geom_jitter(aes(color = RnD_bucket),alpha = 6/10)+
  #geom_jitter(alpha = 1/10)+
  #geom_smooth(aes(color = Sector, fill = Sector), method = "lm")+
  geom_smooth(aes(color = store_id, fill = store_id), method = "lm")+#MinVol_bucket Qual_bucket Pat_bucket
  geom_smooth(method = "lm")+
  theme_bw()+theme(axis.text.x = element_text(vjust=0.5, size=10,hjust=1),axis.text.y = element_text(vjust=0.5, size=10,hjust=1))+
  #scale_colour_discrete(name = "MinVol (buckets)", labels = c("Bottom 25%", "Mid 50%", "Top 25%"),breaks=c("-1","0","1"))+#MinVol Qual
  #scale_colour_discrete(name = "RnD (buckets)", labels = c("Bottom 25%", "Lower Mid 25%", "Upper Mid 25%","Top 25%"),breaks=c("4","3","2","1"))+ #Patents RnD
  #scale_fill_discrete(name = "RnD (buckets)", labels = c("Bottom 25%", "Lower Mid 25%", "Upper Mid 25%","Top 25%"),breaks=c("4","3","2","1"))+
  #facet_wrap(~Sector,scales = "free")+
  #facet_wrap(~Year, scales = 'free_x')+
  labs(x="Delivery",y = "Waste")#+ #ROE Returns

cor(store_sim$store_sales,store_sim$waste_at_store)

#### clustering ####

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

names_store = c("start_of_day_stock","store_sales","store_forecast","waste_at_store","delivery_from_depot")
store_c = Store_detail
head(store_c)

store_c_p = aggregate(.~ product_id, data = store_c, sum)
head(store_c_p)
dim(store_c_p)
store_c_s_p = aggregate(.~ store_id + product_id, data = store_c, sum)
head(store_c_s_p)
dim(store_c_s_p)

store_c_p[,names_store]= scale(store_c_p[,names_store], center = TRUE, scale = TRUE)
head(store_c_p)

store_c_s_p[,names_store]= scale(store_c_s_p[,names_store], center = TRUE, scale = TRUE)
head(store_c_s_p)

fviz_nbclust(store_c_s_p[,names_store], kmeans, method = "wss")
fviz_nbclust(store_c_s_p[,names_store], kmeans, method = "silhouette")

k2 <- kmeans(store_c_s_p[,names_store], centers = 5, nstart = 25)
str(k2)
k2

fviz_cluster(k2, data = store_c_p[,names_store]) #Does as well PCA

store_c_s_p %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster) %>%
         #product_id = row.names(USArrests)) %>%
  ggplot(aes(delivery_from_depot, waste_at_store, color = factor(cluster), label = product_id)) +
  geom_text()


####### waste : predicted vs actual #######

#Store_detail$waste_at_store[Store_detail$day == 0] = 0
#Store_detail$delivery_from_depot[Store_detail$day == 0] = 0
#Store_detail$start_of_day_stock[Store_detail$day == 0] = round(1.5*Store_detail$store_sales[Store_detail$day == 0])

#Store_detail$start_of_day_stock[Store_detail$day == 1] = 
#  Store_detail$start_of_day_stock[Store_detail$day == 0] - Store_detail$store_sales[Store_detail$day == 0]

store_1_2 = Store_detail[Store_detail$store_id!=3,]
summary(store_1_2)
store_1_2 <- store_1_2[order(store_1_2$day),] 
store_1_2 <- store_1_2[order(store_1_2$product_id),] 
store_1_2 <- store_1_2[order(store_1_2$store_id),] 

for (i in 1:max(Product_meta$item_allocation_shelf_life) ) {
  
  store_1_2[[paste0('day_shelf_', i)]] = NA
}

store_1_2$day_shelf_1[store_1_2$day == 0] = 
  store_1_2$start_of_day_stock[store_1_2$day == 0] - store_1_2$store_sales[store_1_2$day == 0] - store_1_2$waste_at_store[store_1_2$day == 0]

store_1_2_m = merge(x = store_1_2,
                       y = Product_meta,
                       by = c("product_id"))



store_1_2_m <- store_1_2_m[order(store_1_2_m$day),] 
store_1_2_m <- store_1_2_m[order(store_1_2_m$product_id),] 
store_1_2_m <- store_1_2_m[order(store_1_2_m$store_id),] 
head(store_1_2_m)
summary(store_1_2_m)

d_pred_del = d

d_pred_del <- d_pred_del[order(d_pred_del$day),] 
d_pred_del <- d_pred_del[order(d_pred_del$product_id),] 
d_pred_del <- d_pred_del[order(d_pred_del$store_id),] 

d_pred_del$delivery_pred = round(d_pred_del$delivery_pred)
d_pred_del$delivery_pred[d_pred_del$delivery_pred<0] = 0

summary(d_pred_del$delivery_pred)

store_1_2_m$delivery_from_depot[store_1_2_m$day >= 7] = d_pred_del$delivery_pred[d_pred_del$day <= 62]

sum(store_1_2_m$delivery_from_depot != store_1_2$delivery_from_depot)
sum(store_1_2_m$delivery_from_depot[store_1_2_m$day >= 7] != d_pred_del$delivery_from_depot[d_pred_del$day <= 62])

sale_not_equal = 0

for (i in 1:length(unique(store_1_2_m$store_id)) ) {
  for (k in 1:length(unique(store_1_2_m$product_id)) ) {
    for (j in 1:(length(unique(store_1_2_m$day)) - 1) ) {
      
      r = store_1_2_m[store_1_2_m$store_id==i&store_1_2_m$product_id==k&store_1_2_m$day==j,]
      r_1 = store_1_2_m[store_1_2_m$store_id==i&store_1_2_m$product_id==k&store_1_2_m$day==(j-1),]
      
      print(r_1$waste_at_store)
      
      r$start_of_day_stock= r_1$start_of_day_stock + r_1$delivery_from_depot - r_1$waste_at_store - r_1$store_sales
      
      #print(sold)
      
      if ((r$start_of_day_stock + r$delivery_from_depot) >= r$store_sales) {
        
        sold = r$store_sales
        
        print(sold)
        
        for (m in j:1){
          if (r_1[[paste0('day_shelf_', m)]] <= sold){
            
            print(r_1[[paste0('day_shelf_', m)]] <= sold)
            r[paste0('day_shelf_', m+1)] = 0
            
            #print(r[paste0('day_shelf_', m+1)])
            
            print(r_1[,c(paste0('day_shelf_', m))][1])
            sold = sold - r_1[[paste0('day_shelf_', m)]]
            print(sold)
            
          } else {
            
            r[paste0('day_shelf_', m+1)] = r_1[[paste0('day_shelf_', m)]] - sold
            sold = 0
          }
          
        }
        
        r$day_shelf_1 = r$delivery_from_depot - sold
        
        print(!is.na(r[[paste0('day_shelf_', r$item_allocation_shelf_life)]]))
        if (!is.na(r[[paste0('day_shelf_', r$item_allocation_shelf_life)]])) {
          
          r$waste_at_store = r[[paste0('day_shelf_', r$item_allocation_shelf_life)]]
          
          r[paste0('day_shelf_', r$item_allocation_shelf_life)] = 0
          
        } else {
          
          r$waste_at_store = 0
          
        }
        
        
      } else {
        
        print("Sales not equal")
        sale_not_equal = sale_not_equal + 1
        
        r$store_sales = r$start_of_day_stock + r$delivery_from_depot
        
        sold = r$store_sales
        
        for (m in j:1){
          if (r_1[[paste0('day_shelf_', m)]] <= sold){
            
            r[paste0('day_shelf_', m+1)] = 0
            sold = sold - r_1[[paste0('day_shelf_', m)]]
            
          } else {
            
            r[paste0('day_shelf_', m+1)] = r_1[[paste0('day_shelf_', m)]] - sold
            sold = 0
          }
          
        }
        
        r$day_shelf_1 = r$delivery_from_depot - sold
        
        if (!is.na(r[[paste0('day_shelf_', r$item_allocation_shelf_life)]])) {
          
          r$waste_at_store = r[[paste0('day_shelf_', r$item_allocation_shelf_life)]]
          
          r[paste0('day_shelf_', r$item_allocation_shelf_life)] = 0
          
        } else {
          
          r$waste_at_store = 0
          
        }
        
        
      }
      print(paste("store",i))
      print(paste("product",k))
      print(paste("day",j))
      
      store_1_2_m[store_1_2_m$store_id==i&store_1_2_m$product_id==k&store_1_2_m$day==j,] = r
      
    }
    
  }
  
  
}

sum(store_1_2$store_sales!=store_1_2_m$store_sales)
sum(store_1_2$waste_at_store!=store_1_2_m$waste_at_store)
sum(store_1_2_m$waste_at_store)
sum(store_1_2$waste_at_store)
sum(store_1_2_m$store_sales)
sum(store_1_2$store_sales)

waste_day_agg = aggregate(x = store_1_2_m$waste_at_store, by = list(store_1_2_m$day), FUN = 'sum')
names(waste_day_agg) = c('Day','Waste')
w = aggregate(x = store_1_2$waste_at_store, by = list(store_1_2$day), FUN = 'sum')
waste_day_agg$actual_wate = w$x

library(reshape)
waste_day_agg_melt = melt(waste_day_agg, id.vars = "Day")

waste = ggplot(data = waste_day_agg_melt, aes(x = Day, y = value, color=variable)) +
  geom_line()+
  scale_x_continuous(limits =c(0,max(waste_day_agg$Day)+1),breaks=seq(0,max(waste_day_agg$Day)+1,7))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5),panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5),legend.position='top')+
  labs(title= "Total waste by day for Stores 1 and 2",x = "Day", y = "Total waste")+
  scale_color_discrete(name="",    
                       breaks=c("actual_wate", "Waste"),
                       labels=c("Actual waste", "Model waste")
  )

png("Actual_and_model_waste_stores_1_2.png",width=1500, height=1300, res=300)
waste
dev.off()

total_sales_comparision = data.frame(Day = store_1_2$day, Actual_sales = store_1_2$store_sales, Adjusted_model_sales = store_1_2_m$store_sales)

total_sales_somparision_agg = aggregate(total_sales_comparision[,2:3], by = list(total_sales_comparision$Day), FUN = 'sum')
names(total_sales_somparision_agg) = c('Day','Actual_sales','Adjusted_model_sales')

library(reshape)
total_sales_somparision_agg_melt = melt(total_sales_somparision_agg, id.vars = "Day")

sales = ggplot(data = total_sales_somparision_agg_melt, aes(x = Day, y = value, fill = variable, color = variable)) +
  geom_line()+
  coord_cartesian(xlim=c(0,max(total_sales_somparision_agg$Day)+1))+
  scale_x_continuous(breaks=seq(0,max(total_sales_somparision_agg$Day)+1,7))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5),panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5),legend.position='top')+
  labs(title= "Total sales by day for Stores 1 and 2",x = "Day", y = "Total sales")+
  scale_color_discrete(name="",    
                       breaks=c("Actual_sales", "Adjusted_model_sales"),
                       labels=c("Actual sales", "Model sales")
  )

png("Actual_and_model_sales_stores_1_2.png",width=1500, height=1300, res=300)
sales
dev.off()


total_sales = c((sum(total_sales_comparision$Actual_sales)/sum(total_sales_comparision$Actual_sales))*100,
                (sum(total_sales_comparision$Adjusted_model_sales)/sum(total_sales_comparision$Actual_sales))*100)

total_waste = c((sum(waste_day_agg$actual_wate)/sum(waste_day_agg$actual_wate))*100,
                (sum(waste_day_agg$Waste)/sum(waste_day_agg$actual_wate))*100)

sale_waste_nums = c(sum(total_sales_somparision_agg$Actual_sales), sum(total_sales_somparision_agg$Adjusted_model_sales),
                    sum(waste_day_agg$actual_wate),sum(waste_day_agg$Waste))


total_sales_waste_df = data.frame(total_sales = c(total_sales,total_waste),
                                  sale_waste_nums = sale_waste_nums,
                                  model = c('Actual','Model','Actual','Model'),
                                  sales_waste = c('Sales','Sales','Waste','Waste'))




bar_sales_waste =   ggplot(data=total_sales_waste_df, aes(x=model,y=total_sales,fill=sales_waste))+
                    geom_bar(stat = "identity",position=position_dodge())+
                    geom_text(aes(label=sale_waste_nums), position=position_dodge(width=0.9), vjust=-0.25)+
                    scale_y_continuous(breaks=seq(0,100,10))+
                    theme_bw()+
                    theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5),panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5),legend.position = 'top')+
                    labs(title= "Total sales and waste as a precent of actual wate and sales \n Stores 1 and 2, first 63 days",x = "", y = "% of actual wate and sales")+
                    scale_fill_discrete(name="",    
                                        breaks=c("Sales", "Waste")
                    )

png("Actual_and_model_waste_sales_bar_stores_1_2.png",width=1700, height=1500, res=300)
bar_sales_waste
dev.off()
#do same for waste












