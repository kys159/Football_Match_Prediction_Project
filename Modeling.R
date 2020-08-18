library(mxnet)
library(party)

install.packages('pkgEnv')
home_model_tune<-mx.symbol.Variable('data')%>%
  mx.symbol.FullyConnected(num_hidden=1,name='out')%>%
  mx.symbol.LogisticRegressionOutput(name='output') 

mx.set.seed(123)
home_tune_2<-mx.model.FeedForward.create(
  symbol=home_model_tune,
  X = train_fin,
  y = train_result_mat$result_home,
  eval.data=list(data=valid_x_fin,label=valid_x_result_mat$result_home),
  ctx=mx.cpu(), num.round=100, array.batch.size=128,
  learning.rate=2e-6, momentum=0.9,
  eval.metric=mx.metric.logistic_acc,
  array.layout = "rowmajor"
)

home_tune_mxnet_2<-(t(predict(home_tune_2,train_fin))%>%data.frame())

home_valid<-(t(predict(home_tune_2,valid_x_fin))%>%data.frame())


away_model_tune<-mx.symbol.Variable('data')%>%
  mx.symbol.FullyConnected(num_hidden=1,name='fc')%>%
  mx.symbol.LogisticRegressionOutput(name='output')

mx.set.seed(123)
away_tune_2<-mx.model.FeedForward.create(
  symbol=away_model_tune,
  X = train_fin,
  y = train_result_mat$result_away,
  eval.data=list(data=valid_x_fin,label=valid_x_result_mat$result_away),
  ctx=mx.cpu(), num.round=500, array.batch.size=128,
  learning.rate=2e-6, momentum=0.9,
  eval.metric=mx.metric.logistic_acc,
  array.layout = "rowmajor"
)

away_tune_mxnet_2<-(t(predict(away_tune_2,train_fin))%>%data.frame())

away_valid<-(t(predict(away_tune_2,valid_x_fin))%>%data.frame())


mmn<-data.frame(train_result_mat$tot_result_factor,train_home_result,train_away_result)

table(mmn$train_home_result,mmn$train_result_mat.tot_result_factor)
mmmn<-ifelse(mmn$train_home_result==1 & mmn$train_away_result==0,2,
       ifelse(mmn$train_home_result==0 & mmn$train_away_result==1,1,3))


train_result_data<-cbind(result=train_result_mat$tot_result_factor,
                         away=away_tune_mxnet_2[,1],
                         home=home_tune_mxnet_2[,1]
)%>%data.frame()%>%mutate(draw=(1-away)*(1-home),home_sub_away=(home-away))%>%
  select(result,away,home,draw,home_sub_away)

prob_check_train<-c()
  for (j in 1:40){
    away_qnt=0.2+0.01*j
    for(k in 1:40){
      home_qnt=0.2+0.01*k
      prob_data_num_check<-train_result_data[,c(2:3)]%>%max.col()
      prob_data_num_check[((train_result_data$away<away_qnt & train_result_data$home<home_qnt)|
                               (train_result_data$away>away_qnt & train_result_data$home>home_qnt))
                            ]<-3
      prob_factor_check<-factor(prob_data_num_check,labels=c('away','home','draw'),levels=c(1,2,3))
      i_th_acc_mat<-data.frame(j=away_qnt,k=home_qnt,
                               acc=Accuracy(prob_factor_check,train_result_mat$tot_result_factor),
                               draw_prop=sum(prob_factor_check=='draw')/length(train_result_mat$tot_result_factor))
      prob_check_train<-rbind(prob_check_train,i_th_acc_mat)
    }
  }

prob_check_train%>%arrange(desc(acc),desc(draw_prop))%>%head(10)

prob_check_train%>%select(acc,draw_prop)%>%unique()%>%arrange(desc(acc),desc(draw_prop))%>%head(10)

#keep_best_number<-best_number
best_number<-prob_check_train%>%arrange(desc(acc),desc(draw_prop))%>%head(1)%>%select(j,k)
prob_data_num_check<-train_result_data[,c(2:3)]%>%max.col()
prob_data_num_check[((train_result_data$away<as.numeric(best_number[1]) & train_result_data$home<as.numeric(best_number[2]))|
                         (train_result_data$away>as.numeric(best_number[1]) & train_result_data$home>as.numeric(best_number[2])))
                      ]<-3
train_factor<-factor(prob_data_num_check,labels=c('away','home','draw'),levels=c(1,2,3))
caret::confusionMatrix(train_factor,train_result_mat$tot_result_factor)


prob_data<-cbind(away=away_valid[,1],
                 home=home_valid[,1]
)%>%data.frame()%>%mutate(draw=(1-away)*(1-home),home_sub_away=(home-away),result=valid_x_result_mat$tot_result_factor)%>%
  select(away,home,draw,home_sub_away,result)
prob_data_num<-prob_data[,c(1:2)]%>%max.col()
prob_data_num[ ((prob_data$away<as.numeric(best_number[1]) & prob_data$home<as.numeric(best_number[2]))|
                    (prob_data$away>as.numeric(best_number[1]) & prob_data$home>as.numeric(best_number[2])))
                ]<-3

prob_factor<-factor(prob_data_num,labels=c('away','home','draw'),levels=c(1,2,3))
caret::confusionMatrix(prob_factor,valid_x_result_mat$tot_result_factor)











test_result_mat
home_test<-(t(predict(home_tune_2,test_final_data))%>%data.frame())
home_test[,1]%>%Desc()
Desc(home_test[,1]~test_result_mat$tot_result_factor)
away_test<-(t(predict(away_tune_2,test_final_data))%>%data.frame())
away_test[,1]%>%Desc()
Desc(away_test[,1]~test_result_mat$tot_result_factor)


test_prob_mat<-data.frame(test_result_mat,away=away_test[,1],
                          home=home_test[,1])%>%
                          mutate(draw=(1-away)*(1-home),result=test_result_mat$tot_result_factor,
                                 home_sub_away=home-away)
test_class_num<-test_prob_mat[,c(4:5)]%>%max.col()
test_class_num[((test_prob_mat$away<as.numeric(best_number[1]) & test_prob_mat$home<as.numeric(best_number[2]))|
                (test_prob_mat$away>as.numeric(best_number[1]) & test_prob_mat$home>as.numeric(best_number[2])))
              ]<-3
table(test_class_num,test_prob_mat$result)
test_factor<-factor(test_class_num,labels=c('away','home','draw'),levels=c(1,2,3))
Accuracy(test_factor,test_prob_mat$result)

caret::confusionMatrix(train_factor,train_result_mat$tot_result_factor)
caret::confusionMatrix(prob_factor,valid_x_result_mat$tot_result_factor)
caret::confusionMatrix(test_factor,test_prob_mat$result)



