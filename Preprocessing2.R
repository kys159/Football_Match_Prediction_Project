##n경기 평균 계산 함수
recent_5_mean<-function(data,group_var,variable,lag_num){
  col_length<-length(group_var)
  data<-data.table(data,key=c('team_id','game_id'))
  final_data<-c()
  for(i in 1:length(variable)){
    lag_data<-data[,c(shift(eval(parse(text=variable[i])),1:lag_num,type='lag')),by=group_var]
    lag_mean_data<-lag_data[, .(rowMeans(.SD,na.rm=T)),.SDcols=(col_length+1):(col_length+lag_num)]
    final_data<-cbind(final_data,lag_mean_data)
  }
  names(final_data)<-variable
  return(final_data)
}


##result mat 생성 함수
make_binary_result<-function(data){
  result_home<-ifelse(data$result=='home',1,0)
  result_away<-ifelse(data$result=='away',1,0)
  tot_result_factor<-factor(data$result,
                            labels=c('away','home','draw'),levels=c('away','home','draw'))
  tot_result<-as.numeric(as.factor(data$result))
  result_matrix<-data.frame(result_home,result_away,tot_result_factor,tot_result)
  return(result_matrix)
}


make_home_sub_away<-function(fin_data){
  home_var<-names(fin_data)[str_detect(names(fin_data),'home')][-1:-2]
  away_var<-names(fin_data)[str_detect(names(fin_data),'away')][-1:-2]
  diff_data_2<-fin_data%>%select(home_var)-fin_data%>%select(away_var)
  names(diff_data_2)<-gsub('home_','diff_',names(diff_data_2))
  names(diff_data_2)<-gsub('_home','_diff',names(diff_data_2))
  return_data<-data.frame(fin_data%>%select(result:importance_away),
                          diff_data_2)
  return(return_data)
  
}

##변수 생성
##최근 5경기 집계
##집계 변수 설정
sum_var<-names(full_data_ver_3)[c(11:21)]

group_var<-c('team_id')
group_var_2<-c('team_id','season')

##변수마다 집계
summary_data_understat<-recent_5_mean(full_data_ver_3,group_var,sum_var,5)

##본 데이터와 결합
summary_data<-data.table(data.frame(full_data_ver_3%>%select(-sum_var,-spi,-prob,-pred_score),
                                    summary_data_understat),
                         key=c('team_id'))


##결측치 처리
recent_5_first_obs<-full_data_ver_3%>%group_by(team_id)%>%filter(season==min(season))%>%
  select(team_id,sum_var)%>%summarise_all(mean)%>%data.frame()%>%arrange(team_id)

for(i in 1:length(unique(summary_data$team_id))){
  summary_data[team_id==recent_5_first_obs$team_id[i] & is.na(importance),
               sum_var]<-recent_5_first_obs[i,-1]
}

names(summary_data)[(names(summary_data) %in% sum_var)]<-paste('recent_5',sum_var,sep='_')
match_by_team<-summary_data%>%select(result_2:team_id,h_a)

##홈 5경기 집계
home_summary_understat<-recent_5_mean(full_data_ver_3%>%filter(h_a=='h'),group_var,sum_var,5)
home_summary_data<-data.table(data.frame(full_data_ver_3%>%filter(h_a=='h')%>%select(-sum_var,-spi,-prob,-pred_score)),
                              home_summary_understat,key=c('team_id','game_id'))

home_recent_5_first_obs<-full_data_ver_3%>%filter(h_a=='h')%>%
  group_by(team_id)%>%filter(season==min(season))%>%
  select(team_id,sum_var)%>%summarise_all(mean)%>%data.frame()

for(i in 1:length(unique(home_summary_data$team_id))){
  home_summary_data[team_id==home_recent_5_first_obs$team_id[i] & is.na(importance),
                    sum_var]<-home_recent_5_first_obs[i,-1]
}
names(home_summary_data)[(names(home_summary_data) %in% sum_var)]<-
  paste('home_match_recent_5',sum_var,sep='_')

##away 5경기 집계
away_summary_understat<-recent_5_mean(full_data_ver_3%>%filter(h_a=='a'),group_var,sum_var,5)
away_summary_data<-data.table(data.frame(full_data_ver_3%>%filter(h_a=='a')%>%select(-sum_var,-spi,-prob,-pred_score)),
                              away_summary_understat,key=c('team_id','game_id'))

away_recent_5_first_obs<-full_data_ver_3%>%filter(h_a=='a')%>%
  group_by(team_id)%>%filter(season==min(season))%>%
  select(team_id,sum_var)%>%summarise_all(mean)%>%data.frame()

for(i in 1:length(unique(away_summary_data$team_id))){
  away_summary_data[(team_id==away_recent_5_first_obs$team_id[i] & is.na(importance)),
                    sum_var]<-away_recent_5_first_obs[i,-1]
}


names(away_summary_data)[(names(away_summary_data) %in% sum_var)]<-
  paste('away_match_recent_5',sum_var,sep='_')


##최근 1경기 집계
lag_1_match_var<-full_data_ver_3[9:21]%>%names()
##변수마다 집계
lag_1_match<-recent_5_mean(full_data_ver_3,group_var,lag_1_match_var,1)
names(lag_1_match)<-paste(lag_1_match_var,'last',sep='_')

##본 데이터와 결합
lag_1_match_data<-data.frame(full_data_ver_3[,c(5:7)],lag_1_match)
##결측치 처리
last_match_first_obs<-full_data_ver_3%>%group_by(team_id)%>%filter(season==min(season))%>%
  select(team_id,lag_1_match_var)%>%summarise_all(mean)%>%data.frame()%>%arrange(team_id)
for(i in 1:length(unique(lag_1_match_data$team_id))){
  lag_1_match_data[(lag_1_match_data$team_id==last_match_first_obs$team_id[i] & is.na(lag_1_match_data$importance)),
                   paste(lag_1_match_var,'last',sep='_')]<-last_match_first_obs[i,-1]
}
lag_1_match_data

##홈 1경기 집계
home_last_match<-recent_5_summarise_2(full_data_ver_3%>%filter(h_a=='h'),lag_1_match_var,'mean',1)
names(home_last_match)<-paste(lag_1_match_var,'last_match',sep='_')

home_last_match_data<-data.frame(full_data_ver_3%>%filter(h_a=='h')%>%select(date,game_id,team_id),
                                 home_last_match)%>%arrange(team_id,date)

home_last_match_first_obs<-full_data_ver_3%>%filter(h_a=='h')%>%
  group_by(team_id)%>%filter(season==min(season))%>%
  select(team_id,lag_1_match_var)%>%summarise_all(mean)%>%data.frame()

for(i in 1:length(unique(home_last_match_data$team_id))){
  home_last_match_data[(home_last_match_data$team_id==home_last_match_first_obs$team_id[i] & 
                          is.na(home_last_match_data$importance)),
                       paste(lag_1_match_var,'last_match',sep='_')]<-home_last_match_first_obs[i,-1]
}

##away 1경기 집계
away_last_match<-recent_5_summarise_2(full_data_ver_3%>%filter(h_a=='a'),lag_1_match_var,'mean',1)
names(away_last_match)<-paste(lag_1_match_var,'last_match',sep='_')

away_last_match_data<-data.frame(full_data_ver_3%>%filter(h_a=='a')%>%select(date,game_id,team_id),
                                 away_last_match)%>%arrange(team_id,date)

away_last_match_first_obs<-full_data_ver_3%>%filter(h_a=='a')%>%
  group_by(team_id)%>%filter(season==min(season))%>%
  select(team_id,lag_1_match_var)%>%summarise_all(mean)%>%data.frame()

for(i in 1:length(unique(away_last_match_data$team_id))){
  away_last_match_data[(away_last_match_data$team_id==away_last_match_first_obs$team_id[i] & 
                          is.na(away_last_match_data$importance)),
                       paste(lag_1_match_var,'last_match',sep='_')]<-away_last_match_first_obs[i,-1]
}



#match_info_data<-final_gmae_data_122%>%select(result,game_id,date,league:away_team,home_team_id:away_team_id)


##shot_Data 변수 생성
add_shot_zone<-function(shot_data){
  penalty_box_x<-0.825
  upper_penalty_box<-0.8
  lower_penalty_box<-0.2
  keeper_horiz_line <- 1-(1-0.825)/3 
  keeper_up_line <- 0.8-0.14
  keeper_down_line <- 0.2+0.14
  shot_data<-shot_data%>%mutate(zone=ifelse(X>penalty_box_x & Y < upper_penalty_box & Y>lower_penalty_box,1,0),
                                keeper_zone=ifelse(X>keeper_horiz_line&Y<keeper_up_line&Y>keeper_down_line,1,0),
                                wonder_goal=ifelse(xG<0.14 & result=='Goal',1,0),
                                easy_goal=ifelse(xG>0.74 & result=='Goal',1,0),
                                Y_2=1-abs(0.5-Y))
  return(shot_data)
}
##데이터 결합을 위해 shot 데이터 처리
understat_game_shot_df_2<-inner_join(understat_game_shot_df,final_gmae_data_122%>%select(game_id,date,home_team,away_team,home_team_id,away_team_id))
understat_game_shot_df_2<-understat_game_shot_df_2%>%mutate(team=ifelse(h_a=='h',home_team,away_team),
                                                            team_id=ifelse(h_a=='h',home_team_id,away_team_id))

understat_game_shot_df_2<-understat_game_shot_df_2%>%mutate(zone=ifelse(X>0.825&Y<0.8& Y>0.2,1,0))
understat_game_shot_df_2<-understat_game_shot_df_2%>%mutate(wonder_goal=ifelse(xG<0.14 & result=='Goal',1,0),
                                                            easy_goal=ifelse(xG>0.74 & result=='Goal',1,0))



set.seed(1234)
x_y_clust_2<-kmeans(understat_game_shot_df_3%>%filter(game_id!=7698)%>%mutate(Y_2=1-abs(0.5-Y))%>%select(X,Y_2,xG),6)

understat_game_shot_df_3<-understat_game_shot_df_3%>%filter(game_id!=7698)%>%mutate(clust=x_y_clust_2$cluster)

understat_game_shot_df_3<-understat_game_shot_df_3%>%filter(game_id!=7698)%>%mutate(clust=x_y_clust_2$cluster)

##shot 데이터 추가 변수 생성
chance_data<-understat_game_shot_df_3%>%group_by(team_id,team,game_id)%>%
  summarise(total_xg=sum(xG),goal=sum(result=='Goal'),mean_xg=mean(xG),
            goal_xg=sum(xG[result=='Goal']),goal_mean_xg=mean(xG[result=='Goal'],na.rm=T),
            n_shot=n(),
            goal_acc_1=goal/n_shot,##전체 슛중에 골수
            goal_acc_2=goal-goal_xg,##골의 xg와 골의 차이, 음수이면 goal_xg가 더 크다 즉 넣을걸 못넣었다.
            ##양수이면 goal이 더 많다 즉 확률이 적은것도 넣었다.
            goal_acc_3=goal-total_xg,
            ##전체 xg와 goal의 차이 음수이면 기회창출대비 골수가 적다
            normal_chance=sum(clust==6|clust==1),
            normal_chance_goal=sum((clust==6|clust==1)&result=='Goal'),
            easy_chance=sum(clust==6),
            easy_chance_goal=sum(clust==6&result=='Goal'),
            hard_chance=sum(clust %in% c(2,3,4,5)),
            hard_chance_goal=sum(clust %in% c(2,3,4,5)&result=='Goal'),
            normal_prop=normal_chance/n_shot,
            easy_prop=easy_chance/n_shot,
            hard_prop=hard_chance/n_shot,
            normal_goal_prop=normal_chance_goal/goal,
            easy_goal_prop=easy_chance_goal/goal,
            hard_goal_prop=hard_chance_goal/goal,
            total_setp=sum(situation!='OpenPlay'),setp_goal=sum(result=='Goal'&situation!='OpenPlay'),
            total_setp_xg=sum(xG[situation!='OpenPlay']),setp_goal_xg=sum(xG[result=='Goal'&situation!='OpenPlay']),
            setp_prop=total_setp/n_shot,setp_xg_prop=total_setp_xg/total_xg
  )%>%data.frame()%>%arrange(team_id,game_id)

for(i in 1:ncol(chance_data)){
  chance_data[is.nan(chance_data[,i]),i]<-0
}

##match info 와 chance data 결합
chance_var<-names(chance_data)[c(which(names(chance_data)=='total_xg'):30)]

match_info_2
shot_data%>%arrange(team_id)

chance_data_2<-left_join(shot_data%>%select(game_id,team_id,team,h_a,date,season),
                         chance_data,by=c('game_id'='game_id','team_id'='team_id','team'='team'))
chance_data_2[is.na(chance_data_2)]<-0

chance_data_2<-left_join(chance_data_2,
                         match_info%>%select(game_id,team_home,team_id_home,
                                             team_away,team_id_away),
                         by=c('game_id'='game_id'))

chance_data_2<-chance_data_2%>%mutate(ver_team=ifelse(h_a=='h',team_away,team_home),
                                      ver_team_id=ifelse(h_a=='h',team_id_away,team_id_home))%>%select(-team_home,-team_away,-team_id_home,-team_id_away)

chance_data_2<-chance_data_2%>%
  select(names(chance_data_2)[!(names(chance_data_2) %in% chance_var)],chance_var)

chance_data_2<-left_join(chance_data_2,chance_data_2%>%select(game_id,ver_team,ver_team_id,chance_var),
                         by=c('game_id'='game_id','team'='ver_team','team_id'='ver_team_id'),
                         suffix = c("", "_A"))

##chance data 집계
match_var<-names(chance_data_2)[1:(which(names(chance_data_2)=='total_xg')-1)]
chance_sum_var<-chance_data_2%>%select(-match_var)%>%names()

##전체 찬스데이터 5경기 집계
chance_summary_5<-recent_5_mean(chance_data_2,group_var,chance_sum_var,5)
chance_summary_data_5<-data.table(data.frame(chance_data_2%>%select(-chance_sum_var),chance_summary_5),key='team_id')

chance_5_first_obs<-chance_data_2%>%
  group_by(team_id)%>%filter(season==min(season))%>%
  select(team_id,chance_sum_var)%>%summarise_all(mean)%>%data.frame()

for(i in 1:length(unique(chance_summary_data_5$team_id))){
  chance_summary_data_5[(team_id==chance_5_first_obs$team_id[i] & is.na(total_xg)),
                        chance_sum_var]<-chance_5_first_obs[i,-1]
}

names(chance_summary_data_5)[(names(chance_summary_data_5) %in% chance_sum_var)]<-
  paste('recent_5',chance_sum_var,sep='_')


##홈경기 찬스데이터 집계
home_chance_data<-chance_data_2%>%filter(h_a=='h')
home_chance_summary_5<-recent_5_mean(home_chance_data,group_var,chance_sum_var,5)
home_chance_summary_data_5<-data.table(data.frame(home_chance_data%>%select(-chance_sum_var),
                                                  home_chance_summary_5),key='team_id')

home_chance_5_first_obs<-home_chance_data%>%
  group_by(team_id)%>%filter(season==min(season))%>%
  select(team_id,chance_sum_var)%>%summarise_all(mean)%>%data.frame()

for(i in 1:length(unique(home_chance_summary_data_5$team_id))){
  home_chance_summary_data_5[(team_id==home_chance_5_first_obs$team_id[i] & 
                                is.na(total_xg)),
                             chance_sum_var]<-home_chance_5_first_obs[i,-1]
}
names(home_chance_summary_data_5)[(names(home_chance_summary_data_5) %in% chance_sum_var)]<-
  paste('home_recent_5',chance_sum_var,sep='_')

##어웨이경기 찬스데이터 집계
away_chance_data<-chance_data_2%>%filter(h_a=='a')
away_chance_summary_5<-recent_5_mean(away_chance_data,group_var,chance_sum_var,5)
away_chance_summary_data_5<-data.table(data.frame(away_chance_data%>%select(-chance_sum_var),away_chance_summary_5),
                                       key='team_id')
away_chance_5_first_obs<-away_chance_data%>%
  group_by(team_id)%>%filter(season==min(season))%>%
  select(team_id,chance_sum_var)%>%summarise_all(mean)%>%data.frame()

for(i in 1:length(unique(away_chance_summary_data_5$team_id))){
  away_chance_summary_data_5[(team_id==away_chance_5_first_obs$team_id[i] & 
                                is.na(total_xg)),
                             chance_sum_var]<-away_chance_5_first_obs[i,-1]
}
names(away_chance_summary_data_5)[(names(away_chance_summary_data_5) %in% chance_sum_var)]<-
  paste('away_recent_5',chance_sum_var,sep='_')


##shot data 지난경기 집계
##전체 찬스데이터 1경기 집계
last_match_chance<-recent_5_summarise_2(chance_data,chance_sum_var,'mean',1)

last_match_chance_data<-data.frame(chance_data%>%select(match_var),last_match_chance)
chance_5_first_obs<-chance_data%>%
  group_by(team_id)%>%filter(season==min(season))%>%
  select(team_id,chance_sum_var)%>%summarise_all(mean)%>%data.frame()
for(i in 1:length(unique(last_match_chance_data$team_id))){
  last_match_chance_data[(last_match_chance_data$team_id==chance_5_first_obs$team_id[i] & 
                            is.na(last_match_chance_data$total_xg)),
                         chance_sum_var]<-chance_5_first_obs[i,-1]
}
names(last_match_chance_data)[9:ncol(last_match_chance_data)]<-paste(chance_sum_var,'last_match',sep='_')
names(last_match_chance_data)

##홈경기 찬스데이터 집계
home_chance_data<-chance_data%>%filter(h_a=='h')
home_last_match_chance<-recent_5_summarise_2(home_chance_data,chance_sum_var,'mean',1)
home_last_match_chance_data<-data.frame(home_chance_data%>%select(match_var),home_last_match_chance)
home_chance_5_first_obs<-home_chance_data%>%
  group_by(team_id)%>%filter(season==min(season))%>%
  select(team_id,chance_sum_var)%>%summarise_all(mean)%>%data.frame()
for(i in 1:length(unique(home_last_match_chance_data$team_id))){
  home_last_match_chance_data[(home_last_match_chance_data$team_id==home_chance_5_first_obs$team_id[i] & 
                                 is.na(home_last_match_chance_data$total_xg)),
                              chance_sum_var]<-home_chance_5_first_obs[i,-1]
}
names(home_last_match_chance_data)
names(home_last_match_chance_data)[9:ncol(home_last_match_chance_data)]<-
  paste('home',chance_sum_var,'last_match',sep='_')
##어웨이경기 찬스데이터 집계
away_chance_data<-chance_data%>%filter(h_a=='a')
away_last_match_chance<-recent_5_summarise_2(away_chance_data,chance_sum_var,'mean',1)
away_last_match_chance_data<-data.frame(away_chance_data%>%select(match_var),away_last_match_chance)
away_chance_5_first_obs<-away_chance_data%>%
  group_by(team_id)%>%filter(season==min(season))%>%
  select(team_id,chance_sum_var)%>%summarise_all(mean)%>%data.frame()
for(i in 1:length(unique(away_last_match_chance_data$team_id))){
  away_last_match_chance_data[(away_last_match_chance_data$team_id==away_chance_5_first_obs$team_id[i] & 
                                 is.na(away_last_match_chance_data$total_xg)),
                              chance_sum_var]<-away_chance_5_first_obs[i,-1]
}
names(away_last_match_chance_data)
names(away_last_match_chance_data)[9:ncol(away_last_match_chance_data)]<-
  paste('away',chance_sum_var,'last_match',sep='_')



##전체 데이터 결합
fin_data_fin<-left_join(match_info,summary_data%>%select(-date,-h_a,-season,-league,-result_2,-team),
                        by=c('game_id'='game_id','team_id_home'='team_id'),
                        suffix=c('','_home'))

fin_data_fin<-left_join(fin_data_fin,summary_data%>%select(-date,-h_a,-season,-league,-result_2,-team),
                        by=c('game_id'='game_id','team_id_away'='team_id'),
                        suffix=c('_home','_away'))

fin_data_fin<-left_join(fin_data_fin,home_summary_data%>%select(-date,-h_a,-season,-league,-result_2,-team),
                        by=c('game_id'='game_id','team_id_home'='team_id'))

fin_data_fin<-left_join(fin_data_fin,away_summary_data%>%select(-date,-h_a,-season,-league,-result_2,-team),
                        by=c('game_id'='game_id','team_id_away'='team_id'))

fin_data_fin<-left_join(fin_data_fin,chance_summary_data_5%>%
                          select(-date,-h_a,-season,-team,-ver_team,-ver_team_id),
                        by=c('game_id'='game_id','team_id_home'='team_id')
)

fin_data_fin<-left_join(fin_data_fin,chance_summary_data_5%>%
                          select(-date,-h_a,-season,-team,-ver_team,-ver_team_id),
                        by=c('game_id'='game_id','team_id_away'='team_id'),
                        suffix=c('_home','_away'))


fin_data_fin<-left_join(fin_data_fin,home_chance_summary_data_5%>%
                          select(-date,-h_a,-season,-team,-ver_team,-ver_team_id),
                        by=c('game_id'='game_id','team_id_home'='team_id'))

fin_data_fin<-left_join(fin_data_fin,away_chance_summary_data_5%>%
                          select(-date,-h_a,-season,-team,-ver_team,-ver_team_id),
                        by=c('game_id'='game_id','team_id_away'='team_id'))
#fin_data_fin_2<-left_join(fin_data_fin,fin_last_match)

fin_data_fin_2<-(fin_data_fin)
position_summary_data_2<-position_summary_data%>%select(game_id,home_team_id,away_team_id,names(position_summary_data)[str_detect(names(position_summary_data),'recent')])
fin_data_fin_2<-inner_join(fin_data_fin,position_summary_data_2,
                           by=c('team_id_home'='home_team_id','team_id_away'='away_team_id',
                           'game_id'='game_id'))
fin_data_fin_3<-inner_join(fin_data_fin_2,lag_1_match_data,
                           by=c('game_id'='game_id','team_home'='team','team_id_home'='team_id'))
fin_data_fin_3%>%View()
fin_data_fin_3<-inner_join(fin_data_fin_3,lag_1_match_data,
                           by=c('game_id'='game_id','team_away'='team','team_id_away'='team_id'),
                           suffix=c('_home','_away'))
##홈-원정 변수 생성
fin_data_fin_fin<-make_home_sub_away(fin_data_fin_3)

##최종 데이터
fin_fin_fin_data<-fin_data_fin_fin

##데이터 분할
set.seed(123)
index_vec<-caret::createDataPartition(fin_fin_fin_data$result,p=0.8)
train_x<-fin_fin_fin_data[index_vec$Resample1,]
valid_x<-fin_fin_fin_data[-index_vec$Resample1,]

##선형 종속인 변수 제거
lin_comb<-findLinearCombos(train_x[,c(18:(ncol(train_x)))])
delete_var<-names(train_x[,c(18:(ncol(train_x)))])[lin_comb$remove]
train_x_nz_nl<-train_x%>%select(-delete_var)

##상관관계가 높은 변수 제거
train_x_nz_nl_nc <- train_x_nz_nl
descrCor <- cor(train_x_nz_nl_nc[,c(18:(ncol(train_x_nz_nl_nc)))])
highlyCorDescr <- findCorrelation(descrCor, cutoff = .4)
cor_delete_var<-names(train_x_nz_nl_nc[,c(18:(ncol(train_x_nz_nl_nc)))])[highlyCorDescr]
train_x_nz_nl_nc_fin <- train_x_nz_nl_nc%>%select(-cor_delete_var)


train_x_nz_nl_nc_fin<-train_x_nz_nl_nc_fin%>%arrange(league,team_id_home,game_id)
train_result_mat<-make_binary_result(train_x_nz_nl_nc_fin)

##각 리그별 scale 진행
split_train<-split(train_x_nz_nl_nc_fin,as.factor(train_x_nz_nl_nc_fin$league))
split_train_bundes<-split_train$bundesliga%>%
  select(-result:-team_id_away)

split_train_epl<-split_train$epl%>%
  select(-result:-team_id_away)

split_train_la<-split_train$`la liga`%>%
  select(-result:-team_id_away)

split_train_ligue<-split_train$`ligue 1`%>%
  select(-result:-team_id_away)

split_train_serie<-split_train$`serie a`%>%
  select(-result:-team_id_away)

center_scale_bundes<-preProcess(split_train_bundes,method=c('center','scale'))
train_bundes_fin<-predict(center_scale_bundes,split_train_bundes)

center_scale_epl<-preProcess(split_train_epl,method=c('center','scale'))
train_epl_fin<-predict(center_scale_epl,split_train_epl)

center_scale_la<-preProcess(split_train_la,method=c('center','scale'))
train_la_fin<-predict(center_scale_la,split_train_la)

center_scale_ligue<-preProcess(split_train_ligue,method=c('center','scale'))
train_ligue_fin<-predict(center_scale_ligue,split_train_ligue)

center_scale_serie<-preProcess(split_train_serie,method=c('center','scale'))
train_serie_fin<-predict(center_scale_serie,split_train_serie)

train_fin<-rbind(train_bundes_fin,train_epl_fin,train_la_fin,train_ligue_fin,train_serie_fin)%>%data.matrix()

model_var_name<-train_fin%>%colnames()
valid_x$result
valid_x_nz_nl_nc_fin<-valid_x%>%select(model_var_name)%>%data.matrix()

##valid set 에서 scale 진행
valid_x_result_mat<-make_binary_result(valid_x)
scale_by_league<-function(data){
  fin_data<-c()
  for(i in 1:nrow(data)){
    if(data$league[i]=='bundesliga'){
      i_row_data<-predict(center_scale_bundes,data[i,]%>%select(model_var_name))
      fin_data<-rbind(fin_data,i_row_data)
    }
    else if(data$league[i]=='epl'){
      i_row_data<-predict(center_scale_epl,data[i,]%>%select(model_var_name))
      fin_data<-rbind(fin_data,i_row_data)
    }
    else if(data$league[i]=='la liga'){
      i_row_data<-predict(center_scale_la,data[i,]%>%select(model_var_name))
      fin_data<-rbind(fin_data,i_row_data)
    }
    else if(data$league[i]=='ligue 1'){
      i_row_data<-predict(center_scale_ligue,data[i,]%>%select(model_var_name))
      fin_data<-rbind(fin_data,i_row_data)
    }
    else if(data$league[i]=='serie a'){
      i_row_data<-predict(center_scale_serie,data[i,]%>%select(model_var_name))
      fin_data<-rbind(fin_data,i_row_data)
    }
  }
  return(fin_data)
}

valid_x_fin<-scale_by_league(valid_x)%>%data.matrix()

valid_y_factor<-factor(valid_x_nz_nl_nc_fin$result,
                       labels=c('away','home','draw'),levels=c('away','home','draw'))
