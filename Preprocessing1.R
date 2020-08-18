library(party)
library(mxnet)
library(data.table)
library(DescTools)
library(caret)
library(scales)
library(dplyr)
library(stringr)
library(ggplot2)
library(reshape)

setwd('C:/Users/dudtj/Desktop/영석/비타민/데이콘/해축/data/데이터')
##n경기 평균 내는 함수
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


##데이터 불러오기
fivethirtyeight_pred<-fread('data/데이터/fivethirtyeight_pred.csv',encoding='UTF-8')%>%data.frame()
fivethirtyeight_pred$home_team<-iconv(fivethirtyeight_pred$home_team,from='UTF-8')
fivethirtyeight_pred$away_team<-iconv(fivethirtyeight_pred$away_team,from='UTF-8')

understat_game_df<-fread('data/데이터/understat_game_df.csv',encoding='UTF-8')%>%data.frame()
understat_game_df$home_team<-tolower(understat_game_df$home_team)
understat_game_df$away_team<-tolower(understat_game_df$away_team)

understat_game_player_df<-fread('data/데이터/understat_game_player_df.csv')%>%data.frame()
understat_game_player_df%>%names()
understat_game_player_df[,c(3)]<-iconv(understat_game_player_df[,c(3)],to='UTF-8')

understat_game_shot_df<-readr::read_csv('data/데이터/understat_game_shot_df.csv',locale="UTF-8")%>%data.frame()
understat_game_shot_df$player<-iconv(tolower(understat_game_shot_df$player),to = 'Latin-1')
understat_game_shot_df$player_assisted<-iconv(understat_game_shot_df$player_assisted,from='UTF-8')
position_summary_data<-fread('football_byebye.csv')

##팀명 데이터
team_data<-fread('team_name_data_final.csv')%>%data.frame()

##리그명, 팀명 통일시키기
five_5league<-fivethirtyeight_pred%>%filter(league %in% c('French Ligue 1','Barclays Premier League',
                                                          'Spanish Primera Division','Italy Serie A','German Bundesliga'))

five_5league<-five_5league%>%mutate(league_2=ifelse(league=='Barclays Premier League','epl',
                                                    ifelse(league=='French Ligue 1','ligue 1',
                                                           ifelse(league=='Spanish Primera Division','la liga',
                                                                  ifelse(league=='Italy Serie A','serie a','bundesliga')))))
five_5league$home_team<-tolower(five_5league$home_team)
five_5league$away_team<-tolower(five_5league$away_team)

five_5league_2<-left_join(five_5league,team_data,by=c('league_2'='league','home_team'='home_team'))
five_5league_2<-left_join(five_5league_2,team_data,by=c('league_2'='league','away_team'='home_team'))
five_5league_2<-five_5league_2%>%select(-league,-home_team,-away_team)
five_5league_2<-five_5league_2[,c(19:21,1:18)]
names(five_5league_2)[1:3]<-c('league','home_team','away_team')

understat_game_df_2<-understat_game_df%>%filter(date>='2016-08-12',league!='rfpl')
understat_game_df_3<-left_join(understat_game_df_2,team_data,by=c('league'='league','home_team'='home_team'))
understat_game_df_3<-left_join(understat_game_df_3,team_data,by=c('league'='league','away_team'='home_team'))

understat_game_df_3<-understat_game_df_3%>%select(-home_team,-away_team)
understat_game_df_3<-understat_game_df_3[,c(12:13,1:11)]
names(understat_game_df_3)[1:2]<-c('home_team','away_team')

understat_game_df_4<-understat_game_df_3%>%arrange(home_team,date)%>%
  mutate(year=Year(date))%>%group_by(home_team,year)%>%mutate(game_num=1:n())%>%data.frame()

five_5league_3<-five_5league_2%>%arrange(home_team,date)%>%
  mutate(year=Year(date))%>%group_by(home_team,year)%>%mutate(game_num=1:n())%>%data.frame()

team_id<-understat_game_df_4%>%select(league,home_team,home_team_id)%>%unique()
final_gmae_data<-left_join(five_5league_3,team_id,
                           by=c('league'='league','home_team'='home_team'))
final_gmae_data<-left_join(final_gmae_data,team_id,
                           by=c('league'='league','away_team'='home_team'))
names(final_gmae_data)[24:25]<-c('home_team_id','away_team_id')


final_gmae_data_11<-left_join(final_gmae_data,understat_game_df_4%>%
                                select(game_id,home_team_id,away_team_id,year,game_num,xg_home,xg_away),
                              by=c('year'='year','game_num'='game_num','home_team_id'='home_team_id','away_team_id'='away_team_id'),
                              suffix=c('_five','_under'))

final_gmae_data_11<-final_gmae_data_11%>%select(-year)

final_gmae_data_11
final_gmae_data_12<-left_join(final_gmae_data_11,understat_game_df_4%>%select(result,game_id))
final_gmae_data_12%>%names()

##기간 설정
final_gmae_data_122<-final_gmae_data_12%>%filter(date>='2016-08-12',date<='2019-10-27')
final_gmae_data_122%>%names()

##홈팀 변수 추출
game_data_home_var<-names(final_gmae_data_122)[str_detect(names(final_gmae_data_122),'home')]

##원정팀 변수 추출
game_data_away_var<-names(final_gmae_data_122)[str_detect(names(final_gmae_data_122),'away')]

##홈팀 데이터 추출
home_data<-final_gmae_data_122%>%select(league,date,game_id,game_data_home_var,
                                        xg_away_five,xg_away_under,nsxg_away,away_score,adj_score_away,result)
names(home_data)<-gsub('home_','',names(home_data))
names(home_data)<-gsub('_home','',names(home_data))
names(home_data)<-gsub('_away','A',names(home_data))
names(home_data)<-gsub('away_','A_',names(home_data))


home_data<-home_data%>%mutate(result_2=ifelse(result=='home','win',
                                                  ifelse(result=='away','lose','draw')),
                              season=ifelse(date>='2016-08-01'&date<='2017-06-30','16_17',
                                            ifelse(date>='2017-08-01'&date<='2018-06-30','17_18',
                                                   ifelse(date>='2018-08-01'&date<='2019-06-30','18_19',
                                                          '19_20'))),
                              h_a='h')%>%select(-result)

##원정팀 데이터 추출
away_data<-final_gmae_data_122%>%select(league,date,game_id,names(final_gmae_data_122)[str_detect(names(final_gmae_data_122),'away')],xg_home_five,xg_home_under,nsxg_home,home_score,adj_score_home,result)
names(away_data)<-gsub('away_','',names(away_data))
names(away_data)<-gsub('_away','',names(away_data))
names(away_data)<-gsub('_home','A',names(away_data))
names(away_data)<-gsub('home_','A_',names(away_data))
names(away_data)

away_data<-away_data%>%mutate(result_2=ifelse(result=='home','lose',
                                                  ifelse(result=='away','win','draw')),
                              season=ifelse(date>='2016-08-01'&date<='2017-06-30','16_17',
                                            ifelse(date>='2017-08-01'&date<='2018-06-30','17_18',
                                                   ifelse(date>='2018-08-01'&date<='2019-06-30','18_19',
                                                          '19_20'))),
                              h_a='a')%>%select(-result)

##홈팀, 원정팀 데이터 결합
full_data_ver_2<-rbind(home_data,away_data)
full_data_ver_2<-full_data_ver_2%>%arrange(team_id,date)

full_data_ver_3<-full_data_ver_2%>%group_by(team_id,season)%>%mutate(n_round=1:n())%>%data.frame()%>%
  arrange(team_id,date)

##final data_creation
##importance 대체
##결측치 이전 까지의 importance의 평균으로 대체
team_imp<-full_data_ver_3%>%group_by(team_id,season)%>%filter(!is.na(importance))%>%
  mutate(imp=cummean(importance))%>%select(team_id,season,n_round,imp)%>%data.frame()

for(i in 1:nrow(full_data_ver_3)){
  if(is.na(full_data_ver_3[i,'importance'])){
    full_data_ver_3[i,'importance']<-
      team_imp%>%filter(team_id==full_data_ver_3$team_id[i]&n_round==(full_data_ver_3$n_round[i]-1)&
      season==full_data_ver_3$season[i])%>%
      select(imp)
  }
}


full_data_ver_3<-full_data_ver_3%>%select(result_2,season,league:game_id,team,team_id,spi:adj_scoreA,h_a)
full_data_name<-full_data_ver_3%>%names()
#boxplot(full_data_ver_3$importance~full_data_ver_3$result_2)
#full_data_ver_3%>%names()

##경기 정보 데이터 생성
make_match_info<-function(by_team_data){
  match_info<-left_join(by_team_data%>%filter(h_a=='h')%>%select(result_2:team_id,spi:importance),
                        by_team_data%>%filter(h_a=='a')%>%select(game_id:team_id,spi:importance),
                        by=c('game_id'='game_id'),suffix=c('_home','_away'))%>%
    mutate(result=ifelse(result_2=='lose','away',
                         ifelse(result_2=='win','home','draw')))%>%
    select(result,league,season,game_id,date,team_home,team_away,team_id_home,team_id_away,spi_home,spi_away,prob_home,prob_away,
           pred_score_home,pred_score_away,importance_home,importance_away)
  return(match_info)
}

match_info<-make_match_info(full_data_ver_3)
names(match_info)


