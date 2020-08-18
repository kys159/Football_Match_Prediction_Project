#!/usr/bin/env python
# coding: utf-8

# In[4]:


import asyncio
import json
import aiohttp
import pandas as pd
from understat import Understat
import numpy as np
import warnings
import os
from tqdm import tqdm
warnings.filterwarnings('ignore')
os.chdir('C:\\Users\\82104\\Desktop\\영석')


# In[5]:


get_ipython().system('pip install nest_asyncio')


# In[6]:


import nest_asyncio
nest_asyncio.apply()


# In[7]:


async def get_game_df(league_name,year):
    async with aiohttp.ClientSession() as session:
        understat = Understat(session)
        fixtures = await understat.get_league_results(league_name,year)
        a = pd.DataFrame({'game_id':[fixtures[i]['id'] for i in range(len(fixtures))],
                          'date':[fixtures[i]['datetime'] for i in range(len(fixtures))],
                          'home_team':[fixtures[i]['h']['title'] for i in range(len(fixtures))],
                          'away_team':[fixtures[i]['a']['title'] for i in range(len(fixtures))],
                          'home_team_id':[fixtures[i]['h']['id'] for i in range(len(fixtures))],
                          'away_team_id':[fixtures[i]['a']['id'] for i in range(len(fixtures))],
                          'xg_home':[fixtures[i]['xG']['h'] for i in range(len(fixtures))],
                          'xg_away':[fixtures[i]['xG']['a'] for i in range(len(fixtures))],
                          'home_prob':[fixtures[i]['forecast']['w'] for i in range(len(fixtures))],
                          'draw_prob':[fixtures[i]['forecast']['d'] for i in range(len(fixtures))],
                          'away_prob':[fixtures[i]['forecast']['l'] for i in range(len(fixtures))],
                          'home_score':[fixtures[i]['goals']['h'] for i in range(len(fixtures))],
                          'away_score':[fixtures[i]['goals']['a'] for i in range(len(fixtures))]})

        a['result'] = np.where(a['home_score']>a['away_score'],'home',np.where(a['home_score']==a['away_score'],'draw','away'))
        a['home_prob']=a['home_prob'].astype(float)
        a['away_prob']=a['away_prob'].astype(float)
        a['draw_prob']=a['draw_prob'].astype(float)
        a['home_score']=a['home_score'].astype(float)
        a['away_score']=a['away_score'].astype(float)
        a['pred']=a.iloc[:,8:11].idxmax(axis=1).str.slice(0,4)
        a['date'] = pd.to_datetime(a['date'])
        a['league']=league_name
        a['season']=str(year)+'/'+str(year+1)
        a = a[a.columns[-2:].append(a.columns[:-2])]
    return a


# In[8]:


league_list=['epl','la liga','bundesliga','serie a','ligue 1','rfpl']
year_list=[2019,2020]
agg={}
for league in league_list:
    for year in year_list:
        agg[league+str(year)]=asyncio.run(get_game_df(league,year))
    print(league)
game_df = pd.concat(agg,ignore_index=True)


# In[11]:


game_df.tail()


# In[12]:


game_df.to_csv("game_df.csv",index=False)


# In[13]:


async def get_game_player_df(game_id):
    async with aiohttp.ClientSession() as session:
        understat = Understat(session)
        players = await understat.get_match_players(game_id)
        h = pd.DataFrame({'player_id':[players['h'][i]['player_id'] for i in players['h'].keys()],
                      'player':[players['h'][i]['player'] for i in players['h'].keys()],
                      'position':[players['h'][i]['position'] for i in players['h'].keys()],
                      'team_id':[players['h'][i]['team_id'] for i in players['h'].keys()],
                      'h_a':[players['h'][i]['h_a'] for i in players['h'].keys()],
                      'time':[players['h'][i]['time'] for i in players['h'].keys()],
                      'xG':[players['h'][i]['xG'] for i in players['h'].keys()],
                      'xA':[players['h'][i]['xA'] for i in players['h'].keys()],
                      'xGChain':[players['h'][i]['xGChain'] for i in players['h'].keys()],
                      'xGBuildup':[players['h'][i]['xGBuildup'] for i in players['h'].keys()],
                      'shots':[players['h'][i]['shots'] for i in players['h'].keys()],
                      'goals':[players['h'][i]['goals'] for i in players['h'].keys()],
                      'own_goals':[players['h'][i]['own_goals'] for i in players['h'].keys()],
                      'key_passes':[players['h'][i]['key_passes'] for i in players['h'].keys()],
                      'assists':[players['h'][i]['assists'] for i in players['h'].keys()],
                      'yellow_card':[players['h'][i]['yellow_card'] for i in players['h'].keys()],
                      'red_card':[players['h'][i]['red_card'] for i in players['h'].keys()],
                      'roster_in':[players['h'][i]['roster_in'] for i in players['h'].keys()],
                      'roster_out':[players['h'][i]['roster_out'] for i in players['h'].keys()],
                      'positionOrder':[players['h'][i]['positionOrder'] for i in players['h'].keys()]
                      })
        a = pd.DataFrame({'player_id':[players['a'][i]['player_id'] for i in players['a'].keys()],
                      'player':[players['a'][i]['player'] for i in players['a'].keys()],
                      'position':[players['a'][i]['position'] for i in players['a'].keys()],
                      'team_id':[players['a'][i]['team_id'] for i in players['a'].keys()],
                      'h_a':[players['a'][i]['h_a'] for i in players['a'].keys()],
                      'time':[players['a'][i]['time'] for i in players['a'].keys()],
                      'xG':[players['a'][i]['xG'] for i in players['a'].keys()],
                      'xA':[players['a'][i]['xA'] for i in players['a'].keys()],
                      'xGChain':[players['a'][i]['xGChain'] for i in players['a'].keys()],
                      'xGBuildup':[players['a'][i]['xGBuildup'] for i in players['a'].keys()],
                      'shots':[players['a'][i]['shots'] for i in players['a'].keys()],
                      'goals':[players['a'][i]['goals'] for i in players['a'].keys()],
                      'own_goals':[players['a'][i]['own_goals'] for i in players['a'].keys()],
                      'key_passes':[players['a'][i]['key_passes'] for i in players['a'].keys()],
                      'assists':[players['a'][i]['assists'] for i in players['a'].keys()],
                      'yellow_card':[players['a'][i]['yellow_card'] for i in players['a'].keys()],
                      'red_card':[players['a'][i]['red_card'] for i in players['a'].keys()],
                      'roster_in':[players['a'][i]['roster_in'] for i in players['a'].keys()],
                      'roster_out':[players['a'][i]['roster_out'] for i in players['a'].keys()],
                      'positionOrder':[players['a'][i]['positionOrder'] for i in players['a'].keys()]
                      })
        final = pd.concat([a,h])
        final['game_id']=game_id
        final = final[final.columns[-1:].append(final.columns[:-1])]
    return final
# asyncio.run(get_game_player_df(5447))


# In[14]:


agg={}
for game_id in game_df['game_id']:
    try:
        agg[game_id] = asyncio.run(get_game_player_df(game_id))
    except:
        print(game_id)


# In[15]:


game_player_df = pd.concat(agg,ignore_index=True)


# In[16]:


game_player_df.to_csv("game_player_df.csv",index=False)


# In[17]:


game_player_df.tail


# In[19]:


async def get_game_shot_df(game_id):
    async with aiohttp.ClientSession() as session:
        understat = Understat(session)
        players = await understat.get_match_shots(game_id)

        h = pd.DataFrame({'season':[players['h'][i]['season'] for i in range(len(players['h']))],
                      'game_id':[players['h'][i]['match_id'] for i in range(len(players['h']))],
                      'minute':[players['h'][i]['minute'] for i in range(len(players['h']))],
                      'player_id':[players['h'][i]['player_id'] for i in range(len(players['h']))],
                      'player':[players['h'][i]['player'] for i in range(len(players['h']))],
                      'h_a':[players['h'][i]['h_a'] for i in range(len(players['h']))],
                      'X':[players['h'][i]['X'] for i in range(len(players['h']))],
                      'Y':[players['h'][i]['Y'] for i in range(len(players['h']))],
                      'shotType':[players['h'][i]['shotType'] for i in range(len(players['h']))],
                      'xG':[players['h'][i]['xG'] for i in range(len(players['h']))],
                      'result':[players['h'][i]['result'] for i in range(len(players['h']))],
                      'situation':[players['h'][i]['situation'] for i in range(len(players['h']))],
                      'player_assisted':[players['h'][i]['player_assisted'] for i in range(len(players['h']))],
                      'lastAction':[players['h'][i]['lastAction'] for i in range(len(players['h']))]})

        a = pd.DataFrame({'season':[players['a'][i]['season'] for i in range(len(players['a']))],
                      'game_id':[players['a'][i]['match_id'] for i in range(len(players['a']))],
                      'minute':[players['a'][i]['minute'] for i in range(len(players['a']))],
                      'situation':[players['a'][i]['situation'] for i in range(len(players['a']))],
                      'player_id':[players['a'][i]['player_id'] for i in range(len(players['a']))],
                      'player':[players['a'][i]['player'] for i in range(len(players['a']))],
                      'h_a':[players['a'][i]['h_a'] for i in range(len(players['a']))],
                      'X':[players['a'][i]['X'] for i in range(len(players['a']))],
                      'Y':[players['a'][i]['Y'] for i in range(len(players['a']))],
                      'shotType':[players['a'][i]['shotType'] for i in range(len(players['a']))],
                      'xG':[players['a'][i]['xG'] for i in range(len(players['a']))],
                      'result':[players['a'][i]['result'] for i in range(len(players['a']))],
                      'player_assisted':[players['a'][i]['player_assisted'] for i in range(len(players['a']))],
                      'lastAction':[players['a'][i]['lastAction'] for i in range(len(players['a']))]})

        final = pd.concat([a,h])
    return final
# asyncio.run(get_game_shot_df(5447))


# In[20]:


agg={}
for game_id in game_df['game_id']:
    try:
        agg[game_id] = asyncio.run(get_game_shot_df(game_id))
    except:
        print(game_id)


# In[21]:


game_shot_df = pd.concat(agg,ignore_index=True)


# In[22]:


game_shot_df.to_csv("game_shot_df.csv",index=False)


# In[29]:


async def get_game_future_df(league_name):
    async with aiohttp.ClientSession() as session:
        understat = Understat(session)
        fixtures = await understat.get_league_fixtures(league_name,2019)
        a = pd.DataFrame({'game_id':[fixtures[i]['id'] for i in range(len(fixtures))],
                          'date':[fixtures[i]['datetime'] for i in range(len(fixtures))],
                          'home_team_id':[fixtures[i]['h']['id'] for i in range(len(fixtures))],
                          'home_team':[fixtures[i]['h']['title'] for i in range(len(fixtures))],
                          'away_team_id':[fixtures[i]['a']['id'] for i in range(len(fixtures))],
                          'away_team':[fixtures[i]['a']['title'] for i in range(len(fixtures))]})
    return a
# asyncio.run(get_game_future_df('epl'))


# In[30]:


agg=[asyncio.run(get_game_future_df(league_name)) for league_name in ['epl','la liga','bundesliga','serie a','ligue 1','rfpl']]
game_future_df = pd.concat(agg,ignore_index=True)
game_future_df.to_csv("game_future_df.csv",index=False)


# In[31]:


game_future_df=pd.read_csv("game_future_df.csv")


# In[32]:


team_list = pd.concat([game_df['home_team'],game_df['away_team']]).unique().tolist()
year_list = [2019,2020]


# In[33]:


async def get_team_stats(team_name,season):
    async with aiohttp.ClientSession() as session:
        understat = Understat(session)
        team_stats = await understat.get_team_stats(team_name, season)
        final = pd.DataFrame({'formation':[team_stats['formation'][key]['stat'] for key in team_stats['formation'].keys()],
              'minute':[team_stats['formation'][key]['time'] for key in team_stats['formation'].keys()],
              'shots':[team_stats['formation'][key]['shots'] for key in team_stats['formation'].keys()],
              'goals':[team_stats['formation'][key]['goals'] for key in team_stats['formation'].keys()],
              'xG':[team_stats['formation'][key]['xG'] for key in team_stats['formation'].keys()],
              'shots_a':[team_stats['formation'][key]['against']['shots'] for key in team_stats['formation'].keys()],
              'goals_a':[team_stats['formation'][key]['against']['goals'] for key in team_stats['formation'].keys()],
              'xGA':[team_stats['formation'][key]['against']['xG'] for key in team_stats['formation'].keys()]})
        final['season']=season
        final['team']=team_name
        final = final[['season','team','formation','minute','shots','goals','xG','shots_a','goals_a','xGA']]
    return final


# In[34]:


agg={}
for team in team_list:
    for year in year_list:
        agg[team+str(year)]=asyncio.run(get_team_stats(team,year))
    print(team)


# In[35]:


team_stats_df = pd.concat(agg,ignore_index=True)
team_stats_df.to_csv("team_stats_df.csv",index=False)


# In[20]:


# import os
# os.chdir('C:\\Users\\PC\\Documents\\축구\\soccer-spi')
# five = pd.read_csv("spi_matches.csv")
# five = five.loc[five['league']=='Barclays Premier League']
# five = five.loc[(five['date']>='2019-08-08') & (five['date']<='2020-05-14')].reset_index().drop(['index'],axis=1)
# five['prob1'].corr(five['score1']>five['score2'])
# five=five.rename(columns={'score1':'home_score','score2':'away_score','prob1':'home_prob','prob2':'away_prob','probtie':'draw_prob'})
# five['result'] = np.where(five['home_score']>five['away_score'],'home',np.where(five['home_score']==five['away_score'],'draw','away'))
# five['home_prob']=five['home_prob'].astype(float)
# five['away_prob']=five['away_prob'].astype(float)
# five['draw_prob']=five['draw_prob'].astype(float)
# five['home_score']=five['home_score'].astype(float)
# five['away_score']=five['away_score'].astype(float)
# five['pred']=five[['home_prob','away_prob','draw_prob']].idxmax(axis=1).str.slice(0,4)
# (five['pred']==five['result']).mean()

