#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd
fivethirtyeight_pred = pd.read_csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv")


# In[2]:


fivethirtyeight_pred.head()


# In[6]:


fivethirtyeight_pred.columns


# In[4]:


fivethirtyeight_pred = fivethirtyeight_pred[['league','date','team1','team2','spi1','spi2','prob1','probtie','prob2','proj_score1','proj_score2','importance1','importance2','score1','score2','xg1','xg2','nsxg1','nsxg2', 'adj_score1', 'adj_score2']]


# In[5]:


fivethirtyeight_pred.rename(columns = {'team1':'home_team',
                                       'team2':'away_team',
                                       'spi1':'spi_home',
                                       'spi2':'spi_away',
                                       'prob1':'prob_home',
                                       'prob2':'prob_away',
                                       'probtie':'prob_tie',
                                       'proj_score1':'pred_home_score',
                                       'proj_score2':'pred_away_score',
                                       'importance1':'importance_home',
                                       'importance2':'importance_away',
                                       'score1':'home_score',
                                       'score2':'away_score',
                                       'xg1':'xg_home',
                                       'xg2':'xg_away',
                                       'nsxg1':'nsxg_home',
                                       'nsxg2':'nsxg_away',
                                       'adj_score1' : 'adj_score_home',
                                       'adj_score2' : 'adj_score_away'}, inplace=True)


# In[7]:


fivethirtyeight_pred.to_csv("fivethirtyeight_df.csv",index=False)

