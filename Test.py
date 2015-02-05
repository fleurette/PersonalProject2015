import tweepy

from Data import *
from Keys import *

auth = tweepy.OAuthHandler(consumerKey, consumerSecret)
auth.set_access_token(accessToken, accessSecret)

api = tweepy.API(auth)

test = tweepy.api.user_timeline("imp_proj_01")

for (person,gender) in persons:
  

