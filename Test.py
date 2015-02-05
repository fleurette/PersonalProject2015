import tweepy

from Data import *
from Keys import *

auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_token_secret)

api = tweepy.API(auth)

test = tweepy.api.user_timeline("imp_proj_01")
print 42


