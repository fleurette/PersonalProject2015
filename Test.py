import tweepy
from Data import *
from Keys import *
from DataCurating import *
from DBManagement import *

X = 1

# Get API object
api = initialiseAPI()
# For each profiles
for data in persons[0:X]:
  person = Person(data)
  user = getUser(api, person)
  tweets = getTweets(api, person)
  # Record stuff in database

