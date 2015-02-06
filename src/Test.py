import tweepy
from SelectedData import *
from APIManagement import *
from DataCurating import *
from DBManagement import *

X = 1

# Get API object
api = getAPI()
# For each profiles
for person in persons[0:X]:
  # Mine data, strip it
  user = extractProfile(getUser(api, person),person)
  tweets = extractTweets(getTweets(api, person))
  # Record stuff in database
  

