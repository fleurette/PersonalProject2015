import tweepy

consumerKey = ""
consumerSecret = ""
accessKey = ""
accessSecret = ""

def getAPI():
  auth = tweepy.OAuthHandler(consumerKey, consumerSecret)
  auth.set_access_token(accessKey, accessSecret)
  api = tweepy.API(auth)
  
  return api
