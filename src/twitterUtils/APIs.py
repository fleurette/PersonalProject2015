import tweepy

accounts = [
{ 
  "consumerKey":"",
  "consumerSecret":"",
  "accessKey":"",
  "accessSecret":""
}
]

def getAPIS():
  return [getAPI(account) for account in accounts]

def getAPI(account):
  auth = tweepy.OAuthHandler(account["consumerKey"], account["consumerSecret"])
  auth.set_access_token(account["accessKey"], account["accessSecret"])
  return tweepy.API(auth)
