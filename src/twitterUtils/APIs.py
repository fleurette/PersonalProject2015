import tweepy

accounts = [
{ 
  consumerKey:"41Nq7mtmT0DxMrY5yjpBH0b6a",
  consumerSecret:"2qvk6Qkd4wcLGx72QhIj1gKPfgK0BPuu3cCl3KU3sSl1uq8P6i",
  accessKey:"2976517792-kZ7JzXAHH0xww3DAlpDYAT71x0CkmcvKBMlFEeR",
  accessSecret:"Uv16uH2l8AEL1ywEdUZAxd6II7K9CsqteOHKGYH0kwId6"
}
]

def getAPIS():
  return [getAPI(account) for account in accounts]

def getAPI(account):
  auth = tweepy.OAuthHandler(account["consumerKey"], account["consumerSecret"])
  auth.set_access_token(account["accessKey"], account["accessSecret"])
  api = tweepy.API(auth)
  
  return api
