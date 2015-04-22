import tweepy

accounts = [
{ 
  "consumerKey":"41Nq7mtmT0DxMrY5yjpBH0b6a",
  "consumerSecret":"2qvk6Qkd4wcLGx72QhIj1gKPfgK0BPuu3cCl3KU3sSl1uq8P6i",
  "accessKey":"2976517792-kZ7JzXAHH0xww3DAlpDYAT71x0CkmcvKBMlFEeR",
  "accessSecret":"Uv16uH2l8AEL1ywEdUZAxd6II7K9CsqteOHKGYH0kwId6"
},
{
  "consumerKey":"8cMgCMd4ltX8btFNnuJ9DdDbr",
  "consumerSecret":"kkDzxBZRWargVpWJTWLdNgNF4akZgw9b7tdOecdQTThniV9tuH",
  "accessKey":"3022041676-ONruCp9ypnceIAmVVTQv6HZh6HXSSxusrIg5ttM",
  "accessSecret":"AMEcIKQNXicNvrv5X58bkZdtZMci8BWI8UQEdOB803Rit"
},
{
  "consumerKey":"e3Andnk8YgW9EBJn9doPiFsy8",
  "consumerSecret":"VWwW9EwMS5Fir5VpqkIssnhxtODHGBhgfCKr5kWwiPe2PdmyiF",
  "accessKey":"3193014111-OPdw9vFWdDAM02xVUdVh2kkNn4pwa1Rixlos9AY",
  "accessSecret":"9zuO5fLejGooBrhMc2KxczyZfbv6WrhPkg1h3yqsHTosB"
}
]

def getAPIs():
  return [getAPI(account) for account in accounts]

def getAPI(account):
  auth = tweepy.OAuthHandler(account["consumerKey"], account["consumerSecret"])
  auth.set_access_token(account["accessKey"], account["accessSecret"])
  return tweepy.API(auth)
