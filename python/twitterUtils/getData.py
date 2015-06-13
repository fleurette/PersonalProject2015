import time
import tweepy

breakTime = 10*50

# Date is the minimum date we want to reach when mining tweets
def getTweets(api, accountId):	
  try:
    allTweets = []
    newTweets = api.user_timeline(screen_name=accountId,count=200)
    allTweets.extend(newTweets)

    while len(newTweets) > 0:
      newTweets = api.user_timeline(screen_name=accountId,count=200,max_id=allTweets[-1].id - 1)
      allTweets.extend(newTweets)

    return allTweets

  except Exception as e:
    if(str(e)=="Not authorized."):
      raise Exception(accountId + " is a private profile, skipping.")
    print "Rate limit exceeded, going to sleep while treating " + accountId
    print e
    time.sleep(breakTime)
    return getTweets(api, accountId)

def getUser(api,accountId,profile):
  try:
    user = api.get_user(accountId)
    result = dict()
    result["_id"] = accountId
    result["gender"] = profile["gender"]
    result["followers_count"] = user.followers_count
    result["lang"] = user.lang
    result["friends_count"] = user.friends_count
    result["statuses_count"] = user.statuses_count
    result["location"] = user.location
    result["followers_count"] = user.followers_count
    result["dob"] = profile["dob"]
    result["time_zone"] = user.time_zone
    return result
  except Exception as e:
    if(str(e)=="Not authorized."):
      raise Exception(accountId + " is a private profile, skipping.")
    print "Rate limit exceeded, going to sleep while treating " + accountId
    print e
    time.sleep(breakTime)
    return getUser(api, accountId)
