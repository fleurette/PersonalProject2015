import time
import tweepy

throttleTime = 5

# Date is the minimum date we want to reach when mining tweets
def getTweets(api, account):	
  try:
    time.sleep(throttleTime)
    allTweets = []	
    newTweets = api.user_timeline(screen_name=account["_id"],count=200)
    allTweets.extend(newTweets)
    oldest = allTweets[-1].id - 1

    # Extract date of oldest tweet
    oldestDate = allTweets[-1].created_at

    while len(newTweets) > 0:
      time.sleep(throttleTime)
      newTweets = api.user_timeline(screen_name=account["_id"],count=200,max_id=oldest)
      allTweets.extend(newTweets)
      oldest = allTweets[-1].id - 1
      # Extract date of oldest tweet
      oldestDate = allTweets[-1].created_at

    # Compare dates
    if (oldestDate > account['dob']):
      return False
    else:
      return allTweets

  except Exception as e:
    if e.reason == "[{u'message': u'Rate limit exceeded', u'code': 88}]":
      print "Rate limit exceeded, going to sleep for five minutes"
      time.sleep(60*5) #Sleep for 5 minutes
      return getTweets(api, account)

def getUser(api, person):
  try:
    time.sleep(throttleTime)
    return api.get_user(person["_id"])
  except Exception as e:
    if e.reason == "[{u'message': u'Rate limit exceeded', u'code': 88}]":
      print "Rate limit exceeded, going to sleep for five minutes"
      time.sleep(60*5) #Sleep for 5 minutes
      return getUser(api, person)
