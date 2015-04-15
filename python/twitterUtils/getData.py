import time
import tweepy

breakTime = 6*50

# Date is the minimum date we want to reach when mining tweets
def getTweets(api, accountId):	
  try:
    allTweets = []
    newTweets = [None]

    while len(newTweets) > 0:
      newTweets = api.user_timeline(screen_name=accountId,count=200,max_id=oldest)
      allTweets.extend(newTweets)
      oldest = allTweets[-1].id - 1

    return allTweets

  except Exception as e:
    print "Rate limit exceeded, going to sleep"
    time.sleep(breakTime)
    return getData(api, account)
