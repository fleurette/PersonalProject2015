import time
import tweepy

breakTime = 6*50

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
    print "Rate limit exceeded, going to sleep"
    print e
    time.sleep(breakTime)
    return getTweets(api, accountId)
