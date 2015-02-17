import time
import tweepy

def getTweets(api, person):	
  try:
    allTweets = []	
    newTweets = api.user_timeline(screen_name=person.screenName,count=200)
    allTweets.extend(newTweets)
    oldest = allTweets[-1].id - 1

    print "Fetched " + str(len(newTweets)) + " tweets. Currently have " + str(len(allTweets))
    
    while len(newTweets) > 0:
      newTweets = api.user_timeline(screen_name=person.screenName,count=200,max_id=oldest)
      allTweets.extend(newTweets)
      oldest = allTweets[-1].id - 1
      print "Fetched " + str(len(newTweets)) + " tweets. Currently have " + str(len(allTweets))
  
    return allTweets

  except Exception as e:
    if e.reason == "[{u'message': u'Rate limit exceeded', u'code': 88}]":
      print "Rate limit exceeded, going to sleep for five minutes"
      time.sleep(60*5) #Sleep for 5 minutes
      return getTweets(api, person)

def getUser(api, person):
  try:
    return api.get_user(person.screenName)
  except Exception as e:
    if e.reason == "[{u'message': u'Rate limit exceeded', u'code': 88}]":
      print "Rate limit exceeded, going to sleep for five minutes"
      time.sleep(60*5) #Sleep for 5 minutes
      return getUser(api, person)
