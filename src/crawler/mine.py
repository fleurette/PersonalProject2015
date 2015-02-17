import time

def getTweets(api, person):	
  allTweets = []	
  checkLimit(api)
  newTweets = api.user_timeline(screen_name=person.screenName,count=200)
  
  allTweets.extend(newTweets)
  oldest = allTweets[-1].id - 1
  
  while len(newTweets) > 0:
    checkLimit(api)
    newTweets = api.user_timeline(screen_name=person.screenName,count=200,max_id=oldest)
    allTweets.extend(newTweets)
    oldest = allTweets[-1].id - 1
  
  return allTweets

def getUser(api, person):
  return api.get_user(person.screenName)

def checkLimit(api):
  time.sleep(1)
  remaining = api.rate_limit_status()
  timelineLookup = remaining["resources"]["statuses"]["/statuses/user_timeline"]["remaining"]
  userLookup = remaining["resources"]["users"]["/users/show/:id"]["remaining"]
  print str(timelineLookup) + " remaining API calls for timeline lookUp."
  print str(userLookup) + " remaining API calls for user lookUp."
  if (timelineLookup <= 2) or (userLookup <= 2):
    print "API Limit has been reached"
    print "Going to sleep for ten minutes before retstarting mining operations"
    time.sleep(600)
