def getTweets(api, person):	
  allTweets = []	
  newTweets = api.user_timeline(screen_name=person.screenName,count=200)
  
  allTweets.extend(newTweets)
  oldest = allTweets[-1].id - 1
  
  while len(newTweets) > 0:
    newTweets = api.user_timeline(screen_name=person.screenName,count=200,max_id=oldest)
    allTweets.extend(newTweets)
    oldest = allTweets[-1].id - 1
  
  return allTweets

def getUser(api, person):
  return api.get_user(person.screenName)
