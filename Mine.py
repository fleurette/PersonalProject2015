def getTweets(api, person):	
  allTweets = []	
  newTweets = api.user_timeline(screen_name=person.screen_name,count=200)
  
  allTweets.extend(newTweets)
  oldest = alltweets[-1].id - 1
  
  while len(new_tweets) > 0:
    newTweets = api.user_timeline(screen_name=person.screen_name,count=200,max_id=oldest)
    allTweets.extend(new_tweets)
    oldest = alltweets[-1].id - 1
  
  return alltweets

def getUser(api, person)
  return api.get_user(person.screen_name)
