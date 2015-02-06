def extractTweets(tweets):
  strippedTweets = []
  
  for tweet in tweets:
    strippedTweets.append(Tweet(tweet))

  return strippedTweets

def extractProfile(user, person):
  return Profile(user, person)

class Tweet:
  def __init__(self, tweet):
    self.ID = tweet.id
    self.creationTime = tweet.created_at
    self.truncated = tweet.truncated
    self.retweeted = tweet.retweeted

class Profile:
  def __init__(self, user, person):
    self.screenName = user.screen_name
    self.name = user.name
    self.location = user.location
    self.protected = user.protected
    self.timeZone = user.time_zone
    self.utcOffset = user.utc_offset
    self.statusesCount = user.statuses_count
    self.followersCount = user.followers_count
    self.friendsCount = user.friends_count
    self.language = user.lang
    self.createdAt = user.created_at
    self.gender = person.gender
