import tweepy #https://github.com/tweepy/tweepy
 
def initialiseApi(consumerKey, consumerSecret, accessKey, accessSecret):
	auth = tweepy.OAuthHandler(consumerKey, consumerSecret)
	auth.set_access_token(accessKey, accessSecret)
	api = tweepy.API(auth)
	
	return api
 
def get_all_tweets(api, screen_name):	
	alltweets = []	
	new_tweets = api.user_timeline(screen_name=screen_name,count=200)
	
	alltweets.extend(new_tweets)
	oldest = alltweets[-1].id - 1
	
	while len(new_tweets) > 0:
		new_tweets = api.user_timeline(screen_name = screen_name,count=200,max_id=oldest)
		alltweets.extend(new_tweets)
		oldest = alltweets[-1].id - 1

	return alltweets