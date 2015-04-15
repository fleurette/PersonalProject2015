def compileData(rawTweets,account):
  result = dict()
  result["_id"] = account["_id"]
  result["dob"] = account["dob"]
  result["gender"] = account["gender"]
  result["tweetTimes"] = map(lambda(tweet):tweet.created_at, rawTweets)

  return result
