def compileData(rawData,account):
  result = dict()
  result["tweets"] = map(lambda(tweet):tweet.created_ad, tweets)
  result["_id"] = account["_id"]
  result["gender"] = account["gender"]
  result["dob"] = account["dob"]

  return result
