def interTweetDistribution(tweets):
  result = [0]
  if(len(tweets) >= 2):
    [result.append((prev['creationTime']-cur['creationTime']).total_seconds()) for (prev,cur) in zip(tweets,tweets[1:])]
  return result

def mean(numbers):
  length = len(numbers)
  if(length>0):
    return sum(numbers)/length
  else:
    return 0

def getTweetTimes(tweets):
  return map(lambda(tweet): tweet["creationTime"], tweets)
