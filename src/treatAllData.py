import pprint
import sys
import dataManagement
import crawler
import DB

# Get DB
try:
  interface = DB.DBInterface("../DBCredentials.dat")
  print "DB correctly initialized"
except Exception as e:
  print "Program crashed during database initialisation. Exiting."
  print e
  sys.exit()

# Iterate over each person in database
persons = interface.getProfiles()
for person in persons:
  try:
    data = {}
    data["gender"] = person["gender"]
    data["interTweets"] = dataManagement.interTweetDistribution(person["tweets"])
    data["tweetTimes"] = dataManagement.getTweetTimes(person["tweets"])
    data["mean"] = dataManagement.mean(data["interTweets"])
    data["_id"] = person["_id"]
    interface.writeData(data)
  except Exception as e:
    print "Gotcha"
