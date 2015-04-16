import threading
import pprint
import time
import sys
import dbUtils
import dataUtils
import twitterUtils

# Get database
try:
  dbInterface = dbUtils.dbInterface("../dbCredentials.dat")
  print "Database correctly initialized"
except Exception as e:
  print "Program crashed during database initialisation. Exiting."
  sys.exit()

# Define thread mining function
def mineAccounts(profiles,dbInterface,api): 
  for profile in profiles:
    profileId = profile["_id"]
    if not dbInterface.existsData(profileId):
      try:
        print "\nStarting data collection for " + profileId
        rawTweets = twitterUtils.getTweets(api, profileId)
        data = dataUtils.compileData(rawTweets,profile)
        dbInterface.writeData(data)
        print "\nData collected and saved for " + profileId
      except Exception as e:
        print "Caught exception"
        print e
    else:
      print "\n" + profileId + " already exists in database, skipping..."
    dbInterface.deleteProfile(profileId)



def pop(array, n):
  result = []
  for i in range(min(n,len(array))):
    result.append(array.pop())
  return result

# Configurations
threads = [{"is_alive": (lambda: False), "api":api} for api in twitterUtils.getAPIs()]
cycleLength = 60*20
maxAccounts = 30

# Get persons from collected data
while(True):
  # If all threads are unactive and users is empty, query the database
  noneAlive = (reduce(lambda t1,t2: (not t1["is_alive"]())  and (not t2["is_alive"]()), threads))
  if(noneAlive):
    profiles = [profile for profile in dbInterface.getProfiles()]
    for thread in threads:
      threadProfiles = pop(profiles,maxAccounts)
      _thread = threading.Thread(target=mineAccounts, args=(threadProfiles, dbInterface, thread["api"]))
      _thread.start()
      thread["is_alive"] = _thread.is_alive
  time.sleep(cycleLength)
