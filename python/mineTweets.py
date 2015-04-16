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



def popFirstN(array, n):
  i = 0
  result = []
  while i < n and len(array):
    result.append(array.pop())
    i = i + 1
   
  return result

# Configurations
threads = [{"is_alive": (lambda: False), "api":api} for api in twitterUtils.getAPIs()]
cycleLength = 60*20
maxAccounts = 15
profiles = []

# Get persons from collected data
while(True):
  # If all threads are unactive and users is empty, query the database
  if(not(reduce(lambda t1,t2: t1["is_alive"]()  and t2["is_alive"](), threads)) and (not len(profiles))):
    profiles = [profile for profile in dbInterface.getProfiles()]
    # Distribute users over threads
    for thread in threads:
      # If the thread is alive and there are remaining profiles
      if(len(profiles) and (not thread["is_alive"]())):
        selectedAccounts = popFirstN(profiles, maxAccounts)
        _thread = threading.Thread(target=mineAccounts, args=(selectedAccounts, dbInterface, thread["api"]))
        _thread.start()
        thread["is_alive"] = _thread.is_alive
  
  print "Going to sleep for " + str(cycleLength) + " seconds."
  time.sleep(cycleLength)
