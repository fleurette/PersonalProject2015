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
def mineAccounts(accounts,dbInterface, api): 
 for account in accounts:
   if not dbInterface.existsProfile(account["_id"]):
     try:
       print "\nTreating user " + account["_id"] + "\nStarting data collection"

       user = dataUtils.extractProfile(twitterUtils.getUser(api, account),account)
       tweets = twitterUtils.getTweets(api, account)
       if(not tweets): 
         print "Deleting all information for account " + account["_id"]
         print "DOB before oldest tweet"
         dbInterface.deleteAll(account["_id"])
         dbInterface.deleteAccount(account["_id"])
       else:
         tweets = dataUtils.extractTweets(tweets)
         data = dataUtils.extractData(user,tweets)
         dbInterface.writeData(data)
         dbInterface.writeProfile(user, tweets)
         dbInterface.deleteAccount(account["_id"])

         print "Information correctly collected and recorded for user " + account["_id"]
     except Exception as e:
       dbInterface.deleteAll(account["_id"])
       print "Failed to collect information for user " + account["_id"] + ". Error was: "
       print str(e)

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
accounts = []

# Get persons from collected data
while(True):
  # If all threads are unactive and users is empty, query the database
  if(not(reduce(lambda t1,t2: t1["is_alive"]()  and t2["is_alive"](), threads)) and (not len(accounts))):
    accounts = [account for account in dbInterface.getAccounts()]
    # Distribute users over threads
    for thread in threads:
      # If the thread is alive and there are remaining accounts
      if(len(accounts) and (not thread["is_alive"]())):
        selectedAccounts = popFirstN(accounts, maxAccounts)
        _thread = threading.Thread(target=mineAccounts, args=(selectedAccounts, dbInterface, thread["api"]))
        _thread.start()
        thread["is_alive"] = _thread.is_alive
  
  print "Going to sleep for " + str(cycleLength) + " seconds."
  time.sleep(cycleLength)
