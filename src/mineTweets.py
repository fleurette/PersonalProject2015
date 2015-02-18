import thread
import sys
import dbInterface
import dataTreatment
import twitterUtils

# Get database
try:
  dbInterface = dbInterface.dbInterface("../dbCredentials.dat")
  print "Database correctly initialized"
except Exception as e:
  print "Program crashed during database initialisation. Exiting."
  sys.exit()

# Define thread mining function
def mineAccounts(persons,dbInterface): 
  for person in persons:
    if not dbInterface.existsProfile(person.screenName):
      try:
        print "\nTreating user " + person.screenName + "\nStarting data collection"

        user = dataTreatment.extractProfile(crawler.getUser(api, person),person)
        tweets = dataTreatment.extractTweets(crawler.getTweets(api, person))
        data = dataTreatment.extractData(user,tweets)

        dbInterface.writeProfile(user, tweets)
        dbInterface.writeData(data)
  
        dbInterface.deleteAccount(user)

        print "Information correctly collected and recorded for user " + person.screenName
      except Exception as e:
        dbInterface.deleteAll(person.screenName)
        print "Failed to collect information for user " + person.screenName + ". Error was: " + e 

def popFirstN(array, n):
  if(len(array) < n):
    result = array
  else:
    i = 0
    result = []
    while i < n 
      result.append(array.pop())
      i = i + 1
   
  return result

# Configurations
threads = [{"is_Alive": (lambda: False), "api":api} for api in twitterUtils.getAPIs()]
cycleLength = 60*10
maxAccounts = 15
accounts = []

# Get persons from collected data
while(true):
  # If all threads are unactive and users is empty, query the database
  if(reduce(lambda t1,t2: t1["is_Alive"]()  and t2["is_Alive"](), threads) and (not len(accounts))):
    accounts = dbInterface.Accounts()
  # Distribute users over threads
  for thread in threads:
    # If the thread is alive and there are remaining accounts
    if((not len(accounts)) and (not thread["is_Alive"]()):
      _thread = Thread(target=mineAccounts, args=(popFirstN(accounts, maxAccounts), dbInterface)
      _thread.start()
      thread["is_Alive"] = _thread.is_Alive
  
  print "Going to sleep for " + str(cycleLength) + " seconds."
  time.sleep(cycleLength)
