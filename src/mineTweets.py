import thread
import sys
import dataManagement
import crawler
import DB

## Get API object
api = crawler.getAPI()

## Get database
try:
  dbInterface = DB.DBInterface("../DBCredentials.dat")
  print "Database correctly initialized"
except Exception as e:
  print "Program crashed during database initialisation. Exiting."
  sys.exit()

def mineAccounts(persons,dbInterface): 
  for person in persons:
    if not dbInterface.existsUser(person.screenName):
      try:
        print ""
        print "Treating user " + person.screenName
        print "Starting data collection"

        user = dataManagement.extractProfile(crawler.getUser(api, person),person)
        tweets = dataManagement.extractTweets(crawler.getTweets(api, person))
        data = dataManagement.extractData(user,tweets)

        dbInterface.writeUser(user, tweets)
        dbInterface.writeData(data)
        print "Information correctly collected and recorded for user " + person.screenName
      except Exception as e:
        interace.deleteAll(person.screenName)
        print "Failed to collect information for user " + person.screenName + ". Error was: " + e 

# Every twenty seconds get person names to mine, distribute them across APIs

print "All data has been treated"
