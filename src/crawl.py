import pprint
import time
import sys
import dataManagement
import crawler
import DB

## Get API object
api = crawler.getAPI()
## Get DB
try:
  interface = DB.DBInterface("../DBCredentials.dat")
  print "DB correctly initialized"
except Exception as e:
  print "Program crashed during database initialisation. Exiting."
  print e
  sys.exit()
 
for person in dataManagement.persons:
  print "Treating user " + person.screenName
  if interface.existsUser(person.screenName):
    print "User already stored in database"
  else:
    print "Starting data collection"
    try:
      user = dataManagement.extractProfile(crawler.getUser(api, person),person)
      tweets = dataManagement.extractTweets(crawler.getTweets(api, person))
      data = dataManagement.extractData(user,tweets)
      print "Information correctly collected for user " + person.screenName
      try:
        interface.writeUser(user, tweets)
        interface.writeData(data)
        print "Information correctly recorded for user " + person.screenName
      except Exception as e:
        # Abort process and delete all written information
        interace.deleteAll(user["screenName"])
        print "Failed to write information for user " + person.screenName + ". Error was:"
        print e
        print "Skipping ...."
        pass
    except Exception as e:
      print "Failed to collect information for user " + person.screenName + ". Error was:"
      print e
      print "Skipping ...."
      pass

print "All data has been treated"
