import pprint
import sys
import dataManagement
import crawler
import DB

## Get API object
api = crawler.getAPI()
## Get db
try:
  db = DB.initialiseDB('../DBCredentials.dat')
  print "DB correctly initialized"
except Exception as e:
  print "Program crashed during database initialisation. Exiting."
  sys.exit()
 
for person in dataManagement.persons:
  print "Treating user" + person.screenName
  if DB.existsUser(db, person.screenName):
    print "User already stored in database"
  else:
    print "Starting data collection"
    try:
      user = dataManagement.extractProfile(crawler.getUser(api, person),person)
      tweets = dataManagement.extractTweets(crawler.getTweets(api, person))
      print "Information correctly collected for user " + person.screenName
      try:
        DB.writeUser(db, user, tweets)
        print "Information correctly recorded for user " + person.screenName
      except Exception as e:
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
