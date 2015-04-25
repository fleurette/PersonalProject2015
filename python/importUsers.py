import dbUtils
import json
import dateutil.parser

basePath = 'csvDataSheets/'
malePath = basePath + 'twitter_profiles_males.csv'
femalePath = basePath + 'twitter_profiles_females.csv'

# Get database
try:
  dbInterface = dbUtils.dbInterface("../dbCredentials.dat")
except Exception as e:
  print "Program crashed during database initialisation. Exiting."
  print e
  sys.exit()

def importProfiles(profiles,fieldNames,gender):
  for profile in profiles:
    result = {}
    values = profile.split(',')
    # Build query string
    for idx,field in enumerate(fieldNames):
      result[field] = values[idx]
    result['gender'] = gender
    if(len(result['dob']) is not 0):
      result['dob'] = (dateutil.parser.parse(result['dob'],dayfirst=True)).strftime('%m/%d/%Y')
    try:
      dbInterface.writeProfile(result)
      print "Added user " + result['_id'] + " to the database."
    except Exception as e:
      print "Error when adding user " + result['_id'] + " to the database."
      print e

# Get data, assume same fields for males and females
with open(malePath) as f:
  fileContent = f.read().splitlines()
  importProfiles(fileContent[1:],fileContent[0].split(','),'M')
with open(femalePath) as f:
  fileContent = f.read().splitlines()
  importProfiles(fileContent[1:],fileContent[0].split(','),'F')
