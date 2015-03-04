import dbUtils
import json
import dateutil.parser

directory = '../csvDataSheets/'
males = directory + 'twitter_accounts_males.csv'
females = directory + 'twitter_accounts_females.csv'

# Get database
try:
  dbInterface = dbUtils.dbInterface("../dbCredentials.dat")
  print "Database correctly initialized"
except Exception as e:
  print "Program crashed during database initialisation. Exiting."
  sys.exit()

# Get data, assume same fields for males and females
with open(males) as f:
  content = f.read().splitlines()
  fields = content[0].split(',')
  maleAccounts = content[1:]
with open(females) as f:
  femaleAccounts = f.read().splitlines()[1:]

# Insert every account into users collection
for account in femaleAccounts:
  query = {}
  values = account.split(',')
  # Build query string
  for idx,field in enumerate(fields):
    query[field] = values[idx]
  query['gender'] = 'F'
  query['dob'] = dateutil.parser.parse(query['dob'],dayfirst=True)
  # Insert into database
  try:
    dbInterface.writeAccount(query)
    print "Succesfully added user " + query['_id'] + " to the database."
  except Exception as e:
    print e

for account in maleAccounts:
  query = {}
  values = account.split(',')
  # Build query string
  for idx,field in enumerate(fields):
    query[field] = values[idx]
  query['gender'] = 'M'
  query['dob'] = dateutil.parser.parse(query['dob'],dayfirst=True)
  # Insert into database
  try:
    dbInterface.writeAccount(query)
    print "Succesfully added user " + query['_id'] + " to the database."
  except Exception as e:
    print e
