import dbUtils

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
  fields = content[0]
  accounts = content[1:]
with open(females) as f:
  accounts += f.read().splitlines()[1:]

# Insert every account into users collection
for account in accounts:
  # Build query string
  values = map(lambda x: '"' + x '"', + accounts.split(','))
  queryString = '{' + ','.join([field+':'+value for(field,value) in zip(fields,values)])  + '}'
  # Insert into database
  try:
    dbInterface.writeUser(queryString)
  except Exception as e:
    print e
