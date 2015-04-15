import dbUtils

# Get database
try:
  dbInterface = dbUtils.dbInterface("../dbCredentials.dat")
  print "Database correctly initialized"
  dbInterface.dropAll()
  print "Dropped all collections"
except Exception as e:
  print "Received error message"
  print String(e)
  sys.exit()

