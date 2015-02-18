import thread
import numpy
import sys
import dbUtils
import dataUtils
import twitterUtils
from math import ceil

# Get database
try:
  dbInterface = dbUtils.dbInterface("../dbCredentials.dat")
  print "Database correctly initialized"
except Exception as e:
  print "Program crashed during database initialisation. Exiting."
  print e
  sys.exit()

APIs = twitterUtils.getAPIs()
users = numpy.array([user for user in dbInterface.getUsers()])

# Write user
minimumIndex = int((1+ceil(len(users))/len(APIs)))
splitIndices = range(minimumIndex,len(users),minimumIndex)
queries = zip(APIs, numpy.split(users,splitIndices))

print splitIndices
print queries[0]
print queries[1]

