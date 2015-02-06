from pymongo import MongoClient

def initialiseDB():
  # Read DBCredential file
  with open('DBCredentials.dat') as f:
    content = f.read().splitlines()
  
  client = MongoClient(content[3], int(content[2]))
  
  return client
