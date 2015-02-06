from pymongo import MongoClient

def initialiseDB(Credentials):
  # Read DBCredential file
  with open(Credentials) as f:
    content = f.read().splitlines()
  
  client = MongoClient(content[3], int(content[2]))
  db = client.test
  
  return db.collection

def writeUsersDB(db, user, tweets):
  data = user.__dict__
  data['tweets'] = tweets.__dict__

  db.insert(data)
