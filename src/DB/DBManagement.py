from pymongo import MongoClient

def initialiseDB(Credentials):
  # Read DBCredential file
  with open(Credentials) as f:
    content = f.read().splitlines()
  
  client = MongoClient(content[3], int(content[2]))
  db = client[content[4]]
  return db[content[5]]
  

def writeUser(db, user, tweets):
  data = user.__dict__
  data['_id'] = user.screenName
  data['tweets'] = map(lambda x:x.__dict__, tweets)

  db.insert(data)


def existsUser(db, screenName):
  return db.find_one({"_id": screenName})
