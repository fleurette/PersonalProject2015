from pymongo import MongoClient

class DBInterface:
  def __init__(self, credentials):
    # Read DBCredential file
    with open(credentials) as f:
      content = f.read().splitlines()
    client = MongoClient(content[3], int(content[2]))
    database = client[content[4]]
    self.profiles = database[content[5]]
    self.maleData = database[content[6]]
    self.femaleData = database[content[7]]

  def writeUser(self, user, tweets):
    data = user.__dict__
    data['_id'] = user.screenName
    data['tweets'] = map(lambda x:x.__dict__, tweets)
  
    self.profiles.insert(data)
  
  def getUser(self, screenName):
    return self.profiles.find({"_id": screenName})
  
  def existsUser(self, screenName):
    return self.profiles.find({"_id": screenName}).count()
  
  def getProfiles(self):
    return (self.profiles).find()

  def writeData(self,data):
    if(data["gender"] == "M"): 
      self.maleData.insert(data)
    else:
      self.femaleData.insert(data)
