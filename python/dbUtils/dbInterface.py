from pymongo import MongoClient

class dbInterface:
  def __init__(self, credentials):
    # Read DBCredential file
    with open(credentials) as f:
      content = f.read().splitlines()
    client = MongoClient(content[3], int(content[2]))
    database = client[content[4]]
    self.maleData = database[content[5]]
    self.femaleData = database[content[6]]
    self.profiles = database[content[7]]

  def dropAll(self):
    self.femaleData.drop()
    self.maleData.drop()
    self.profiles.drop()

  def getProfiles(self):
    return self.profiles.find()

  def writeProfile(self, query):
    self.profiles.insert(query)

  def deleteProfile(self,accountId):
    self.profiles.remove({"_id":accountId})

  def deleteData(self,accountId):
    self.maleData.remove({"_id": accountId})
    self.femaleData.remove({"_id": accountId})

  def writeData(self,data):
    if(data["gender"] == "M"): 
      self.maleData.insert(data)
    else:
      self.femaleData.insert(data)
