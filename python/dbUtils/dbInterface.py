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
    self.femaleUsers = database[content[8]]
    self.maleUsers = database[content[9]]

  def dropAll(self):
    self.femaleData.drop()
    self.maleData.drop()
    self.profiles.drop()
    self.femaleUsers.drop()
    self.maleUsers.drop()

  def getProfiles(self):
    return self.profiles.find()

  def writeProfile(self, query):
    self.profiles.insert(query)

  def deleteProfile(self,accountId):
    self.profiles.remove({"_id":accountId})

  def writeTweets(self,tweets):
    if(tweets["gender"] == "M"): 
      self.maleData.insert(tweets)
    else:
      self.femaleData.insert(tweets)

  def writeUser(self,user):
    if(user["gender"] == "M"): 
      self.maleUsers.insert(user)
    else:
      self.femaleUsers.insert(user)

  def existsData(self,accountId):
    return bool(
      self.maleData.find({"_id": accountId}).count()
      | self.femaleData.find({"_id": accountId}).count()
    )
