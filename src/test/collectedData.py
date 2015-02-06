import unittest
import src.dataManagement as dataManagement

class testCollectedData(unittest.TestCase):

  # There should be data in the file, data should be converted
  def testPersons(self):
    self.assertTrue(len(dataManagement.selectedPersons) > 0)
    self.assertTrue(len(dataManagement.persons) > 0)

  # Person class should have a field for name and gender
  def testPersonClass(self):
    selectedPerson = dataManagement.selectedPersons[0]
    person = dataManagement.Person(selectedPerson)
   
    self.assertEqual(person.screenName, selectedPerson[0])
    self.assertEqual(person.gender, selectedPerson[1])

if __name__ == "__main__":
  unittest.main()
