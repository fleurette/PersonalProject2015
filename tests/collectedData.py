import unittest
import os

from filename import *

class TestCollectedData(unittest.TestCase):

  def testHasData(self): 
    self.assertTrue(selectedPersons.length > 0)
    

if __name__ == '__main__':
  unittest.main()
