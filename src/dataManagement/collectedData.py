class Person:
  def __init__(self,data):
    self.screenName = data[0]
    self.gender = data[1]

def test():
  print "helloe"

selectedPersons = [
  ('imp_proj_01','M')
  ('AdamDreece','M')
  ('AhmadK111','M'),
  ('AndrewCrigler','M'),
  ('antgentile','M'),
  ('AramRawf','M'),
  ('AsvrilaRhmnila','F'),
  ('Betzha_','F'),
  ('bloateddwarf','M'),
  ('CharlieCW','F'),
  ('crossroads03','M'),
  ('Delii_Nolove','F'),
  ('effortlessOne','M'),
  ('GeeGeeWong','F'),
  ('Hanee253','F'),
  ('hasifasro','M'),
  ('Ifellforhimhard','F'),
  ('IfyisSpiffy','F'),
  ('ImranKhanPTI','M'),
  ('isabel_caba','F'),
  ('LeggoMyMeggoYo','F'),
  ('maninblack53','M'),
  ('Mark_J_Perry','M'),
  ('MartinaMelluso','F'),
  ('MattZeitlin','M'),
  ('misslanta','F'),
  ('mrstevenallen','M'),
  ('nastyshot88','M'),
  ('pamelasantis_','F'),
  ('primawesome','M'),
  ('rioguzman1','M'),
  ('safkelebek','F'),
  ('SerenaJojo','F'),
  ('TeresaFCG','F'),
  ('WMarquia','F'),
  ('XpressoReads','F'),
  ('manoiret','F'),
  ('ritikabajaj24','F'),
  ('Brittany22870','F'),
  ('alllfyalff','F'),
  ('WendyCOrtiz','F'),
  ('DalenFlynn','F'),
  ('LaSharaMcDonald','F'),
  ('NathanDyer24','M'),
  ('smod123','M'),
  ('BigDawGDotCom','M'),
  ('badpunkigirl','F'),
  ('DRINADRIN','F'),
  ('Amy_Lou_Who24','F'),
  ('Indacc','M'),
  ('NathynMasters','M'),
  ('sitinurhidayu16','F'),
  ('MissMuliru','F'),
  ('tbenas','F'),
  ('Garry__Gaga','M'),
  ('GenevaDabu','F'),
  ('JamilahMusic','F'),
  ('StacyJCarlson','F'),
  ('RealHubbard','F'),
  ('strngarmJOKER','M'),
  ('GarMannL','F'),
  ('cali_kev63','M'),
  ('aylaysa','F'),
  ('Asen_Chi_Own','F'),
  ('Aleta_Pardalis','F'),
  ('athikav','F'),
  ('MrsLyndseyWard','F'),
  ('_cagesmith','M'),
  ('KatieGinga','F')
]

persons = []
for person in selectedPersons:
  persons.append(Person(person))
