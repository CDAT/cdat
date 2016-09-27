
import vcs,numpy,cdms2,MV2,os,sys
import vcs.testing.regression as regression

wmo = ['w00', 'w01', 'w02', 'w03', 'w04', 'w05', 'w06', 'w07', 'w08', 'w09',
       'w10', 'w11', 'w12', 'w13', 'w14', 'w15', 'w16', 'w17', 'w18', 'w19',
       'w20', 'w21', 'w22', 'w23', 'w24', 'w25', 'w26', 'w27', 'w28', 'w29',
       'w30', 'w31', 'w32', 'w33', 'w34', 'w35', 'w36', 'w37', 'w38', 'w39',
       'w40', 'w41', 'w42', 'w43', 'w44', 'w45', 'w46', 'w47', 'w48', 'w49',
       'w50', 'w51', 'w52', 'w53', 'w54', 'w55', 'w56', 'w57', 'w58', 'w59',
       'w60', 'w61', 'w62', 'w63', 'w64', 'w65', 'w66', 'w67', 'w68', 'w69',
       'w70', 'w71', 'w72', 'w73', 'w74', 'w75', 'w76', 'w77', 'w78', 'w79',
       'w80', 'w81', 'w82', 'w83', 'w84', 'w85', 'w86', 'w87', 'w88', 'w89',
       'w90', 'w91', 'w92', 'w93', 'w94', 'w95', 'w96', 'w97', 'w98', 'w99',
       'w100', 'w101', 'w102']

x = regression.init()

m = x.createmarker()
M=7
m.worldcoordinate=[0,M,0,M]
m.type = wmo
m.color=[242,]
m.size=[10.,]
xs = []
ys=[]
for Y in range(7):
  for X in range(15):
    ys.append([M-M*(Y+1)/8.,])
    xs.append([M*(X+1)/16.,])
m.x = xs
m.y = ys
m.list()
x.plot(m, bg=1)
regression.run(x, "wmo_markers.png");

