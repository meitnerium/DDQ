import numpy as np

de=0.1026277
b=0.72
m=918

a=np.sqrt(2.0*m)/b
print('Matematica:')
s=[]
d=[]
for n in range(19):
  d.append(n)
  s.append(-de+(2.0*np.sqrt(de)*(0.5+n))/a-((0.5+n)**2.0)/a**2.0)
np.savetxt('eivalm.dat',np.c_[d,s],fmt='%05d%28.15e')
