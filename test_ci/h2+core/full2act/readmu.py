import numpy as np
import matplotlib.pyplot as plt
import os

print('Preping stuff...')
xptmax=9812
mu=[]

#f=open('mus.dat','w')
for i in range(xptmax):
  z=[[0.e0 for x in range(10)] for y in range(10)]
  ci=[]
  cj=[]
  lines=open('../../src/+rhf%04d.dat'%(i+1)).readlines()
  for j in range(len(lines)):
    if (len(lines[j])==9):
      ok=''
      decort=list(lines[j])
      for n in range(8):
        ok=ok+decort[n]
      if (ok==' $VEC   '):
        a=j+1
        for k in range(2):
          decort=list(lines[a+k])
          ok=''
          for u in range(len(decort)):
            ok=ok+decort[u]
          for l in range(5):
            ci.append(float(ok[15*l+5:15*l+20]))
        for k in range(2,4):
          decort=list(lines[a+k])
          ok=''
          for u in range(len(decort)):
            ok=ok+decort[u]
          for l in range(5):
            cj.append(float(ok[15*l+5:15*l+20]))
#        print(ci,cj)
  lines=open('src/1e%04d.out'%(i+1)).readlines()
  for j in range(len(lines)):
    if (len(lines[j])==20):
      ok=''
      decort=list(lines[j])
      for n in range(19):
        ok=ok+decort[n]
      if (ok==' Z DIPOLE INTEGRALS'):
        a=j+4
        for k in range(10):
          decort=list(lines[a+k])
          ok=''
          for u in range(len(decort)):
            ok=ok+decort[u]
          for l in range(k+1):
            if (l<=4):
              z[k][l]=(float(ok[11*l+15:11*l+26]))
        a=a+13
        for k in range(5):
          decort=list(lines[a+k])
          ok=''
          for u in range(len(decort)):
            ok=ok+decort[u]
          for l in range(k+1):
            z[k+5][l+5]=(float(ok[11*l+15:11*l+26]))
#        for k in range(10):
#          print(z[k])
  for j in range(0,10):
    for k in range(j+1,10):
      z[j][k]=z[k][j]
#Write mus
  temp=0.e0
  for j in range(10):
    for k in range(10):
      temp=temp+(ci[j]*cj[k]*z[j][k])
#    print(z[j])
  mu.append(temp)
  print('Mu Reading Progression: %3d%%'%(100*(i+1)/xptmax))
#print(mu)
np.savetxt('mus.dat',np.c_[mu],fmt='%21.10e')
