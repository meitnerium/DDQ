#Import modules
import numpy as np
import matplotlib.pyplot as plt

#Draw eigenvalues on potential surface
x=np.loadtxt('+pot.dat',usecols=[0])
v=np.loadtxt('+pot.dat',usecols=[1])
n=np.loadtxt('eigenvalues.dat',usecols=[0])
e=np.loadtxt('eigenvalues.dat',usecols=[1])
n=n.tolist()
e=e.tolist()
x=x.tolist()
v=v.tolist()
plt.plot(x,v,'b')
for i in range(len(n)):
  horizx=[]
  horize=[]
  for j in range(len(x)):
    if (e[i]>v[j]):
      horizx.append(x[j])
      horize.append(e[i])
  print('Drawing eigenstate %d/%d: %3d%%'%(i,len(n)-1,int(1.e2*float(i)/float(len(n)-1))))
  plt.scatter(horizx,horize,color='r',s=0.00005)
plt.title('Eigenvalues on Potential')
plt.xlabel('Internuclear distance (Bohrs)')
plt.ylabel('Energy (Hartree)')
#plt.ylim(1.1*min(v),0.9*v[len(v)-1])
plt.tight_layout()
plt.savefig('pot+eivals.png')
plt.close()

#Draw eigenfunctions
dx=x[2]-x[1]
for i in range(len(n)):
  y=np.loadtxt('psi%05d.dat'%i,usecols=[1])
  y=y.tolist()
#Normalize
  integral=0.e0
  for j in range(len(y)):
    integral=integral+((y[j]**2.e0)*dx)
  for j in range(len(y)):
    y[j]=y[j]/np.sqrt(integral)
  plt.plot(x,y,'r')
  plt.title('${\Psi}_{%5d}$'%i)
  plt.xlabel('Internuclear distance (Bohrs)')
  plt.ylabel('Intensity (a.u.)')
  plt.tight_layout()
  plt.savefig('psi%05d.png'%i)
  plt.close()
  print('Drawing eigenfunctions %d/%d: %3d%%'%(i,len(n)-1,int(1.e2*float(i)/float(len(n)-1))))
