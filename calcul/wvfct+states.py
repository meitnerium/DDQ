import numpy as np
import matplotlib.pyplot as plt

x=np.loadtxt('ks_morse/k0/wvfct.dat',usecols=[0])
for i in range(11):
  yini=np.loadtxt('ks_morse/k%d/wvfct.dat'%i,usecols=[1])
  yini2=yini**2.0e0
  norm=np.trapz(yini2)
  yini=yini/np.sqrt(norm)
  yini=yini**2.0e0
  for j in range(16):
    yetat=np.loadtxt('leivec.dat',usecols=[j+1])
    yetat2=yetat**2.0e0
    norm=np.trapz(yetat2)
    yetat=yetat/np.sqrt(norm)
    yetat=yetat**2.0e0
    plt.plot(x,yini,label='chi0*cos(kr), k=%d'%i)
    plt.plot(x,yetat,label='Etat %d'%j)
    plt.legend()
    plt.title('k=%d, Etat %d'%(i,j))
    plt.xlim(0,10)
    plt.savefig('overlap/%02d%02d.png'%(i,j))
    plt.close()
