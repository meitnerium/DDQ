import numpy as np
import matplotlib.pyplot as plt

x=np.arange(9812)
y1=np.loadtxt('h2+core/full2act/cis.dat',usecols=[1])
z1=np.loadtxt('h2+core/full2act/cis.dat',usecols=[2])
y2=np.loadtxt('H2+/full2act/cis.dat',usecols=[1])
z2=np.loadtxt('H2+/full2act/cis.dat',usecols=[2])
plt.plot(x,z1,x,z2)
plt.ylim(-0.5,0)
plt.show()
