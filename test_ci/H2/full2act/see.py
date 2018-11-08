import numpy as np
import matplotlib.pyplot as plt

x=np.loadtxt('cis.dat',usecols=[0])
y=np.loadtxt('cis.dat',usecols=[1])
plt.plot(x,y)
plt.ylim(-1.5,-0.7)
plt.savefig('see.png')
plt.close()
