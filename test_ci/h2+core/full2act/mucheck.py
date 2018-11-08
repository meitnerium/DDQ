import numpy as np
import matplotlib.pyplot as plt

y=np.loadtxt('mus.dat',usecols=[0])
x=range(9812)
plt.plot(x,y)
#plt.ylim(0,10**(-15))
plt.savefig('mucheck.png')
plt.close()
