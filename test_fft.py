import numpy as np
import matplotlib.pyplot as plt
x = np.linspace(-5,5,1024)
gauss = np.exp(-x**2)
plt.plot(x,np.abs(gauss)**2)
plt.show()

gauss_four = np.fft.fft(gauss)
#xconj = np.fft.fftfreq(x)
plt.plot(np.abs(gauss_four)**2)
