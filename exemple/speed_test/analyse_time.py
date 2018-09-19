import numpy as np
import matplotlib.pyplot as plt

# real time
# 48 16m55.407s
# 24 proc 22m9.867s
# 12 proc 35m46.955s
# 6 proc 63m35.402s
# 3 procs 119m33.393s
# 1 proc 341m41.929s
#
proctime1=(341*60+42)


core=[1,3,6,12,24,48]
time=[341*60+42,119*60+33,63*60+35,35*60+47,22*60+10,16*60+55]
ax1=plt.subplot(1, 1, 1)
plt.plot(core,time)
plt.xlabel("Number of cores")
plt.ylabel("Running time (sec.)")
#plt.show()
#plt.close()
ax2 = ax1.twinx()
speedup3=proctime1/(119*60+33)
speedup6=proctime1/(63*60+35)
speedup12=proctime1/(35*60+47)
speedup24=proctime1/(22*60+10)
speedup48=proctime1/(16*60+55)
print("speedup3="+str(speedup3))
print("speedup6="+str(speedup6))
print("speedup12="+str(speedup12))
print("speedup24="+str(speedup24))
print("speedup48="+str(speedup48))

ax2.plot(core,[100,speedup3/3*100,speedup6/6*100,speedup12/12*100,speedup24/24*100,speedup48/48*100],'r')
ax2.set_ylabel(r'Efficienty (%)')#,fontsize=16)
plt.show()
plt.close()

