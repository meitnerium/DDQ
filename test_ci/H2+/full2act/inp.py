import numpy as np
import matplotlib.pyplot as plt
import os

print('Preping stuff...')
xptmax=9812
xmin=0.189
dx=0.001
xpt=0#don't touch
x=[]
y1=[]
y2=[]
g1=[]
u1=[]
g2=[]
u2=[]
for i in range(xptmax):
  g1.append(0.0)
  u1.append(0.0)
  g2.append(0.0)
  u2.append(0.0)
#Vectors
for i in range(xptmax):
  pot=open("rhf%04d.inp"%(i+1),"w")
  pot.write(" $BASIS GBASIS=N31 NGAUSS=6 NDFUNC=1 NPFUNC=1 $END\n")
  pot.write(" $CONTRL SCFTYP=UHF RUNTYP=ENERGY ICHARG=1 MULT=2 $END\n")
  pot.write(" $CONTRL COORD=ZMT UNITS=BOHR NPRINT=9 $END\n")
  pot.write(" $DATA \n")
  pot.write("Title\n")
  pot.write("C1\n")
  pot.write("H\n")
  x.append(xmin+(float(i)*dx))
  pot.write("H 1 %6.3f\n"%(x[i]))
  pot.write(" $END\n")
  pot.close()
  os.system("~/rungms rhf%04d > rhf%04d.out"%(i+1,i+1))
  os.system("mv /tmp/rhf%04d.dat ."%(i+1))
  os.system("rm rhf%04d.inp"%(i+1))
  os.system("rm rhf%04d.out"%(i+1))
  pot=open("1e%04d.inp"%(i+1),"w")
  pot.write(" $BASIS GBASIS=N31 NGAUSS=6 NDFUNC=1 NPFUNC=1 $END\n")
  pot.write(" $CONTRL SCFTYP=MCSCF RUNTYP=ENERGY CITYP=GENCI ICHARG=1 MULT=2 $END\n")
  pot.write(" $CONTRL COORD=ZMT UNITS=BOHR NPRINT=3 $END\n")
  pot.write(" $DET NCORE=0 NACT=2 NELS=1 NSTATE=2 $END\n")
  pot.write(" $CIGEN NCORE=0 NACT=2 NELS=1 NSTATE=2 $END\n")
  pot.write(" $GUESS GUESS=MOREAD NORB=10 $END\n")
  pot.write(" $GCILST\n")
  pot.write("3\n")
  pot.write("2 0\n")
  pot.write("1 1\n")
  pot.write("0 2\n")
  pot.write(" $END\n")
  pot.write(" $VEC\n")
#Read vectors
  lines=open('rhf%04d.dat'%(i+1)).readlines()
  for j in range(len(lines)):
    if (len(lines[j])==9):
      ok=''
      decort=list(lines[j])
      for n in range(5):
        ok=ok+decort[n]
      if (ok==' $VEC'):
        a=j
        for m in range(20):
          ok=''
          a=a+1
          decort=list(lines[a])
          for n in range(80):
            ok=ok+decort[n]
          pot.write(ok+"\n")
  pot.write(" $END\n")
  pot.write(" $DATA \n")
  pot.write("Title\n")
  pot.write("C1\n")
  pot.write("H\n")
  pot.write("H 1 %6.3f\n"%(x[i]))
  pot.write(" $END\n")
  pot.close()
  os.system("~/rungms 1e%04d > 1e%04d.out"%(i+1,i+1))
  os.system("mv /tmp/1e%04d.dat ."%(i+1))
  os.system("rm rhf%04d.dat"%(i+1))
  os.system("rm 1e%04d.inp"%(i+1))
  os.system("rm 1e%04d.dat"%(i+1))
#Read ci states
  lines=open('1e%04d.out'%(i+1)).readlines()
  for j in range(len(lines)):
    if (len(lines[j])==39):
      ok=''
      decort=list(lines[j])
      for n in range(38):
        ok=ok+decort[n]
      if (ok==' PRINTING ALL NON-ZERO CI COEFFICIENTS'):
        a=j+2
        letsgo=True
        while letsgo:
          decort=list(lines[a])
          ok=''
          for u in range(len(decort)):
            ok=ok+decort[u]
          if (ok[:9]==' STATE   '):
            state=ok[9]
            if (state=='1'):
              y1.append(float(ok[19:40]))
            if (state=='2'):
              y2.append(float(ok[19:40]))
          if (ok[:10]==' 10 | 00 |'):
            if (state=='1'):
              g1[xpt]=(float(ok[112:125]))
            if (state=='2'):
              g2[xpt]=(float(ok[112:125]))
          if (ok[:10]==' 01 | 00 |'):
            if (state=='1'):
              u1[xpt]=(float(ok[112:125]))
            if (state=='2'):
              u2[xpt]=(float(ok[112:125]))
          if (ok[:49]==' ..... DONE WITH DETERMINANT CI COMPUTATION .....'):
            xpt=xpt+1
            letsgo=False
          a=a+1
  print('Progression: %3d%%'%(100*(i+1)/xptmax))
  os.system("rm 1e%04d.out"%(i+1))
#Write cis and csfs
np.savetxt('cis.dat',np.c_[x,y1,y2],fmt='%6.3f\t%21.10f\t%21.10f')
np.savetxt('state1.dat',np.c_[x,g1,u1],fmt='%6.3f\t%21.10f\t%21.10f')
np.savetxt('state2.dat',np.c_[x,g2,u2],fmt='%6.3f\t%21.10f\t%21.10f')
#Plot all
plt.plot(x,y1,label='State 1')
plt.plot(x,y2,label='State 2')
plt.xlabel('Internuclear distance (Bohrs)')
plt.ylabel('Energy (Hartrees)')
plt.title('MCSCF CIs')
plt.legend()
plt.tight_layout()
plt.savefig('cis.png')
plt.close()
plt.plot(x,g1,label=r'$|\sigma_g|$')
plt.plot(x,u1,label=r'$|\sigma_u|$')
plt.xlabel('Internuclear distance (Bohrs)')
plt.ylabel('Relative proportion')
plt.title('CI State 1')
plt.legend()
plt.tight_layout()
plt.savefig('ci1.png')
plt.close()
plt.plot(x,g2,label=r'$|\sigma_g|$')
plt.plot(x,u2,label=r'$|\sigma_u|$')
plt.xlabel('Internuclear distance (Bohrs)')
plt.ylabel('Relative proportion')
plt.title('CI State 2')
plt.legend()
plt.tight_layout()
plt.savefig('ci2.png')
plt.close()
