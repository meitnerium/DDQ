import numpy as np
import matplotlib.pyplot as plt
import os

print('Preping stuff...')
xptmax=9812
xpt=0
x=[]
y1=[]
y2=[]
y3=[]
y4=[]
gg1=[]
gu1=[]
ug1=[]
uu1=[]
gg2=[]
gu2=[]
ug2=[]
uu2=[]
gg3=[]
gu3=[]
ug3=[]
uu3=[]
gg4=[]
gu4=[]
ug4=[]
uu4=[]
for i in range(xptmax):
  gg1.append(0.0)
  gu1.append(0.0)
  ug1.append(0.0)
  uu1.append(0.0)
  gg2.append(0.0)
  gu2.append(0.0)
  ug2.append(0.0)
  uu2.append(0.0)
  gg3.append(0.0)
  gu3.append(0.0)
  ug3.append(0.0)
  uu3.append(0.0)
  gg4.append(0.0)
  gu4.append(0.0)
  ug4.append(0.0)
  uu4.append(0.0)
#Vectors
for i in range(xptmax):
  pot=open("rhf%04d.inp"%(i+1),"w")
  pot.write(" $BASIS GBASIS=N31 NGAUSS=6 NDFUNC=1 NPFUNC=1 $END\n")
  pot.write(" $CONTRL SCFTYP=RHF RUNTYP=ENERGY $END\n")
  pot.write(" $CONTRL COORD=ZMT UNITS=BOHR NPRINT=9 $END\n")
  pot.write(" $DATA \n")
  pot.write("Title\n")
  pot.write("C1\n")
  pot.write("H\n")
  x.append(0.189+(float(i)*0.001))
  pot.write("H 1 %6.3f\n"%(x[i]))
  pot.write(" $END\n")
  pot.close()
  os.system("~/rungms rhf%04d > rhf%04d.out"%(i+1,i+1))
  os.system("mv /tmp/rhf%04d.dat ."%(i+1))
  os.system("rm rhf%04d.inp"%(i+1))
  os.system("rm rhf%04d.out"%(i+1))
  pot=open("1e%04d.inp"%(i+1),"w")
  pot.write(" $BASIS GBASIS=N31 NGAUSS=6 NDFUNC=1 NPFUNC=1 $END\n")
  pot.write(" $CONTRL SCFTYP=MCSCF RUNTYP=ENERGY CITYP=GENCI $END\n")
  pot.write(" $CONTRL COORD=ZMT UNITS=BOHR NPRINT=3 $END\n")
  pot.write(" $DET NCORE=0 NACT=2 NELS=2 NSTATE=4 $END\n")
  pot.write(" $CIGEN NCORE=0 NACT=2 NELS=2 NSTATE=4 $END\n")
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
    if (len(lines[j])==55):
      ok=''
      decort=list(lines[j])
      for n in range(54):
        ok=ok+decort[n]
      if (ok==' SMALL CI MATRIX, JUST USING INCORE DIAGONALIZATION...'):
        a=j+5
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
            if (state=='3'):
              y3.append(float(ok[19:40]))
            if (state=='4'):
              y4.append(float(ok[19:40]))
          if (ok[:10]==' 10 | 10 |'):
            if (state=='1'):
              gg1[xpt]=(float(ok[112:125]))
            if (state=='2'):
              gg2[xpt]=(float(ok[112:125]))
            if (state=='3'):
              gg3[xpt]=(float(ok[112:125]))
            if (state=='4'):
              gg4[xpt]=(float(ok[112:125]))
          if (ok[:10]==' 10 | 01 |'):
            if (state=='1'):
              gu1[xpt]=(float(ok[112:125]))
            if (state=='2'):
              gu2[xpt]=(float(ok[112:125]))
            if (state=='3'):
              gu3[xpt]=(float(ok[112:125]))
            if (state=='4'):
              gu4[xpt]=(float(ok[112:125]))
          if (ok[:10]==' 01 | 10 |'):
            if (state=='1'):
              ug1[xpt]=(float(ok[112:125]))
            if (state=='2'):
              ug2[xpt]=(float(ok[112:125]))
            if (state=='3'):
              ug3[xpt]=(float(ok[112:125]))
            if (state=='4'):
              ug4[xpt]=(float(ok[112:125]))
          if (ok[:10]==' 01 | 01 |'):
            if (state=='1'):
              uu1[xpt]=(float(ok[112:125]))
            if (state=='2'):
              uu2[xpt]=(float(ok[112:125]))
            if (state=='3'):
              uu3[xpt]=(float(ok[112:125]))
            if (state=='4'):
              uu4[xpt]=(float(ok[112:125]))
          if (ok[:49]==' ..... DONE WITH DETERMINANT CI COMPUTATION .....'):
            xpt=xpt+1
            letsgo=False
          a=a+1
  print('Progression: %3d%%'%(100*(i+1)/xptmax))
  os.system("rm 1e%04d.out"%(i+1))
#Write cis and csfs
np.savetxt('cis.dat',np.c_[x,y1,y2,y3,y4],fmt='%6.3f\t%21.10f\t%21.10f\t%21.10f\t%21.10f')
np.savetxt('state1.dat',np.c_[x,gg1,gu1,ug1,uu1],fmt='%6.3f\t%21.10f\t%21.10f\t%21.10f\t%21.10f')
np.savetxt('state2.dat',np.c_[x,gg2,gu2,ug2,uu2],fmt='%6.3f\t%21.10f\t%21.10f\t%21.10f\t%21.10f')
np.savetxt('state3.dat',np.c_[x,gg3,gu3,ug3,uu3],fmt='%6.3f\t%21.10f\t%21.10f\t%21.10f\t%21.10f')
np.savetxt('state4.dat',np.c_[x,gg4,gu4,ug4,uu4],fmt='%6.3f\t%21.10f\t%21.10f\t%21.10f\t%21.10f')
#Plot all
plt.plot(x,y1,label='State 1')
plt.plot(x,y2,label='State 2')
plt.plot(x,y3,label='State 3')
plt.plot(x,y4,label='State 4')
plt.xlabel('Internuclear distance (Bohrs)')
plt.ylabel('Energy (Hartrees)')
plt.title('MCSCF CIs')
plt.legend()
plt.tight_layout()
plt.savefig('cis.png')
plt.close()
plt.plot(x,gg1,label=r'$|\sigma_g \bar{\sigma}_g|$')
plt.plot(x,gu1,label=r'$|\sigma_g \bar{\sigma}_u|$')
plt.plot(x,ug1,label=r'$|\sigma_u \bar{\sigma}_g|$')
plt.plot(x,uu1,label=r'$|\sigma_u \bar{\sigma}_u|$')
plt.xlabel('Internuclear distance (Bohrs)')
plt.ylabel('Relative proportion')
plt.title('CI State 1')
plt.legend()
plt.tight_layout()
plt.savefig('ci1.png')
plt.close()
plt.plot(x,gg2,label=r'$|\sigma_g \bar{\sigma}_g|$')
plt.plot(x,gu2,label=r'$|\sigma_g \bar{\sigma}_u|$')
plt.plot(x,ug2,label=r'$|\sigma_u \bar{\sigma}_g|$')
plt.plot(x,uu2,label=r'$|\sigma_u \bar{\sigma}_u|$')
plt.xlabel('Internuclear distance (Bohrs)')
plt.ylabel('Relative proportion')
plt.title('CI State 2')
plt.legend()
plt.tight_layout()
plt.savefig('ci2.png')
plt.close()
plt.plot(x,gg3,label=r'$|\sigma_g \bar{\sigma}_g|$')
plt.plot(x,gu3,label=r'$|\sigma_g \bar{\sigma}_u|$')
plt.plot(x,ug3,label=r'$|\sigma_u \bar{\sigma}_g|$')
plt.plot(x,uu3,label=r'$|\sigma_u \bar{\sigma}_u|$')
plt.xlabel('Internuclear distance (Bohrs)')
plt.ylabel('Relative proportion')
plt.title('CI State 3')
plt.legend()
plt.tight_layout()
plt.savefig('ci3.png')
plt.close()
plt.plot(x,gg4,label=r'$|\sigma_g \bar{\sigma}_g|$')
plt.plot(x,gu4,label=r'$|\sigma_g \bar{\sigma}_u|$')
plt.plot(x,ug4,label=r'$|\sigma_u \bar{\sigma}_g|$')
plt.plot(x,uu4,label=r'$|\sigma_u \bar{\sigma}_u|$')
plt.xlabel('Internuclear distance (Bohrs)')
plt.ylabel('Relative proportion')
plt.title('CI State 4')
plt.legend()
plt.tight_layout()
plt.savefig('ci4.png')
plt.close()
