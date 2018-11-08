import numpy as np
import os

os.system("rm -f /tmp/*.dat *.dat *.out *.inp *.png Makefile potgraph.py")

medys=input("Use MEDYS, [Y]es or [N]o? {RETURN for default}\n")
if (medys==''):
  medys='n'
  chrg=0
  mult=1
  eq=np.float128(1.4)
  rmin=np.float128(0.2)
  rmax=np.float128(5.0)
  npts=49
else:
  medys=medys.lower()
  chrg=int(input("What's the molecular charge?\n"))
  mult=int(input("What's the multiplicity?\n"))
  eq=np.float128(input("What's the approx. equilibrum (Bohr - float)?\n"))
  rmin=np.float128(input("From [over 0.18897] (Bohr - float)?...\n"))
  rmax=np.float128(input("..to (Bohr - float)?\n"))
  npts=int(input("How many points?\n"))
if npts>=2:
  dr=(rmax-rmin)/np.float128(npts-1)

#Geometry optimization
pot=open("geom_opt.inp","w")
pot.write(" $SYSTEM TIMLIM=60 MWORDS=1 $END\n")
if (mult==1):
  pot.write(" $CONTRL ICHARG="+str(chrg)+" MULT="+str(mult)+" RUNTYP=OPTIMIZE SCFTYP=RHF UNITS=BOHR $END\n")
else:
  pot.write(" $CONTRL ICHARG="+str(chrg)+" MULT="+str(mult)+" RUNTYP=OPTIMIZE SCFTYP=UHF UNITS=BOHR $END\n")
pot.write(" $BASIS GBASIS=N31 NGAUSS=6 NDFUNC=1 NPFUNC=1 DIFFSP=.TRUE. $END\n")
pot.write(" $STATPT OPTTOL=0.0001 NSTEP=20 $END\n")
pot.write("\n")
pot.write(" $DATA\n")
pot.write("H2 GEOMETRY OPTIMIZATION\n")
pot.write("C1\n")
pot.write("H     1.0     0.00000     0.00000    -%1.5f\n"%(eq/2))
pot.write("H     1.0     0.00000     0.00000     %1.5f\n"%(eq/2))
pot.write(" $END\n")
pot.close()

#Potential calculation
if npts>=2:
  for i in range(npts):
    name=''
    name_=str(rmin+(np.float128(i)*dr))
    name_=list(name_)
    for n in range(len(name_)):
      if (name_[n]=='.'):
        name=name+'_'
      else:
        name=name+name_[n]
    pot=open("%s_sp.inp"%(name),"w")
    pot.write(" $SYSTEM TIMLIM=60 MWORDS=1 $END\n")
    if (mult==1):
      pot.write(" $CONTRL ICHARG="+str(chrg)+" MULT="+str(mult)+" RUNTYP=ENERGY SCFTYP=RHF AIMPAC=.TRUE. UNITS=BOHR $END\n")
    else:
      pot.write(" $CONTRL ICHARG="+str(chrg)+" MULT="+str(mult)+" RUNTYP=ENERGY SCFTYP=UHF AIMPAC=.TRUE. UNITS=BOHR $END\n")
    pot.write(" $BASIS GBASIS=N31 NGAUSS=6 NDFUNC=1 NPFUNC=1 DIFFSP=.TRUE. $END\n")
    pot.write(" $STATPT OPTTOL=0.0001 NSTEP=20 $END\n")
    pot.write("\n")
    pot.write(" $DATA\n")
    pot.write("H2 AT INTERNUCLEAR DISTANCE "+str(rmin+(np.float128(i)*dr))+" BOHR\n")
    pot.write("C1\n")
    pot.write("H     1.0     0.00000     0.00000    -%1.5f\n"%((rmin+(np.float128(i)*dr))/2.0))
    pot.write("H     1.0     0.00000     0.00000     %1.5f\n"%((rmin+(np.float128(i)*dr))/2.0))
    pot.write(" $END\n")
    pot.close()
elif npts==1:
  name=''
  name_=str(rmin)
  name_=list(name_)
  for n in range(len(name_)):
    if (name_[n]=='.'):
      name=name+'_'
    else:
      name=name+name_[n]
  pot=open("%s_sp.inp"%(name),"w")
  pot.write(" $SYSTEM TIMLIM=60 MWORDS=1 $END\n")
  if (mult==1):
    pot.write(" $CONTRL ICHARG="+str(chrg)+" MULT="+str(mult)+" RUNTYP=ENERGY SCFTYP=RHF AIMPAC=.TRUE. UNITS=BOHR $END\n")
  else:
    pot.write(" $CONTRL ICHARG="+str(chrg)+" MULT="+str(mult)+" RUNTYP=ENERGY SCFTYP=UHF AIMPAC=.TRUE. UNITS=BOHR $END\n")
  pot.write(" $BASIS GBASIS=N31 NGAUSS=6 NDFUNC=1 NPFUNC=1 DIFFSP=.TRUE. $END\n")
  pot.write(" $STATPT OPTTOL=0.0001 NSTEP=20 $END\n")
  pot.write("\n")
  pot.write(" $DATA\n")
  pot.write("H2 AT INTERNUCLEAR DISTANCE "+str(rmin)+" BOHR\n")
  pot.write("C1\n")
  pot.write("H     1.0     0.00000     0.00000    -%1.5f\n"%((rmin)/2.0))
  pot.write("H     1.0     0.00000     0.00000     %1.5f\n"%((rmin)/2.0))
  pot.write(" $END\n")
  pot.close()

#MCSCF calculation for MEDYS
if (medys=='y'):
  if npts>=2:
    for i in range(npts):
      name=''
      name_=str(rmin+(np.float128(i)*dr))
      name_=list(name_)
      for n in range(len(name_)):
        if (name_[n]=='.'):
          name=name+'_'
        else:
          name=name+name_[n]
      pot=open("%s_mcscf.inp"%(name),"w")
      pot.write(" $SYSTEM TIMLIM=60 MWORDS=1 $END\n")
      pot.write(" $CONTRL ICHARG="+str(chrg)+" MULT="+str(mult)+" RUNTYP=ENERGY SCFTYP=MCSCF AIMPAC=.TRUE. UNITS=BOHR $END\n")
      pot.write(" $BASIS GBASIS=N31 NGAUSS=6 NDFUNC=1 NPFUNC=1 DIFFSP=.TRUE. $END\n")
      pot.write(" $STATPT OPTTOL=0.0001 NSTEP=20 $END\n")
      pot.write("\n")
      pot.write(" $DATA\n")
      pot.write("H2 AT INTERNUCLEAR DISTANCE "+str(rmin+(np.float128(i)*dr))+" BOHR\n")
      pot.write("C1\n")
      pot.write("H     1.0     0.00000     0.00000    -%1.5f\n"%((rmin+(np.float128(i)*dr))/2.0))
      pot.write("H     1.0     0.00000     0.00000     %1.5f\n"%((rmin+(np.float128(i)*dr))/2.0))
      pot.write(" $END\n")
      pot.close()
  elif npts==1:
    name=''
    name_=str(rmin)
    name_=list(name_)
    for n in range(len(name_)):
      if (name_[n]=='.'):
        name=name+'_'
      else:
        name=name+name_[n]
    pot=open("%s_mcscf.inp"%(name),"w")
    pot.write(" $SYSTEM TIMLIM=60 MWORDS=1 $END\n")
    pot.write(" $CONTRL ICHARG="+str(chrg)+" MULT="+str(mult)+" RUNTYP=ENERGY SCFTYP=MCSCF AIMPAC=.TRUE. UNITS=BOHR $END\n")
    pot.write(" $BASIS GBASIS=N31 NGAUSS=6 NDFUNC=1 NPFUNC=1 DIFFSP=.TRUE. $END\n")
    pot.write(" $STATPT OPTTOL=0.0001 NSTEP=20 $END\n")
    pot.write("\n")
    pot.write(" $DATA\n")
    pot.write("H2 AT INTERNUCLEAR DISTANCE "+str(rmin)+" BOHR\n")
    pot.write("C1\n")
    pot.write("H     1.0     0.00000     0.00000    -%1.5f\n"%((rmin)/2.0))
    pot.write("H     1.0     0.00000     0.00000     %1.5f\n"%((rmin)/2.0))
    pot.write(" $END\n")
    pot.close()

#Potential graph
pot=open("potgraph.py","w")
pot.write("import numpy as np\n")
pot.write("import matplotlib.pyplot as plt\n")
pot.write("\n")
pot.write("x=[]\n")
pot.write("y=[]\n")
pot.write("xm=[]\n")
pot.write("ym=[]\n")
pot.write("for p in range("+str(npts+1)+"):\n")
pot.write("  if (p=="+str(npts)+"):\n")
pot.write("    lines=open('geom_opt.out').readlines()\n")
pot.write("    ok1=''\n")
pot.write("    ok2=''\n")
pot.write("    that1=''\n")
pot.write("    that2=''\n")
pot.write("    for i in range(len(lines)):\n")
pot.write("      if (len(lines[i])==50):\n")
pot.write("        decort=list(lines[i])\n")
pot.write("        for n in range(29):\n")
pot.write("          ok1=ok1+decort[n]\n")
pot.write("        if (ok1=='          TOTAL ENERGY      ='):\n")
pot.write("          for m in range(29,49):\n")
pot.write("            that1=that1+decort[m]\n")
pot.write("        ok1=''\n")
pot.write("      elif (len(lines[i])==47):\n")
pot.write("        decort=list(lines[i])\n")
pot.write("        for n in range(46):\n")
pot.write("          ok2=ok2+decort[n]\n")
pot.write("        if (ok2=='      ***** EQUILIBRIUM GEOMETRY LOCATED *****'):\n")
pot.write("          decort=list(lines[i+5])\n")
pot.write("          for m in range(49,61):\n")
pot.write("            that2=that2+decort[m]\n")
pot.write("        ok2=''\n")
pot.write("    ym.append(np.float128(that1))\n")
pot.write("    xm.append(np.float128(2.0)*np.float128(1.889726125)*np.float128(that2))\n")
pot.write("  else:\n")
pot.write("    name=''\n")
pot.write("    name_=str("+str(rmin)+"+(np.float128(p)*"+str(dr)+"))\n")
pot.write("    name_=list(name_)\n")
pot.write("    for n in range(len(name_)):\n")
pot.write("      if (name_[n]=='.'):\n")
pot.write("        name=name+'_'\n")
pot.write("      else:\n")
pot.write("        name=name+name_[n]\n")
pot.write("    lines=open('%s_sp.out'%(name)).readlines()\n")
pot.write("    ok=''\n")
pot.write("    that=''\n")
pot.write("    for i in range(len(lines)):\n")
pot.write("      if (len(lines[i])==57):\n")
pot.write("        decort=list(lines[i])\n")
pot.write("        for n in range(37):\n")
pot.write("          ok=ok+decort[n]\n")
pot.write("        if (ok=='                       TOTAL ENERGY ='):\n")
pot.write("          for m in range(37,56):\n")
pot.write("            that=that+decort[m]\n")
pot.write("        ok=''\n")
pot.write("    x.append("+str(rmin)+"+(np.float128(p)*"+str(dr)+"))\n")
pot.write("    y.append(np.float128(that))\n")
pot.write("    that=''\n")
pot.write("np.savetxt('pot.dat',np.c_[x,y],fmt='%20.10e	%20.10e')\n")
pot.write("np.savetxt('well.dat',np.c_[xm,ym],fmt='%20.10e	%20.10e')\n")
pot.write("fig=plt.figure()\n")
pot.write("plt.plot(x,y)\n")
pot.write("plt.xlabel('Internuclear distance')\n")
pot.write("plt.ylabel('Energy')\n")
pot.write("plt.title('Potential energy')\n")
pot.write("plt.savefig('PES.png')\n")
pot.write("plt.close()\n")
pot.close()

#Makefile
pot=open("Makefile","w")
pot.write("all: geom_opt.out\n")
pot.write("\n")
pot.write("geom_opt.out: geom_opt.inp\n")
pot.write("\t~/rungms geom_opt > geom_opt.out\n")
pot.write("\tcp /tmp/geom_opt.dat .\n")
if npts>=2:
  for i in range(npts):
    name=''
    name_=str(rmin+(np.float128(i)*dr))
    name_=list(name_)
    for n in range(len(name_)):
      if (name_[n]=='.'):
        name=name+'_'
      else:
        name=name+name_[n]
    pot.write("\t~/rungms %s_sp > %s_sp.out\n"%(name,name))
    pot.write("\tcp /tmp/%s_sp.dat .\n"%(name))
    if (medys=='y'):
      pot.write("\t~/rungms %s_mcscf > %s_mcscf.out\n"%(name,name))
      pot.write("\tcp /tmp/%s_mcscf.dat .\n"%(name))
elif npts==1:
    name=''
    name_=str(rmin)
    name_=list(name_)
    for n in range(len(name_)):
      if (name_[n]=='.'):
        name=name+'_'
      else:
        name=name+name_[n]
    pot.write("\t~/rungms %s_sp > %s_sp.out\n"%(name,name))
    pot.write("\tcp /tmp/%s_sp.dat .\n"%(name))
    if (medys=='y'):
      pot.write("\t~/rungms %s_mcscf > %s_mcscf.out\n"%(name,name))
      pot.write("\tcp /tmp/%s_mcscf.dat .\n"%(name))
pot.write("\tpython3 potgraph.py\n")
pot.close()
