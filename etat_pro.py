import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import simps

class Grid():
    def __init__(self,value, ndim=1, dim=[1024],name=["X"],unit=["u.a."]):
        self.ndim = ndim
        self.dim = dim
        self.value = value
        self.name = name
        self.unit = unit

class Potential():
    def __init__(self,ncanal,grid,value,name=["Energy"],unit=["u.a."]):
        self.grid = grid
        self.value = value
        self.ncanal = ncanal
        self.name = name
        self.unit = unit

class Field():
    def __init__(self,tgrid,e0,value):
        self.tgrid = tgrid
        self.e0 = e0
        self.value = value


def morse_nu(xmu,smalla,diss,nu,r,requ,e0):
    biga = (np.sqrt(2.0*xmu))/smalla
    bigc = biga*np.sqrt(diss)
    enu = -((bigc-nu-.5)**2.0)/biga**2.0
    alpha = bigc-nu-0.50
    arg=np.exp(-smalla*(r-requ))
    x=2.0*bigc*arg
    m = 2*(int(bigc)-nu)-1
    morse = np.polynomial.laguerre.Laguerre() #coef, domain=None, window=None)
    morse = morse * (x**(int(bigc)))
    morse = morse / (x**nu)
    morse = morse / np.sqrt(x)
    morse = morse * np.exp(-x/2.0)

    norm = smalla * np.math.factorial(nu) * (2.0*bigc - 2.0*nu - 1.0)
    norm = norm / np.math.factorial(m+nu)

    norm = np.sqrt(norm)
    morse = morse*norm
    return morse


def morse(z0,s,r,req,x1):
    return z0*(np.exp(-2.0*s*(r-req)) - 2.0*x1*np.exp(-s*(r-req)))

def xmu12(s,y,r,req):
    #
    return 1.070 + (.3960/(s*y))*(1.0-np.exp(-s*y*(r-req)))
    #if((r>12.0) and (xmu12 > 0.5*r)):
    #    xmu12 = 0.50*r


def eval(p0,rc0,alpha, r):
#subroutine eval(cw1, cw2, delr, rdeb, p0, rc0, alpha, npos)
    cw1 = np.zeros(len(r), dtype=np.complex64)
    cw2 = np.zeros(len(r), dtype=np.complex64)
    cpoi = np.sqrt(np.sqrt(2.0 * alpha / np.pi + 1j*(0.0)))
    arg = (-alpha * (r - rc0) ** 2 + 1j * (p0 * (r - rc0)))
    cw1 = cpoi * np.exp(arg)

    return cw1,cw2


def zexptdt(r,masse,dt):
    xk = np.zeros(len(r))
    etdt = np.zeros(len(r), dtype=np.complex64)
    xk1 = 2.0 * np.pi / (len(r) * (r[1]-r[0]))
    for nr in range(len(r)):
        if nr<len(r/2):
            xk[nr] = (nr) * xk1
        else:
            xk[nr] = -(len(r) - nr) * xk1
        arg = ((xk[nr] * xk[nr]) / (2.0 * masse)) * dt
        etdt[nr] = np.exp(-1j * arg)
    return etdt


time = Grid(ndim=1,dim=1024,value=np.linspace(0,8000,1024))
x = Grid(ndim=1,dim=1024,value=np.linspace(0.1,15.0,1024))

print(x.value)
data = np.array([[np.array(morse(z0=.1026277, s=.72, r=x.value, req=2., x1=1.)),
                  np.array(xmu12(s=.72,y = -.055,r=x.value,req=2.))],
                 [np.array(xmu12(s=.72,y = -.055,r=x.value,req=2.)),
                  np.array(morse(z0=.1026277, s=.72, r=x.value, req=2., x1=-1.11))]])
potH2 = Potential(grid=x, ncanal=2,
                  value=data)
plt.plot(potH2.grid.value,potH2.value[0,0],potH2.grid.value,potH2.value[1,1])
plt.xlabel(potH2.grid.name[0]+" ("+potH2.grid.unit[0]+")")
plt.ylabel(potH2.name[0]+" ("+potH2.unit[0]+")")
plt.show()
plt.close()

plt.plot(potH2.grid.value,potH2.value[0,1],potH2.grid.value,potH2.value[1,0])
plt.xlabel(potH2.grid.name[0]+" ("+potH2.grid.unit[0]+")")
plt.ylabel(potH2.name[0]+" ("+potH2.unit[0]+")")
plt.show()
plt.close()

#l = 10.6 micron, w: 4.298429489722382E-003a.u., w(cm1):943.396230595294,
# period: 1461.73980451426a.u.,35.3577950256804 fs
w = 4.298429489722382E-003
tau2fs=2.418884326505E-2
# tc = 35 fs = 1TL
tc = 35.3577950256804/tau2fs
phase = 0.
e0=1E13 # W/cm2
e0ua=np.sqrt(e0/3.5094475E16)
champ = np.cos(w*(time.value-tc)+phase)*e0ua
#efield = Field(tgrid=time, e0=e0ua, value=champ)
efield = Field(tgrid=time, e0=e0ua*0., value=champ*0.)

plt.plot(efield.tgrid.value,efield.value)
plt.show()
plt.close()
#         eval(p0, rc0,alpha,r)
cw1,cw2 = eval(0.0,2.0,1.0,  x.value)
plt.plot(potH2.grid.value,np.abs(cw1)**2.0)
plt.show()
plt.close()
dt = time.value[1]-time.value[0]
print("dt=",dt)
masse=918.0762887608628 #!=masse reduite de proton/2 en u.a
etatv = morse_nu(masse,0.72,2.79250/27.20,0,x.value,2.0,0)
plt.plot(etatv)
plt.show()
