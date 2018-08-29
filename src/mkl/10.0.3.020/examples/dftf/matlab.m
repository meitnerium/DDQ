xmax=2000.0
xmin=-2000.0
xeq=0.0
alpha=0.50
npos=1024
dx=(xmax-xmin)/(npos-1)
dkx=(2.0*pi)/(npos*dx)

kxmin=-pi/dx
tp=0
wcm1=2200
gaussp=10
w=wcm1/219474.63068
wg=1/1000000000000000000000
delta=0.0
wvec=-2000:2:2000
ee=exp(-2.*(wvec.^gaussp).*wg)
plot(ee)
