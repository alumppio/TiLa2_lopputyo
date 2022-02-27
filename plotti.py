import matplotlib.pyplot as plt 
import numpy as np

aika=np.loadtxt('sairastiedot.txt',usecols=0)
terveet=np.loadtxt('sairastiedot.txt',usecols=1)
sairaat=np.loadtxt('sairastiedot.txt',usecols=2)
immuunit=np.loadtxt('sairastiedot.txt',usecols=3)

n=sairaat[0]+terveet[0]+immuunit[0]
n=int(n)

fig=plt.figure()

plt.plot(aika,terveet,color='red',label='Terveet')
plt.plot(aika,sairaat,color='green',label='Sairaat')
plt.plot(aika,immuunit,color='blue',label='Immuunit')

plt.xlim([-0.5,200])
plt.ylim([0,2500])
plt.xlabel('Aika')
plt.ylabel('Määrä')
plt.title("Koko: [50 x 50] \n Satunnaiset kävelijät: {a:0d} kpl".format(a=n))
plt.legend()
plt.show()

fig.savefig('kavelijat{a:0d}_40.png'.format(a=n))