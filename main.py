import urllib.request as r,json,os
D=0
V=[0,100,320,330,500,900,0]
N=[-21,-19,-12,-8,8,12,19,21]
G=[-11,-10,-9,-1,1,9,10,11]
B=[-11,-9,9,11]
R=[-10,-1,1,10]
def P(f):
 p=f.split();b=[9]*120
 for i in range(64):b[(i//8+2)*10+i%8+1]=0
 r=7;q=0
 for ch in p[0]:
  if ch=='/':r-=1;q=0
  elif ch.isdigit():q+=int(ch)
  else:v='pnbrqk'.index(ch.lower())+1;b[r*10+q+21]=v*(ch.isupper() or -1);q+=1
 w=1 if p[1]=='w' else -1;c=0
 for ch in p[2]:
  if ch in 'KQkq':c|=1<<'KQkq'.index(ch)
 e=-1
 if p[3]!='-':e=(int(p[3][1])+1)*10+ord(p[3][0])-96
 return b,w,c,e
def A(b,s,x):
 for d in (9,11):
  if b[s-d*x]==x:return 1
 for d in N:
  if b[s+d]==2*x:return 1
 for d in G:
  if b[s+d]==6*x:return 1
 for d in G:
  t=s+d
  while b[t]==0:t+=d
  if b[t]*x==5 or b[t]*x==(3 if d in B else 4):return 1
 return 0
def K(b,w):return next((s for s in range(21,99) if b[s]==6*w),-1)
def L(b,w,c,e):
 m=[]
 def J(s,t):
  if t>90 or t<29:
   for p in 'qrbn':m.append((s,t,p))
  else:m.append((s,t,''))
 for s,v in enumerate(b):
  if not 1<=v*w<=6:continue
  a=abs(v)
  if a==1:
   d=10*w;t=s+d
   if b[t]==0:
    J(s,t)
    if s//10==3+5*(w<0) and b[s+2*d]==0:m.append((s,s+2*d,''))
   for t in (s+d-1,s+d+1):
    if b[t]!=9 and b[t]*w<0 or t==e:J(s,t)
  elif a==2:
   for d in N:
    t=s+d
    if b[t]!=9 and b[t]*w<=0:m.append((s,t,''))
  elif a==6:
   for d in G:
    t=s+d
    if b[t]!=9 and b[t]*w<=0:m.append((s,t,''))
  else:
   D=B if a==3 else R if a==4 else G
   for d in D:
    t=s+d
    while b[t]==0:m.append((s,t,''));t+=d
    if b[t]!=9 and b[t]*w<0:m.append((s,t,''))
 for bit,k,r,es in ((1,25,28,(26,27)),(2,25,21,(24,23,22)),(4,95,98,(96,97)),(8,95,91,(94,93,92))):
  if (bit<4)==(w>0) and c&bit and b[r]==4*(bit<4 or -1) and all(b[x]==0 for x in es) and not any(A(b,x,-w) for x in (k,es[0],es[1])):m.append((k,es[1],''))
 return [mv for mv in m if not A((n:=U(b,w,c,e,mv))[0],K(n[0],w),-w)]
def U(b,w,c,e,mv):
 f,t,p=mv;b2=b[:];v=b2[f];c2=c
 if abs(v)==1 and t==e and b2[t]==0:b2[t-10*w]=0
 b2[t]=v;b2[f]=0
 if p:b2[t]=v//abs(v)*(2+'nbrq'.index(p))
 if abs(v)==6 and abs(t-f)==2:
  a,b=(f+3,f+1) if t>f else (f-4,f-1);b2[b]=b2[a];b2[a]=0
 for sq,bit in ((28,1),(21,2),(98,4),(91,8)):
  if f==sq or t==sq:c2&=~bit
 if abs(v)==6:c2&=~(3 if v>0 else 12)
 e2=(f+t)//2 if abs(v)==1 and abs(t-f)==20 else -1
 return b2,-w,c2,e2
def E(b,w,c,e):
 s=0
 for q,v in enumerate(b):
  if v==0 or v==9:continue
  a=abs(v);g=v//a;s+=V[a]*g
  if a==1:s+=((q//10-2) if v>0 else 7-(q//10-2))*6*g
 return s*w
T={}
def S(b,w,c,e,d,a,B,p):
 global D;D+=1
 if D>10000:1/0
 if d<=0:return Q(b,w,c,e,a,B,p)
 k=(tuple(b),w,c,e);z=T.get(k)
 if z and z[0]>=d and (z[2]==0 or z[2]==1 and z[1]>=B or z[2]==2 and z[1]<=a):return z[1]
 mv=L(b,w,c,e)
 if not mv:return -20000+p if A(b,K(b,w),-w) else 0
 mv.sort(key=lambda m2:-V[abs(b[m2[1]])])
 be=-99999;bm=mv[0];al=a
 for m2 in mv:
  n=U(b,w,c,e,m2);v=-S(*n,d-1,-B,-al,p+1)
  if v>be:be=v;bm=m2
  if v>al:al=v
  if al>=B:break
 T[k]=(d,be,(be<=a)*2+(be>=B))
 return be
def Q(b,w,c,e,a,B,p):
 global D;D+=1
 if D>10000:1/0
 i=A(b,K(b,w),-w);mv=L(b,w,c,e)
 if not mv:return -20000+p if i else 0
 s=E(b,w,c,e)
 if s>=B:return B
 if s>a:a=s
 o=sorted([(V[abs(b[m2[1]])],m2) for m2 in mv if b[m2[1]] or m2[2] or i],reverse=True)
 for _,m2 in o:
  n=U(b,w,c,e,m2);v=-Q(*n,-B,-a,p+1)
  if v>a:a=v
  if a>=B:break
 return a
def C(f,lu):
 global D
 b,w,c,e=P(f);mv=L(b,w,c,e)
 bm=mv[0]
 for d in range(1,30):
  try:
   D=0;be=-99999;al=-99999;cb=mv[0]
   for m2 in mv:
    n=U(b,w,c,e,m2);v=-S(*n,d-1,-99999,-al,1)
    if v>be:be=v;cb=m2
    if v>al:al=v
   bm=cb
  except:break
 return ''.join(chr(96+x%10)+str(x//10-1) for x in bm[:2])+bm[2]
def H(u,d):
 q=r.Request(u,data=json.dumps(d).encode(),headers={'Content-Type':'application/json'})
 return json.loads(r.urlopen(q).read())
def m():
 u='http://'+os.getenv('MAKEFUN_ENV_URL','').split('://')[-1]
 while 1:
  d=H(u+'/game',{})
  if d['status']!='your_move':return
  try:mv=C(d['fen'],d['legal_moves'])
  except:mv=d['legal_moves'][0]
  try:H(u+'/move',{'game':d['game'],'move':mv})
  except:pass
if __name__=='__main__':m()
