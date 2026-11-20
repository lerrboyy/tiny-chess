import json,os,sys,urllib.request as U
V=(0,100,320,330,500,900,0)
N=25000
d=0
P=("","22222222344554432334433223455432","01111110122222211233332112344321","11111111122222211233332112344321","22222222333333331222222112222221","12222221222222222233332222333322")
X=(-21,-19,-12,-8,8,12,19,21)
Y=(-11,-10,-9,-1,1,9,10,11)
Z=(-10,-1,1,10)
W=(-11,-9,9,11)
R={(25,27):(28,26),(25,23):(21,24),(95,97):(98,96),(95,93):(91,94)}
B=lambda s:21<=s<=98 and 1<=s%10<=8
G={2:X,6:Y,4:Z,3:W,5:Z+W}
def p(x):
 b=[0]*120;p,s,c,e=x.split()[:4]
 for r,w in enumerate(p.split('/')):
  i=0
  for h in w:
   if h.isdigit():i+=int(h)
   else:b[(9-r)*10+i+1]=('pnbrqk'.index(h.lower())+1)*(1 if h.isupper()else-1);i+=1
 return b,s=='w',c,-1 if e=='-'else(int(e[1])+1)*10+ord(e[0])-96
def a(b,q,w):
 p=1 if w else-1
 for k in(q-9,q-11)if w else(q+9,q+11):
  if b[k]==p:return 1
 for k in X:
  s=q+k
  if b[s]==p*2:return 1
 for k in Y:
  s=q+k
  if b[s]==p*6:return 1
 for k in Z+W:
  s=q
  while 1:
   s+=k
   if not B(s):break
   g=b[s]
   if g:
    if(g>0)==w and abs(g) in((4,5)if k in Z else(3,5)):return 1
    break
 return 0
def g(b,w,c,e):
 m=[]
 for s in range(120):
  p=b[s]
  if not p or(p>0)!=w:continue
  t=abs(p);r=s//10-2
  if t==1:
   k=10 if w else-10;h=1 if w else 6;d=s+k;J='qnrb'if(w and r==6)or(not w and r==1)else['']
   if B(d) and not b[d]:
    for j in J:m.append((s,d,j))
    if r==h and not b[d+k] and 21<=d+k<=98:m.append((s,d+k,''))
   for x in(k-1,k+1):
    d=s+x
    if B(d):
     g=b[d]
     if(g and(g>0)!=w)or d==e:
      for j in J:m.append((s,d,j))
  else:
   for k in G[t]:
    z=s
    while 1:
     z+=k
     if not B(z):break
     g=b[z]
     if not g:m.append((s,z,''))
     else:
      if(g>0)!=w:m.append((s,z,''))
      break
     if t in(2,6):break
 for f,t,R_ in((25,27,'K'),(25,23,'Q'),(95,97,'k'),(95,93,'q')):
  D=1 if t>f else-1;E=[f+D,f+2*D]+([f+3*D]if t<f else[])
  if R_ in c and not any(b[x] for x in E) and not any(a(b,x,not w) for x in(f,f+D,f+2*D)):m.append((f,t,''))
 return m
def u(b,w,c,e,m):
 f,t,j=m;n=list(b);p=n[f];n[f]=0;x=c;y=-1
 if abs(p)==6 and(f,t)in R:A,B=R[(f,t)];n[A]=0;n[B]=4 if w else-4
 for z in(f,t):
  for i in{25:"KQ",95:"kq",21:"Q",28:"K",91:"q",98:"k"}.get(z,''):x=x.replace(i,'')
 if abs(p)==1 and t==e:n[e+(-10 if w else 10)]=0
 if abs(p)==1 and abs(t-f)==20:y=(f+t)//2
 if j:n[t]=('pnbrqk'.index(j)+1)*(1 if w else-1)
 else:n[t]=p
 return n,not w,x or'-',y
def l(b,w,c,e):
 z=[];v=6 if w else-6
 for m in g(b,w,c,e):
  n,_,_,_=u(b,w,c,e,m)
  if not a(n,n.index(v),not w):z.append(m)
 return z
def o(b,w):
 s=0
 for i in range(120):
  p=b[i]
  if not p:continue
  t=abs(p);r=i//10-2;f=i%10-1
  v=V[t]+((ord(P[t][min(r,7-r)*8+f])-50)*10 if t!=6 else 0)
  s+=v if p>0 else-v
 return s if w else-s
def s(b,w,c,e,h,al,be):
 global d;d+=1
 if d>N:return 0,None
 if not h:
  sp=o(b,w)
  if sp>=be:return be,None
  if al<sp:al=sp
  z=[m for m in l(b,w,c,e)if b[m[1]] or m[2]]
 else:
  z=l(b,w,c,e)
  if not z:
   v=6 if w else-6
   if a(b,b.index(v),not w):return-99999+(10-h),None
   return 0,None
 z.sort(key=lambda m:V[abs(b[m[1]])],reverse=True)
 k=z[0] if h else None
 for m in z:
  n,x,y,z_=u(b,w,c,e,m)
  v,_=s(n,x,y,z_,h-1 if h else 0,-be,-al);v=-v
  if v>=be:return be,None
  if v>al:al=v;k=m if h else k
 return al,k
def c(f,x):
 global d;d=0;b,w,c_,e=p(f);z=l(b,w,c_,e);k=None;fn=lambda q:chr(q%10+96)+str(q//10-1)
 for h in range(1,10):
  if d>=N:break
  j=None;v_=-99999
  for m in z:
   if d>=N:break
   n,x_,y,z_=u(b,w,c_,e,m)
   v,_=s(n,x_,y,z_,h-1,-100000,100000);v=-v
   if d>=N:break
   if v>v_:v_=v;j=m
  if j and d<N:k=j
  else:break
 if k is not None:
  t=fn(k[0])+fn(k[1])+k[2]
  if t in x:return t
 return min(x,key=lambda m:-V[abs(b[(int(m[3])+1)*10+ord(m[2])-96])])
def m():
 e=os.getenv('MAKEFUN_ENV_URL') or sys.exit(1)
 r=lambda x,d:json.load(U.urlopen(U.Request(e+x,data=d)))
 while 1:
  g=r('/game',b'{}')
  if g['status']=='done':break
  while g['status']=='your_move':
   k=c(g['fen'],g['legal_moves'])
   g=r('/move',json.dumps({'game':g['game'],'move':k}).encode())
m()
