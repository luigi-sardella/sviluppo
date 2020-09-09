#! usr/bin/env python
# -*- coding: latin-1 -*-
from math import *
from array import *
import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
from scipy import linalg, optimize
from random import randint

def integrale(func,a,b):
    # Sottoprogramma di integrazione numerica
    return qsimp(func,a,b)

def trapzd(func,a,b,s,n):
   # Calcola col metodo dei trapezi un integrale definito
   if n==1:
      s = 0.5 * (b-a) * ( func(a) + func(b) )
   else:
      it = 2**(n-2) # pow(2,(n-2))
      tnm = 1. * it
      delta = ( b - a ) / tnm
      x = a + 0.5 * delta
      sum = 0
      for j in range(1,it+1):
          sum = sum + func(x)
          x = x + delta
      s = 0.5 * ( s + (b-a) * sum / tnm )
   return s

def qsimp(func,a,b):
    # Calcola col metodo di Simpson un integrale definito
    # Copr. 1986-92 Numerical Recipes Software m2D[!.
    # USES trapzd
    EPS = 1e-6
    JMAX = 20
    os = ost = -1e30
    st = None
    for j in range(1,JMAX+1):
        st = trapzd(func,a,b,st,j)
        s = (4*st-ost)/3
        if abs(s-os) < EPS*abs(os):
           return s
        os = s
        ost = st
    print ("Too many steps in qsimp \n")

def solve(func,x_start):
    # Zero di una funzione
    #print '....  solve  ....'
    x1, x2 = x_start, x_start*1.1
    x1, x2 = zbrac(func,x1,x2)
    tol = (1e-6)*(x1+x2)/2
    return zbrent(func,x1,x2,tol)

def solve2(func,x1,x2):
    # Zero di una funzione
    tol = (1e-6)*(x1+x2)/2
    return zbrent(func,x1,x2,tol)

def solveInter(func):
    # Zero di una funzione, calcolato interattivamente
    x1, x2 = scrsho(func)
    tol = (1e-6)*(x1+x2)/2
    return zbrent(func,x1,x2,tol)

def zbrac(func,x1,x2):
    # Delimita uno zero di una funzione, che poi sara' calcolato da zbrent
    # Copr. 1986-92 Numerical Recipes Software m2D[!.
    FACTOR = 1.6
    NTRY = 50
    if x1 == x2:
       print ("You have to guess an initial range in zbrac \n")
    for j in range(1,NTRY+1):
        f1, f2 = func(x1), func(x2)
        if f1*f2 < 0:
           return x1, x2
        if abs(f1) < abs(f2):
           x1 = x1 + FACTOR*(x1-x2)
           f1 = func(x1)
        else:
           x2 = x2 + FACTOR*(x2-x1)
           f2 = func(x2)
    print ("Nessuno zero")

def zbrent(func,x1,x2,tol):
    # Calcola la radice di una funzione col metodo di Brent
    # Copr. 1986-92 Numerical Recipes Software m2D[!.
    ITMAX = 100
    EPS = 3e-8
    a, b = x1, x2
    fa, fb = func(a), func(b)
    if (fa>0 and fb>0) or (fa<0 and fb<0):
       print ("Root must be bracketed for zbrent \n")
    c, fc = b, fb
    for iter in range(1,ITMAX+1):
        if (fb>0 and fc>0) or (fb<0 and fc<0):
           c, fc = a, fa
           e = d = b - a
        if abs(fc) < abs(fb):
           a, fa = b, fb
           b, fb = c, fc
           c, fc = a, fa
        tol1 = 2*EPS*abs(b) + 0.5*tol
        xm = .5*(c-b)
        if (abs(xm) <= tol1) or (fb == 0):
           return b
        if abs(e)>=tol1 and abs(fa)>abs(fb):
           s = fb / fa
           if a==c:
              p = 2*xm*s
              q = 1-s
           else:
              q = fa/fc
              r = fb/fc
              p = s*( 2*xm*q*(q-r) - (b-a)*(r-1) )
              q = (q-1)*(r-1)*(s-1)
           if p>0:
              q = -q
           p = abs(p)
           if 2*p < min( 3*xm*q-abs(tol1*q), abs(e*q) ):
              e = d
              d = p/q
           else:
              e = d = xm
        else:
           e = d = xm
        a, fa = b, fb
        if abs(d) > tol1:
           b = b + d
        else:
           b = b + tol1*signum(xm)
        fb = func(b)
    print ("zbrent exceeding maximum iterations \n")
    return b

def ArCer( d ):
    return pi * pow(d,2) / 4

def DiaCer( a ):
    return sqrt( (4/pi) * a )

def min(x1,x2):
    if x1 < x2:
       return x1
    else:
       return x2

def max(x1,x2):
    if x1 > x2:
       return x1
    else:
       return x2

def signum(x):
    if x > 0:
       return 1
    elif x < 0:
       return -1
    else:
       return 0

def sign(x,s):
    if s >= 0:
       return x
    elif s < 0:
       return -x

def eqv(a,b):
    return (a and b) or (not a and not b)    

def StamDato(variabile,titolo,unita,stampo='si'):
    # Inizializza variabili, stampa il titolo e l'unita' di misura
    try:
        valore = eval(variabile)
    except TypeError:
        valore = variabile
    ParQDx = ']'
    ParQSx = ' ['
    if unita == '':
       ParQDx = ParQSx = ''
    if stampo == 'si':
       print (("%s %g %s") .format ( titolo+" ", valore, ParQSx+unita+ParQDx))
    return valore

def Pot(x,a):
    # Potenza x^a dove x non sia necessariamente numerico
    try:
        return pow(x,a)
    except TypeError:
        y = eval(x)
        return pow(y,a)

class Matrice:
   "Matrici: a=Matrice(5,4), a.get(1,3), a.put(2,1,34.)"
   def __init__(self,nr,nc):
       self.nr, self.nc = nr, nc
       ne = nr * nc
       self.elem = []
       for i in range(1,ne+1):
           self.elem.append(0)
   def k(self,i,j):
       if i<1 or i>self.nr or j<1 or j>self.nc :
          print ('\n')
          print (' i = ', i, ' nr = ', self.nr)
          print (' j = ', j, ' nc = ', self.nc)
          raise IndiciFuoriCampo
       else:
          return (i-1)*self.nc + j - 1
   def get(self,i,j):
       return self.elem[self.k(i,j)]
   def put(self,i,j,e):
       self.elem[self.k(i,j)] = e
   def add(self,i,j,e):
       self.elem[self.k(i,j)] += e


class MatrArray(Matrice):
   "Matrici: a=MatrArray('f',5,4), a.get(1,3), a.put(2,1,34.)"
   def __init__(self,typecode,nr,nc):
       self.nr, self.nc = nr, nc
       ne = nr * nc
       if typecode in ('b','B','h','H','i','I','l','L'):
          iniz = 0
       elif typecode in ('f','d'):
          iniz = 0.
       else:
          iniz = ''
       self.elem = array(typecode,[])
       for i in range(1,ne+1):
           self.elem.append(iniz)

class Vettore:
   "Vettori: v=Vettore(5), v.get(3), v.put(2, 34.)"
   def __init__(self,nr):
       self.nelem = self.nc = self.nr = nr
       self.elem = []
       for i in range(1,nr+1):
           self.elem.append(0)
   def get(self,i):
       if i<1 or i>self.nr:
          print ('\n')
          print (' i = ', i, ' nr = ', self.nr)
          raise IndiciFuoriCampo
       else:
          return self.elem[i-1]
   def put(self,i,e):
       if i<1 or i>self.nr:
          print ('\n')
          print (' i = ', i, ' nr = ', self.nr)
          raise IndiciFuoriCampo
       else:
          self.elem[i-1] = e
   def add(self,i,e):
       if i<1 or i>self.nr:
          print ('\n')
          print (' i = ', i, ' nr = ', self.nr)
          raise IndiciFuoriCampo
       else:
          self.elem[i-1] += e

class VettArray(Vettore):
   "Vettori: v=VettArray('f',5), v.get(3), v.put(2, 34.)"
   def __init__(self,typecode,nr):
       self.nelem = self.nc = self.nr = nr
       if typecode in ('b','B','h','H','i','I','l','L'):
          iniz = 0
       elif typecode in ('f','d'):
          iniz = 0.
       else:
          iniz = ''
       self.elem = array(typecode,[])
       for i in range(1,nr+1):
           self.elem.append(iniz)

def Grafico(fx,x1,x2):  # *** Stampa un grafico della funzione fx nell'intervallo x1, x2 ***
  ISCR,JSCR = 30, 19                   # Numero delle posizioni orizzontali e verticali
  scr = Matrice(ISCR,JSCR)
  y   = Vettore(ISCR)
  blank,zero,yy,xx,ff = ' ','-','l','-','x'
  x1, x2 = float(x1), float(x2)
  for j in range(1,JSCR):              # Riempie i lati verticali col carattere 'l'
    scr.put(1,j,yy)
    scr.put(ISCR,j,yy)
  for i in range(2,ISCR):              # Riempie cima e fondo col carattere '-'
    scr.put(i,1,xx)
    scr.put(i,JSCR,xx)
    for j in range(2,JSCR):            # Riempie l'interno con spazi ' '
      scr.put(i,j,blank)
  dx = (x2-x1)/(ISCR-1)
  x = x1
  ybig = 0.                            # I limiti includono 0
  ysml = ybig
  for i in range(1,ISCR+1):
    fxx = fx(x)
    y.put(i,fxx)                       # Valuta la funzione ad intervalli uguali,
    if fxx < ysml: ysml = fxx          # trova il massimo ed il minimo
    if fxx > ybig: ybig = fxx
    x = x + dx
  if ybig == ysml: ybig = ysml+1.      # Si assicura di separare cima e fondo
  dyj = (JSCR-1)/(ybig-ysml)
  jz = int(1-ysml*dyj)
  for i in range(1,ISCR+1):            # Nota quale riga corrisponde a 0.
    scr.put(i,jz,zero)                 # Pone un indicatore all'altezza della funzione e a 0.
    j = int(1+(y.get(i)-ysml)*dyj)
    scr.put(i,j,ff)
  print ('\n')
  for j in range(JSCR, 0, -1):
    for i in range(1,ISCR):
      print (scr.get(i,j), end="")
    if j == JSCR:
#     print ('{0:s} {1:s} {2:5.4g}'.format (scr.get(ISCR,j), ' Massimo = ', ybig))
      print(scr.get(ISCR,j), end = " ")
      print ('{0:s} {1:5.4g}'.format (' Massimo = ', ybig))
    elif j == 1:
#     print (("%s %s %5.4g") .format (scr.get(ISCR,j), ' Minimo = ', ysml)
      print(scr.get(ISCR,j), end = " ")
      print ('{0:s} {1:5.4g}'.format (' Minimo = ', ysml))
    else:
      print (scr.get(ISCR,j))
  print ('x1 = ', x1, '  x2 = ', x2)

#  (C) Copr. 1986-92 Numerical Recipes Software m2D[!.

def scrsho(fx):     # *** Grafico interattivo della funzione fx nell'intervallo x1, x2 ***
    ISCR,JSCR = 30, 19                        # Numero delle posizioni orizzontali e verticali
    scr = Matrice(ISCR,JSCR)
    y   = Vettore(ISCR)
    blank,zero,yy,xx,ff = ' ','-','l','-','x'
    while (1):
       print  ('\n Enter x1,x2 (= to stop)')    # Altro grafico, o esce se x1 = x2
       x1,x2 = eval(input())
       if x1==x2:
          try:
             return x1out, x2out
          except NameError:
             break
       x1, x2 = float(x1), float(x2)
       for j in range(1,JSCR):                # Riempie i lati verticali col carattere 'l'
           scr.put(1,j,yy)
           scr.put(ISCR,j,yy)
       for i in range(2,ISCR):                # Riempie cima e fondo col carattere '-'
           scr.put(i,1,xx)
           scr.put(i,JSCR,xx)
           for j in range(2,JSCR):            # Riempie l'interno con spazi ' '
               scr.put(i,j,blank)
       dx = (x2-x1)/(ISCR-1)
       x = x1
       ybig = 0.                              # I limiti includono 0
       ysml = ybig
       for i in range(1,ISCR+1):
           fxx = fx(x)
           y.put(i,fxx)                       # Valuta la funzione ad intervalli uguali,
           if fxx < ysml: ysml = fxx          # trova il massimo ed il minimo
           if fxx > ybig: ybig = fxx
           x = x + dx
       if ybig == ysml: ybig = ysml+1.        # Si assicura di separare cima e fondo
       dyj = (JSCR-1)/(ybig-ysml)
       jz = int(1-ysml*dyj)
       for i in range(1,ISCR+1):              # Nota quale riga corrisponde a 0.
           scr.put(i,jz,zero)                 # Pone un indicatore all'altezza della funzione e a 0.
           j = int(1+(y.get(i)-ysml)*dyj)
           scr.put(i,j,ff)
       print ('\n')
       for j in range(JSCR, 0, -1):
           for i in range(1,ISCR):
               print (scr.get(i,j), end=" ")
           if j == JSCR:
               print(scr.get(ISCR,j), end = " ")
               print ('{0:s} {1:5.4g}'.format (' Massimo = ', ybig))
           elif j == 1:
               print(scr.get(ISCR,j), end = " ")
               print ('{0:s} {1:5.4g}'.format (' Minimo = ', ysml))
           else:
               print (scr.get(ISCR,j))
       print ('x1 = ', x1, '  x2 = ', x2)
       x1out, x2out = x1, x2

#  (C) Copr. 1986-92 Numerical Recipes Software m2D[!.
#
from sys import *
from math import *
#from Numeric import *
def StampaPlot (A,ID=['X','Y1','Y3','Y3'],mulSca=1,XSCALE=0.,calcAsc=0,deltAsc=0.,precAsc=5,precOrd=2):
#
#    Tracciamento sul tabulato di sino a tre funzioni sullo stesso
#    grafico, insieme ad una tabulazione delle stesse funzioni.
#
#    Argomenti di chiamata :
#
#    A[M,N]    = Matrice contenente le ordinate; in A[M,0] le ascisse
#    ID[3]     = Identificazione delle curve e dell'ascissa
#    mulSca    = Se 1 ogni curva viene tracciata col suo proprio 
#                fattore di scala
#    XSCALE    = Ordinata corrispondente al 100% sul grafico;
#                XSCALE viene automaticamente aumentata sino al valore
#                max abs A(I,J) , se e' inferiore a questo valore
#    calcAsc   = Se 0 si usa il contenuto di V;
#                se 1 si calcolano le ascisse
#    deltAsc   = L'incremento costante di V(J)
#
#*****************************************************************************
#
   lineLun = 81
   Spost  = precAsc-3
#
   ScaMul = [0.,0.,0.,0.]
   NUMBER = ' 123456789'
#
   scale = XSCALE
   Amax = 0.
   Amin = 0.
   CurMax = 0.
   CurMin = 0.
   crossX = None
#
   M,N = A.shape
   print ('\n\n')
   if N > 4:
      print ("\nNon più di tre curve!")
      exit()
   for i in range(1,N):
      for j in range(1,M):
         Aji = A[j,i]
         if Aji > CurMax: CurMax = Aji
         if Aji < CurMin: CurMin = Aji
      if not crossX: crossX = CurMin * CurMax < 0.
      if CurMax > Amax: Amax = CurMax
      if CurMin < Amin: Amin = CurMin
      if mulSca:
         if CurMax >= abs( CurMin ):
            ScaMul[i] = CurMax
         else:
            ScaMul[i] = CurMin
      CurMax = 0.
      CurMin = 0.
   if not mulSca: crossX = Amax * Amin < 0.
#
   if scale < Amax: scale = Amax
   if scale < abs(Amin): scale = abs(Amin)
   if Amax <= 0.: scale = - scale
#
   if scale == 0.:
      print ('\nDati nulli ')
      exit()
#
   for i in range(1,N):
      if mulSca:
         print ('  CURVA%2i : il 100%% corrisponde a %10.3e  -  %s'.format(i, ScaMul[i], ID[i]))
      else:
         print ('  CURVA%2i  -  %s'.format(i, ID[i]))
#
   if not mulSca: print ("  Il 100%% corrisponde a %10.3e").format(scale)
   print ('\n')
#
   if crossX:
        print (' '*(Spost+15)+'-100'+' '*5+'-80'+' '*5+'-60'+' '*5+'-40'+' '*5+'-20'+
               ' '*6+'0'+' '*6+'20'+' '*6+'40'+' '*6+'60'+' '*6+'80'+' '*6+'100 %' )
   else:
        print ( ' '*(Spost+17)+'0'+' '*6+'10'+' '*6+'20'+' '*6+'30'+' '*6+'40'+' '*6+'50'+
                ' '*6+'60'+' '*6+'70'+' '*6+'80'+' '*6+'90'+' '*6+'100 %' )
#
   print (' '*(Spost+13), '   +    '*11),
   for icurv in range(1,N-1):
      print ('  CURVA %1i  '.format(icurv), end="")
   print ('  CURVA %1i  '.format(N-1))
#
   ascis = 0.
#
   for j in range(1,M): 
#
      ascis = ascis + deltAsc
# 
      if j == 1 or j == M-1:
        car = '.'
      else:
        car = ' '
      LINE = []
      for k in range(0,lineLun+1): LINE.append(car)
#
      LINE[1],LINE[lineLun] = '.', '.'
      if crossX: LINE[lineLun/2+1] = '.'  
#
      for i in range(1,N):
         if mulSca:
            if ScaMul[i] != 0.:
               r = (A[j,i] / ScaMul[i]) * float(lineLun-1)
            else:
               r = 0.
         else:
            r = (A[j,i] / scale) * float(lineLun-1)
#
         if crossX: r = r / 2. + float(lineLun-1)/2
         r = r + 1.4999999
         index = int(r)
#
         if (LINE[index] == ' ') or (LINE[index] == '.' ):
            LINE[index] = NUMBER[i]
         else:
            LINE[index] = 'X'       
#
      LineCar = ''
      for k in range(0,lineLun+1): LineCar = LineCar + LINE[k]
      if calcAsc:
         vv = ascis
      else:
         vv = A[j,0]
#
      FormatAsc = '  |%+.'+str(precAsc)+'e  %s   '
      FormatOrd =   ' %+.'+str(precOrd)+'e'
      print (FormatAsc.format(vv,LineCar), end="")
      for icurv in range(1,N-1):
         print (FormatOrd.format(A[j,icurv]), end="")
      print (FormatOrd.format(A[j,N-1]))
   
   print (' '*(Spost+13), '   +    '*11),
   for icurv in range(1,N-1):
      print ('  CURVA %1i  '.format(icurv), end="")
   print ('  CURVA %1i  '.format(N-1))
#
   if crossX:
        print (' '*(Spost+15)+'-100'+' '*5+'-80'+' '*5+'-60'+' '*5+'-40'+' '*5+'-20'+
               ' '*6+'0'+' '*6+'20'+' '*6+'40'+' '*6+'60'+' '*6+'80'+' '*6+'100 %' )
   else:
        print ( ' '*(Spost+17)+'0'+' '*6+'10'+' '*6+'20'+' '*6+'30'+' '*6+'40'+' '*6+'50'+
                ' '*6+'60'+' '*6+'70'+' '*6+'80'+' '*6+'90'+' '*6+'100 %' )
#
   print ("""
         *
        ***
       *****
         *
         ******  """, ID[0])
   print ('\n')
#
   for i in range(1,N):
      if mulSca:
         print ('  CURVA%2i : il 100%% corrisponde a %10.3e  -  %s'.format(i, ScaMul[i], ID[i]))
      else:
         print ('  CURVA%2i  -  %s'.format(i, ID[i]))
#
#
class interpol:
   "Interpolazione da una tabella, data con due vettori"
    
   def __init__(self,ascisse,ordinate,ordine,nome = 'tabella'):
      self.nome = nome
      self.x = x = ascisse
      self.y = ordinate
      self.ordine = ordine
      self.npunti = x.nelem
      if self.y.nelem != x.nelem:
         print ("Errore in ", nome, ": differenti lunghezze")
      for i in range(1,self.npunti-1):
         if eqv(x.get(i)>x.get(i+1), x.get(i+1)<x.get(i+2)):
            print ("Errore in ", nome, ": ascisse non ordinate")
            print ('i,x(i),x(i+1),x(i+2): ',i,x.get(i),x.get(i+1),x.get(i+2))
            break
           
   def locate(self,xv):
      # Dato un valore xv fornisce un indice j tale che xv sia
      # compreso fra x(j) e x(j+1); j=0 o j=npunti indicano che
      # xv è fuori dell'intervallo. Inoltre fornisce l'indice k
      # che punta alla prima delle n componenti di x su cui si
      # interpola.
      jl = 0
      ju = self.npunti+1
      while ju - jl > 1:
         jm = (ju+jl)//2
         if eqv( self.x.get(self.npunti) > self.x.get(1), xv > self.x.get(jm) ):
            jl = jm
         else:
            ju = jm
      j = jl
      return j, min(max(j-(self.ordine-1)//2,1),self.npunti+1-self.ordine)
          
   def polint(self,x):
      c = VettArray('d',self.npunti)
      d = VettArray('d',self.npunti)
      ordine = self.ordine
      j, k = self.locate(x)
      ns = 1
      dy = 'n.a.'
      dif = abs(x-self.x.get(k))
      for i in range(1,ordine+1):
         dift = abs(x-self.x.get(i+k-1))
         if dift < dif:
            ns = i
            dif = dift
         c.put(i,self.y.get(i+k-1))
         d.put(i,self.y.get(i+k-1))
      y = self.y.get(ns+k-1)
      ns -= 1
      for m in range(1,ordine-1+1):
         for i in range(1,ordine-m+1):
            ho = self.x.get(i+k-1)-x
            hp = self.x.get(i+k-1+m)-x
            w = c.get(i+1)-d.get(i)
            den = ho - hp
            if den==0: print ('failure in polint')
            den = w / den
            d.put(i,hp*den)
            c.put(i,ho*den)
         if 2*ns < ordine-m:
            dy = c.get(ns+1)
         else:
            dy = d.get(ns)
            ns -= 1
         y += dy
      return y #, dy
       
#  (C) Copr. 1986-92 Numerical Recipes Software m2D[!.

def ludcmp(a):
   'Programma per la decomposizione LU di Crout di una matrice'
   # Given a matrix a(1:n,1:n) this routine replaces it by the LU
   # decomposition of a rowwise decomposition of itself. a is output
   # (...) indx(1:n) is an output vector that records the row
   # permutation effected by the partial pivoting; d is output as
   # +/-1 depending on whether the number of row interchanges was
   # even or odd, respectively. This routine is used in combination
   # with lubksb to solve linear equations or invert a matrix.

   TINY=1.0e-20
   d=1.                    # No row interchanges yet
   n = a.nc
   if n != a.nr: 
      print ('Matrice non quadrata!')
      exit
   vv = VettArray('d',n)   # vv stores the implicit scaling of each row
   indx = VettArray('i',n)
   for i in range(1,n+1):  # Loop over rows to get the implicit 
      aamax=0.             # scaling information
      for j in range(1,n+1):
         aij = abs(a.get(i,j))
         if aij > aamax: aamax = aij
      if aamax == 0.: 
        print ('singular matrix in ludcmp')
        exit
      vv.put(i,1./aamax)
   for j in range(1,n+1):  # Loop over rows of Crout's method
      for i in range(1,j-1+1): # Equation (2.3.12) except
         sum = a.get(i,j)
         for k in range(1,i-1+1): sum += -a.get(i,k)*a.get(k,j)
         a.put(i,j,sum)
      aamax=0.
      for i in range(j,n+1):
         sum = a.get(i,j)
         for k in range(1,j-1+1): sum += -a.get(i,k)*a.get(k,j)
         a.put(i,j,sum)
         dum = vv.get(i)*abs(sum)
         if dum >= aamax:
           imax = i
           aamax = dum
      if j != imax:
        for k in range(1,n+1):
           dum = a.get(imax,k)
           a.put(imax,k,a.get(j,k))
           a.put(j,k,dum)
        d = -d
        vv.put(imax,vv.get(j))
      indx.put(j,imax)
      if a.get(j,j) == 0.: a.put(j,j,TINY)
      if j != n:
        dum = 1./a.get(j,j)
        for i in range(j+1,n+1): a.put(i,j,a.get(i,j)*dum)
   return a, indx, d

#  (C) Copr. 1986-92 Numerical Recipes Software m2D[!.

def lubksb(a,indx,b):
   n = a.nc
   ii = 0
   for i in range(1,n+1):
      ll = indx.get(i)
      sum = b.get(ll)
      b.put(ll,b.get(i))
      if ii != 0:
        for j in range(ii,i-1+1): sum += -a.get(i,j)*b.get(j)
      elif sum != 0.: ii = i
      b.put(i,sum)
   for i in range(n,1-1,-1):
      sum = b.get(i)
      for j in range(i+1,n+1): sum += -a.get(i,j)*b.get(j)
      b.put(i,sum/a.get(i,i))
   return b
 
#  (C) Copr. 1986-92 Numerical Recipes Software m2D[!.

if __name__ == '__main__':
   # Prova dei programmi del modulo

   def funzione(x):
       return pow(x,2)

   def funzione_da_annullare(x):
       return funzione(x) - 4

   print ("\n... Prova dell'integrale di x^2 da 0 a 1")
   print (integrale(funzione,0,1))

   print ('\n... Prova del solutore: x^2 - 4 = 0')
   print (solve2(funzione_da_annullare,1,3))

   print ('\n... Prova del solutore interattivo: x^2 - 4 = 0')
   print (solveInter(funzione_da_annullare))

   print ("\n... Prova di StamDato: col primo argomento '5*5' ")
   print (StamDato( '5*5','cinque per cinque',"Nessuna unita'" ))
   print ("... ora col primo argomento 25, senza indicazione di unita':")
   print (StamDato(  25,  'cinque per cinque','' ))

   print ('\n... Prova di Pot')
   print (Pot( 4, 2 ))
   print (Pot( funzione(2), 2 ))

   print ('\n... Prova di Grafico')
   def funzione(x):
       return sin(x)
   Grafico(funzione,0,2*pi)

   print ('\n... Prova di scrsho')
   print (scrsho(funzione))

   print ("... Prova della classe MatrArray: a(i,j) = float( (i-1)*nc + j ), i=1,nr; j=1,nc ")
   nr, nc = 3, 4
   a = MatrArray('f',nr,nc)
   print (" Righe: ", a.nr," , ", end=" ")
   print (" colonne: ", a.nc)
   for i in range(1,nr+1):
       for j in range(1,nc+1):
          a.put(i,j, float( (i-1)*nc + j) )
   for i in range(1,nr+1):
       print (' ')
       for j in range(1,nc+1):
           print (a.get(i,j), end=" ")

   print ("\n... Prova della classe VettArray ")
   nr = 7
   v = VettArray('l',nr)
   print (" Dimensione: ", v.nr)
   print (' ')
   for i in range(1,nr+1):
       v.put(i, 10*i )
   for i in range(1,nr+1):
       print (v.get(i), end=" ")

   print ('\n... v.nr = ',v.nr, '; v.nc = ',v.nc, '; v.nelem = ',v.nelem)

   print ("\n... Prova della classe interpol ")

   def fun(x):
      return sin(x)
   x = VettArray('d',100)
   y = VettArray('d',100)
   for i in range(1,101):
      xx = i * 0.1
      x.put(i,xx)
      y.put(i,fun(xx))
   tab1 = interpol(x,y,1,'1.o ordine')
   tab2 = interpol(x,y,2,'2.o ordine')
   tab3 = interpol(x,y,3,'3.o ordine')
   tab4 = interpol(x,y,4,'4.o ordine')
   while 1:
      xx = float(input('Ascissa 0..10 [999 per finire]:'))
      if xx == 999: break
      print (xx, fun(xx))
      print (tab1.locate(xx),tab1.polint(xx))
      print (tab2.locate(xx),tab2.polint(xx))
      print (tab3.locate(xx),tab3.polint(xx))
      print (tab4.locate(xx),tab4.polint(xx))
      print (' ')

   print ('\nProve dei programmi di soluzione dei sistemi lineari:')
   print ('\nProva della triangolarizzazione')
   n = 7
   fd = 'd'
   a  = MatrArray(fd,n,n)
   x  = MatrArray(fd,n,n)
   xu = MatrArray(fd,n,n)
   xl = MatrArray(fd,n,n)
   jndx = VettArray('i',n)
   for i in range(1,n+1):
      for j in range(1,n+1): a.put(i,j,float(randint(-9,9)))
   for i in range(1,n+1):
      for j in range(1,n): print (a.get(i,j), end=" ")
      print (a.get(i,n))
#     perform the decomposition)
   a,indx,d = ludcmp(a)
#     compose separately the lower and upper matrices
   for k in range(1,n+1):
      for l in range(1,n+1):
         if l > k:
           xu.put(k,l,a.get(k,l))
           xl.put(k,l,0.0)
         elif l < k:
           xu.put(k,l,0.0)
           xl.put(k,l,a.get(k,l))
         else:
           xu.put(k,l,a.get(k,l))
           xl.put(k,l,1.0)
#     compute product of lower and upper matrices for
#     comparison with original matrix.
   for k in range(1,n+1):
      jndx.put(k,k)
      for l in range(1,n+1):
         x.put(k,l,0.0)
         for j in range(1,n+1): x.add(k,l,xl.get(k,j)*xu.get(j,l))
   print ('\nProduct of lower and upper matrices (unscrambled):')
   for k in range(1,n+1):
      dum = jndx.get(indx.get(k))
      jndx.put(indx.get(k),jndx.get(k))
      jndx.put(k,dum)
   for k in range(1,n+1):
      for j in range(1,n+1):
         if jndx.get(j) == k:
           for l in range(1,n): print (x.get(j,l), end=" ")
           print (x.get(j,n))
   print ('\nLower matrix of the decomposition:')
   for k in range(1,n+1):
      for l in range(1,n): print (xl.get(k,l), end=" ")
      print (xl.get(k,n))
   print ('\nUpper matrix of the decomposition:')
   for k in range(1,n+1):
      for l in range(1,n): print (xu.get(k,l), end=" ")
      print (xu.get(k,n))
   print ('\nProva della risostituzione'     )
   n = 7
   m = 3
   fd = 'd'
   a = MatrArray(fd,n,n)
   c = MatrArray(fd,n,n)
   b = MatrArray(fd,n,m)
   x = VettArray(fd,n)
   for i in range(1,n+1):
      for j in range(1,n+1): a.put(i,j,float(randint(-9,9)))
   print ('\nMatrice dei coefficienti:')
   for i in range(1,n+1):
      for j in range(1,n): print (a.get(i,j), end=" ")
      print (a.get(i,n))
   for i in range(1,n+1):
      for j in range(1,m+1): b.put(i,j,float(randint(-9,9)))
   print ('\nTermini noti:')
   for i in range(1,n+1):
      for j in range(1,m): print (b.get(i,j), end=" ")
      print (b.get(i,m))
#  save matrix a for later testing
   for i in range(1,n+1):
      for j in range(1,n+1): c.put(i,j,a.get(i,j))
#  perform the decomposition
   c,indx,d = ludcmp(c)
#  solve equations for each right-hand vector
   for k in range(1,m+1):
      for l in range(1,n+1): x.put(l,b.get(l,k))
      x = lubksb(c,indx,x)
#  test results with original matrix
      print ('\nRight-hand side vector:')
      for l in range(1,n): print (b.get(l,k), end=" ")
      print (b.get(n,k))
      print ('\nResult of matrix applied to solution vector')
      for l in range(1,n+1):
         b.put(l,k,0.0)
         for j in range(1,n+1): b.add(l,k,a.get(l,j)*x.get(j))
      for l in range(1,n): print (b.get(l,k), end=" ")
      print (b.get(n,k))
      print ('\n***********************************')

#  (C) Copr. 1986-92 Numerical Recipes Software m2D[!.
