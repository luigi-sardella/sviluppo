t mat-eqn.for
      subroutine MatEqn ( kk, c, f, ki, RecCpK, RecCpF, 
     >                   d, UnMat, x, ipivot, Nmax, n )

!----------------------------------------------------------
!
!--> Legge le matrici generate dal programma termico
!         [ K ] di conducibilita' termica
!         [ C ] di capacita' termica
!         { F } vettore dei termini noti 
!    e genera da essi un file di equazioni utilizzabile direttamente 
!    da SimCrea.
!    Si passa dalla forma implicita :
!
!        [ C ] * { DX/DTime }  +  [ K ] * { X } + { F } = 0 
!
!     alla forma esplicita :
!   
!        { DX/DTime } = - (  [ RecCpK ] * { X } + [ RecCpF ]  )
!
!     dove :
!
!       [ RecCpK ] = [ C ] ^-1 * [ K ]   
!       [ RecCpF ] = [ C ] ^-1 * { F }  
!   
      parameter ( nUnitE=22, nUnitD=23 )
!
      double precision kk(Nmax,Nmax), c(Nmax,Nmax), f(Nmax),
     >            ki(Nmax), RecCpK(Nmax,Nmax), RecCpF(Nmax)
      double precision d(Nmax), UnMat(Nmax,Nmax), x(Nmax,Nmax)
      integer ipivot( Nmax )
!
      call files ( nUnitE, nUnitD )
!
      call UniMat( UnMat, Nmax )
      call factor( c, n, d, ipivot, iflag, Nmax )
      write (*,'(1x,'' iflag [ se = 0 Singolare ] = '',i3)') iflag
!
      do j = 1, n
        call subst ( c, ipivot, UnMat(1,j), n, x(1,j), Nmax )
      end do
!
      call MOLMAT ( x, kk, RecCpK, Nmax, n, n, n )
      call MOLMAT ( x, f , RecCpF, Nmax, n, n, 1 )
!
      call CambSgn ( RecCpK, Nmax, Nmax, n, n )
      call CambSgn ( RecCpF, Nmax, 1, n, 1 )
!
      call EqnCrea ( RecCpK, RecCpF, NomFil, Nmax, n, nUnitE )
      call DatCrea ( n, nUnitD )
!
      return
      end
!
!
      subroutine MOLMAT ( A, U, V, Nmax, nrA, ncA, ncU )  !  A * U --> V
!
      double precision A(Nmax,Nmax), U(Nmax,Nmax), V(Nmax,Nmax), t
!
      do ir = 1, nrA
         do ic = 1, ncU
            t = 0.
            do j = 1, ncA
               t = t + A(ir,j) * U(j,ic)
            end do
            V(ir,ic) = t
         end do
      end do
!
      return
      end
!
	subroutine factor (w,n,d,ipivot,iflag,Nmax)

c------ INPUT -------------------------------

       double precision  w(Nmax,Nmax)
       integer n,Nmax

c------ OUTPUT  -----------------------------  

c      double precision w(n,n)
       integer iflag,ipivot(n)

c------ INTERNE -----------------------------
       double precision d(n),awikod,colmax,ratio,rowmax
       integer  i,istar,j,k
       double precision temp
c--------------------------------------------

	iflag=1
	do 9 i=1,n
	ipivot(i)=i
	rowmax=0.0
	do 5 j=1,n
	rowmax=max(rowmax,abs(w(i,j)))
5	continue
	if ( rowmax .eq. 0.0) then
	    iflag=0
         rowmax=1.0
	endif
	d(i)=rowmax
9	continue
	if ( n .le. 1) return
	do 20 k=1,n-1
	colmax=abs(w(k,k))/d(k)
	istar=k
	do 13 i=k+1,n
	awikod=abs(w(i,k))/d(i)
	if ( awikod .gt. colmax )  then
	    colmax=awikod
	    istar=i
	endif
13	continue
	if ( colmax .eq. 0.0 ) then
	    iflag=0
	else
	    if ( istar .gt. k) then
             iflag=-iflag
	        i=ipivot(istar)
	        ipivot(istar)=ipivot(k)
             ipivot(k)=i
	        temp=d(istar)
	        d(istar)=d(k)
             d(k)=temp
             do 15 j=1,n
		temp=w(istar,j)
	    w(istar,j)=w(k,j)
		w(k,j)=temp
15	continue
        endif
16	do 19 i=k+1,n
	w(i,k)=w(i,k)/w(k,k)
	ratio=w(i,k)
	do 19 j=k+1,n
	w(i,j)=w(i,j)-ratio*w(k,j)
19	continue
	endif
20	continue
	if ( w(n,n) .eq. 0.0 )  iflag=0
	return
	end
!
!
	subroutine subst ( w,ipivot,b,n,x ,Nmax )

c----------INPUT---------------------------
       double precision w(Nmax,Nmax),b(n)
       integer ipivot(n),n,Nmax 

c----------OUTPUT--------------------------
       double precision x(n)

c----------LOCALI--------------------------

       double precision sum
       integer i,ip,j

	if ( n .le. 1)  then
 	    x(1)=b(1)/w(1,1)
	    return
	endif
	ip=ipivot(1)
	x(1)=b(ip)
	do 15 i=2,n
	sum=0.
	do 14 j=1,i-1
	sum=w(i,j)*x(j)+sum
14	continue
	ip=ipivot(i)
	x(i)=b(ip)-sum
15	continue
	x(n)=x(n)/w(n,n)
	do 20 i=n-1,1,-1
	sum=0.
	do 19 j=i+1,n
	sum=w(i,j)*x(j)+sum
19	continue
	x(i)=(x(i)-sum)/w(i,i)
20	continue
	return
	end
!
!
      subroutine UNIMAT ( A, n )  
!
      double precision A( n, n )
!
      do ir = 1, n
         do ic = 1, n
            A(ir,ic) = 0.
         end do
         A(ir,ir) = 1.
      end do
!
      return
      end
!
!
      subroutine EqnCrea( A, F, NomFil, Nmax, n, nUnit )
!
      double precision A(Nmax,Nmax), F(Nmax)
      character linea*66
      parameter eps = 1.e-12
      logical UltLin
!
      do i = 1, n
!
       if( abs(F( i )) .gt. eps ) then
        write(nUnit,'('' dx_('',i2,'') = '',1pe12.5 )' ) i, F( i )
       else
        write(nUnit,'('' dx_('',i2,'') = '',1pe12.5 )' ) i
       end if
!
       kont3 = 0
!
       UltLin = .false.
       do j = 1, n
!
        if( abs(A( i,j )) .gt. eps ) then
         UltLin = .true.
         kont3 = kont3 + 1
         linea(1:1) = '>'
         write( linea( (kont3-1)*20+2 : ), 100 ) A(i,j), j
  100    format ( sp, 1x, 1pe12.5, '*x_(', ss, i2, ')' )
         if ( kont3 .eq. 3 ) then
          kont3 = 0
          UltLin = .false.
          write(nUnit, '( a )' ) linea( 1 : Lung(linea) )
          write( linea, '( 66('' '') )' )
         end if
        end if
!
       end do
!
       if ( UltLin ) write(nUnit, '( a )' ) linea( 1 : Lung(linea) )
!
      end do
!
      return
      end
!
!
      integer function Lung ( riga )
!
      character riga*66
!
      i = 66
      do while ( riga(i:i) .eq. ' ' .and. i .gt. 1 )
         i = i - 1
      end do
!
      Lung = i
!
      return
      end
!
      subroutine DatCrea ( n, nD )
!
      character Numer*2
!
      write ( nD, '('' ***> Calcolo termico con Simula'')' )
      write ( nD, '('': ( Fine della descrizione del calcolo )'')' )
      write ( nD, '( i3, '' equazioni del primo ordine'' )' ) n
      do i = 1, n
         write ( nD, '( i3 )' ) i
      end do         
      write ( nD, '('': ( Fine lista delle variabili)'')' )
      write ( nD, '('': ( Fine lista delle funzioni)'')' )
      do i = 1, n
         write ( Numer, '(i2)' ) i
         write(nD,200) 0., 'Valore iniziale di '//'X'//Numer
         write(nD,200) 1.E-06,  'Tolleranza di '//'X'//Numer
      end do
!
      write(nD,300) -1.E-03, 'Intervallo d''integrazione', ' sec '
      write(nD,300)  1.E-02, 'Intervallo di stampa', ' sec '
      write(nD,300)  0., 'Istante iniziale della simulazione', ' sec '
      write(nD,300)  2., 'Istante finale della simulazione', ' sec '
!
      return
!
  200 format (' ', 1pe10.3, ' . . . ', a )
  300 format (' ', 1pe10.3, ' . . . ', a, '[', a, ']' )
!
      end
!
!
      subroutine FILES ( nUnitE, nUnitD )
!
      character*9 FilEqn, FilDat
!
  251 type *, ' '
      type '('' Nome del file delle equazioni  ? '',$)'
      accept '( a )', FilEqn
      open ( unit = nUnitE, file = FilEqn//'.eqn', type = 'unknown', 
     >       carriagecontrol = 'FORTRAN' , iostat = IOS )
      if ( IOS .ne. 0 ) then
        type *,' Il file non esiste, o il nome e'' sbagliato; riprovare'
        goto 251
      end if
!
  351 type *, ' '
      type '('' Nome del file dei dati ( D = '',a,'' ) ? '',$)', 
     >      FilEqn
      accept '( a )', FilDat
      if ( FilDat .eq. ' ' ) FilDat = FilEqn
      open ( unit = nUnitD, file = FilDat//'.dat', type = 'unknown', 
     >       carriagecontrol = 'FORTRAN' , iostat = IOS )
      if ( IOS .ne. 0 ) then
        type *,' Il file non esiste, o il nome e'' sbagliato; riprovare'
        goto 351
      end if
!
      return
      end
!
!
      subroutine CambSgn ( A,Nrimax,Ncomax,Nri,Nco )
!     
      double precision A (Nrimax,Ncomax)
      integer	       Nrimax , Ncomax ,Nri,Nco
!
      do i = 1 , Nri
        do j = 1 , Nco
          A (i,j ) = - A (i,j)
        end do
      end do
!
      return
      end
$ 