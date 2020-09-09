t simplex.for
C     *****************************************************************
C     *                                                               *
C     *   Programma per l'ottimizzazione col metodo del simplesso.    *
C     *                                                               *
C     *****************************************************************
C
      implicit double precision ( a-h, o-z )
      logical restart
C
      parameter ( iDimTa = 50 )
C      . . . Massima dimensione delle tabelle di ingresso
      parameter ( nVarMx = 30 )
C      . . . Numero massimo di variabili da determinare
      parameter ( nEqnMx = 30 )
C      . . . Numero massimo di equazioni
      parameter ( NumFun = 30 )
C      . . . Numero massimo di funzioni in uscita
      parameter ( NP = nVarMx + 1 )
C      . . . Ordine ( numero dei punti ) massimo del simplesso
C
      double precision SIMP(NP,NP), NEXT(NP), BarCen(NP), MEAN(NP),
     >                 Error(NP), P(NP), Q(NP), STEP(NP), MaxErr(NP)
      double precision f(NumFun), RHS(nEqnMx)
      integer H(NP), L(NP)
C
      character DesVar(nVarMx)*50, DesFun(NumFun)*50
C
      call Smplx( Simp, Next, BarCen, Mean, Error, P, Q, Step, H, L, 
     >            RHS, f, MaxErr, NP, iDimTa, nVarMx, nEqnMx, 
     >            NumFun, DesVar, DesFun )
C
      stop
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C 
      subroutine Smplx( Simp, Next, BarCen, Mean, Error, P, Q, Step, H, 
     >                  L, RHS, f, MaxErr, NP, iDimTa, nVarMx, nEqnMx, 
     >                  NumFun, DesVar, DesFun )
      implicit double precision ( a-h, o-z )
C
C-------------------- Versione VMS
      real*4 TempIn 
C--------------------
      double precision SIMP(NP,NP), NEXT(NP), BarCen(NP), MEAN(NP),
     >                 Error(NP), P(NP), Q(NP), STEP(NP), MaxErr(NP)
      double precision f(NumFun), RHS(nEqnMx)
      integer H(NP), L(NP)
C
      character DesVar(nVarMx)*50, DesFun(NumFun)*50
      character FrmStr*80, FrmIte*3, FrmIt1*3, fact*10
      character absis*50, Numer*2, data*10, ora*8, FilRis*9
C
      logical restart
C
      call DATE( data )
      call TIME( ora )
      call FILES ( restart, FilRis )
C
      write(1,*) ' **************************************************'
      write(1,*) ' *                                                *'
      write(1,*) ' *   Calcolo di minimizzazione con l''algoritmo    *'
      write(1,*) ' *   del simplesso                                *'
      write(1,*) ' *                                                *'
      write(1,*) ' *------------------------------------------------*'
      write(1,*) ' *                                                *'
      write(1,*) ' *              ',  data, ' ',ora,'               *'
      write(1,*) ' *                                                *'
      write(1,*) ' *               *** ',FilRis,' ***                *'
      write(1,*) ' *                                                *'
      write(1,*) ' **************************************************'
      write(1,*) ' '
C
      call LetOpz( nVar, nEqn, nFun, DesFun, NumFun, restart )
      nPun = nVar + 1
      call LetDat ( restart )
      write(1,*) ' '
      call LetTab ( restart )
C
      TempIn = SECNDS ( 0. ) 
C
      call Costan ( NP, NVarMx, NumFun, DesVar, DesFun, SIMP, STEP )
      call VlPart ( SIMP, STEP, MaxErr, DesVar, restart, MxIter,  
     >              nVar, NP)
      call Costan ( NP, NVarMx, NumFun, DesVar, DesFun, SIMP, STEP )
      call INIZIA ( SIMP, P, Q, STEP, RHS, f, nPun, NP, nEqn, nVar, 
     >              NumFun )
      call iteraz ( SIMP, NEXT, BarCen, H, L, MEAN, Error, P, Q, STEP,
     >              MaxErr, NP, MxIter, nIter, nEqn, nVar,
     >              NumFun, f, RHS )
C
  100 format ( ' *** Tempo di calcolo = ', f10.2, ' secondi ***' ) 
      write (1,*) ' ' 
      write (1,100) SECNDS ( TempIn )
      write (1,*) ' '
      type 100, SECNDS ( TempIn )
C
      write (1,*) ' '
      write (1,*) ' ***>     Errori di calcolo      <***'
      write (1,*) ' '
      write (*,*) ' '
      write (*,*) ' ***>     Errori di calcolo      <***'
      write (*,*) ' '
      write( FrmStr, '( 72('' ''))' )  
      if ( nVar .gt. 1 ) then
      k = 1
      do 9903 nlin = 1, nVar / 5 + MIN( 1, MOD( nVar, 5 ) )
        k1 = MIN( k + 4, nVar )
        write ( FrmIte, '(i3)' ) k1 - k + 1
        FrmStr = '(' //FrmIte// '('' E'',i2,''='',1pe10.3,'','') )' 
        write (1, FrmStr )  ( j, Error( j ), j = k, k1 )
        write (*, FrmStr )  ( j, Error( j ), j = k, k1 )
        k = k + 5
 9903 continue
      else
        write (1, '( '' E 1 = '', 1pe10.3 )' ) Error(1)
        write (*, '( '' E 1 = '', 1pe10.3 )' ) Error(1)
      end if
C
      call risult ( SIMP, MEAN, nIter, nVar, NP, DesVar, DesFun, 
     >              f, nFun, NumFun )
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine FILES ( restart, FilRis )
C
C--> Chiamata da : Corpo
C
      logical restart
      character sn*1, stato*3, FilRis*9, FilDat*9, FilEqn*9
C
      restart = .false.
      stato = 'new'
C
   51 write(*,*) ' '
      write(*, '( '' Nome del file dei risultati ? '', $ )' )
      read(*, '( a )' ) FilRis
C      
      open ( unit = 1, file = FilRis//'.ris', status = 'new', 
     >       iostat = IOS )
      if ( IOS .ne. 0 ) then
      write(*,*) ' Il file non esiste, o il nome e'' sbagliato; riprova'
      goto 51
      end if
C
      FilDat = FilRis
      write(*,*) ' '
      write(*,'( '' Si usano dati gia" esistenti ? ( S=D / N ) '', $ )')
      read(*, '( a )' )  sn
      write(*,*) ' '
C
  151 continue
      if ( sn .ne. 'N' .and. sn .ne. 'n' ) then 
         restart = .true.
         stato = 'old'
         write(*,'( '' Nome del file dei dati (D = '', a, '') ? '',$ )')
     >         FilRis
         read(*, '( a )' ) FilDat
         if ( FilDat .eq. ' ' ) FilDat = FilRis
      end if
C
      open ( unit = 2, file = FilDat//'.dat', status = stato, 
     >       iostat = IOS )
      if ( IOS .ne. 0 ) then
      write(*,*) ' Il file non esiste, o il nome e'' sbagliato; riprova'
      goto 151
      end if
C
  251 write(*,*) ' '
      write(*,'('' Nome del file delle equazioni ( D = '',a,'' ) ? '',
     >      $ ) ' ) FilDat
      read(*, '( a )' ) FilEqn
      if ( FilEqn .eq. ' ' ) FilEqn = FilDat
      open ( unit = 3, file = FilEqn//'.eqn', status = 'old', 
     >       iostat = IOS, readonly )
      if ( IOS .ne. 0 ) then
      write(*,*) ' Il file non esiste, o il nome e'' sbagliato; riprova'
      goto 251
      end if
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine LetOpz ( nVar, nEqn, nFun, DesFun, NumFun, restart )
C
C--> Chiamata da : Corpo
C
      implicit double precision ( a-h, o-z )
C
      character riga*110, stampa*1, SN*1
      character DesFun(NumFun)*50
      logical restart, prt
C
      write(*,*) ' '
      write(*,'( '' Si stampano le equazioni ? ( S / N=D ) '', $ )')
      read(*, '( a )' ) stampa
C
      if ( .not. restart ) then
         write(*,*) ' '
         write(*,*) '          Descrizione del calcolo '
         write(*,*) '    ( terminare con un doppio ritorno )'
         write(*,*) ' '
   11    read (*,'( a )' ) riga 
         if ( riga .ne. ' ' ) then
            LDes = Lung ( riga, 110 )
            write (1,'(a)') riga(1:LDes)
            write (2,'(a)') riga(1:LDes)
            goto 11
         end if
         write (2,'(a)') ': ( Fine della descrizione del calcolo )'
      else
   35    read ( 2,'(a)' ) riga
         if ( riga(1:1) .ne. ':' ) then
            LDes = Lung ( riga, 110 )
            write (1,'(a)') riga(1:LDes)
            goto 35
         end if
      end if
C
      nVar = 0
      nEqn = 0
      nFun = 0
  450 read(3,'(a)',end=650) riga
       if ( index( riga, '\') .ne. 0 ) nVar = nVar + 1
       if ( index( riga, '&EQ') .ne. 0 ) nEqn = nEqn + 1
       if ( index( riga, '?') .ne. 0 ) nFun = nFun + 1
       goto 450
  650 rewind 3 
C
      write(1,*) ' '
      write
     > (1,'('' Ci sono '',i2, '' equazioni con '', i2, '' variabili'')')
     > nEqn, nVar    
      write(1,*) ' '
C
      if ( stampa .eq. 'S' .or. stampa .eq. 's' ) then
C
         write(1,*) ' Il termine noto e'' dato dalla procedura :'
         write(1,*) ' '
C
  400    read(3,'(a)',end=600) riga
         Lrig = Lung ( Riga, 110 )
         write(1,'(a)') ' | '//riga(1:Lrig)
         goto 400
  600    close ( unit = 3 )
C
      end if
      write (1,*) ' '
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      integer function Lung ( riga, n )
C
      character riga*(*)
C
      i = n
      if ( .not. ( riga(i:i) .eq. ' ' .and. i .gt. 1 ) ) goto 9992
 9991 i = i - 1
      if ( riga(i:i) .eq. ' ' .and. i .gt. 1 ) goto 9991
 9992 continue
C
      Lung = i
C
      return
      end
C      
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C 
      subroutine RWdati ( q, tit, UnMis, restart, def, ScRisu )
C
      implicit double precision ( a-h, o-z )
C
      character tit*50, UnMis*15
      logical restart, ScRisu
C
      if ( .not. restart ) then
         write(*,*) ' '
         LungTi = max ( 1, Lung(tit,50) )
         LungUn = max ( 1, Lung(UnMis,15) )
         write(*,100) tit(1:LungTi), UnMis(1:LungUn), def
  100    format (' ', a, ' (', a, '; def.= ', 1pe9.2, ') ? ', $ )
         read(*,*) q
         if ( q .eq. 0. ) q = def
         write(2,200) q, tit, UnMis
  200    format (' ', 1pe10.3, ' . . . ', a, '[', a, ']' )
      else
         read(2,210) q, tit, UnMis
  210    format ( 1x, 1f10.0, 7x, a, 1x, a )
      end if
C
      if ( ScRisu ) then
      write(1,*) ' '
      write(1,300) tit, q, UnMis
  300 format(' *** ', a, ' = ', 1pe10.3, ' ', a, ' *** ' )
      end if
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine RWtabl ( array, tit, fact, restart, TabInp, LunTab )
C
      character tit*50, fact*10, frmt*16, FrmIte*3, TabInp*(*)
      double precision array(0:*)
      logical restart
C
      if ( .not. restart ) then
         write( FrmIte, '( i3 )' ) LunTab
         frmt = '(' // FrmIte // '(1x,1pe9.2))'
         read ( TabInp, frmt ) ( array(i), i = 1, LunTab )
         if ( fact .ne. ' ' ) then
            write ( 2, '( 5a )' ) ' ', tit, ' ( x ', fact, ' )'
         else
            write ( 2, '( 2a )' ) ' ', tit
         end if
         write ( 2, '(6(1x,1pe9.2))' ) ( array(i), i = 1, LunTab )
      else
         read ( 2, '( a )' )  tit
         read ( 2, '(6(1x,1pe9.2))' ) ( array(i), i = 1, LunTab )
      end if
      array(0) = dble( LunTab )
      if ( fact .ne. ' ' ) then
         write ( 1, '( 5a )' ) ' ', tit, ' ( x ', fact, ' )'
      else
         write ( 1, '( 2a )' ) ' ', tit
      end if
      write ( 1, '(6(1x,1pe9.2))' ) ( array(i), i = 1, LunTab )
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12........
C
      subroutine Interp ( XX, YY, X, Y, DY )
C--> Funzione Y = f( X ), assegnata mediante le tabelle XX e YY
      double precision XX(0:*), YY(0:*), X, Y, DY
C
      Nx = ifix( real( XX(0) ) )
      Ny = ifix( real( YY(0) ) )
      N  = min( Nx, Ny )
      if ( X .le. XX(1) ) then
       Y  = YY(1)
       DY = 0.
      else if ( X .ge. XX(N) ) then
       Y  = YY(N)
       DY = 0.
      else
       i = 1
   10  i = i + 1
       if ( XX(i) .gt. X ) then
        DY = ( YY(i) - YY(i-1) ) / ( XX(i) - XX(i-1) )
        Y  = YY(i-1) + ( X - XX(i-1) ) * DY
       else
        goto 10
       end if
      end if
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12........
C
      subroutine IntrpC ( XX, YY, X, Y, DY )
C--> Funzione Y = f( X ), assegnata mediante le tabelle XX e YY
C    ( interpolazione circolare sulle due tabelle )
      double precision XX(0:*), YY(0:*), X, Y, DY, XN
C
      Nx = ifix( real( XX(0) ) )
      Ny = ifix( real( YY(0) ) )
      N  = min( Nx, Ny )
      XN = XX(N) - XX(1)
      X = XX(1) + mod( X, XN )
      i = 1
   10 i = i + 1
      if ( XX(i) .gt. X ) then
       DY = ( YY(i) - YY(i-1) ) / ( XX(i) - XX(i-1) )
       Y  = YY(i-1) + ( X - XX(i-1) ) * DY
      else
       goto 10
      end if
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine VlPart (SIMP, STEP, MaxErr, DesVar, restart, MxIter, 
     >                   nVar, NP)
C
      implicit double precision ( a-h, o-z )
      double precision SIMP(NP,NP), STEP(NP), MaxErr(NP)
      character DesVar(NP-1)*50, tit*50, UnMis*15, SN*1, Numer*2
      logical restart, prt
C
      if ( .not. restart ) then
         type 31
   31    format( ' Numero massimo di iterazioni ( D = 100 ) ? ', $ )
         accept *, MxIter
         if ( MxIter .le. 0 ) MxIter = 100
         write ( 2, 32 ) MxIter
   32    format( ' ', i4, ' iterazioni al massimo ')
      else
         read ( 2, '( bn, i5 )' ) MxIter
      end if
C
      write(1,*) ' '
      write(1,33) MxIter
      write(1,*) ' '
   33 format( ' Numero massimo di iterazioni : ', i4 )
      write (1,*) ' '
C
      UnMis = ' '
      do i = 1, nVar
         write ( Numer, '( i2 )' ) i
C
         TIT = ' Val. iniz. '//'V'//Numer//'-'//DesVar(i)
         call rwdati ( SIMP(i,1), TIT, UnMis, restart, 0., .false. )
C
         TIT = ' Passo iniziale '
         StepDf = SIMP(i,1) / 3.
         call rwdati ( STEP(i), TIT, UnMis, restart, StepDf, .false. )
C
         TIT = ' Tolleranza '
         call rwdati ( MaxErr(i), TIT, UnMis, restart, 1.E-06, .false. )
C
      end do
C
      write(1,*) ' '
      write(1,*)' -----------------------------------------------------'
      write(1,*)' Valori iniziali delle variabili, passi e tolleranze :'
      write(1,*)' -----------------------------------------------------'
      write(1,*) ' '
      write(1,*)   '                valore iniz.     passo     toller. '
      write(1,*) ' '
      do i = 1, nVar
         write ( Numer, '( i2 )' ) i
         write(1,700) 'V'//Numer, SIMP(i,1), STEP(i), MaxErr(i),
     >                DesVar(i)( 1 : Lung( DesVar(i), 50 ) ) 
  700 format('   Variabile ',a3,' : ',1pe10.4,3x,1pe10.4,1x,1pe10.4, a )
      end do
C
      write(1,*) ' '
C
      return
C
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine SumRes ( TAB, nRig, nCol, iPun, RHS, f, nEqn, nVar, 
     >                    NumFun, CalFnz )
C
      implicit double precision ( a-h, o-z )
C
      double precision TAB(nRig*nCol), RHS(nEqn), f(NumFun)
C
      logical valfin, CalFnz
C 
      k = nRig * ( iPun - 1 ) + 1
C
      call Equaz( Tab(k), RHS, f, nEqn, nVar, NumFun, CalFnz )
C
      ErrFun = 0.
      do 100 j = 1, nEqn
  100  ErrFun = ErrFun + RHS(j)**2
      TAB( k + nVar ) = ErrFun 
C
      return
C
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine INIZIA ( SIMP, P, Q, STEP, RHS, f, n, NP, nEqn, nVar, 
     >                   NumFun )
C
      implicit double precision ( a-h, o-z )
C
      double precision P(NP), Q(NP), STEP(NP), SIMP(NP,NP), 
     >                 RHS(nEqn), f(NumFun)
C
      parameter rad2 = 1.414214
C
      do i = 1, n - 1
         P(i) = STEP(i) * (sqrt(float(n)) + n - 2) / ( (n-1) * rad2)
         Q(i) = STEP(i) * (sqrt(float(n))     - 1) / ( (n-1) * rad2)
      end do
C
      call SumRes ( SIMP, NP, NP, 1, RHS, f, nEqn, nVar, 
     >              NumFun, .false. )
      do i = 2, n
         do j = 1, n - 1
            SIMP(j,i) = SIMP(j,1) + Q(j)
         end do
         SIMP(i-1,i) = SIMP(i-1,1) + P(i-1)
         call SumRes (SIMP, NP, NP, i, RHS, f, nEqn, nVar, 
     >                NumFun, .false. )
      end do
C
c      write(1,*) ' '
c      write(1,*) ' Simplesso iniziale :'
c      write(1,*) ' '
c      do i = 1, n
c         write(1,100) ( SIMP(j,i), j = 1, n )
c      end do
c  100 format(1x, 7(1pe10.4,1x) )
C
      return
C
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C 
      subroutine iteraz ( SIMP, NEXT, BarCen, H, L, MEAN, Error, P, Q, 
     >                    STEP, MaxErr, NP, MxIter, nIter, nEqn, nVar,
     >                    NumFun, f, RHS )
C
      parameter alfa = 1.0           ! coefficiente di riflessione, > 0 
      parameter beta = 0.5           ! coefficiente di contrazione, da 0 a 1
      parameter gamma = 2.0          ! coefficiente di espansione, > 1
C
      implicit double precision ( a-h, o-z )
C
      double precision SIMP(NP,NP), NEXT(NP), BarCen(NP), MEAN(NP),
     >                 Error(NP), P(NP), Q(NP), STEP(NP), MaxErr(NP)
      double precision f(NumFun), RHS(nEqn)
      integer H(NP), L(NP)
      logical fatto
C
      m = nVar
      n = nVar + 1
C
      do i = 1, n
         L(i) = 1
         H(i) = 1
      end do
C
      call ORDER (SIMP, L, H, n, NP)
C
C      call FIRST (SIMP,n, NP)
C
      nIter = 0
      fatto = .false.
C   
      do while (.not.fatto .and. nIter.le.MxIter)
C
         nIter = nIter + 1
C
         do i = 1, n
            BarCen(i) = 0.
         end do
C
         do i = 1, n
            if ( i .ne. H(n) ) then
               do j = 1, m
                  BarCen(j) = BarCen(j) + SIMP(j,i)
               end do
            end if                             ! Calcolo del baricentro
         end do                                ! del simplesso.
!
         do i = 1, m                           ! Primo tentativo di riflettere.
            BarCen(i) = BarCen(i) / m
            NEXT(i) = (1.+alfa) * BarCen(i) - alfa * SIMP(i,H(n))
         end do 
C                                              ! Nuovo vertice = Riflessione
C                                              ! speculare del peggiore.
         call SumRes (NEXT, NP, 1, 1, RHS, f, nEqn, nVar, 
     >                NumFun, .false. )
!
         if ( NEXT(n) .le. SIMP(n,L(n)) ) then ! Meglio del migliore ?
!
            call NEWVER (SIMP, NEXT, H, n, NP) ! Accettato,
C                                              ! ed espanso.
            do i = 1, m                              
               NEXT(i) = gamma * SIMP(i,H(n)) + (1.-gamma) * BarCen(i)
            end do
!
            call SumRes (NEXT, NP, 1, 1, RHS, f, nEqn, nVar, 
     >                   NumFun, .false. )
C                                              ! Ancora meglio ?
            if ( NEXT(n) .le. SIMP(n,L(n)) )  
     >                    call NEWVER (SIMP, NEXT, H, n, NP)
!
         else                                  ! Se non e' meglio del migliore:
!
            if ( NEXT(n) .le. SIMP(n,H(n)) ) then
               call NEWVER ( SIMP, NEXT,  H, n, NP)
C                                              ! (meglio del peggiore)
            else                           
C                                              ! Peggio del peggiore: contrarre.
               do i = 1, m
                  NEXT(i) = beta * SIMP(i,H(n)) + (1.-beta) * BarCen(i)
               end do
               call SumRes (NEXT, NP, 1, 1, RHS, f, nEqn, nVar, 
     >                      NumFun, .false. )
!
               if ( NEXT(n) .le. SIMP(n,H(n)) ) then
                  call NEWVER ( SIMP, NEXT,  H, n, NP)
C                                              ! Contrazione accettata;
               else                            ! altrimenti se va ancora male
                  do i=1, n                    ! si stringono i vertici cattivi.
                     do j=1, m
                       SIMP(j,i)=beta*( SIMP(j,i)+SIMP(j,L(n)) )
                     end do
                     call SumRes ( SIMP, NP, NP, i, RHS, f, nEqn, nVar, 
     >                             NumFun, .false. )
                  end do
               end if
C
            end if
C
         end if 
C
         call ORDER ( SIMP, L, H, n, NP)
C
C--> Controllo della convergenza :
C
         fatto = .true.
         do j=1, m
           Error(j)=(SIMP(j,H(j))-SIMP(j,L(j)))/SIMP(j,H(j))
           if ( abs(Error(j)) .gt. MaxErr(j) ) fatto = .false.
         end do 
C
      end do ! while
C
      call SumRes ( SIMP, NP, NP, i, RHS, f, nEqn, nVar, 
     >              NumFun, .true. )
C
      return
C
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine risult ( SIMP, MEAN, nIter, nVar, NP, DesVar, DesFun, 
     >                    f, nFun, NumFun )
C
      implicit double precision ( a-h, o-z )
C
      double precision SIMP(NP,NP), MEAN(NP)
      character DesVar(NP-1)*50, DesFun( NumFun )*50, Numer*2
      double precision f(NumFun)
C
      write(1,*) ' '
      write(1,*) ' Numero di iterazioni = ', nIter
c      write(1,*) ' '
c      write(1,*) ' Simplesso finale :'
c      write(1,*) ' '
c      do i = 1, nVar+1
c         write(1,'(1x, 7(1pe10.3,1x) )' ) ( SIMP(j,i), j = 1, nVar )
c      end do
c      write(1,*) ' '
C
C   Media dei parametri :
C
      do i = 1, nVar+1
         MEAN(i) = 0.
         do j = 1, nVar+1
            MEAN(i) = MEAN(i) + SIMP(i,j)
         end do
         MEAN(i) = MEAN(i) / (nVar+1)
      end do
C
      write(1,*) ' '
      write(1,*) ' *** Risultati ***'
      write(1,*) ' '
C
      do i = 1, nVar
        write ( Numer, '(i2)' ) i
        write(*,'( 5x, 1pe12.5, 1x, a )') MEAN(i), ' : '//'V'//Numer
     >               //' - '//DesVar(i)( 1 : Lung( DesVar(i), 50 ) ) 
        write(1,'( 5x, 1pe12.5, 1x, a )') MEAN(i), ' : '//'V'//Numer
     >               //' - '//DesVar(i)( 1 : Lung( DesVar(i), 50 ) ) 
      end do
C  
      write(1,*) ' '
      write(1,750) MEAN(nVar+1)
  750 format(' Minimo trovato : ',1pe12.5 )
C
      if ( nFun .gt. 0 ) then
       write(1,*) ' '
       write(1,*) ' --> Come risultato dei calcoli si sono inoltre'
       write(1,*) '     valutate le seguenti quantita'' :   '
       write(1,*) ' '
       write(*,*) ' '
       write(*,*) ' --> Come risultato dei calcoli si sono inoltre'
       write(*,*) '     valutate le seguenti quantita'' :   '
       write(*,*) ' '
       do 100 i = 1, nFun 
        write ( Numer, '(i2)' ) i
        write(*,'( 5x, 1pe12.5, 1x, a )') f(i), ' : '//'F'//Numer
     >          //' - '//DesFun(i)( 1 : Lung( DesFun(i), 50 ) ) 
        write(1,'( 5x, 1pe12.5, 1x, a )') f(i), ' : '//'F'//Numer
     >          //' - '//DesFun(i)( 1 : Lung( DesFun(i), 50 ) ) 
  100  continue
      end if

C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine NEWVER ( SIMP, NEXT, H, n, NP)
C
      implicit double precision ( a-h, o-z )
C
      double precision SIMP(NP,NP), NEXT(NP)
C
      integer H(n)
C
      do i = 1, n
         SIMP(i,H(n)) = NEXT(i)
      end do
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine ORDER ( SIMP, L, H,  n, NP)
C
      implicit double precision ( a-h, o-z )
C
      double precision SIMP(NP,NP)
C
      integer L(NP), H(NP)
C
      do j = 1, n 
         do i = 1, n
            if ( SIMP(j,i) .lt. SIMP(j,L(j)) ) L(j) = i
            if ( SIMP(j,i) .gt. SIMP(j,H(j)) ) H(j) = i
         end do
      end do
C
      return
C
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
$ 