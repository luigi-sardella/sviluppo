C     *****************************************************************
C     *                                                               *
C     *        Programma principale del codice di simulazione.        *
C     *                                                               *
C     *****************************************************************
C
      implicit double precision ( a-h, o-z )
C
      parameter ( NumPun = 2000) ! Numero di punti delle curve 
      parameter ( NumVar = 20  ) ! Ordine massimo del sistema di equazioni, <100
      parameter ( NumCur = 50  ) ! Numero massimo di curve da tracciare
      parameter ( NMaxXY = 10  ) ! Numero massimo di curve X - Y
      parameter ( MxDiTa = 50 )  ! Massima dimensione delle tabelle di ingresso
      parameter ( NumSca = NumCur/3+1 ) 
      parameter ( LiLiDi = 200 ) ! Dimensione massima della lista linkata dei gr
afici
      parameter ( NumWin = 20 )  ! Numero massimo di finestre
      parameter ( NumFFT = 2048 )! Dimensioni massime della FFT
      parameter ( nVarMx = 10 )  ! Numero massimo di variabili da determinare
      parameter ( nEqnMx = 10 )  ! Numero massimo di equazioni
      parameter ( nRipMx = 18 )  ! Numero massimo di ripetizioni
      parameter ( LuNoFi = 30 )  ! Massima lunghezza del nome di un file
      parameter ( NP = nVarMx+1 )! Ordine ( numero dei punti ) massimo del simpl
esso
      parameter ( Wid_D = 36. )  ! Larghezza suggerita delle finestre
      parameter ( Alt_D = 26. )  ! Altezza suggerita delle finestre
C
      double precision SMPLX(NP,NP), NEXT(NP), BarCen(NP), MEAN(NP),
     >    RHS(nEqnMx), Error(NP), P(NP), Q(NP), DelX(NP), MaxErr(NP)
      integer H(NP), L(NP)
C
      dimension Tab(NumPun,NumCur), w(NumPun), TabXY(NumPun,NMaxXY) 
      dimension x(NumVar), e(NumVar), e_max(NumVar), XTip(NumVar)
      dimension z(NumVar,0:5), dz(NumVar,0:4), dumm(NumVar)
      dimension iVarPl(NumVar), iFunPl(NumCur), f(NumCur)
      dimension LisXY( NMaxXY, 2 ), lista(2*NMaxXY)
      dimension LinLis(LiLiDi), listVF(9), listG(NumVar+NumCur)
      dimension TabGr(NumPun,0:NumVar+NumCur,0:nRipMx)
      dimension NPImp(NumCur), iAbbin(NumCur) 
      logical MultSc( NumSca )
      complex*16 Afft( NumFFT )      
C
      character DesVar(NumVar)*50, DesFun(NumCur)*50, id(NumCur)*62
      character DscImp(NumCur)*100, TabInp(MxDiTa)*10, TitWin(NumWin)*80
      character*(LuNoFi) FilDat, FilEqn, FilRis, FilRs1, FilSol, FilSl1,
     >                   FilImp
C
      call CORPO ( Tab, w, z, dz, e, e_max, x, f, XTip, iVarPl, iFunPl, 
     >             MultSc, id, DesVar, DesFun, NumPun, NumVar, NumCur, 
     >             NumSca, LisXY, lista, TabXY, NMaxXY, TabInp, MxDiTa, 
     >             LinLis, listVF, listG, TabGr, LiLiDi, TitWin, NumWin, 
     >             Afft, NumFFT, dumm, SMPLX, Next, BarCen, Mean, Error, 
     >             P, Q, DelX, H, L, RHS, MaxErr, NP, nEqnMx, nVarMx,
     >             Wid_D, Alt_D, nRipMx, LuNoFi, FilDat, FilEqn, FilRis, 
     >             FilRs1, FilSol, FilSl1, FilImp, NPImp, iAbbin,DscImp)
C
      stop
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C 
      subroutine  CORPO (Tab,w,z,dz,e,e_max, x, f, XTip, iVarPl, iFunPl, 
     >             MultSc, id, DesVar, DesFun, NumPun, NumVar, NumCur, 
     >             NumSca, LisXY, lista, TabXY, NMaxXY, TabInp, MxDiTa, 
     >             LinLis, listVF, listG, TabGr, LiLiDi, TitWin, NumWin, 
     >             Afft, NumFFT, dumm, SMPLX, Next, BarCen, Mean, Error, 
     >             P, Q, DelX, H, L, RHS, MaxErr, NP, nEqnMx, nVarMx,
     >             Wid_D, Alt_D, nRipMx, LuNoFi, FilDat, FilEqn, FilRis, 
     >             FilRs1, FilSol, FilSl1, FilImp, NPImp, iAbbin,DscImp)
C
C--> Chiamata da : Main
C-->      Chiama : Files, LetOpz, IniTol, LetDat, Tabula, Inizia, Costan,
C                  CraKlo, CambDT, Plot
C
      implicit double precision ( a-h, o-z )
      real*4 Wid_D, Alt_D, Wid, Alt
C
C-------------------- Versione VMS
      real*4 TempIn
C--------------------
      dimension Tab(NumPun,NumCur), w(NumPun), TabXY(NumPun,NMaxXY) 
      dimension x(NumVar), e(NumVar), e_max(NumVar), XTip(NumVar) 
      dimension z(NumVar,0:5), dz(NumVar,0:4), dumm(NumVar)
      dimension iVarPl(NumVar), iFunPl(NumCur), f(NumCur)
      dimension LisXY( NMaxXY, 2 ), lista(2*NMaxXY)
      dimension LinLis(LiLiDi), listVF(9), listG(NumVar+NumCur)
      dimension TabGr(NumPun,0:NumVar+NumCur,0:nRipMx )
      dimension NPImp(NumCur), iAbbin(NumCur) 
      complex*16 Afft( NumFFT )      
C
      character DesVar(NumVar)*50, DesFun(NumCur)*50, id(NumCur)*62
      character DscImp(NumCur)*100, TabInp(MxDiTa)*10, TitWin(NumWin)*80
C
      character FrmStr*80, FrmIte*3, FrmIt1*3, fact*10, TemCal*11
      character absis*50, Numer*2, data*10, ora*8, NumFil*4, sn*1
      character*(*) FilRis, FilRs1, FilSol, FilSl1, FilImp, FilDat
C
      logical restart, prt, DTnuov, VarPas, col110, risolvi
      logical rivedo, WinNow, SavWin, NewPar, NuoCur, proseg, Sovrap
      logical MultSc( NumSca ), U_Mscl, seTaIm, Msurat, FFTtab
C
      data NumPas /8/, NumSeg /10/, DDT /1.5D0/, TolCen /1.D-06/,
     >     TolIst /1.5D0/, DelXNu /0.1D0/     
C
      double precision SMPLX(NP,NP), NEXT(NP), BarCen(NP), MEAN(NP),
     >    RHS(nEqnMx), Error(NP), P(NP), Q(NP), DelX(NP), MaxErr(NP)
      integer H(NP), L(NP)
      common /Import/ NImp, iAscIm, Iprev
C
C---------------------------------------------------- Versione VMS
      call DATE( data )
      call TIME( ora )
C----------------------------------------------------
C---------------------------------------------------- Versione MS-DOS
C      integer*2 iyr,imon,iday,ihr,imin,isec,i100,jhr,jmin,jsec,j100      
C      call GETDAT( iyr,imon,iday )
C      call GETTIM( ihr,imin,isec,i100 )
C      write(data,'(i2,1h-,i2,1h-,i4)') iday,imon,iyr     
C      write(ora, '(i2,1h:,i2,1h:,i2)') ihr,imin,isec
C----------------------------------------------------
      call LetPar ( NumPas, NumSeg, DDT, TolCen, TolIst, NewPar, DelXNu)
      call OpzIni ( rivedo, NuoCur, WinNow, SavWin, proseg, T_pros,
     >              risolvi, Sovrap )
      call FILES ( restart, FilRis, FilDat, FilEqn, FilSol, 
     >             rivedo, proseg, LuNoFi )
C
      if ( .not. rivedo ) then
C
      write(1,*) ' ********************************************'
      write(1,*) ' *                                          *'
      write(1,*) ' *   Calcolo di simulazione con il metodo   *'
      write(1,*) ' *         di Crane - Klopfenstein          *'
      write(1,*) ' *                                          *'
      write(1,*) ' *           ',  data, ' ',ora,'            *'
      write(1,*) ' *                                          *'
      write(1,*) ' *                                          *'
      write(1,*) ' ********************************************'
      write(1,*) ' '
      write(1,*) ' ---> Archivio dei risultati : ',
     >           FilRis( 1 : Lung(FilRis,LuNoFi) )
C
      call LetOpz ( NumVar, n, iFunMx, NumCur, nVarPl, nFunPl, 
     > nRisul, iVarPl,iFunPl,DesVar,DesFun,restart,prt,col110, MultSc, 
     > NumSca, nVar, nEqn )
C     if ( nVar .ne. nEqn ) stop ' Num. eq. algebr. non = num. incogn.'
      call LeOpXY ( NMaxXY, NumXY, LisXY, lista, LunLis, DesVar, DesFun,
     >              NumVar, NumCur, n, restart )
      call LetDat ( restart )
      call LetTab ( restart, I_rip, J_rip )
      open ( unit = 11, file = FilRis(:Lung(FilRis,LuNoFi))//'.ij',
     >       status = 'new', form = 'unformatted' )
      write (11) I_rip, J_rip
      call Costan ( NP, NumVar, NumCur, DesVar, DesFun, Xtip,SMPLX,DelX,
     >              1, 1, seTaIm )
      if ( seTaIm ) call OpzImp( TabGr, NPImp, iAbbin, DscImp,FilImp, 
     >                           NumPun, NumVar, NumCur, nRipMx, Msurat)
C      if ( I_rip .gt. 1 .or. J_rip .gt. 1 ) SavWin = .false.
      if ( nEqn .ne. 0 ) 
     > call VlPart ( SMPLX, DelX, MaxErr, DesVar, restart, MxIter,  
     >              nVar, NP, TolCen )
      call IniTol ( x, n, NumVar, dt, DtStam, T_fi, T_in, 
     >              restart, VarPas, DesVar, NumPun )
      if ( nEqn .ne. 0 ) 
     > call InSmpl ( SMPLX, P, Q, DelX, RHS, f, n, NP, nEqn, nVar, 
     >               nEqnMx, NumCur, x, dumm, t, T_in, T_fi,
     >               TabGr, NumPun, NumVar, nRipMx )
      write(1,*) ' '
      call LetLis ( LinLis, listVF, listG, LiLiDi, LunLiG, NumVar, 
     >            NumCur, TitWin, NumWin, FilRis, N_wind, LuNoFi )
C
      if ( proseg ) then
       write(*,*) ' > Si prosegue dalla simulazione precedente.'
       write(1,*) ' '
       write(1,*) ' > Si prosegue dalla simulazione precedente.'
       write(1,*) ' '
      end if
C
      TolSup = TolCen * TolIst
      TolInf = TolCen / TolIst
C
      else
C
      open ( unit = 11, file = FilRis(:Lung(FilRis,LuNoFi))//'.ij',
     >       status = 'old', form = 'unformatted' )
      read (11) I_rip, J_rip
C
      end if ! .not rivedo
C
      FilRs1 = FilRis
      FilSl1 = FilSol
C
      do i_ = 1, I_rip
      do j_ = 1, J_rip
C
      if ( i_.eq.1 .and. j_.eq.1 ) then
        write(*,'(''  FFT sul tabulato ? (S/N=D) : '', $ )' ) 
        read(*,'(a)') sn
        if( sn.eq.'s' .or. sn.eq.'S' ) then
          FFTtab = .true.
        else
          FFTtab = .false.
        end if
      end if
C
      ijTG = N_TG ( I_rip, J_rip, i_, j_ )
C
      if ( I_rip .gt. 1 .or. J_rip .gt. 1 ) then
        if ( I_rip .eq. 1 .or. J_rip .eq. 1 ) then
          LFS = min( Lung( FilSol, LuNoFi ), LuNoFi-3 )
          LFR = min( Lung( FilRis, LuNoFi ), LuNoFi-3 )
          write( NumFil(:2), '(i2.2)' ) max( i_, j_ )
          FilSl1 = FilSol(:LFS) // '-' // NumFil(:2)
          FilRs1 = FilRis(:LFR) // '-' // NumFil(:2)
        else
          LFS = min( Lung( FilSol, LuNoFi ), LuNoFi-5 )
          LFR = min( Lung( FilRis, LuNoFi ), LuNoFi-5 )
          write( NumFil, '(2i2.2)' ) i_, j_
          FilSl1 = FilSol(:LFS) // '-' // NumFil
          FilRs1 = FilRis(:LFR) // '-' // NumFil
        end if
        if ( i_.eq.1 .and. j_.eq.1 ) then
          close( unit = 4, disp = 'DELETE' )
        else
          close( unit = 4 )
        end if 
        open ( unit = 4, file = FilSl1//'.rst', status = 'unknown',
     >         form = 'unformatted' )
       if ( proseg ) then
         if ( i_.eq.1 .and. j_.eq.1 ) then
           close( unit = 7, disp = 'DELETE' )
         else
           close( unit = 7 )
         end if
         open ( unit = 7, file = FilRs1//'.rst', status = 'new',
     >          form = 'unformatted' )
       end if
      end if
C
C ---> Inizio della simulazione
C
      if ( .not. rivedo ) then
C
      t = 0.D0
      Iprev = 1
      j = 1
      inZ  = 0
      inDZ = 0
      iPas = 0
      iSeg = 0
      DTnuov = .FALSE.
      tStam  = 0.D0
      do k = 1, n
        e_max( k ) = 0.D0
      end do
      do k = 1, NumCur
        f(k) = 0.D0
      end do
      call Costan ( NP, NumVar, NumCur, DesVar, DesFun, Xtip,SMPLX,DelX,
     >              i_, j_, seTaIm )
      if ( NuoCur ) then
 1000    read ( 4, end = 2000 ) T_f
         goto 1000
 2000    rewind 4
         T_fi = min( T_fi, T_f )
      end if
      if ( T_in .le. 0.D0 ) then
        call Tabula (w, Tab, x, f, n, iVarPl, iFunPl, nVarPl, nFunPl,
     >        NumVar, NumCur, NumPun, t, j, T_in, NuoCur, proseg,
     >        SMPLX, NP, nVar )
        call TabuXY ( TabXY, lista, LunLis, x, f, NumXY, NMaxXY, 
     >              NumPun, NumVar, NumCur, j, T_in )
        call TabGra ( TabGr, listG, LunLiG, x, f, NumPun, NumVar, 
     >              NumCur, t, j, T_in, nRipMx, ijTG )
      end if
      f ( NumCur ) = dt
      if ( i_ .eq. 1 .and. j_ .eq. 1) then
      DesFun( NumCur - 2 ) = ' Quante iterazioni per ogni passo :'
      DesFun( NumCur - 1 ) = ' Passo di integrazione ( sec ) '
      DesFun( NumCur ) = ' Il piu" grande errore corrente :  '
C--------------------------------------- Versione VMS
      TempIn = SECNDS ( 0. ) 
C---------------------------------------
C--------------------------------------- Versione MS-DOS
C      call GETTIM( ihr,imin,isec,i100 )
C---------------------------------------
C
      end if
C
      if ( .not. NuoCur .and. i_ .eq. 1 .and. j_ .eq. 1 ) then
       write(*,*) ' '
       write(*, '( 8x, a, a8, 20x, 4a )' )  
     >  'Inizio alle ',ora,' delta_T','  Tempo ',' ErrCorr','  ErrMax'
       write(*,*) ' '
      end if
      write(*,*) ' '
      Er_max = 0.D0
C
      if ( T_pros .eq. 0.D0 ) T_pros = T_fi
      if ( NuoCur .or. proseg ) then 
 4000  continue
       if ( nVar .eq. 0 ) then
        read ( 4, end = 3000 ) t, ( x(i), i=1,n), f(NumCur), f(NumCur-1)
       else
        read ( 4, end = 3000 ) t, ( x(i), i=1,n), (SMPLX(i,1),i=1,nVar),
     >                         f(NumCur), f(NumCur-1), f(NumCur-2)
       end if
       if ( proseg ) T_start = t
       if ( t .le. min( T_pros, T_fi ) ) then
        call TerNot ( x,dumm,f,n,NumCur,t,dt,DtStam,T_in,T_fi,.true.,
     >             SMPLX, RHS, nEqnMx, nVarMx, .true., risolvi, i_, j_
     >              , TabGr, NumPun, NumVar, nRipMx )
        call Tabula (w, Tab, x, f, n, iVarPl, iFunPl, nVarPl, nFunPl,
     >          NumVar, NumCur, NumPun, t, j, T_in, NuoCur, proseg,
     >          SMPLX, NP, nVar )
        call TabuXY ( TabXY, lista, LunLis, x, f, NumXY, NMaxXY, 
     >              NumPun, NumVar, NumCur, j, T_in )
        call TabGra ( TabGr, listG, LunLiG, x, f, NumPun, NumVar, 
     >              NumCur, t, j, T_in, nRipMx, ijTG )
        j = j + 1
        nPunti = j
        goto 4000
       end if        
 3000  continue
      end if
C
      if ( .not. NuoCur ) then
C
       call Inizia ( z, dz, x, f, n, t, dt, inZ, inDZ, tStam, DtStam, 
     >           T_in, T_fi, T_start, j, w, Tab, iVarPl, iFunPl, nVarPl, 
     >           nFunPl, NumVar, NumCur, NumPun, TabXY, lista, LunLis, 
     >           NumXY, NMaxXY, TabGr, listG, LunLiG, 
     >           SMPLX, NEXT, BarCen, H, L, MEAN, Error, P, Q, 
     >           DelX, MaxErr, NP, MxIter, nIter, nEqn, nVar, nEqnMx, 
     >           nVarMx, RHS, dumm, risolvi, DelXNu, proseg, i_, j_,
     >           nRipMx, ijTG )
C
 9902  continue
C
         t = t + dt
C
         if ( DTnuov .and. VarPas ) then
          call Inizia(z, dz, x, f, n, t, dt, inZ, inDZ, tStam, DtStam, 
     >           T_in, T_fi, T_start, j, w, Tab, iVarPl, iFunPl, nVarPl, 
     >           nFunPl, NumVar, NumCur, NumPun, TabXY, lista, LunLis, 
     >           NumXY, NMaxXY, TabGr, listG, LunLiG, 
     >           SMPLX, NEXT, BarCen, H, L, MEAN, Error, P, Q, 
     >           DelX, MaxErr, NP, MxIter, nIter, nEqn, nVar, nEqnMx, 
     >           nVarMx, RHS, dumm, risolvi, DelXNu, proseg, i_, j_,
     >           nRipMx, ijTG )
          DTnuov = .FALSE.
         end if
C
         call CraKlo ( z, dz, f, e, n, NumCur, t, dt, inZ, inDZ, i1z4, 
     >                 NumVar, T_in, T_fi,
     >           SMPLX, NEXT, BarCen, H, L, MEAN, Error, P, Q, 
     >           DelX, MaxErr, NP, MxIter, nIter, nEqn, nVar, nEqnMx, 
     >           nVarMx, RHS, dumm, risolvi, DelXNu, proseg, i_, j_ 
     >           , TabGr, NumPun, nRipMx )
         do k = 1, n
          e(k) = abs( e( k ) / XTip( k ) )
         end do
C
         if ( risolvi .and. nIter .ge. MxIter ) then
           if (I_rip.eq.1 .and. J_rip.eq.1 ) then
             stop ' Non riesce a risolvere le equazioni'
           else
             write (*,'(a)') 
     >      ' Non riesce a risolvere le equazioni - ' // NumFil
             write (1,'(a)') 
     >      ' Non riesce a risolvere le equazioni - ' // NumFil
             goto 5000
           end if
         end if
C
         tStam = tStam + dt
         if ( tStam .ge. DtStam ) then
            tStam = 0.D0
            Er_cor = 0.D0
            do 9901 k = 1, n
               Ek = e( k ) 
               Er_cor = max( Er_cor, Ek )
               Er_max = max( Er_max, Ek )
               e_max( k ) = max( e_max( k ), Ek ) 
 9901       continue
            if ( Er_cor.gt.0.01 .or. Er_max.gt.0.01 ) goto 5000
            f ( NumCur - 2 ) = float( nIter )
            f ( NumCur - 1 ) = dt
            f ( NumCur ) = Er_cor
C
            if ( t .ge. T_in ) then
            j = j + 1
            call TerNot (z(1,i1z4), dumm, f, n,NumCur,t+dt,0.D0,DtStam,
     >      T_in, T_fi, .true., SMPLX, RHS, nEqnMx, nVarMx, .true., 
     >      risolvi, i_, j_, TabGr, NumPun, NumVar, nRipMx )
            call Tabula (w, Tab, z(1,i1z4), f, n, iVarPl, iFunPl, 
     >      nVarPl, nFunPl, NumVar, NumCur, NumPun, t, j, 
     >      T_in, NuoCur, proseg, SMPLX, NP, nVar )
            call TabuXY ( TabXY, lista, LunLis, z(1,i1z4), f, NumXY, 
     >                    NMaxXY, NumPun, NumVar, NumCur, j, T_in )
            call TabGra ( TabGr, listG, LunLiG, z(1,i1z4), f, NumPun, 
     >             NumVar, NumCur, t, j, T_in, nRipMx, ijTG )
            end if
C
            iSeg = iSeg + 1
            if ( iSeg .eq. NumSeg ) then
             iSeg = 0
             NStar = 47 * ( t - T_start ) / ( T_fi - T_start )
             if ( NStar .gt. 0 .and. NStar .lt. 47 ) then
             write( FrmIte, '(i3)' ) NStar
             write( FrmIt1, '(i3)' ) 47 - NStar
             FrmStr = '(''+'',' // FrmIte // '(''*''),' // FrmIt1 //
     >                '(''.''), 4(1x, 1PD7.1 ) )'
             write(*,FrmStr) dt, t, Er_cor, Er_max
             end if
            end if
         end if
C
         if ( VarPas ) then
           iPas = iPas + 1
           if ( iPas .eq. NumPas ) then
              iPas = 0
              call CambDT ( e, dt, DtStam, DDT, TolSup, TolInf, 
     >                      DTnuov, n)
              if ( DTnuov ) then
               if ( NStar .gt. 0 .and. NStar .lt. 47 ) then
                write( FrmIte, '(i3)' ) NStar
                write( FrmIt1, '(i3)' ) 47 - NStar
                FrmStr = '(''+'',' // FrmIte // '(''*''),' // FrmIt1 //
     >                   '(''.''), 4(1x, 1PD7.1 ) )'
                write(*,FrmStr) dt, t, Er_cor, Er_max
               else
                write(*,'(''+'',47(''.''), 4(1x, 1PD7.1 ) )' ) 
     >                dt, t, Er_cor, Er_max
               end if
              end if
           end if
         end if
C
        nPunti = j
C
        if ( t .lt. T_fi .and. nPunti .lt. NumPun ) goto 9902
C
C ---> Fine della simulazione
C--------------------------------------------------------------- Versione VMS
C
      write(1,*) ' '
      write(1,*) ' ***> Valori massimi delle valutazioni <***'
      write(1,*) ' ***>     degli errori di calcolo      <***'
      write(1,*) ' '
      if ( I_rip.eq.1 .and. J_rip.eq.1 ) then
        write(*,*) ' '
        write(*,*) ' ***> Valori massimi delle valutazioni <***'
        write(*,*) ' ***>     degli errori di calcolo      <***'
        write(*,*) ' '
      end if
      write( FrmStr, '( 72('' ''))' )  
      k = 1
      do 9903 nlin = 1, n / 5 + MIN( 1, MOD( n, 5 ) )
         k1 = MIN( k + 4, n )
         write( FrmIte, '(i3)' ) k1 - k + 1
         FrmStr = '(' //FrmIte// '('' E'',i2,''='',1PD10.3, 1x ) )' 
         write(1, FrmStr )  ( j, e_max( j ), j = k, k1 )
         if (I_rip.eq.1 .and. J_rip.eq.1) 
     >     write(*,FrmStr)  ( j, e_max( j ), j = k, k1 )
         k = k + 5
 9903 continue
C
      end if ! ( .not. NuoCur )
C
      if ( nRisul .ge. 1 ) 
     > call StaRis ( f, iFunPl, DesFun, nFunPl, nRisul, NumCur,
     >               I_rip, J_rip, i_, j_ )
C
      if ( .not. NuoCur ) then
C
  100 format ( ' *** Tempo di calcolo = ', f10.2, ' secondi ***' ) 
      if ( i_ .eq. I_rip .and. j_ .eq. J_rip ) then
        write(1,*) ' ' 
        write(1,100) SECNDS ( TempIn )
        write(1,*) ' '
        type 100, SECNDS ( TempIn )
      end if
C---------------------------------------------------------------
C--------------------------------------------------------------- Versione MS-DOS
C      call GETTIM ( jhr,jmin,jsec,j100 )
C      if( j100 .ge. i100 ) then
C         j100 = j100 - i100
C      else
C         jsec = jsec - 1
C         j100 = 100 + j100 - i100
C      end if
C      if( jsec .ge. isec ) then
C         jsec = jsec - isec
C      else
C         jmin = jmin - 1
C         jsec = 60 + jsec - isec
C      end if
C      if( jmin .ge. imin ) then
C         jmin = jmin - imin
C      else
C         jhr = jhr - 1
C         jmin = 60 + jmin - imin
C      end if
C      write(TemCal,'(i2,1h:,i2,1h:,i2,1h.,i2)') jhr,jmin,jsec,j100
C      write(1,*) ' '
C      write(1, '( '' Tempo di calcolo : '', a )' ) TemCal
C      write(1,*) ' '
C      write(*, '( '' Tempo di calcolo : '', a )' ) TemCal
C--------------------------------------------------------------------
C
      end if ! ( .not. NuoCur )
C
      absis = ' Tempo '
C
      nCurve = min ( nVarPl + nFunPl, NumCur )
C
      NumFT = 0
      iFFT = int( log( float(nPunti) ) / log(2.D0) ) + 1
      FreqMx = 1.D0 / ( 6.D0 * DtStam )
C
      if ( nVarPl .ge. 1 ) then
        do i = 1, nVarPl
          ind = iVarPl( i )
          write( Numer, '(i2)' ) ind
          LDes = lung( DesVar, 50 )
          id(i) = ' Var. '//'X'//Numer//' - '//DesVar(ind)(:LDes)
          if ( FFTtab ) then
            write(*,'(1x,a,'' ; FFT ? (S/N=D) : '', $ )' ) 
     >       DesVar(ind)(:LDes)
            read(*,'(a)') sn
            if( sn.eq.'s' .or. sn.eq.'S' ) then
              call inc( NumFT )
              indFT = nCurve + NumFT
              id( indFT ) = DesVar(ind) // ' ( FFT )'
              call PasBas( Tab(1,i), Tab(1,indFT), nPunti )
              do j = 1, 2**iFFT
                if ( j .le. nPunti ) then
                  Afft( j ) = dcmplx( Tab(j,indFT) - sum )
                else
                  Afft( j ) = dcmplx( 0.D0 )
                end if
              end do
C
              call FFT ( Afft, iFFT, 2**iFFT )
C
              AmodMn = PosMax
              do j = 1, nPunti
                Amodu = max( cdabs( Afft( j ) ), 10.D00*PosMin )
                  if ( AmodMn .gt. Amodu ) AmodMn = Amodu
                  Tab(j,indFT) = Amodu 
              end do
            end if
          end if
        end do
      end if
C
      if ( nFunPl .ge. 1 ) then
        do i = 1, nFunPl
          ind = iFunPl( i )
          write( Numer, '(i2)' ) ind
          LDes = lung( DesVar, 50 )
          id( nVarPl + i ) = ' Fun. '//'F'//Numer
     >                       //' - '//DesFun(ind)(:LDes)
          if ( FFTtab ) then
            write(*,'(1x,a,'' ; FFT ? (S/N=D) : '', $ )' ) 
     >       DesFun(ind)(:LDes)
            read(*,'(a)') sn
            if( sn.eq.'s' .or. sn.eq.'S' ) then
              call inc( NumFT )
              indFT = nCurve + NumFT
              id( indFT ) = DesFun(ind) // ' ( FFT )'
              call PasBas( Tab(1,i+nVarPl), Tab(1,indFT), nPunti )
              do j = 1, 2**iFFT
                if ( j .le. nPunti ) then
                  Afft( j ) = dcmplx( Tab(j,indFT) - sum )
                else
                  Afft( j ) = dcmplx( 0.D0 )
                end if
              end do
C
              call FFT ( Afft, iFFT, 2**iFFT )
C 
              AmodMn = PosMax
              do j = 1, nPunti
                Amodu = max( cdabs( Afft( j ) ), 10.D00*PosMin )
                  if ( AmodMn .gt. Amodu ) AmodMn = Amodu
                  Tab(j,indFT) = Amodu 
              end do
            end if
          end if
        end do
      end if
C
      if ( col110 ) then
         n1Plot = nCurve / 9
         n2Plot = mod ( nCurve, 9 )
         if ( n1Plot .gt. 0 ) then
           do k = 1, n1Plot
             call Plot( 9,nPunti,NumPun,w, Tab(1,9*(k-1)+1), absis,
     >           id(9*(k-1)+1), 0.D0, 0, 0.D0, 1, MultSc(k), prt )
           end do
         end if
         if ( n2Plot .gt. 0 ) 
     >      call Plot(n2Plot,nPunti,NumPun,w, Tab(1,n1Plot*9+1), absis,
     >            id(9*n1Plot+1), 0.D0,0,0.D0,1,MultSc(n1plot+1), prt )
      else
         n1Plot = nCurve / 3
         n2Plot = mod ( nCurve, 3 )
         if ( n1Plot .gt. 0 ) then
           do k = 1, n1Plot
             call Plot1( 3,nPunti,NumPun,w, Tab(1,3*(k-1)+1), absis,
     >                   id(3*(k-1)+1), 0.D0,0,0.D0,1,MultSc(k) )
           end do
         end if
         if ( n2Plot .gt. 0 ) 
     >      call Plot1(n2Plot,nPunti,NumPun,w, Tab(1,n1Plot*3+1), absis,
     >                id(3*n1plot+1), 0.D0,0,0.D0,1,MultSc(n1plot+1) )
      end if
C
      if ( NumFT .ge. 1 ) then
      absis = ' Frequenze '
      Dfreq = 1.D0 / ( T_fi - T_in )
      Nfreq = int( FreqMx / Dfreq )      
      if ( col110 ) then
         n1Plot = NumFT / 9
         n2Plot = mod ( NumFT, 9 )
         if ( n1Plot .gt. 0 ) then
           do k = 1, n1Plot
             kFFT = 9 * (k-1) + nCurve + 1 
             call Plot( 9,Nfreq,NumPun,w, Tab(1,kFFT), absis,
     >             id(kFFT), 0.D0, 1, Dfreq, 1, .true., prt )
           end do
         end if
         if ( n2Plot .gt. 0 ) then
            kFFT = 9 * n1Plot + nCurve + 1 
            call Plot(n2Plot,Nfreq,NumPun,w, Tab(1,kFFT), absis,
     >            id(kFFT), 0.D0, 1, Dfreq, 1, .true., prt )
         end if
      else
         n1Plot = NumFT / 3
         n2Plot = mod ( NumFT, 3 )
         if ( n1Plot .gt. 0 ) then
           do k = 1, n1Plot
             kFFT = 3 * (k-1) + nCurve + 1 
             call Plot1( 3,Nfreq,NumPun,w, Tab(1,kFFT), absis,
     >                   id(kFFT), 0.D0, 1, Dfreq, 1, .true.)
           end do
         end if
         if ( n2Plot .gt. 0 ) then
            kFFT = 3 * n1Plot + nCurve + 1 
            call Plot1(n2Plot,Nfreq,NumPun,w, Tab(1,kFFT), absis,
     >                id(kFFT), 0.D0, 1, Dfreq, 1, .true.)
         end if
      end if
      end if
C
      call GestXY ( TabXY, LisXY, NumXY, lista, DesVar, DesFun, 
     >              NumVar, NumCur, nPunti, NumPun, NMaxXY )
C
      end if ! .not. rivedo
C
      if ( .not.rivedo .and. SavWin .and. .not.(WinNow.or.Sovrap) ) then
       iFFT = int( log( float(nPunti) ) / log(2.D0) ) + 1
       FreqMx = 1.D0 / ( 6.D0 * DtStam )
       call faiFFT ( listG , LinLis, TabGr, nPunti, iFFT, Afft,
     >              NumFFT, NumPun, NumVar, NumCur, LiLiDi, LunLiG,
     >              DesVar, DesFun, iFunMx, FreqMx, N_wind, 
     >              nRipMx, ijTG )
       call WinSav ( listG , LinLis, TabGr, DesFun, DesVar, nPunti, 
     >              NumPun, NumVar, NumCur, LiLiDi, FilRs1, LunLiG, 
     >              TitWin, NumWin, DtStam, T_fi, data, ora, 
     >              nRipMx, ijTG )
      else if ( rivedo ) then
       call WinRet ( listG , LinLis, TabGr, DesFun, DesVar, nPunti, 
     >              NumPun, NumVar, NumCur, LiLiDi, FilRs1, LunLiG, 
     >              TitWin, NumWin, DtStam, T_fi, data, ora, 
     >              nRipMx, ijTG )
       FreqMx = 1.D0 / ( 6.D0 * DtStam )
c       if ( WinNow ) then
        if ( i_.eq.1 .and. j_.eq.1 ) 
     >    call OpzGra ( TemIni, FreIni, FinTem, FinFre, FreqMx, T_fi,
     >               kPunti, U_Mscl, Wid_D, Alt_D, Wid, Alt, .true. )
        call GestGr ( listG , LinLis, TabGr, DesFun, DesVar, nPunti,
     >               NumPun, NumVar, NumCur, LiLiDi, FilRs1, LunLiG, 
     >               TitWin, NumWin, Wid,Alt,FinTem, FinFre, FreqMx, 
     >               data, ora, NewPar, i_, j_, nRipMx, ijTG, LuNoFi,
     >               NumFil, TemIni, FreIni, kPunti, U_Mscl )
c       end if
      else if ( WinNow .or. Sovrap ) then
       iFFT = int( log( float(nPunti) ) / log(2.D0) ) + 1
       FreqMx = 1.D0 / ( 6.D0 * DtStam )
       call faiFFT ( listG , LinLis, TabGr, nPunti, iFFT, Afft,
     >              NumFFT, NumPun, NumVar, NumCur, LiLiDi, LunLiG,
     >              DesVar, DesFun, iFunMx, FreqMx, N_wind, 
     >              nRipMx, ijTG )
       if ( SavWin )
     > call WinSav ( listG , LinLis, TabGr, DesFun, DesVar, nPunti, 
     >              NumPun, NumVar, NumCur, LiLiDi, FilRs1, LunLiG, 
     >              TitWin, NumWin, DtStam, T_fi, data, ora, 
     >              nRipMx, ijTG )
       if ( WinNow ) then
        if ( i_.eq.1 .and. j_.eq.1 ) 
     >    call OpzGra ( TemIni, FreIni, FinTem, FinFre, FreqMx, T_fi,
     >               kPunti, U_Mscl, Wid_D, Alt_D, Wid, Alt, .true. )
        call GestGr ( listG , LinLis, TabGr, DesFun, DesVar, nPunti,
     >              NumPun, NumVar, NumCur, LiLiDi, FilRs1, LunLiG, 
     >              TitWin, NumWin, Wid, Alt, FinTem, FinFre, FreqMx, 
     >              data, ora, NewPar, i_, j_, nRipMx, ijTG, LuNoFi,
     >              NumFil, TemIni, FreIni, kPunti, U_Mscl )
       end if
      end if
C
 5000 continue
      end do ! ( i_ )
      end do ! ( j_ )
C
      if ( WinNow .or. Sovrap .or. rivedo ) then
C
        if ( Sovrap .or. rivedo .and. (I_rip.gt.1.or.J_rip.gt.1) ) then
          call OpzGra ( TemIni, FreIni, FinTem, FinFre, FreqMx, T_fi,
     >                kPunti, U_Mscl, Wid_D, Alt_D, Wid, Alt, .false. )
          call GeGr_r ( listG , LinLis, TabGr, DesFun, DesVar, nPunti, 
     >               NumPun, NumVar, NumCur, LiLiDi, FilRis, LunLiG, 
     >               Wid, Alt, FinTem, FinFre, FreqMx, data, ora, 
     >               NewPar, I_rip, J_rip, nRipMx, LuNoFi, TemIni,
     >               FreIni, kPunti, iAbbin, Msurat, DscImp )
        end if
C
        write( *, '( '' Premere RETURN per uscire '' )' )
        read ( *, '( a )' ) 
      else
        return
      end if
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine LetPar ( NumPas, NumSeg, DDT, TolCen, TolIst, NewPar,
     >                    DelXNu )
      double precision DDT, TolCen, TolIst, DelXNu
      logical NewPar
C
      NewPar = .false.
      open ( unit = 5, file = 'simula.par', status = 'old', 
     >       iostat = IOS, readonly )
      if ( IOS .eq. 0 ) then
       NewPar = .true.
       read ( 5, * )
       read ( 5, * )
       read ( 5, * ) NumPas
       read ( 5, * ) NumSeg
       read ( 5, * ) DDT
       read ( 5, * ) TolCen
       read ( 5, * ) TolIst
       read ( 5, * ) DelXNu
      end if
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine OpzIni ( rivedo, NuoCur, WinNow, SavWin, proseg,
     >                    T_pros, risolvi, Sovrap )
C
      double precision T_pros
      logical rivedo, NuoCur, WinNow, SavWin, proseg, risolvi, Sovrap
      character*1 sino
C
  100 continue
      write(*, *) ' '
      write(*,'( '' Solo per rivedere i grafici ? (S/N=D/A) '', $ )')
      read(*,'( a )' ) sino
      if ( sino .eq. 'S' .or. sino .eq. 's' ) then 
       rivedo = .true.
       SavWin = .false.
       return
      else if ( sino .eq. 'A' .or. sino .eq. 'a' ) then 
       write(*,*) ' Si risponde Si quando si stia lavorando su una '
       write(*,*) ' stazione grafica, solo per osservare dei disegni'
       write(*,*) ' gia" ottenuti in precedenza. Occorre che ci sia' 
       write(*,*) ' l"archivio con estensione FIN. Se si da" solo un'
       write(*,*) ' ritorno il programma assume No, e si dispone a '
       write(*,*) ' tracciare di nuovo i disegni.'
       goto 100
      else
       rivedo = .false.
      end if
C
  200 continue
      write(*, *) ' '
      write(*,'( '' Vedere subito le curve ? (S/N=D/A) '', $ )')
      read(*,'( a )' ) sino
      if ( sino .eq. 'S' .or. sino .eq. 's' ) then 
       WinNow = .true.
      else if ( sino .eq. 'A' .or. sino .eq. 'a' ) then 
       write(*,*) ' Si risponde Si quando si stia lavorando su una '
       write(*,*) ' stazione grafica, iniziando un calcolo al cui '
       write(*,*) ' termine si vogliono mostrate su schermo le fine-'
       write(*,*) ' stre dei risultati. Se si da" solo un ritorno il'
       write(*,*) ' programma assume No : non si vuole o non si puo"'
       write(*,*) ' vedere le finestre subito (probabilmente non si'
       write(*,*) ' dispone al momento di un terminale grafico).'
       goto 200
      else
       WinNow = .false.
      end if
C
  700 continue
      write(*, *) ' '
      write(*,'('' Sovrapporre curve calc. ripetuti ? (S/N=D/A) '',$ )')
      read(*,'( a )' ) sino
      if ( sino .eq. 'S' .or. sino .eq. 's' ) then 
       Sovrap = .true.
      else if ( sino .eq. 'A' .or. sino .eq. 'a' ) then 
       write(*,*) ' Si risponde Si quando si stia lavorando su una '
       write(*,*) ' stazione grafica, iniziando un calcolo al cui '
       write(*,*) ' termine si vogliono mostrate su schermo le fine-'
       write(*,*) ' stre dei risultati. Se si da" solo un ritorno il'
       write(*,*) ' programma assume No : non si vuole o non si puo"'
       write(*,*) ' vedere le finestre subito (probabilmente non si'
       write(*,*) ' dispone al momento di un terminale grafico).'
       goto 700
      else
       Sovrap = .false.
      end if
C
  300 continue
      write(*, *) ' '
      write(*,'( '' Archiviare i grafici ? (S=D/N/A) '', $ )')
      read(*,'( a )' ) sino
      if ( sino .eq. 'N' .or. sino .eq. 'n' ) then 
       SavWin = .false.
      else if ( sino .eq. 'A' .or. sino .eq. 'a' ) then 
       write(*,*) ' La risposta assunta dal programma e" Si ,'
       write(*,*) ' il che significa che si creera" l"archivio' 
       write(*,*) ' di estensione FIN contenente le finestre'
       write(*,*) ' grafiche elaborate in questo lancio.'
       goto 300
      else
       SavWin = .true.
      end if
C
  400 continue
      write(*, *) ' '
      write(*,
     >'( '' Nuove curve da un lancio precedente ? (S/N=D/A) '', $ )')
      read(*,'( a )' ) sino
      if ( sino .eq. 'S' .or. sino .eq. 's' ) then 
       NuoCur = .true.
      else if ( sino .eq. 'A' .or. sino .eq. 'a' ) then 
       write(*,*) ' Rispondendo esplicitamente Si avvertiamo il'
       write(*,*) ' programma che le varie funzioni incognite sono'
       write(*,*) ' gia" state calcolate in precedenza e salvate '
       write(*,*) ' in un archivio di ripartenza, di estensione RST;'
       write(*,*) ' evitando le iterazioni si abbrevia moltissimo il' 
       write(*,*) ' tracciamento delle nuove finestre.'
       goto 400
      else
       NuoCur = .false.
      end if
C
      if ( NuoCur ) then
       proseg = .false.
      else
  500  continue
       write(*, *) ' '
       write(*,
     > '('' Si prosegue da un lancio precedente ? (S/N=D/A) '', $ )')
       read(*,'( a )' ) sino
       if ( sino .eq. 'S' .or. sino .eq. 's' ) then 
        proseg = .true.
       else if ( sino .eq. 'A' .or. sino .eq. 'a' ) then 
        write(*,*) ' A volte si vuole proseguire un calcolo, magari'
        write(*,*) ' con un nuovo valore dell"intervallo di integra-'
        write(*,*) ' zione, o semplicemente per vedere come conti-'
        write(*,*) ' nuano curve gia" ottenute.' 
        write(*,*) ' Se si risponde di SI il programma chiedera" '
        write(*,*) ' successivamente se si tratta di proseguire dall"'
        write(*,*) ' istante finale della simulazione precedente, '
        write(*,*) ' o piuttosto da un istante intermedio.'
        goto 500
       else
        proseg = .false.
       end if
      end if
C
      T_pros = 0.D0
      if ( proseg ) then
       write(*, *) ' '
       write(*, *) ' Istante dal quale si vuole riprendere il calcolo'
       write(*,
     > '('' ( se = 0, istante finale del lancio precedente ) ?  '',$ )')
       read(*,*) T_pros
      end if
C
      risolvi = .false.
      if ( .not. NuoCur .and. .not. proseg ) then
  600  continue
       write(*, *) ' '
       write(*,
     >'('' Risolve le equazioni sin dal primo passo ? (S/N=D/A) '', 
     >   $ )')
       read(*,'( a )' ) sino
       if ( sino .eq. 'S' .or. sino .eq. 's' ) then 
        risolvi = .true.
       else if ( sino .eq. 'A' .or. sino .eq. 'a' ) then 
        write(*,*) ' Questa domanda riguarda le modalita" di'
        write(*,*) ' risoluzione di un eventuale sistema di'
        write(*,*) ' equazioni algebriche. La risposta suggerita,'
        write(*,*) ' No, fa si che al primo passo di integrazione'
        write(*,*) ' il sistema algebrico non venga risolto.'
        write(*,*) ' A volte puo" far comodo il contrario invece.'
        goto 600
       end if
      end if
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine FILES (restart, FilRis, FilDat, FilEqn, FilSol, 
     >                  rivedo, proseg, LuNoFi )
C
C--> Chiamata da : Corpo
C
      logical restart, rivedo, proseg
      character*(*) FilRis, FilDat, FilEqn, FilSol 
      character sn*1, stato*3
C
      restart = .false.
      stato = 'new'
C
   51 write(*,*) ' '
      write(*, '( '' Archivio dei risultati ? '', $ )' )
      read(*, '( a )' ) FilRis
C
      if ( rivedo ) then
        FilSol = FilRis
        return
      end if
C
      open ( unit = 1, file = FilRis//'.ris', status = 'new', 
     >       iostat = IOS )
      if ( IOS .ne. 0 ) then
      write(*,*) ' L''archivio non esiste, o il nome e'' sbagliato.'
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
         write(*,'( '' Archivio dei dati (D = '', a, '') ? '',$ )')
     >         FilRis(:Lung(FilRis,LuNoFi))
         read(*, '( a )' ) FilDat
         if ( FilDat .eq. ' ' ) FilDat = FilRis
      end if
C
      open ( unit = 2, file = FilDat//'.dat', status = stato, 
     >       iostat = IOS )
      if ( IOS .ne. 0 ) then
      write(*,*) ' L''archivio non esiste, o il nome e'' sbagliato.'
      goto 151
      end if
C
  251 write(*,*) ' '
      write(*,'('' Archivio delle equazioni ( D = '',a,'' ) ? '',
     >      $ ) ' ) FilDat(:Lung(FilDat,LuNoFi))
      read(*, '( a )' ) FilEqn
      if ( FilEqn .eq. ' ' ) FilEqn = FilDat
      open ( unit = 3, file = FilEqn//'.eqn', status = 'old', 
     >       iostat = IOS, readonly )
      if ( IOS .ne. 0 ) then
      write(*,*) ' L''archivio non esiste, o il nome e'' sbagliato.'
      goto 251
      end if
C
      write(*,*) ' '
      write(*,'('' Archivio delle soluzioni ( D = '',a,'' ) ? '',
     >      $ ) ' ) FilDat(:Lung(FilDat,LuNoFi))
      read(*, '( a )' ) FilSol
      if ( FilSol .eq. ' ' ) FilSol = FilDat
      open ( unit = 4, file = FilSol//'.rst', status = 'unknown',
     >       form = 'unformatted' )
      if ( proseg )
     >open ( unit = 7, file = FilRis//'.rst', status = 'new',
     >       form = 'unformatted' )
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine LetOpz ( NumVar, iOrd, iFunMx, NumCur, nVarPl, nFunPl, 
     > nRisul, iVarPl,iFunPl,DesVar,DesFun,restart,prt,col110, MultSc, 
     > NumSca, nVar, nEqn )
C
C--> Chiamata da : Corpo
C
      implicit double precision ( a-h, o-z )
C
      character riga*110, stampa*1, SN*1
      character DesVar(NumVar)*50, DesFun(NumCur)*50
      dimension iVarPl( NumVar ), iFunPl( NumCur )
      logical restart, prt, col110, MultSc( NumSca )
C
      write(*,*) ' '
      write(*,'( '' Si stampano le equazioni ? ( S / N=D ) '', $ )')
      read(*, '( a )' ) stampa
C
      col110 = .FALSE.
      iGrupp = 3
      prt    = .FALSE.
      write(*,*) ' '
      write(*, '( '' Curve su 110 colonne ? ( S / N=D ) '',$ )' )
      read(*, '( a )' ) SN
      if ( SN .eq. 'S' .or. SN .eq. 's' ) then
         col110 = .TRUE.
         iGrupp = 9
         write(*,*) ' '
         write(*, '('' Tabulazione dei risultati ? ( S / N=D ) '',$)')
         read(*, '( a )' ) SN
         if ( SN .eq. 'S' .or. SN .eq. 's' ) prt = .true.
      end if
C
      if ( .not. restart ) then
         write(*,*) ' '
         write(*,*) '          Descrizione del calcolo '
         write(*,*) '    ( terminare con un doppio ritorno )'
         write(*,*) ' '
   11    read (*,'( a )' ) riga 
         if ( riga .ne. ' ' ) then
            LDes = Lung ( riga, 110 )
            write(1,'(a)') riga(:LDes)
            write(2,'(a)') riga(:LDes)
            goto 11
         end if
         write(2,'(a)') ': ( Fine della descrizione del calcolo )'
      else
   35    read ( 2,'(a)' ) riga
         if ( riga(1:1) .ne. ':' ) then
            LDes = Lung ( riga, 110 )
            write(1,'(a)') riga(:LDes)
            goto 35
         end if
      end if
C
      iOrd = 0
  450 read(3,'(a)',end=650) riga
      if ( index( riga, 'dx_') .ne. 0 ) then
       read(riga(index(riga,'(')+1:index(riga,')')-1),'(bn,i20)') komp
       iOrd = max ( iOrd, komp )
      else if (index(riga,'~').ne.0 .or. index(riga,'d/dt ').ne.0) then
       iOrd = iOrd + 1
      end if
      goto 450
  650 rewind 3 
C
      write(1,*) ' '
      write(1,'('' Risoluzione di '',i2, '' equazioni differenziali'')')
     >       iOrd
      write(1,*) ' '
C
      iFunMx = 0
  451 read(3,'(a)',end=651) riga
      indPV  = max( index( riga, ';' ), 1 )
      indxpi = index( riga(:indPV), '?' )
      indxf  = index( riga(:indPV), 'f_' )
      if ( indxpi .ne. 0 ) then
       read(riga(indxpi+1:index(riga,';')-1),'(bn,i20)') komp
       iFunMx = max ( iFunMx, komp )
      else if ( indxf .ne. 0 ) then
       read(riga(indxf+3:index(riga,')')-1),'(bn,i20)') komp
       iFunMx = max ( iFunMx, komp )
      end if
      goto 451
  651 rewind 3 
C
      nVar = 0
      nEqn = 0
      nFun = 0
  460 read(3,'(a)',end=660) riga
       if ( index( riga, '\') .ne. 0 ) nVar = nVar + 1
       if ( index( riga, '&') .ne. 0 ) nEqn = nEqn + 1
       if ( index( riga, '?') .ne. 0 ) nFun = nFun + 1
       goto 460
  660 rewind 3 
C
      if ( nEqn .eq. 1 ) then
       write(1,'('' insieme ad una equazione algebrica '')') 
       write(1,*) ' '
      else if ( nEqn .gt. 1 ) then
       write
     > (1,'('' insieme a '',i2, '' equazioni algebriche '')') nEqn
       write(1,*) ' '
      end if
C
      if ( stampa .eq. 'S' .or. stampa .eq. 's' ) then
C
         write(1,*) ' PARAMETRI ED EQUAZIONI :'
         write(1,*) ' '
C
  400    read(3,'(a)',end=600) riga
         Lrig = Lung ( Riga, 110 )
         write(1,'(a)') ' | '//riga(:Lrig)
         goto 400
  600    close ( unit = 3 )
C
      end if
C
      do 700 i = 1, NumSca
  700 MultSc( i ) = .TRUE.
C
      if ( .not. restart ) then
       write(*,*) ' '
       write(*,*) ' --> Se almeno uno dei numeri delle variabili'
       write(*,*) '     o delle funzioni in ogni gruppo di 3 o di 9'
       write(*,*) '     verra'' dato col segno - , allora si usera'' '
       write(*,*) '     lo stesso fattore di scala per tutte le curve'
       write(*,*) '     di quel gruppo.'
       write(*,*) ' '
         write(*, '('' Variabili da tracciare (<='',i2,'') : '' )' )
     >           iOrd
         write(*,*) ' ( terminare con uno 0 )'
         write(*,*) ' '
C
         nVarPl = 0
C
   30    write(*, '( '' Variabile numero ( 0 = fine) : '', $ )' )
         read(*,*) iVar
         if ( iVar .ne. 0 ) then
            if ( iVar .gt. iOrd ) then
             write(*,*) ' Non ci sono tante variabili! riscrivere '
             goto 30
            end if
            nVarPl = nVarPl + 1
            if ( iVar .lt. 0 ) then
               iVar = abs( iVar )
               MultSc( (nVarPl-1)/iGrupp + 1 ) = .FALSE.
            end if
            iVarpl( nVarPl ) = iVar
            LDV = Lung ( DesVar(iVar), 50 )
            write(2,'( i3, 1x, a )' ) 
     >            iVarPl(nVarPl), DesVar(iVar)(:LDV)
            goto 30
         end if
         write(2,'(a)') ': ( Fine della lista delle incognite )'
C
      else
C
         nVarPl = 0
   40    read ( 2,'(a)' ) riga
         if ( riga(1:1) .ne. ':' ) then
            nVarPl = nVarPl + 1           
            read (riga, '( bn, i3 )' ) iVarPl(nVarPl)
            if ( iVarPl(nVarPl) .lt. 0 ) then
               iVarPl(nVarPl) = abs( iVarPl(nVarPl) )
               MultSc( (nVarPl-1)/iGrupp + 1 ) = .FALSE.
            end if
            goto 40
         end if
C
      end if
C
      if ( .not. restart ) then
         write(*,*) ' '
         write(*, '('' Funzioni da tracciare (<='',i2,'') : '' )' )
     >        NumCur - nVarPl
         write(*,*) ' ( terminare con uno 0 )'
         write(*,*) ' '
C
         nFunPl = 0
   45    write(*, '( '' Funzione numero ( 0 = fine ) : '', $ )' )
         read(*,*) iFun
         if ( iFun .ne. 0 ) then
            if ( iFun .gt. NumCur ) then
               write(*,*) ' Errore! riscrivere '
               goto 45
            end if
            nFunPl = nFunPl + 1
            if ( iFun .lt. 0 ) then
               iFun = abs( iFun )
               MultSc( (nVarPl+nFunPl-1)/iGrupp + 1 ) = .FALSE.
            end if
            iFunPl( nFunPl ) = iFun
            LDV = Lung ( DesFun(iFun), 50 )
            write(2,'( i3, 1x, a)' ) 
     >             iFunPl(nFunPl), DesFun(nFunPl)(:LDV)
            if ( nFunPl .lt. NumCur - nVarPl ) goto 45
         end if
         write(2,'(a)') ': (Fine della lista delle variabili d"uscita)'
C
      else
C
         nFunPl = 0
   50    read ( 2,'(a)' ) riga
         if ( riga(1:1) .ne. ':' ) then
            nFunPl = nFunPl + 1           
            read (riga, '( bn, i3 )' ) iFunPl(nFunPl)
            if ( iFunPl(nFunPl) .lt. 0 ) then
               iFunPl(nFunPl) = abs( iFunPl(nFunPl) )
               MultSc( (nVarPl+nFunPl-1)/iGrupp + 1 ) = .FALSE.
            end if
            goto 50
         end if
C
      end if
C
      write(1,*) ' '
      if ( .not. restart ) then
         write(*,*) ' '
         write(*, '('' Risultati da tracciare (<='',i2,'') : '' )' )
     >        NumCur - nVarPl - nFunPl
         write(*,*) ' ( terminare con uno 0 )'
         write(*,*) ' '
C
         nRisul = 0
   55    write(*, '( '' Risultato numero ( 0 = fine ) : '', $ )' )
         read(*,*) iFun
         if ( iFun .ne. 0 ) then
            if ( iFun .gt. NumCur ) then
               write(*,*) ' Errore! riscrivere '
               goto 55
            end if
            nRisul = nRisul + 1
            nTotal = nFunpl + nRisul
            iFunPl( nTotal ) = iFun
            LDV = Lung ( DesFun(iFun), 50 )
            write(2,'( i3, 1x, a )' ) 
     >            iFunPl(nTotal), DesFun(iFun)(:LDV)
            if ( nTotal .lt. NumCur - nVarPl ) goto 55
         end if
         write(2,'(a)') ': ( Fine della lista dei risultati )'
C
      else
C
         nRisul = 0
  60     read ( 2,'(a)' ) riga
         if ( riga(1:1) .ne. ':' ) then
            nRisul = nRisul + 1           
            nTotal = nFunpl + nRisul
            read (riga, '( bn, i3 )' ) iFunPl(nTotal)
            goto 60
         end if
C
      end if
C
      write(1,*) ' '
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine LeOpXY ( NMaxXY, NumXY, LisXY, lista, LunLis, DesVar,
     >                    DesFun, NumVar, NumCur, iOrd, restart )
C
C--> Chiamata da : Corpo
C
      implicit double precision ( a-h, o-z )
C
      character DesVar(NumVar)*50, DesFun(NumCur)*50, riga*110
      dimension LisXY( NMaxXY, 2 ), lista(2*NMaxXY)
      logical restart
C
      if ( .not. restart ) then
       write(*,*) ' '
       write(*,*) ' --> Dare le coppie di numeri che indicano le '
       write(*,*) ' grandezze da mettere in ascissa e in ordinata nei'
       write(*,*) ' vari grafici; positivi, se ci si riferisce ad una'
       write(*,*) ' funzione, come f_(3); col segno - se ci si'
       write(*,*) ' riferisce ad una delle incognite, es. x_(2).'
       write(*,*) ' Si termina con uno zero.'
       write(*,*) ' '
C
       NumXY = 0
       LunLis = 0
C
   30  continue
C
       write(*, '('' In ascissa ( + : f_, - : x_, 0 : fine ) : '',$ )' )
        read(*,*) lxy
        if ( lxy .ne. 0 ) then
         if ( lxy .lt. -iOrd ) then
          write(*,*) ' Non ci sono tante variabili! riscrivere '
          goto 30
         end if
         NumXY = NumXY + 1
         LisXY(Numxy,1) = lxy
         if ( lxy .gt. 0 ) then
          write(2,'( i3, 1x, '' In ascissa  :'', a )' ) 
     >         LisXY(NumXY,1), DesFun(lxy)(:Lung( DesFun(lxy), 50 ) )
         else
          write(2,'( i3, 1x, '' In ascissa  :'', a )' ) 
     >         LisXY(NumXY,1), DesVar(-lxy)(:Lung( DesVar(-lxy), 50 ) )
         end if
         call Clista ( LisXY, lista, 1, NumXY, LunLis, NMaxXY )
C
         write(*, '('' In ordinata ( + : f_, - : x_ ) : '',$ )' )
         read(*,*) LisXY(Numxy,2) 
         if ( lxy .gt. 0 ) then
          write(2,'( i3, 1x, '' In ordinata  :'', a )' ) 
     >         LisXY(NumXY,2), DesFun(lxy)(:Lung( DesFun(lxy), 50 ) )
         else
          write(2,'( i3, 1x, '' In ordinata  :'', a )' ) 
     >         LisXY(NumXY,2), DesVar(-lxy)(:Lung( DesVar(-lxy), 50 ) )
         end if
         call Clista ( LisXY, lista, 2, NumXY, LunLis, NMaxXY )
C
         goto 30
        end if
        write(2,'(a)') ': ( Fine della lista dei grafici X - Y )'
C
      else
C
        NumXY = 0
   40   read ( 2,'(a)' ) riga
        if ( riga(1:1) .ne. ':' ) then
          NumXY = NumXY + 1           
          read (riga, '( bn, i3 )' ) LisXY(NumXY,1)
          read ( 2  , '( bn, i3 )' ) LisXY(NumXY,2)
          call Clista ( LisXY, lista, 1, NumXY, LunLis, NMaxXY )
          call Clista ( LisXY, lista, 2, NumXY, LunLis, NMaxXY )
          goto 40
         end if
C
      end if
      write(1,'( 1h1 )' )
      write(1,*) ' '
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine Clista ( LisXY, lista, k, NumXY, LunLis, NMaxXY )
C
      dimension LisXY( NMaxXY, 2 ), lista(2*NMaxXY)
      logical cegia
C
      i = 0
   15 continue
        i = i + 1
        cegia = lista(i) .eq. LisXY(NumXY,k)
      if ( .not. cegia .and. i .lt. LunLis ) goto 15
      if ( .not. cegia ) then
        lista(LunLis+1) = LisXY(NumXY,k)
        LunLis = LunLis + 1
      end if
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
      subroutine IniTol( x, n, NumVar, dt, DtStam, T_fi, T_in, 
     >                    restart, VarPas, DesVar, NumPun )
C
C--> Chiamata da : Corpo
C-->      Chiama : RWdati
C
      implicit double precision ( a-h, o-z )
C
      dimension x(NumVar)
      character DesVar(NumVar)*50, tit*50, UnMis*15, Numer*2
      logical restart, VarPas
C
c     do 9908 i = 1, n
c        write( Numer, '(i2)' ) i
c        tit = 'Val. iniz. '//'X'//Numer//'-'//DesVar(i)
c        UnMis = ' '
c        call RWdati( x(i), tit, UnMis, restart, 0.D0 )
c9908 continue
C
      if ( .not. restart ) then
       write(*,*) ' '
       write(*,*) ' --> Se DT verra'' dato col segno - , si variera'' '
       write(*,*) '     il passo di integrazione .'
       write(*,*) ' '
      end if
      tit = 'Intervallo d''integrazione'
      UnMis = ' sec '
      call RWdati ( dt, tit, UnMis, restart, 1.D-3 )
      if ( dt .lt. 0.D0 ) then
         dt = - dt
         VarPas = .TRUE.
      else
         VarPas = .FALSE.
      end if
C
      tit = 'Intervallo di stampa'
      UnMis = ' sec '
      call RWdati ( DtStam, tit, UnMis, restart, 1.D-2 )
C
      if ( DtStam .le. dt ) then
         DtStam = dt
      else
C-->     Assicuriamoci che un intervallo di stampa contenga un numero
C        intero di passi di integrazione ...
C
         nstam = DtStam / dt
         dt = DtStam / nstam 
      end if
C
      tit = 'Istante iniziale delle stampe'
      UnMis = ' sec '
      call RWdati ( T_in, tit, UnMis, restart, 0.D0 )
C
      tit = 'Istante finale delle stampe'
      UnMis = ' sec '
      call RWdati ( T_fi, tit, UnMis, restart, 20.D0 )
C
      DtStam = max( DtStam, (T_fi-T_in)/(NumPun-1) )
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C 
      subroutine RWdati ( q, tit, UnMis, restart, def )
C
C--> Chiamata da : IniTol, LetDat
C
      implicit double precision ( a-h, o-z )
C
      character tit*50, UnMis*15
      logical restart
C
      if ( .not. restart ) then
         write(*,*) ' '
         LungTi = max ( 1, Lung(tit,50) )
         LungUn = max ( 1, Lung(UnMis,15) )
         write(*,100) tit(:LungTi), UnMis(:LungUn), def
  100    format (' ', a, ' (', a, '); def.= ', 1PD9.2, ' ? ', $ )
         read(*,*) q
         if ( q .eq. 0.D0 ) q = def
         write(2,200) q, tit, UnMis
  200    format (' ', 1PD10.3, ' ', a, '[', a, ']' )
      else
         read(2,210) q, tit, UnMis
  210    format ( 1x, 1f10.0, 1x, a, 1x, a )
      end if
C
      write(1,300) tit, q, UnMis
  300 format(' ', a, ' = ', 1PD9.2, ' ', a )
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine RWtabl ( array0, tit, fact, restart, TabInp, LunTab )
C
      character tit*50, fact*10, frmt*16, FrmIte*3, TabInp*(*), SK*132
      double precision array0(0:*)
      logical restart, RigTab, RigCom
C
      if ( .not. restart ) then
         write( FrmIte, '( i3 )' ) LunTab
         frmt = '(' // FrmIte // '(1x,f9.0))'
         read ( TabInp, frmt ) ( array0(i), i = 1, LunTab )
         if ( fact .ne. ' ' ) then
            write( 2, '( 5a )' ) ' ', tit, ' ( x ', fact, ' )'
         else
            write( 2, '( 2a )' ) ' ', tit
         end if
         write( 2, '(6(1x,1PD9.2))' ) ( array0(i), i = 1, LunTab )
      else
         read ( 2, '( a )' )  tit
         LunTab = 0
  100    continue
           read ( 2, '( a )' ) SK
           if (RigTab(SK).and.index(SK,'Intervallo').eq.0
     >                   .and.index(SK,'iterazioni').eq.0) then
             call rig_num ( SK, array0(LunTab+1), LTab ) 
             LunTab = LunTab + LTab
             goto 100
           else if ( RigCom( SK ) ) then
             goto 100
           else 
             backspace ( 2 )
             goto 200
           end if
  200    continue
      end if
      array0(0) = dble( LunTab )
      if ( fact .ne. ' ' ) then
         write( 1, '( 5a )' ) ' ', tit, ' ( x ', fact, ' )'
      else
         write( 1, '( 2a )' ) ' ', tit
      end if
      write( 1, '(6(1x,1PD9.2))' ) ( array0(i), i = 1, LunTab )
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine Inizia(z, dz, x, f, n, t, dt, inZ, inDZ, tStam, DtStam, 
     >           T_in, T_fi, T_start, j, w, Tab, iVarPl, iFunPl, nVarPl, 
     >           nFunPl, NumVar, NumCur, NumPun, TabXY, lista, LunLis, 
     >           NumXY, NMaxXY, TabGr, listG, LunLiG, 
     >           SMPLX, NEXT, BarCen, H, L, MEAN, Error, P, Q, 
     >           DelX, MaxErr, NP, MxIter, nIter, nEqn, nVar, nEqnMx, 
     >           nVarMx, RHS, dumm, risolvi, DelXNu, proseg, i_, j_,
     >           nRipMx, ijTG )
C
C--> Chiamata da : Corpo
C-->      Chiama : Runge, TerNot, Tabula, TabuXY
C
      implicit double precision ( a-h, o-z )
      dimension z(NumVar,0:5), dz(NumVar,0:4), x(n), dumm(n)
      dimension Tab(NumPun,NumCur), w(NumPun)
      dimension iVarPl(NumVar), iFunPl(NumCur), f(NumCur)
      dimension TabXY(NumPun,2*NMaxXY), lista(2*NMaxXY)
      dimension TabGr(NumPun,0:NumVar+NumCur,0:nRipMx)
      dimension listG(NumVar+NumCur)
      double precision SMPLX(NP,NP), NEXT(NP), BarCen(NP), MEAN(NP),
     >    RHS(nEqnMx), Error(NP), P(NP), Q(NP), DelX(NP), MaxErr(NP)
      integer H(NP), L(NP)
      logical risolvi, proseg
C
      if ( t .eq. T_start ) then 
        do k = 1, n 
          z(k,inZ) = x(k) 
        end do
      end if
      if ( nEqn .ne. 0 .and. risolvi ) 
     >call iteraz ( SMPLX, NEXT, BarCen, H, L, MEAN, Error, P, Q, 
     >              DelX, MaxErr, NP, MxIter, nIter, nEqn, nVar,
     >              nEqnMx, nVarMx, NumCur, f, RHS, z(1,inZ), dumm, 
     >              n, t, T_in, T_fi, DelXNu, proseg, i_, j_ 
     >              , TabGr, NumPun, NumVar, nRipMx )
      call TerNot ( z(1,inZ), dz(1,inDZ), f, n, NumCur,t,0.D0,0.D0,T_in,
     >              T_fi, .false., SMPLX, RHS, nEqnMx, nVarMx, .true.,
     >              risolvi, i_, j_, TabGr, NumPun, NumVar, nRipMx )
C
      inSUM = mod ( inDZ + 4, 5 )
C
      do 9910 i = 1, 3
         inZi   = mod ( inZ + i,     6 )
         inZim1 = mod ( inZ + i - 1, 6 )
         inDZi  = mod ( inDZ + i,    5 )
         call Runge ( z(1,inZim1), z(1,inZi), dz(1,inDZi), dz(1,inSUM), 
     >                f, n, NumCur, t, dt, T_in, T_fi,
     >           SMPLX, NEXT, BarCen, H, L, MEAN, Error, P, Q, 
     >           DelX, MaxErr, NP, MxIter, nIter, nEqn, nVar, nEqnMx, 
     >           nVarMx, RHS, dumm, risolvi, DelXNu, proseg, i_, j_
     >           , TabGr, NumPun, NumVar, nRipMx )
         if ( nEqn .ne. 0 .and. risolvi ) 
     >   call iteraz ( SMPLX, NEXT, BarCen, H, L, MEAN, Error, P, Q, 
     >                 DelX, MaxErr, NP, MxIter, nIter, nEqn, nVar,
     >                 nEqnMx, nVarMx, NumCur, f, RHS, z(1,inZi), dumm, 
     >                 n, t, T_in, T_fi, DelXNu, proseg, i_, j_ 
     >                 , TabGr, NumPun, NumVar, nRipMx )
         call TerNot ( z(1,inZi), dz(1,inDZi),f,n,NumCur,t+dt,0.D0,0.D0,
     >                 T_in, T_fi, .false., SMPLX, RHS, nEqnMx, nVarMx, 
     >          .true., risolvi, i_, j_, TabGr, NumPun, NumVar, nRipMx )
C
         t = t + dt
         tStam = tStam + dt
         if ( tStam .ge. DtStam ) then
            tStam = 0.D0
            f ( NumCur ) = dt
C
            if ( t .ge. T_in ) then
            j = j + 1
            call Tabula (w, Tab, z(1,inZi), f, n, iVarPl, iFunPl, 
     >      nVarPl, nFunPl, NumVar, NumCur, NumPun, t, j, 
     >      T_in, NuoCur, proseg, SMPLX, NP, nVar )
            call TabuXY ( TabXY, lista, LunLis, z(1,inZi), f, NumXY, 
     >                    NMaxXY, NumPun, NumVar, NumCur, j, T_in )
            call TabGra ( TabGr, listG, LunLiG, z(1,inZi), f, NumPun, 
     >             NumVar, NumCur, t, j, T_in, nRipMx, ijTG )
            end if
C
         end if
 9910 continue
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine RUNGE ( x, y, dx, sum, f, n, NumCur, t, dt, T_in, T_fi,
     >                   SMPLX, NEXT, BarCen, H, L, MEAN, Error, P, Q, 
     >           DelX, MaxErr, NP, MxIter, nIter, nEqn, nVar, nEqnMx, 
     >           nVarMx, RHS, dumm, risolvi, DelXNu, proseg, i_, j_ 
     >           , TabGr, NumPun, NumVar, nRipMx )
C
C--> Chiamata da : Inizia
C-->      Chiama : TerNot
C
      implicit double precision ( a-h, o-z )
C 
      dimension   x(n), y(n), dx(n), sum(n), f(NumCur), dumm(n)
      double precision SMPLX(NP,NP), NEXT(NP), BarCen(NP), MEAN(NP),
     >    RHS(nEqnMx), Error(NP), P(NP), Q(NP), DelX(NP), MaxErr(NP)
      dimension TabGr(NumPun,0:NumVar+NumCur,0:nRipMx)
      integer H(NP), L(NP)
      logical risolvi, proseg
C
      if ( nEqn .ne. 0 .and. risolvi ) 
     >call iteraz ( SMPLX, NEXT, BarCen, H, L, MEAN, Error, P, Q, 
     >              DelX, MaxErr, NP, MxIter, nIter, nEqn, nVar,
     >              nEqnMx, nVarMx, NumCur, f, RHS, x, dumm, 
     >              n, t, T_in, T_fi, DelXNu, proseg, i_, j_ 
     >              , TabGr, NumPun, NumVar, nRipMx )
      call TerNot ( x, dx, f, n, NumCur, t,dt, 0.D0, T_in,T_fi, .false.,
     >              SMPLX, RHS, nEqnMx, nVarMx, .true., risolvi, i_, j_
     >              , TabGr, NumPun, NumVar, nRipMx )
C
      do 9911 i = 1, n
         dxdt = dx(i) * dt
         sum(i) = dxdt
         y(i) = x(i) + dxdt / 2.D0
 9911 continue
C
      if ( nEqn .ne. 0 .and. risolvi ) 
     >call iteraz ( SMPLX, NEXT, BarCen, H, L, MEAN, Error, P, Q, 
     >              DelX, MaxErr, NP, MxIter, nIter, nEqn, nVar,
     >              nEqnMx, nVarMx, NumCur, f, RHS, y, dumm, 
     >              n, t+dt/2.D0, T_in, T_fi, DelXNu, proseg, i_, j_ 
     >              , TabGr, NumPun, NumVar, nRipMx )
      call TerNot(y,dx,f,n,NumCur,t+dt/2.D0,0.D0,0.D0,T_in,T_fi,.false.,
     >              SMPLX, RHS, nEqnMx, nVarMx, .true., risolvi, i_, j_
     >              , TabGr, NumPun, NumVar, nRipMx )
C
      do 9912 i = 1, n
         dxdt = dx(i) * dt * 2.D0
         sum(i) = sum(i) + dxdt
         y(i) = x(i) + dxdt / 4.D0
 9912 continue
C
      if ( nEqn .ne. 0 .and. risolvi ) 
     >call iteraz ( SMPLX, NEXT, BarCen, H, L, MEAN, Error, P, Q, 
     >              DelX, MaxErr, NP, MxIter, nIter, nEqn, nVar,
     >              nEqnMx, nVarMx, NumCur, f, RHS, y, dumm, 
     >              n, t, T_in, T_fi, DelXNu, proseg, i_, j_
     >              , TabGr, NumPun, NumVar, nRipMx )
      call TerNot ( y, dx, f, n, NumCur, t, 0.D0,0.D0,T_in,T_fi,.false.,
     >              SMPLX, RHS, nEqnMx, nVarMx, .true., risolvi, i_, j_ 
     >              , TabGr, NumPun, NumVar, nRipMx )
C
      do 9913 i = 1, n
         dxdt = dx(i) * dt * 2.D0
         sum(i) = sum(i) + dxdt
         y(i) = x(i) + dxdt / 2.D0
 9913 continue
C
      if ( nEqn .ne. 0 .and. risolvi ) 
     >call iteraz ( SMPLX, NEXT, BarCen, H, L, MEAN, Error, P, Q, 
     >              DelX, MaxErr, NP, MxIter, nIter, nEqn, nVar,
     >              nEqnMx, nVarMx, NumCur, f, RHS, y, dumm, 
     >              n, t+dt, T_in, T_fi, DelXNu, proseg, i_, j_
     >              , TabGr, NumPun, NumVar, nRipMx )
      call TerNot ( y, dx, f, n,NumCur,t+dt,0.D0,0.D0,T_in,T_fi,.false.,
     >              SMPLX, RHS, nEqnMx, nVarMx, .true., risolvi, i_, j_ 
     >              , TabGr, NumPun, NumVar, nRipMx )
C
      do 9914 i = 1, n
 9914 y(i) = ( sum(i) + dx(i) * dt ) / 6.D0 + x(i)
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine CraKlo (z, dz, f, e, n, NumCur, t, dt, inZ, inDZ, i1z4, 
     >                   NumVar, T_in, T_fi,
     >           SMPLX, NEXT, BarCen, H, L, MEAN, Error, P, Q, 
     >           DelX, MaxErr, NP, MxIter, nIter, nEqn, nVar, nEqnMx, 
     >           nVarMx, RHS, dumm, risolvi, DelXNu, proseg, i_, j_ 
     >           , TabGr, NumPun, nRipMx )
C
C--> Chiamata da : Corpo
C-->      Chiama : TerNot
C
      double precision z(NumVar,0:5), dz(NumVar,0:4), e(n), f(NumCur)
      double precision SMPLX(NP,NP), NEXT(NP), BarCen(NP), MEAN(NP),
     >    RHS(nEqnMx), Error(NP), P(NP), Q(NP), DelX(NP), MaxErr(NP),
     >    dumm(n)
      dimension TabGr(NumPun,0:NumVar+NumCur,0:nRipMx)
      integer H(NP), L(NP)
      logical risolvi, proseg
C
      data a0,a1,a2,a3 / 0.697353D0,-2.017204D0,1.867503D0,-1.547652D0 /
      data b0,b1,b2,b3 / -0.714320D0,1.818609D0,-2.031690D0,2.002247D0 /
C
      iz0  = inZ
      iz1  = mod ( iz0 + 1, 6 )
      iz2  = mod ( iz1 + 1, 6 )
      iz3  = mod ( iz2 + 1, 6 )
      i0z4 = mod ( iz3 + 2, 6 )
      i1z4 = mod ( iz3 + 1, 6 )
      inZ  = iz1
C
      id0 = inDZ
      id1 = mod ( id0 + 1, 5 )
      id2 = mod ( id1 + 1, 5 )
      id3 = mod ( id2 + 1, 5 )
      id4 = mod ( id3 + 1, 5 )
      inDZ = id1
C
C--> Predizione:
C
      do 9915 k = 1, n
 9915    z(k,i0z4) = -a3*z(k,iz3)-a2*z(k,iz2)-a1*z(k,iz1)-a0*z(k,iz0)
     >    + dt * ( b3*dz(k,id3)+b2*dz(k,id2)+b1*dz(k,id1)+b0*dz(k,id0) )
C
C--> Prima valutazione:
C
      if ( nEqn .ne. 0 .and. risolvi ) 
     >call iteraz ( SMPLX, NEXT, BarCen, H, L, MEAN, Error, P, Q, 
     >              DelX, MaxErr, NP, MxIter, nIter, nEqn, nVar,
     >              nEqnMx, nVarMx, NumCur, f, RHS, z(1,i0z4), dumm, 
     >              n, t+dt, T_in, T_fi, DelXNu, proseg, i_, j_
     >              , TabGr, NumPun, NumVar, nRipMx )
      call TerNot ( z(1,i0z4), dz(1,id4), f, n, NumCur, t+dt, dt, 0.D0,
     >              T_in, T_fi, .false., SMPLX, RHS, nEqnMx, nVarMx,
     >         .true., risolvi, i_, j_, TabGr, NumPun, NumVar, nRipMx )
C
C--> Correzione:
C
      dt24 = dt / 24.D0
      do 9916 k = 1, n
 9916   z(k,i1z4) = z(k,iz3) + dt24 * ( 9.D0*dz(k,id4) + 19.D0*dz(k,id3) 
     >                                  -5.D0*dz(k,id2) + dz(k,id1)   )
C
C--> Seconda valutazione:
C
      if ( nEqn .ne. 0 .and. risolvi ) 
     >call iteraz ( SMPLX, NEXT, BarCen, H, L, MEAN, Error, P, Q, 
     >              DelX, MaxErr, NP, MxIter, nIter, nEqn, nVar,
     >              nEqnMx, nVarMx, NumCur, f, RHS, z(1,i1z4), dumm, 
     >              n, t+dt, T_in, T_fi, DelXNu, proseg, i_, j_
     >              , TabGr, NumPun, NumVar, nRipMx )
      call TerNot ( z(1,i1z4), dz(1,id4), f, n, NumCur, t+dt,0.D0,0.D0,
     >              T_in, T_fi, .false., SMPLX, RHS, nEqnMx, nVarMx, 
     >         .true., risolvi, i_, j_, TabGr, NumPun, NumVar, nRipMx )
C
C--> Valutazione dell'errore:
C
      do 9917 k = 1, n
 9917   e(k) = abs ( ( z(k,i1z4) - z(k,i0z4) ) / 16.D0 )
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine CambDT ( e, dt, DtStam, DDT, TolSup, TolInf, 
     >                    DTnuov, n)
C
C--> Chiamata da : Corpo
C
      double precision e(n), dt, DtStam
C
      logical DTnuov, AumDT, DimDT, NonAum
C
      k = 0
 9993    k = k + 1
         NonAum = e(k) .gt. TolInf
      if ( .not. NonAum .and. k .lt. n ) goto 9993
      AumDT = .not. NonAum
C
      k = 0
 9994    k = k + 1
         DimDT = e(k) .gt. TolSup
      if ( .not. DimDT .and. k .lt. n ) goto 9994
C
      DTnuov = AumDT .or. DimDT
      if ( AumDt ) dt = dt * DDT
      if ( DimDT ) dt = dt / DDT
C
      if ( dt .gt. DtStam ) dt = DtStam
C-->     Assicuriamoci che un intervallo di stampa contenga un numero
C        intero di passi di integrazione ...
C
      if ( DTnuov ) then
         nStam = DtStam / dt
         dt = DtStam / nStam 
      end if
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine Tabula (w, Tab, x, f, iOrd, iVarPl, iFunPl, nVarPl, 
     >           nFunPl, NumVar, NumCur, NumPun, t, j, T_in, 
     >           NuoCur, proseg, SMPLX, NP, nVar )
C
C--> Chiamata da : Corpo, Inizia
C
      implicit double precision ( a-h, o-z )
C
      dimension Tab(NumPun,NumCur), w(NumPun), x(NumVar), 
     >          iVarPl(NumVar), iFunPl(NumCur), f(NumCur), SMPLX(NP,NP)
C
      character FrmStr*72, FrmIte*3, FrmIt1*3
      logical NuoCur, proseg
C
      if ( j .le. NumPun ) w(j) = t
      if ( nVarPl .ge. 1 ) then
       do 9918 i = 1, nVarPl
         Tab(j,i) = x( iVarPl(i) )
         if (  j .eq. 2 ) Tab(1,i) = Tab(2,i)
 9918  continue
      end if
      if ( nFunPl .ge. 1 ) then
       do 9919 i = 1, nFunPl
         Tab( j, nVarPl + i ) = f ( iFunPl(i) )
         if (T_in.gt.0.D0 .and. j.eq.2) Tab(1,nVarPL+i)=Tab(2,nVarPl+i)
 9919  continue  
      end if
C
      if ( .not. NuoCur ) then
       if ( proseg ) then
        if ( nVar .eq. 0 ) then
         write( 7 ) t, ( x(i), i = 1, iOrd ), f(NumCur), f(NumCur-1)
        else
         write( 7 ) t, ( x(i), i = 1, iOrd ),(SMPLX(i,1), i = 1, nVar),
     >               f(NumCur), f(NumCur-1), f(NumCur-2)
        end if
       else
        if ( nVar .eq. 0 ) then
         write( 4 ) t, ( x(i), i = 1, iOrd ), f(NumCur), f(NumCur-1)
        else
         write( 4 ) t, ( x(i), i = 1, iOrd ),(SMPLX(i,1), i = 1, nVar),
     >               f(NumCur), f(NumCur-1), f(NumCur-2)
        end if
       end if
      end if
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12........
C
      subroutine TabuXY ( TabXY, lista, LunLis, x, f, NumXY, NMaxXY, 
     >                    NumPun, NumVar, NumCur, j, T_in )
C
C--> Chiamata da : Corpo, Inizia
C
      implicit double precision ( a-h, o-z )
      dimension TabXY(NumPun,2*NMaxXY), lista(2*NMaxXY)
      dimension x(NumVar), f(NumCur)
C
      if ( j .le. NumPun ) then
      do 10 i = 1, LunLis
        ind = lista(i)
        if ( ind .gt. 0 ) then
          TabXY(j,i) = f(ind)
        else        
          TabXY(j,i) = x(-ind)
        end if
        if (  j .eq. 2 ) TabXY(1,i) = TabXY(2,i)
   10 continue
      end if
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine StaRis ( f, iFunPl, DesFun, nFunPl, nRisul, NumCur,
     >                    I_rip, J_rip, i_, j_  )
C
      implicit double precision ( a-h, o-z )
      character DesFun( NumCur )*50, Numer*2
      dimension iFunPl( NumCur ), f(NumCur)
C
      write(1,*) ' '
      write(1,*) ' --> Nel corso della simulazione si sono '
      write(1,*) '     calcolate le seguenti quantita'' :   '
      write(1,*) ' '
      if ( I_rip .gt. 1 .or. J_rip .gt. 1 ) then
        if ( I_rip .eq. 1 .or. J_rip .eq. 1 ) then
          write(1, '('' Identificazione : '',i2.2)' ) max( i_, j_ )
        else
          write(1, '('' Identificazione : '',2i2.2)' ) i_, j_
        end if
      else
        write(*,*) ' '
        write(*,*) ' --> Nel corso della simulazione si sono '
        write(*,*) '     calcolate le seguenti quantita'' :   '
        write(*,*) ' '
      end if
      do 100 i = nFunPl + 1, nFunpl + nRisul
        ind = iFunPl(i)
        write( Numer, '(i2)' ) ind
        if (I_rip.eq.1 .and. J_rip.eq.1) 
     >  write(*,'( 5x, 1PD10.3, 1x, a )') f(ind), ' : '//'F'//Numer
     >                                          //' - '//DesFun(ind)
        write(1,'( 5x, 1PD10.3, 1x, a )') f(ind), ' : '//'F'//Numer
     >                                          //' - '//DesFun(ind)
  100 continue
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine Plot (N,M,NA,V,A,ABSIS,ID,XSCALE,ICRE,DDV,UN,
     >                 MulSca,PRT)
C
C--> Chiamata da : Corpo
C
C    Tracciamento sul tabulato di sino a nove funzioni sullo stesso
C    grafico, seguito da una tabulazione delle stesse funzioni.
C
C    Argomenti di chiamata :
C
C    N         = Quante curve sul grafico; <= 9 .
C    M         = Quanti punti per ogni curva.
C    V(M)      = Vettore contenente le ascisse; max abs V(J) = 999.
C    A(M,N)    = Matrice contenente le ordinate per righe.
C    ABSIS     = Identificazione dell' ascissa ( formato a50 ) .
C    ID(9)     = Identificazione delle curve ( formato a100 ) .
C    XSCALE    = Ordinata corrispondente al 100% sul grafico;
C                XSCALE viene automaticamente aumentata sino al valore
C                max abs A(I,J) , se e' inferiore a questo valore.
C    MulSca    = Se .TRUE. ogni curva viene tracciata col suo proprio 
C                fattore di scala .
C    PRT       = Se .TRUE. vengono stampati i valori numerici.
C    NA        = Dimensione del primo indice di A nel programma chiamante;
C                deve accordarsi con l'istruzione DIMENSION nel programma
C                principale.
C    ICRE      = Se 0 si usa il contenuto di V ;
C                se 1 si calcolano le ascisse.
C    DDV       = L'incremento costante di V(J) .
C    UN        = Unita' FORTRAN di scrittura.
C
C*****************************************************************************
C
      implicit double precision ( a-h, o-z )
C
      dimension V(1), A(NA,1)
      dimension LINE(101), NUMBER(9), ScaMul(9), NCOL(4)
C
      character ABSIS*50, id(9)*62, FrmStr*72, FrmIte*3, FrmIt1*3
      integer UN 
      integer x, blank, dot
      logical crossX, MulSca, prt
C
      data x, blank, dot / 1hX, 1h ,1h. /
      data NUMBER / 1h1, 1h2, 1h3, 1h4, 1h5, 1h6, 1h7, 1h8, 1h9 /
      data NCOL / 5, 3, 2, 2 /
C
      write(UN,'( 1h1 )' )
C
      scale = XSCALE
      Amax = 0.D0
      Amin = 0.D0
      CurMax = 0.D0
      CurMin = 0.D0
      crossX = .FALSE.
C
      do 9920 i = 1, N
         do 9921 j = 1, M
            Aji = A(j,i)
            if (Aji .gt. CurMax) CurMax = Aji
            if (Aji .lt. CurMin) CurMin = Aji
 9921    continue
         if ( .not. crossX ) crossX = CurMin * CurMax .lt. 0.D0
         if ( CurMax .gt. Amax ) Amax = CurMax
         if ( CurMin .lt. Amin ) Amin = CurMin
         if ( MulSca ) then
          if ( CurMax .ge. abs( CurMin ) ) then
           ScaMul(i) = CurMax
          else
           ScaMul(i) = CurMin
          end if
         end if
         CurMax = 0.D0
         CurMin = 0.D0
 9920 continue
      if ( .not. MulSca ) crossX = Amax * Amin .lt. 0.D0
C
      if (scale .lt. Amax) scale = Amax
      if (scale .lt. abs(Amin) ) scale = abs(Amin)
      if ( Amax .le. 0.D0 ) scale = - scale
C
      if ( scale .eq. 0.D0 ) then
         write(UN,*) ' '
         write(UN,*) ' Dati nulli '
         return
      end if
C
      do 9924 i= 1, N
         if (MulSca) then
            write(UN,113) i, ScaMul(i), ID(i)
         else
            write(UN,108) i, ID(i)
         end if
 9924 continue
C
      if ( .not. MulSca) write(UN,102) scale
C
      if ( crossX ) then
         write(UN,103)
      else
         write(UN,104)
      end if
C
      write(UN,105)
C
      ascis = 0.D0
C
      do 9925 j = 1, M
C
         ascis = ascis + DDV
C
         do 9926 k = 1, 101
            LINE(k) = blank
            if ( j .eq. 1 .or. j .eq. M ) LINE(k) = dot
 9926    continue
         LINE(1) = dot
         LINE(101) = dot
         if ( crossX ) LINE(51) = dot
         do 9927 i = 1, N
            if (MulSca) then
               if ( ScaMul(i) .ne. 0.D0 ) then
                  r = A(j,i) / ScaMul(i) * 100.D0
               else
                  r = 0.D0
               end if
            else
               r = A(j,i) / scale * 100.D0
            end if
C
            if ( crossX ) r = r / 2.D0 + 50.D0
            r = r + 1.4999999D0
            index = r
C
            if ( LINE(index) .eq. blank .or. LINE(index) .eq. dot ) then
               LINE(index) = NUMBER(i)
            else
               LINE(index) = x
            end if
C
 9927    continue
C
         if ( ICRE .eq. 0 ) then
            vv = V(j)
         else
            vv = ascis
         end if
C
         write(UN,106) vv, ( LINE(k), k = 1, 101 )
C
 9925 continue
C
      write(UN,105)
C
      if ( crossX ) then
         write(UN,103)
      else
         write(UN,104)
      end if
C
      write(UN,107) ABSIS
C
      do 9928 i= 1, N
         if (MulSca) then
            write(UN,113) i, ScaMul(i), ID(i)
         else
            write(UN,108) i, ID(i)
         end if
 9928 continue
C
      if ( prt ) then
      if ( N .gt. 4 ) then
C
         write( UN, * ) ' '
         write(UN,109) ( blank, NUMBER(i), i = 1, N )
         do 9929 j = 1, M
 9929    write(UN,110) V(j), ( A(j,i), i = 1, N )
C
      else
C
         NColN = NCOL(N)
         NCNm1 = NColN - 1
         write( UN, * ) ' '
         write( FrmStr, '( 72('' ''))' )  
         write( FrmIte, '(i3)' ) NColN
         write( FrmIt1, '(i3)' ) N
         FrmStr = '(''0'',' //FrmIte// '(''   ASCISSA'', 1x, ' 
     >            //FrmIt1// '(2x, a1, ''CURVA '', a1, 1x ) ) / )'
         write(UN,FrmStr) (( blank, NUMBER(i), i= 1,N ), ikont=1,NColN )
C
         write( FrmStr, '( 72('' ''))' )  
         FrmStr = '('' '', ' //FrmIte// '( 1PD10.3, 1x, ' //FrmIt1//
     >            '( 1PD10.3, 1x ) ) )'
         k = 1
         do 9930 nlin = 1, M / NColN + MIN( 1, MOD( M, NColN ) )
            k1 = MIN( k + NCNm1, M )
            write(UN,FrmStr) ( V(j), ( A(j,i), i = 1, N ), j = k, k1 )
            k = k + NColN
 9930    continue
C
      end if
      end if
C
      return
C
  102 format ( 84x, 'Il 100 % corrisponde a ', 1PD10.3 / )
  103 format ( 12x, '-100',7x,'-80',7x,'-60',7x,'-40',7x,'-20',
     >          8x,'0',8x,'20',8x,'40',8x,'60',8x,'80',8x,'100 %' )
  104 format ( 14x,'0',8x,'10',8x,'20',8x,'30',8x,'40',8x,'50',8x,'60',
     >         8x,'70',8x,'80',8x,'90',8x,'100 %' )
  105 format ( '          ', 11(4x,'+',5x) )
  106 format ( ' X', 1PD10.3, 2x, 101a1 )
  107 format ( 6x,'*'/5x,'***'/4x,'*****'/6x,'*'/6x,'******  ', a // )
  108 format ( '  CURVA', i2, '  -  ', a )
  109 format ( '0  ASCISSA', 4x, 9(a1, 'CURVA ', a1, 2x ) / )
  110 format ( 1PD10.3, 2x, 9(1PD10.3) )
  113 format ( '  CURVA', i2,' : il 100 % corrisponde a ', 
     >        1PD10.3 , '  -  ', a )
C
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine Plot1 (N,M,NA,V,A,ABSIS,ID,XSCALE,ICRE,DDV,UN,MulSca)
C
C--> Chiamata da : Corpo
C
C    Tracciamento sul tabulato di sino a tre funzioni sullo stesso
C    grafico, insieme ad una tabulazione delle stesse funzioni.
C
C    Argomenti di chiamata :
C
C    N         = Quante curve sul grafico; <= 3 .
C    M         = Quanti punti per ogni curva.
C    V(M)      = Vettore contenente le ascisse; max abs V(J) = 999.
C    A(M,N)    = Matrice contenente le ordinate per righe.
C    ABSIS     = Identificazione dell' ascissa ( formato a50 ) .
C    ID(9)     = Identificazione delle curve ( formato a100 ) .
C    XSCALE    = Ordinata corrispondente al 100% sul grafico;
C                XSCALE viene automaticamente aumentata sino al valore
C                max abs A(I,J) , se e' inferiore a questo valore.
C    MulSca    = Se .TRUE. ogni curva viene tracciata col suo proprio 
C                fattore di scala .
C    NA        = Dimensione del primo indice di A nel programma chiamante;
C                deve accordarsi con l'istruzione DIMENSION nel programma
C                principale.
C    ICRE      = Se 0 si usa il contenuto di V ;
C                se 1 si calcolano le ascisse.
C    DDV       = L'incremento costante di V(J) .
C    UN        = Unita' FORTRAN di scrittura.
C
C*****************************************************************************
C
      implicit double precision ( a-h, o-z )
C
      dimension V(1), A(NA,1)
      dimension LINE(81), NUMBER(9), ScaMul(9)
C
      character ABSIS*50, id(9)*62, FrmStr*72, FrmIte*3, FrmIt1*3
      integer UN 
      integer x, blank, dot
      logical crossX, MulSca 
C
      data x, blank, dot / 1hX, 1h ,1h. /
      data NUMBER / 1h1, 1h2, 1h3, 1h4, 1h5, 1h6, 1h7, 1h8, 1h9 /
C
      write(UN,'( 1h1 )' )
C
      scale = XSCALE
      Amax = 0.D0
      Amin = 0.D0
      CurMax = 0.D0
      CurMin = 0.D0
      crossX = .FALSE.
C
      do 9931 i = 1, N
         do 9932 j = 1, M
            Aji = A(j,i)
            if (Aji .gt. CurMax) CurMax = Aji
            if (Aji .lt. CurMin) CurMin = Aji
 9932    continue
         if ( .not. crossX ) crossX = CurMin * CurMax .lt. 0.D0
         if ( CurMax .gt. Amax ) Amax = CurMax
         if ( CurMin .lt. Amin ) Amin = CurMin
         if ( MulSca ) then
          if ( CurMax .ge. abs( CurMin ) ) then
           ScaMul(i) = CurMax
          else
           ScaMul(i) = CurMin
          end if
         end if
         CurMax = 0.D0
         CurMin = 0.D0
 9931 continue
      if ( .not. MulSca ) crossX = Amax * Amin .lt. 0.D0
C
      if (scale .lt. Amax) scale = Amax
      if (scale .lt. abs(Amin) ) scale = abs(Amin)
      if ( Amax .le. 0.D0 ) scale = - scale
C
      if ( scale .eq. 0.D0 ) then
         write(UN,*) ' '
         write(UN,*) ' Dati nulli '
         return
      end if
C
      do 9935 i= 1, N
         if (MulSca) then
            write(UN,113) i, ScaMul(i), ID(i)
         else
            write(UN,108) i, ID(i)
         end if
 9935 continue
C
      if ( .not. MulSca) write(UN,102) scale
C
      if ( crossX ) then
         write(UN,103)
      else
         write(UN,104)
      end if
C
      write( FrmStr, '( 72('' ''))' )  
      write( FrmIte, '(i3)' ) N
      FrmStr = '( 11x, 11(3x,''+'',4x),' //FrmIte//
     >         '(2x,''CURVA '',i1,2x) )'
      write(UN,FrmStr) ( icurv, icurv = 1, N )
C
      ascis = 0.D0
C
      do 9936 j = 1, M
C
         ascis = ascis + DDV
C 
         do 9937 k = 1, 81
            LINE(k) = blank
            if ( j .eq. 1 .or. j .eq. M ) LINE(k) = dot
 9937    continue
C
         LINE(1) = dot
         LINE(81) = dot
         if ( crossX ) LINE(41) = dot
C
         do 9938 i = 1, N
C
            if (MulSca) then
               if ( ScaMul(i) .ne. 0.D0 ) then
                  r = A(j,i) / ScaMul(i) * 80.D0
               else
                  r = 0.D0
               end if
            else
               r = A(j,i) / scale * 80.D0
            end if
C
            if ( crossX ) r = r / 2.D0 + 40.D0
            r = r + 1.4999999D0
            index = r
C
            if ( LINE(index) .eq. blank .or. LINE(index) .eq. dot ) then
               LINE(index) = NUMBER(i)
            else
               LINE(index) = x
            end if
C
 9938    continue
C
         if ( ICRE .eq. 0 ) then
            vv = V(j)
         else
            vv = ascis
         end if
C
         write(UN,106)  vv, ( LINE(k), k = 1, 81 ), ( A(j,i), i=1,N )
C
 9936 continue
C
      write(UN,FrmStr) ( icurv, icurv = 1, N )
C
      if ( crossX ) then
         write(UN,103) 
      else
         write(UN,104) 
      end if
C
      write(UN,107) ABSIS
C
      do 9939 i= 1, N
         if (MulSca) then
            write(UN,113) i, ScaMul(i), ID(i)
         else
            write(UN,108) i, ID(i)
         end if
 9939 continue
C
      return
C
  102 format ( 84x, 'Il 100 % corrisponde a ', 1PD10.3 / )
  103 format ( 12x, '-100',5x,'-80',5x,'-60',5x,'-40',5x,'-20',
     >          6x,'0',6x,'20',6x,'40',6x,'60',6x,'80',6x,'100 %' )
  104 format ( 14x,'0',6x,'10',6x,'20',6x,'30',6x,'40',6x,'50',
     >         6x,'60', 6x,'70',6x,'80',6x,'90',6x,'100 %' )
  106 format ( ' |', 1PD10.3, 2x, 81a1, 3x, 3(1x,1PD10.3) )
  107 format ( 6x,'*'/5x,'***'/4x,'*****'
     >        /6x,'*'/6x,'******  ', a // )
  108 format ( '  CURVA', i2, '  -  ', a )
  113 format ( '  CURVA', i2,' : il 100 % corrisponde a ', 
     >        1PD10.3 , '  -  ', a )
C
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12........
C
      subroutine GestXY ( TabXY, LisXY, NumXY, lista, DesVar, DesFun, 
     >                    NumVar, NumCur, nPunti, NumPun, NMaxXY )
C
      implicit double precision ( a-h, o-z )
      dimension TabXY(NumPun,2*NMaxXY)
      dimension lista(2*NMaxXY), LisXY( NMaxXY, 2 )
      character DesVar(NumVar)*50, DesFun(NumCur)*50, DesX*50, DesY*50
C
      do 10 i = 1, NumXY
        iX = LisXY(i,1)
        iY = LisXY(i,2)
        kX = 0
        if ( iX .gt. 0 ) then
         DesX = DesFun( iX)
        else
         DesX = DesVar(-iX)
        end if
        if ( iY .gt. 0 ) then
         DesY = DesFun( iY)
        else
         DesY = DesVar(-iY)
        end if
   20   continue
          kX = kX + 1
        if ( lista(kX) .ne. iX ) goto 20
        kY = 0
   30   continue
          kY = kY + 1
        if ( lista(kY) .ne. iY ) goto 30
C
        call PlotXY ( TabXY(1,kX), TabXY(1,kY), DesX, DesY, nPunti,
     >                '*', .FALSE. )
   10 continue
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine PlotXY ( X, Y, DesX, DesY, nPunti, Pix, SeDot )
C
      implicit double precision ( a-h, o-z )
      dimension X(1), Y(1)
      character Foglio(0:50)*100, Ordin(0:10)*10, Ascis*110, Pixel*1,
     >          DesX*50, DesY*50, skip*3, FrmIte*35, Pix*1
      logical SeDot
C
      do 10 i = 0, 50
        write( Foglio(i), '( 100( '' '' ) )' )
        if ( SeDot ) then
         write( Foglio(i), '( 10( 9x, ''.'' ) )' )
         if ( mod(i,5) .eq. 0 )  write( Foglio(i), '( 50( '' .'' ) )' )
        end if
   10 continue
      do 20 i = 1, 11
        write( Ordin(i), '( 10( '' '' ) )' )
   20 continue
      write( Ascis, '( 110( '' '' ) )' )
      Xmax = X(1)
      Xmin = X(1)
      Ymax = Y(1)
      Ymin = Y(1)
      do 30 i = 2, nPunti
        if ( X(i) .gt. Xmax ) Xmax = X(i)
        if ( X(i) .lt. Xmin ) Xmin = X(i)
        if ( Y(i) .gt. Ymax ) Ymax = Y(i)
        if ( Y(i) .lt. Ymin ) Ymin = Y(i)
   30 continue
      Xint = Xmax - Xmin
      Yint = Ymax - Ymin
      do 40 i = 1, nPunti
        kX = 1 + 99 * ( X(i) - Xmin ) / Xint
        kY = 1 + 49 * ( Y(i) - Ymin ) / Yint
        Pixel = Foglio(kY)(kX:kX)
        if ( Pix .eq. ' ' ) then
          if ( Pixel .eq. ' ' .or. Pixel .eq. '.' ) then
            Pixel = '0'
          else if ( Pixel .ne. '*' ) then
            read ( Pixel, '(i1)' ) ipix
            ipix = ipix + 1
            if ( ipix .le. 9 ) then
              write( Pixel, '(i1)' ) ipix
            else
              Pixel = '*'
            end if
          end if
        else
          Pixel = Pix
        end if
        Foglio(kY)(kX:kX) = Pixel
   40 continue
      DeltY = Yint / 10.D0
      yy = Ymax
      do 50 i = 10, 0, -1
        write( Ordin(i), '(1PD9.2,1x)' ) yy
        yy = yy - DeltY
   50 continue
      DeltX = Xint / 10.D0
      xx = Xmin
      do 60 i = 0, 10
        k = i * 10 + 1
        write( Ascis(k:k+9), '( 1x, 1PD9.2 )' ) xx
        xx = xx + DeltX
   60 continue
C
      write(1, '( 1h1 )' )
      write(1, '( 2x, a )' )      Ascis
      write(1, '( 2x, 11( 9x, ''V'' ) )' )
      write(1, '( 11x, 51( ''. '' ) )' )
      do 70 i = 50, 0, -1
        if ( mod(i,5) .eq. 0 ) then
          j = i / 5
          write(1, '( 1x, 5a )' ) Ordin(j),'.',Foglio(i),'. ',Ordin(j)
        else
          write(1, '( 11x,3a )' )          '.',Foglio(i),'.'
        end if
   70 continue
      write(1, '( 11x, 51( ''. '' ) )' )
      write(1, '( 2x, 11( 9x, ''^'' ) )' )
      write(1, '( 2x, a )' )      Ascis
      write(1,*)                  ' '
      write( skip, '( i3 )' ) (104 - Lung(DesX,50)- Lung(DesY,50)) / 2 
      FrmIte = '(1x, a,' // skip // 'x,''VERSUS'','//skip // 'x,a)'
      write(1, FrmIte ) DesY( 1:Lung(DesY,50)), DesX( 1:Lung(DesX,50))
      return
      end
c
c23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
c
      subroutine LetLis ( LinLis, listVF, listG, LiLiDi, LunLiG, NumVar, 
     >                  NumCur, TitWin, NumWin, FilRis, i_wind, LuNoFi )
c 
      dimension LinLis(LiLiDi), listVF(9), listG(NumVar+NumCur)
      character riga*120, car*1, opzio*1, TitWin(NumWin)*80, FilRis*(*)
      logical Accu
c
      ip = 1
      LinLis( 1 ) = 1
      LunLiG = 0
      i_wind = 0
      write(1,'(a)') ' '
      write(1,'(a)') ' --> Si sono create queste finestre grafiche :'
  100 continue
       i_wind = i_wind + 1
       ip = LinLis( ip )
       do 110 k = 1, 8
  110   listVF( k ) = 0
       write( riga, '( 100('' '') )' )
       write( TitWin(i_wind), '( 80('' '') )' )
       Accu = .false.
       opzio = ' '
       nCurve = 0
       read ( 2, '( a )', end = 200 ) riga
       if ( riga(1:1) .ne. ':' ) then
        write( TitWin(i_wind), '( a, ''; fin. '', i2 )') 
     >             FilRis(:Lung(FilRis,LuNoFi)), i_wind
        IndPE = index( riga, '!' )
        IndPV = index( riga, ';' )
        if ( IndPE .gt. 0 .and. IndPV .eq. 0 ) then
         IndTit = IndPE
        else if ( IndPE .eq. 0 .and. IndPV .gt. 0 ) then
         IndTit = IndPV
        else
         IndTit = min( IndPE, IndPV )
        end if
        if ( IndTit .gt. 0 ) then
         LT = Lung( TitWin(i_wind), 80 )
         TitWin(i_wind)=TitWin(i_wind)(:LT)//' : '//riga(IndTit+1:)
         IndDo = IndTit - 1
        else
         IndDo = 120
        end if
        do 120 i = IndDo, 1, -1
         car = riga(i:i)
         k = ichar( car )
         if ( k .ge. 48 .and. k .le. 57 ) then
          kcifr = k - 48
          if ( Accu ) then
           j = j + 1
           num = num + kcifr * 10**j
          else
           j = 0
           num = kcifr
           Accu = .true.
          end if
         else if ( car.eq.'M' .or. car.eq.'m' .or. 
     >             car.eq.'U' .or. car.eq.'u' .or.
     >             car.eq.'C' .or. car.eq.'c' .or.
     >             car.eq.'B' .or. car.eq.'b' .or.
     >             car.eq.'F' .or. car.eq.'f' ) then
          if ( Accu ) then
           write(*,*) ' Errore 1 : M,U,C,B o F appiccicati a una cifra'
          else
           opzio = car
          end if
         else if ( car.eq.' ' .or. car.eq.'+' ) then
          if ( Accu ) then
           if ( nCurve .lt. 9 ) nCurve = nCurve + 1
           listVF(nCurve) = num
           Accu = .false.
          end if
         else if ( car .eq. '-' ) then
          if ( Accu ) then
           num = -num
          else
           write(*,*) ' Errore 2 : un segno - isolato,'
          end if
         end if
         if ( Accu .and. i.eq.1 ) then
          if ( nCurve .lt. 9 ) nCurve = nCurve + 1
          listVF(nCurve) = num
          Accu = .false.
         end if
  120   continue
        n_asc = listVF(nCurve)
        nCurve = nCurve - 1
        LinLis( ip ) = ip + nCurve + 3
        if ( index( opzio,'U' ) + index( opzio,'u' ) .ne. 0 ) then 
         LinLis( ip+1 ) = 1
        else if ( index( opzio,'C' ) + index( opzio,'c' ) .ne. 0 ) then 
         LinLis( ip+1 ) = 5
        else if ( index( opzio,'F' ) .ne. 0 ) then
         LinLis( ip+1 ) = 0
        else if ( index( opzio,'B' ) .ne. 0 ) then
         LinLis( ip+1 ) = 2
        else if ( index( opzio,'f' ) .ne. 0 ) then
         LinLis( ip+1 ) = 3
        else if ( index( opzio,'b' ) .ne. 0 ) then
         LinLis( ip+1 ) = 4
        else
         LinLis( ip+1 ) = -1
        end if
        LinLis( ip+2 ) = n_asc
        do 130 i = 1, nCurve
  130    LinLis( ip + 2 + i ) = listVF( nCurve + 1 - i )
        if(n_asc.ne.0) call PutLis(listG, n_asc, LunLiG, NumVar, NumCur) 
        do 140 i = 1, nCurve
  140    call PutLis ( listG, listVF(i), LunLiG, NumVar, NumCur ) 
c
        write( 1, '(5x,a)') TitWin(i_wind)(:Lung( TitWin(i_wind),80 ))
        goto 100
       end if ! Riga(1:1) .ne. ':'
c
  200  return
      end
c
c23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
c
      subroutine PutLis ( listG, k, LunLiG, NumVar, NumCur )
c
      dimension listG( NumVar+NumCur)
      logical cegia
c
      i = 0
  100 i = i + 1
       cegia = k .eq. listG( i )
       if ( .not. cegia .and. i .lt. LunLiG ) goto 100
      if ( .not. cegia ) then
       LunLiG = LunLiG + 1
       listG( LunLiG ) = k
      end if
      return
      end 
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12........
C
      subroutine TabGra ( TabGr, listG, LunLiG, x, f, NumPun, NumVar, 
     >                    NumCur, t, j, T_in, nRipMx, ijTG )
C
C--> Chiamata da : Corpo, Inizia
C
      implicit double precision ( a-h, o-z )
      dimension TabGr(NumPun,0:NumVar+NumCur,0:nRipMx)
      dimension x(NumVar), f(NumCur), listG(NumVar+NumCur)
C
      if ( j .le. NumPun ) then
      TabGr(j,0,0) = t
      do 10 i = 1, LunLiG
        ind = listG(i)
        if ( ind .gt. 0 ) then
          TabGr(j,i,ijTG) = f(ind)
        else        
          TabGr(j,i,ijTG) = x(-ind)
        end if
        if (  j .eq. 2 ) TabGr(1,i,ijTG) = TabGr(2,i,ijTG)
   10 continue
        if (  j .eq. 2 ) TabGr(1,0,ijTG) = TabGr(2,0,ijTG)
      end if
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12........
C
