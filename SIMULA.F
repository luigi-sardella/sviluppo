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
CC
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12........
C
      subroutine faiFFT ( listG , LinLis, TabGr, nPunti, iFFT, Afft,
     >                 NumFFT, NumPun, NumVar, NumCur, LiLiDi, LunLiG,
     >                 DesVar, DesFun, iFunMx, FreqMx, i_wind, 
     >                 nRipMx, ijTG )
      implicit double precision ( a-h, o-z )
C
      dimension LinLis(LiLiDi), listG(NumVar+NumCur)
      dimension TabGr(NumPun,0:NumVar+NumCur,0:nRipMx)
      character*50 DesFun(NumCur), DesVar(NumVar)
      complex*16 Afft(NumFFT)
      logical seFFT, BasLin, GraLog
C
      parameter ( PosMax = 1.6D+38, PosMin = 0.3D-38 )
C
      if ( LunLiG .eq. 0 ) return
      i_wind = 0
      IndFun = iFunMx
      ip = 1
  100 continue
C
       seFFT = .false.
       BasLin = .false.
       GraLog = .false.
       i_wind = i_wind + 1
       ncurve = LinLis(ip) - ip - 3
       iOpzGr = LinLis(ip+1)
       n_asc  = LinLis(ip+2)
       if ( iOpzGr.ne.1 .and. iOpzGr.ne.-1 
     >.and. iOpzGr.ne.5 .and. n_asc.eq.0) seFFT=.true.
       if ( iOpzGr .eq. 2 .or. iOpzGr .eq. 4 ) BasLin = .true.
       if ( iOpzGr .eq. 0 .or. iOpzGr .eq. 2 ) GraLog = .true.
C
       if ( seFFT ) then

       do 300 i = ip + 2 + 1, ip + 2 + ncurve
C
        ind = LinLis(i)
        IndCur = InLis( ind, listG, LunLiG, NumVar, NumCur )
        LunLiG = LunLiG + 1
        IndFun = IndFun + 1
        if ( IndFun .gt. NumVar+NumCur ) then
         write(*,*) ' Si stanno definendo troppe curve: '
         write(*,*) ' correggere e ricompilare il Main '
         return
        end if
        LinLis(i) = IndFun
        listG(LunLiG) = IndFun
        if ( ind .gt. 0 ) then
         DesFun(IndFun) = DesFun(ind)
        else
         DesFun(IndFun) = DesVar(-ind)
        end if
C
        sum  = 0.D0
        if ( BasLin ) then
         do 250 j = 1, nPunti
          sum = sum + TabGr(j,IndCur,ijTG)
  250    continue
         sum = sum / nPunti
        end if
        call PasBas(TabGr(1,IndCur,ijTG),TabGr(1,LunLig,ijTG),nPunti)
        do 260 j = 1, 2**iFFT
         if ( j .le. nPunti ) then
          Afft( j ) = dcmplx( TabGr(j,LunLig,ijTG) - sum )
         else
          Afft( j ) = dcmplx( 0.D0 )
         end if
  260   continue
C
        call FFT ( Afft, iFFT, 2**iFFT )
C
        AmodMn = PosMax
        do 270 j = 1, nPunti
         Amodu = max( cdabs( Afft( j ) ), 10.D00*PosMin )
         if ( AmodMn .gt. Amodu ) AmodMn = Amodu
         TabGr(j,LunLiG,ijTG) = Amodu 
  270   continue
C
        if ( GraLog ) then
         do 280 j = 1, nPunti
          TabGr(j,LunLiG,ijTG) = log(TabGr(j,LunLiG,ijTG)) - log(AmodMn) 
  280    continue
        end if
C
  300  continue
C
       end if ! seFFT
C
       ip = LinLis ( ip )
       if ( LinLis(ip) .eq. 0 ) return
      goto 100
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12........
C
      subroutine FFT ( A, n, nb )
C
      parameter ( pi = 3.141592653589793D0 )
C
      complex*16 A( nb ), U, W, T
C
      do j = 1, nb
         A( j ) = A( j ) / nb
      end do
      nbd2 = nb / 2
      nbm1 = nb - 1
      j = 1
C
      do l = 1, nbm1
         if ( l .lt. j ) then
            T = A( j )
            A( j ) = A( l )
            A( l ) = T
         end if
         k = nbd2
         do while ( k .lt. j )
            j = j - k
            k = k / 2
         end do
         j = j + k
      end do
C
      do m = 1, n
         U = ( 1.0d0, 0.0d0 )
         me = 2**m
         k = me / 2
         W = dcmplx( cos( pi/k), -sin( pi/k) )
         do j = 1, k
            do l = j, nb, me
               lpk = l + k
               T = A( lpk ) * U
               A(lpk) = A( l ) - T
               A( l ) = A( l ) + T
            end do
            U = U * W
         end do
      end do
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12........
C
      subroutine PasBas ( U, Y, n )
C
      double precision Y(n), U(n), Coeff(-5:5), sum
      data Coeff
     > / -1.D0, -5.D0, -5.D0, 20.D0, 70.D0, 98.D0,
     >   70.D0, 20.D0, -5.D0, -5.D0, -1.D0/
C
      do 100 i = 6, n - 5
       sum = 0.D0
       do 200 k = -5, 5
        sum = sum + Coeff(k) * U(i-k)
  200  continue
       Y(i) = sum / 256.D0
  100 continue
      do 300 i = 1, 5
       Y(i) = U(i)
  300 continue
      do 400 i = n - 4, n
       Y(i) = U(i)
  400 continue
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12........
C
      subroutine GestGr( listG , LinLis, TabGr, DesFun, DesVar, nPunti, 
     >                  NumPun, NumVar, NumCur, LiLiDi, FilRs1, LunLiG, 
     >                  TitWin, NumWin, Wid,Alt,FinTem, FinFre, FreqMx, 
     >                  data, ora, NewPar, i_, j_, nRipMx, ijTG, LuNoFi,
     >                  NumFil, TemIni, FreIni, kPunti, U_Mscl )
      implicit double precision ( a-h, o-z )
      double precision mantis
C
      include 'sys$library:uisentry'
      include 'sys$library:uisusrdef'
      integer vd_id, wd_id, vcm_id, vcm_size
      real R_vett(8), G_vett(8), B_vett(8), Wid, Alt
C
      data R_vett / 1.0, 0.0, 0.0 , 0.0, 1.0 , 0.8, 0.8, 0.2 /
      data G_vett / 1.0, 0.0, 0.0 , 0.5, 0.0 , 0.0, 0.6, 0.7 /
      data B_vett / 1.0, 0.0, 1.0 , 0.2, 0.0 , 0.9, 0.0, 0.7 /
C     
      data vcm_size / 9 /, Dmarg / 20.D0 /
      data AltCar / 0.5D0 /, WidCar / 0.25D0 /, Gradua / 0.03D0 /
C
      dimension LinLis(LiLiDi), listG(NumVar+NumCur)
      dimension TabGr(NumPun,0:NumVar+NumCur,0:nRipMx), ScaMul( 8 )
      character*50 DesFun(NumCur), DesVar(NumVar), ScaNum( 8 )*10
      character scritt*70, ascis*80, XmaxA*10, XminA*10, GraTxt*5 
      character FilRs1*(*), TitWin(NumWin)*80, Esten*3, data*10, ora*8
      character NumFil*4, sino*1
      logical crossX, MulSca, prt, seFFT, GraLog, BasLin, NewPar, U_Mscl
C
      if ( NewPar .and. i_.eq.1 .and. j_.eq.1 ) then
       read ( 5, * ) 
       read ( 5, * ) 
       do i = 1, 8
        read ( 5, *, end = 10 ) R_vett(i), G_vett(i), B_vett(i)
       end do
   10  continue
       close ( unit = 5 )
       NewPar = .false.
      end if
C
      vcm_id = uis$create_color_map( vcm_size )
C
      if ( LunLiG .eq. 0 ) return
      i_wind = 0
      ip = 1
  100 continue
C
       i_wind = i_wind + 1
       ind2pp = index(TitWin(i_wind),':')
       LunTit = lung( TitWin(i_wind),80 )
       write (*,'('' Si vuole disegnare la finestra '', a, 
     >            '' (S=D/N) ? '', $ )' ) TitWin(i_wind)(ind2pp:LunTit)
       read(*,'( a )' ) sino
       if( sino.eq.'N' .or. sino.eq.'n' ) goto 600
C
       iOpzGr = LinLis(ip+1)
       if ( iOpzGr .eq. 5 ) then
        call cinema( listG , LinLis, TabGr, DesFun, DesVar, nPunti, 
     >               NumPun, NumVar, NumCur, LiLiDi, FilRs1, LunLiG, 
     >               TitWin, NumWin, Wid,Alt,FinTem, FinFre, FreqMx, 
     >               data, ora, i_wind, ip, R_vett, G_vett, B_vett, 
     >               AltCar, WidCar, Gradua, nRipMx, ijTG, LuNoFi,
     >               NumFil, TemIni, FreIni, kPunti  )
        goto 600
       end if
C
       ncurve = LinLis(ip) - ip - 3
       n_asc  = LinLis(ip+2)
       MulSca = .false.
       if ( iOpzGr .eq. -1 ) MulSca = .true.
       if ( ncurve .eq.  1 ) MulSca = .false.
       if ( U_Mscl ) then
         if ( MulSca ) then
           MulSca = .false.
         else
           MulSca = .true.
         end if
       end if
       seFFT = .false.
       if ( iOpzGr.ne.1.and.iOpzGr.ne.-1 .and. n_asc.eq.0) then
        seFFT = .true.
        MulSca = .true.
       end if
       BasLin = .false.
       if ( iOpzGr .eq. 2 .or. iOpzGr .eq. 4 ) BasLin = .true.
       GraLog = .false.
       if ( iOpzGr .eq. 0 .or. iOpzGr .eq. 2 ) GraLog = .true.
       Amax = 0.D0
       Amin = 0.D0
       CurMax = 0.D0
       CurMin = 0.D0
       crossX = .FALSE.
C
       k_asc = InLis( n_asc, listG, LunLiG, NumVar, NumCur )
       k_ = ijTG
       if ( k_asc .eq. 0 ) k_ = 0
       Xmin = TabGr ( 1, k_asc, k_ )
       Xmax = Xmin
       do 200 i = 2, nPunti
        if ( TabGr(i,k_asc,k_) .gt. Xmax ) Xmax = TabGr(i,k_asc,k_) 
        if ( TabGr(i,k_asc,k_) .lt. Xmin ) Xmin = TabGr(i,k_asc,k_) 
  200  continue
C
       nPun = nPunti - 1
C
       if ( seFFT ) then
        DFreq = 1.D0 / ( Xmax - Xmin )
        Xmax = min( FreqMx, FinFre )
        Xmin = max( 0.D00, FreIni )
        iPun = max( 2, int( Xmin/DFreq ) )
        nPun = int( (Xmax-Xmin) / DFreq )
       else if ( k_asc .eq. 0 .and. 
     >           (FinTem.lt.Xmax.or.TemIni.gt.Xmin) ) then
        iPun = 2
        nPun = 2
        do while ( TabGr(nPun,0,0) .lt. TemIni )
         iPun = iPun + 1
         nPun = nPun + 1
        end do
        Xmax = TabGr(nPun,0,0) 
        Xmin = Xmax
        do while ( TabGr(nPun,0,0) .lt. FinTem .and.
     >             nPun .le. nPunti-1 )
         if ( TabGr(nPun,0,0) .gt. Xmax ) Xmax = TabGr(nPun,0,0) 
         if ( TabGr(nPun,0,0) .lt. Xmin ) Xmin = TabGr(nPun,0,0) 
         nPun = nPun + 1
        end do
       end if
C
       Xmarg = ( Xmax - Xmin ) / Dmarg
       write( XmaxA, '( 1PD10.3 )' ) Xmax
       write( XminA, '( 1PD10.3 )' ) Xmin
C
       if ( n_asc .eq. 0 ) then
        if ( seFFT ) then
         ascis = ' In ascissa le frequenze; da '//XminA//' a '//XmaxA
     >         //' Hz'
        else
         ascis = ' In ascissa il tempo; da '//XminA//' a '//XmaxA
     >         //' sec'
        end if
       else  
        if ( n_asc .gt. 0 ) then
         scritt = DesFun( n_asc)
        else
         scritt = DesVar(-n_asc)
        end if
        ascis = ' In ascissa : '//scritt(1:lung(scritt,70)) 
     >          //'; da '//XminA//' a '//XmaxA
       end if
C
       RapSca = 0.D0
       do 300 i = ip + 2 + 1, ip + 2 + ncurve
        iSM = i - ip - 2
        IndCur = InLis( LinLis(i), listG, LunLiG, NumVar, NumCur )
        do 250 j = max(2,iPun), nPun
         Aji = TabGr(j,IndCur,ijTG)
         if (Aji .gt. CurMax) CurMax = Aji
         if (Aji .lt. CurMin) CurMin = Aji
  250   continue
        if ( .not. crossX ) crossX = CurMin * CurMax .lt. 0.D0
        if ( CurMax .gt. Amax ) Amax = CurMax
        if ( CurMin .lt. Amin ) Amin = CurMin
        if ( MulSca ) then
!
        ScaMul(iSM) = 0.D0
        if (ScaMul(iSM) .lt. CurMax) then
          ScaMul(iSM) = CurMax
          AcsLum = CurMin
        end if
        if (ScaMul(iSM) .lt. abs(CurMin) ) then
          ScaMul(iSM) = abs(CurMin)
          AcsLum = CurMax
        end if
        if (CurMax.le.0.D0.or.(crossX.and.abs(CurMin).gt.CurMax)) 
     >      ScaMul(iSM) = - ScaMul(iSM)
        if ( ScaMul(iSM).ne.0.D0 )
     >      RapSca = max( RapSca, abs( AcsLum/ScaMul(iSM) ) )
!
!         if ( CurMax .ge. abs( CurMin ) ) then
!          ScaMul(iSM) = CurMax
!         else
!          ScaMul(iSM) = CurMin
!         end if
!
         write( ScaNum(iSM), '( 1PD10.3 )' ) ScaMul(iSM)
        end if
        CurMax = 0.D0
        CurMin = 0.D0
  300  continue
       if ( .not. MulSca ) crossX = Amax * Amin .lt. 0.D0
C
       scale = 0.D0
       elacs = 0.D0
       if (scale .lt. Amax) then
         scale = Amax
         elacs = Amin
       end if
       if (scale .lt. abs(Amin) ) then
         scale = abs(Amin)
         elacs = Amax
       end if
       if (Amax.le.0.D0.or.(crossX.and.abs(Amin).gt.Amax)) scale=-scale
       if ( .not. MulSca ) write( ScaNum(1), '( 1PD10.3 )' ) scale
C
       if ( scale .eq. 0.D0 ) then
        write( *, * ) ' Dati nulli '
        goto 600
       end if
C
       RiText = float( ncurve ) / 8.D0
       H1 = Alt - AltCar * ( 1.D0 - RiText )
       Ymin = 0.D0
       in_bas = 0
       if ( crossX ) then
         if ( MulSca ) then
           in_bas = int( 10.D0*RapSca ) + 1
         else
           in_bas = abs(int( 10.D0*elacs/scale )) + 1
         end if
         Ymin = -0.1D0 * float( in_bas )
       end if
       vd_id = uis$create_display( Xmin-2*Xmarg, Ymin - 1.D0,
     >                      Xmax+Xmarg,  1.05D0, Wid, Alt, vcm_id)
       kb_id = uis$create_kb( 'sys$workstation' )
C
       wd_id = uis$create_window( vd_id, 'sys$workstation', 
     >                     TitWin(i_wind)(1:LunTit)//'-'//NumFil )
       call uis$enable_viewport_kb ( kb_id, wd_id )
       call uis$set_colors ( vd_id, 0, 8, R_vett, G_vett, B_vett )
       do 410 i = 1, 8
  410   call uis$set_writing_index ( vd_id, 0, i, i )
C
       call uis$plot ( vd_id, 0, Xmin, Ymin, Xmax, Ymin )
       call uis$plot ( vd_id, 0, Xmax,   1.D0, Xmax, Ymin )
       call uis$plot ( vd_id, 0, Xmax,   1.D0, Xmin,   1.D0 )
       call uis$plot ( vd_id, 0, Xmin, Ymin, Xmin,   1.D0 )
       if ( crossX ) call uis$plot ( vd_id, 0, Xmin, 0.D0, Xmax, 0.D0 )
C
       WiCaWC = ( WidCar / Wid ) * ( Xmax - Xmin )
       n_grad = 9
       if ( WidCar * 4 * 8 .gt. 0.75D0 * Wid ) n_grad = 4
       if ( WidCar * 4 * 4 .lt. Wid ) then
        X_grad = ( Xmax - Xmin ) / ( n_grad + 1 )
        Xx = max( abs( Xmax ), abs( Xmin ) )
        Fatt = mantis( Xx ) / Xx
        do 420 i_grad = 1, n_grad
         Xpl = Xmin + i_grad * X_grad
         call uis$plot ( vd_id, 0, Xpl, Ymin, Xpl, Ymin-Gradua )
         call uis$set_aligned_position(vd_id,0,Xpl-3*WiCaWC,Ymin-Gradua)
         write( GraTxt, '( f5.2 )' ) Fatt * Xpl
         call uis$text ( vd_id, 0, GraTxt )
  420   continue
       end if
C
       ScaOrd = 1.D0
       if ( .not. MulSca ) ScaOrd = mantis( abs( scale ) )
       AlCaWC = (AltCar/Alt) * (1.D0-Ymin) * (1.D0+0.3D0*abs(Ymin))
       if ( crossX ) then
        i_iniz = - in_bas
       else
        i_iniz = 1
       end if
       do 430 i_grad = i_iniz, 9
        Ypl = float( i_grad ) / 10.D0
        Grad_Y = Gradua * ( Xmax - Xmin ) / 3.D0
        call uis$plot ( vd_id, 0, Xmin, Ypl, Xmin+Grad_Y, Ypl )
        call uis$plot ( vd_id, 0, Xmax, Ypl, Xmax-Grad_Y, Ypl )
        write( GraTxt, '( f5.2 )' ) Ypl * ScaOrd
        call uis$text ( vd_id, 0, GraTxt, Xmin-8*WiCaWC, Ypl+AlCaWC )
  430  continue
C
       call uis$set_aligned_position ( vd_id, 0, 0.D0, Ymin-Gradua )
C
       call uis$text ( vd_id, 0, ' ' )
       call uis$new_text_line ( vd_id, 0 )
       call uis$text ( vd_id, 0, ' ' )
       call uis$new_text_line ( vd_id, 0 )
       call uis$text ( vd_id, 0, ascis )
       call uis$new_text_line ( vd_id, 0 )
       if ( seFFT .and. GraLog ) then
        call uis$text ( vd_id, 0, 
     >                 ' FFT - Scala delle ordinate logaritmica' )
        call uis$new_text_line ( vd_id, 0 )
       else if ( seFFT .and. .not. GraLog ) then
        call uis$text ( vd_id, 0,
     >                 ' FFT - Scala delle ordinate lineare' )
        call uis$new_text_line ( vd_id, 0 )
       end if
       if ( seFFT .and. BasLin ) then
        call uis$text ( vd_id, 0, 
     >                 '       Correzione della linea di base' )
        call uis$new_text_line ( vd_id, 0 )
       end if
       call uis$text ( vd_id, 0, ' ' )
       call uis$new_text_line ( vd_id, 0 )
C
       do 400 i = ip + 2 + 1, ip + 2 + ncurve
        iSM = i - ip - 2
        icolor = mod( iSM, 8 ) + 1
        ind = LinLis(i)
        IndCur = InLis( ind, listG, LunLiG, NumVar, NumCur )
        if ( seFFT ) then
         X1 = Xmin
        else
         X1 = TabGr ( max(2,iPun), k_asc, k_ )
        end if
        Aji = TabGr ( max(2,iPun), IndCur, ijTG )
        if (MulSca) then
         if ( ScaMul(iSM) .ne. 0.D0 ) then
          Y1 = Aji / ScaMul(iSM) 
         else
          Y1 = 0.D0
         end if
        else
         Y1 = Aji / scale
        end if
        do 500 j = max(iPun+1,3), nPun, max( kPunti, 1 ) 
         if ( seFFT ) then
          X2 = (j-1) * Dfreq + FreIni
         else
          X2 = TabGr( j, k_asc, k_ )
         end if
         Aji = TabGr( j, IndCur, ijTG )
         if (MulSca) then
          if ( ScaMul(iSM) .ne. 0.D0 ) then
           Y2 = Aji / ScaMul(iSM) 
          else
           Y2 = 0.D0
          end if
         else
          Y2 = Aji / scale
         end if
C
         call uis$plot ( vd_id, icolor, X1, Y1, X2, Y2 )
         X1 = X2
         Y1 = Y2
C
  500   continue
C
        if ( ind .gt. 0 ) then
         scritt = DesFun( ind)
        else
         scritt = DesVar(-ind)
        end if
        if ( MulSca ) then
         scritt = scritt(1:lung(scritt,70))//' Scala : '//ScaNum(iSM)
        else if ( iSM .eq. 1 ) then
         scritt = scritt(1:50)//' Scala : '//ScaNum(1)
        end if
C
        call uis$text ( vd_id, icolor, scritt )
        call uis$new_text_line ( vd_id, icolor )
C
  400  continue
C
       write( scritt, '( 70a )' ) ( ' ', i = 1, 70 )
       write( scritt, '( 70a )' ) ( '-', i = 1, LunTit+5 )
       call uis$new_text_line ( vd_id, 0 )
       call uis$text ( vd_id, 0, ' '//scritt )
       call uis$new_text_line ( vd_id, 0 )
       call uis$text ( vd_id, 0, ' '//TitWin(i_wind)(1:LunTit)
     >                           //'-'//NumFil )
       call uis$new_text_line ( vd_id, 0 )
       call uis$text ( vd_id, 0, ' '//scritt )
       call uis$new_text_line ( vd_id, 0 )
       call uis$text ( vd_id, 0, ' '//data//' '//ora )
       write( scritt, '( 70a )' ) ( ' ', i = 1, 70 )
       write( scritt, '( 70a )' ) ( '-', i = 1, 19 )
       call uis$new_text_line ( vd_id, 0 )
       call uis$text ( vd_id, 0, ' '//scritt )
C
       write( Esten, '( a, i2.2 )' ) '.', i_wind
       call hcuis$write_display ( vd_id, 
     >   FilRs1(1:Lung(FilRs1,LuNoFi))//Esten )
       call uis$shrink_to_icon ( wd_id )
  600  continue
       ip = LinLis ( ip )
       if ( LinLis(ip) .eq. 0 ) return
      goto 100
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine cinema( listG , LinLis, TabGr, DesFun, DesVar, nPunti, 
     >                  NumPun, NumVar, NumCur, LiLiDi, FilRis, LunLiG, 
     >                  TitWin, NumWin, Wid,Alt,FinTem, FinFre, FreqMx, 
     >                  data, ora, i_wind, ip, R_vett, G_vett, B_vett, 
     >                  AltCar, WidCar, Gradua, nRipMx, ijTG, LuNoFi,
     >                  NumFil, TemIni, FreIni, kPunti )
      implicit double precision ( a-h, o-z )
      double precision mantis
C
      include 'sys$library:uisentry'
      include 'sys$library:uisusrdef'
      integer vd_id, wd_id, vcm_id, vcm_size, ColCer(5)
      real R_vett(8), G_vett(8), B_vett(8), Wid, Alt
C
      data vcm_size / 9 /, Dmarg / 20.D0 /, NTacDf / 20 /
      data ColCer / 2, 3, 4, 5, 1 /
C
      dimension LinLis(LiLiDi), listG(NumVar+NumCur)
      dimension TabGr(NumPun,0:NumVar+NumCur,0:nRipMx)
      character*50 DesFun(NumCur), DesVar(NumVar), TmaxA*10, TminA*10
      character XmaxA*10, XminA*10, YmaxA*10, YminA*10, ascis*80
      character scritx*70, scrity*70, scritt*70, GraTxt*5, NumFil*4
      character FilRis*(*), TitWin(NumWin)*80, Esten*3, data*10, ora*8
C
      H1 = Wid * 210.D0 / 297.D0
      ncurv2 = LinLis(ip) - ip - 3 
      n_asc  = LinLis(ip+2)
      if ( mod( ncurv2, 2 ) .eq. 0 ) then
       NTacch = NTacDf 
       incr = 0
      else
       NTacch = abs( LinLis( ip + 3 ))
       ncurv2 = ncurv2-1
       incr = 1
      end if
      ncurve = ncurv2/2
      k_asc = InLis( n_asc, listG, LunLiG, NumVar, NumCur )
      k_ = ijTG
      if ( k_asc .eq. 0 ) k_ = 0
C
      nPun = nPunti - 1
      iPun = 2
      nPun = 2
      do while ( TabGr(nPun,k_asc,k_) .lt. TemIni )
         iPun = iPun + 1
         nPun = nPun + 1
      end do
      Tmin = TabGr ( iPun, k_asc, k_ )
      Tmax = Tmin
      do while ( TabGr(nPun,0,0) .lt. FinTem .and.
     >           nPun .le. nPunti-1 )
         if (TabGr(nPun,k_asc,k_).gt.Tmax) Tmax = TabGr(nPun,k_asc,k_) 
         if (TabGr(nPun,k_asc,k_).lt.Tmin) Tmin = TabGr(nPun,k_asc,k_) 
         nPun = nPun + 1
      end do
      Xmarg = ( Tmax - Tmin ) / Dmarg
      write( TmaxA, '( 1PD10.3 )' ) Tmax
      write( TminA, '( 1PD10.3 )' ) Tmin
C
      do 600 i_cur = 1, ncurve
       i_cur1 = 2 * ( i_cur - 1 ) + 1
       in_cv1 = ip + 2 + incr + i_cur1
       in_cv2 = in_cv1 + 1
       nx = LinLis( in_cv1 )
       ny = LinLis( in_cv2 )
       kx = InLis( nx, listG, LunLiG, NumVar, NumCur )
       ky = InLis( ny, listG, LunLiG, NumVar, NumCur )
       if ( i_cur .eq. 1 ) then
        Xmin = TabGr ( max(2,iPun), kx, ijTG )
        Xmax = Xmin
        Ymin = TabGr ( max(2,iPun), ky, ijTG )
        Ymax = Ymin
       end if
C
       do 200 i = max(3,iPun), nPun
        if ( TabGr( i, kx, ijTG ) .gt. Xmax ) Xmax = TabGr( i, kx, ijTG) 
        if ( TabGr( i, kx, ijTG ) .lt. Xmin ) Xmin = TabGr( i, kx, ijTG) 
        if ( TabGr( i, ky, ijTG ) .gt. Ymax ) Ymax = TabGr( i, ky, ijTG) 
        if ( TabGr( i, ky, ijTG ) .lt. Ymin ) Ymin = TabGr( i, ky, ijTG) 
  200  continue
  600 continue
C
      T_tacc = ( min( FinTem, Tmax ) - Tmin ) / NTacch
      write( TmaxA, '( 1PD10.3 )' ) Tmax
      write( TminA, '( 1PD10.3 )' ) Tmin
      ScalX = Xmax - Xmin
      Xmarg = ScalX / Dmarg
      write( XmaxA, '( 1PD10.3 )' ) Xmax
      write( XminA, '( 1PD10.3 )' ) Xmin
      ScalY = Ymax - Ymin
      Ymarg = ScalY / Dmarg
      write( YmaxA, '( 1PD10.3 )' ) Ymax
      write( YminA, '( 1PD10.3 )' ) Ymin
      if ( ScalX .gt. ScalY ) then
       Scala = ScalX
       Ymin = Ymin + ScalY/2.D0 - Scala/2.D0
       Ymax = Ymax - ScalY/2.D0 + Scala/2.D0
      else
       Scala = ScalY
       Xmin = Xmin + ScalX/2.D0 - Scala/2.D0
       Xmax = Xmax - ScalX/2.D0 + Scala/2.D0
      end if
      XYmar = Scala / Dmarg
C
      if ( n_asc .eq. 0 ) then
       ascis = ' Tempo; da '//TminA//' a '//TmaxA
      else  
       if ( n_asc .gt. 0 ) then
        scritt = DesFun( n_asc)
       else
        scritt = DesVar(-n_asc)
       end if
       ascis = ' Parametro : '//scritt(1:lung(scritt,70))
     >         //'; da '//TminA//' a '//TmaxA
      end if
C
      am1 = XYmar * 3.5D0 ! Margine in basso
      am2 = XYmar         ! Margine in alto
      am3 = XYmar * 2.5D0 ! Margine a sinistra
      Scala1 = Scala + am1 + am2
      am4 = Scala1 - Scala - am3
C
      LunTit = lung( TitWin(i_wind),80 )
      vcm_id = uis$create_color_map( vcm_size )
      kb_id = uis$create_kb( 'sys$workstation' )
      vd_id  = uis$create_display( Xmin-am3, Ymin-am1, 
     >(Xmax+am4-Xmin+am3)*297.D0/210.D0+Xmin-am3,Ymax+am2,Wid,H1,vcm_id)
C
      wd_id = uis$create_window( vd_id, 'sys$workstation', 
     >                    TitWin(i_wind)(1:LunTit)//'-'//NumFil )
      call uis$enable_viewport_kb ( kb_id, wd_id )
      call uis$set_colors ( vd_id, 0, 8, R_vett, G_vett, B_vett )
      do 410 i = 1, 8
  410  call uis$set_writing_index ( vd_id, 0, i, i )
C
      call uis$plot ( vd_id, 0, Xmin, Ymin, Xmax, Ymin )
      call uis$plot ( vd_id, 0, Xmax, Ymax, Xmax, Ymin )
      call uis$plot ( vd_id, 0, Xmax, Ymax, Xmin, Ymax )
      call uis$plot ( vd_id, 0, Xmin, Ymin, Xmin, Ymax )
C
C --> Graduazione dell'asse X :
C
      WiCaWC = ( WidCar / Wid ) * Scala
      n_grad = 9
      if ( WidCar * 4 * 8 .gt. 0.75D0 * Wid ) n_grad = 4
      if ( WidCar * 4 * 4 .lt. Wid ) then
       X_grad = Scala / ( n_grad + 1 )
       Xx = max( abs( Xmax ), abs( Xmin ) )
       Fatt = mantis( Xx ) / Xx
       Grad_X = Gradua * Scala
       do 420 i_grad = 1, n_grad
        Xpl = Xmin + i_grad * X_grad
        call uis$plot ( vd_id, 0, Xpl, Ymin, Xpl, Ymin-Grad_X/2 )
        call uis$set_aligned_position(vd_id,0,Xpl-3*WiCaWC,Ymin-Grad_X )
        write( GraTxt, '( f5.2 )' ) Fatt * Xpl
        call uis$text ( vd_id, 0, GraTxt )
  420  continue
      end if
C
C --> Graduazione dell'asse Y ( ai lati della finestra ) :
C
      AlCaWC = ( AltCar / Alt ) * Scala 
      Y_grad = Scala / 10.D0
      Xx = max( abs( Ymax ), abs( Ymin ) )
      Fatt = mantis( Xx ) / Xx
      Grad_Y = Gradua * Scala / 2.D0
      do 430 i_grad = 1, 9
       Ypl = Ymin + i_grad * Y_grad
       call uis$plot ( vd_id, 0, Xmin, Ypl, Xmin+Grad_Y, Ypl )
       call uis$plot ( vd_id, 0, Xmax, Ypl, Xmax-Grad_Y, Ypl )
       write( GraTxt, '( f5.2 )' ) Fatt * Ypl 
       call uis$text ( vd_id, 0, GraTxt, Xmin-12*WiCaWC,Ypl+.7D0*AlCaWC)
  430 continue
C
      call uis$set_aligned_position ( vd_id, 0, Xmax+2.D0*XYmarg, Ymax)
C
      call uis$text ( vd_id, 0, ascis )
      YY = Ymax - AlCAWC
      call uis$set_aligned_position ( vd_id, 0, Xmax+2.D0*XYmarg, YY )
C
      do 400 i_cur = 1, ncurve
       i_cur1 = 2 * ( i_cur - 1 ) + 1
       in_cv1 = ip + 2 + incr + i_cur1
       in_cv2 = in_cv1 + 1
       nx = LinLis( in_cv1 )
       ny = LinLis( in_cv2 )
       kx = InLis( nx, listG, LunLiG, NumVar, NumCur )
       ky = InLis( ny, listG, LunLiG, NumVar, NumCur )
       icolor = mod( i_cur, 8 ) + 1
       X1 = TabGr ( max(2,iPun), kx, ijTG ) 
       Y1 = TabGr ( max(2,iPun), ky, ijTG )
       i_tac1 = 0
       KolCer = 5
       do 500 j = max(3,iPun+1), nPun, max(1,kPunti)
        t  = TabGr ( j, k_asc, k_ )
        X2 = TabGr ( j, kx, ijTG )
        Y2 = TabGr ( j, ky, ijTG )
        call uis$plot   ( vd_id, icolor, X1, Y1, X2, Y2 )
        i_tac2 = nint( t / T_tacc )
        if ( i_tac2 .gt. i_tac1 ) then
         KolCer = ColCer(KolCer)
         if ( i_tac1 .eq. 0 )
     >   call uis$circle ( vd_id, KolCer, X2, Y2, Grad_X/6.D0)
         call uis$circle ( vd_id, KolCer, X2, Y2, Grad_X/9.D0)
         call uis$circle ( vd_id, KolCer, X2, Y2, Grad_X/13.D0)
         call uis$circle ( vd_id, KolCer, X2, Y2, Grad_X/18.D0)
        end if
        if ( j .eq. nPun ) then
         call uis$circle ( vd_id, KolCer, X2, Y2, Grad_X/6.D0)
         call uis$circle ( vd_id, KolCer, X2, Y2, Grad_X/9.D0)
         call uis$circle ( vd_id, KolCer, X2, Y2, Grad_X/13.D0)
         call uis$circle ( vd_id, KolCer, X2, Y2, Grad_X/18.D0)
        end if
        X1 = X2
        Y1 = Y2
        i_tac1 = i_tac2
  500  continue
C
       if ( nx .gt. 0 ) then
        scritx = DesFun( nx)
       else
        scritx = DesVar(-nx)
       end if
       if ( ny .gt. 0 ) then
        scrity = DesFun( ny)
       else
        scrity = DesVar(-ny)
       end if
       scritt = scritx(1:lung(scritx,70)) // ' - ' // scrity
C
       call uis$text ( vd_id, icolor, scritt )
       YY = YY - AlCaWC
       call uis$set_aligned_position ( vd_id, 0, Xmax+2.D0*XYmarg, YY)
C
  400 continue
C
      write( scritt, '( 70a )' ) ( ' ', i = 1, 70 )
      write( scritt, '( 70a )' ) ( '-', i = 1, LunTit+5 )
      YY = YY - AlCaWC
      call uis$set_aligned_position ( vd_id, 0, Xmax+2.D0*XYmarg, YY)
      call uis$text ( vd_id, 0, ' '//scritt )
      YY = YY - AlCaWC
      call uis$set_aligned_position ( vd_id, 0, Xmax+2.D0*XYmarg, YY)
      call uis$text ( vd_id, 0, ' '//TitWin(i_wind)(1:LunTit) 
     >                          //'-'//NumFil )
      YY = YY - AlCaWC
      call uis$set_aligned_position ( vd_id, 0, Xmax+2.D0*XYmarg, YY)
      call uis$text ( vd_id, 0, ' '//scritt )
      YY = YY - AlCaWC
      call uis$set_aligned_position ( vd_id, 0, Xmax+2.D0*XYmarg, YY)
      call uis$text ( vd_id, 0, ' '//data//' '//ora )
      write( scritt, '( 70a )' ) ( ' ', i = 1, 70 )
      write( scritt, '( 70a )' ) ( '-', i = 1, 19 )
      YY = YY - AlCaWC
      call uis$set_aligned_position ( vd_id, 0, Xmax+2.D0*XYmarg, YY)
      call uis$text ( vd_id, 0, ' '//scritt )
C
      write( Esten, '( a, i2.2 )' ) '.', i_wind
      call hcuis$write_display ( vd_id, 
     >  FilRis(1:Lung(FilRis,LuNoFi))//Esten )
      call uis$shrink_to_icon ( wd_id )
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine GeGr_r( listG , LinLis, TabGr, DesFun, DesVar, nPunti, 
     >                  NumPun, NumVar, NumCur, LiLiDi, FilRis, LunLiG, 
     >                  Wid,Alt,FinTem, FinFre, FreqMx, data, ora, 
     >                  NewPar, I_rip, J_rip, nRipMx, LuNoFi, TemIni,
     >                  FreIni, kPunti, iAbbin, Msurat, DscImp )
      implicit double precision ( a-h, o-z )
      double precision mantis
C
      include 'sys$library:uisentry'
      include 'sys$library:uisusrdef'
      integer vd_id, wd_id, vcm_id, vcm_size
      real R_vett(8), G_vett(8), B_vett(8), Wid, Alt
C
      data R_vett / 1.0, 0.0, 0.0 , 0.0, 1.0 , 0.0, 0.84, 0.2 /
      data G_vett / 1.0, 0.0, 0.0 , 0.5, 0.0 , 0.5, 0.0 , 0.5 /
      data B_vett / 1.0, 0.0, 1.0 , 0.2, 0.0 , 0.5, 0.88, 0.0 /
C     
      data vcm_size / 9 /, Dmarg / 20.D0 /
      data AltCar / 0.5D0 /, WidCar / 0.25D0 /, Gradua / 0.03D0 /
C
      dimension LinLis(LiLiDi), listG(NumVar+NumCur)
      dimension iAbbin(NumCur)
      dimension TabGr(NumPun,0:NumVar+NumCur,0:nRipMx)
      character*50 DesFun(NumCur), DesVar(NumVar), ScaNum*10
      character scritt*70, ascis*80, XmaxA*10, XminA*10, GraTxt*5
      character TitFin*80, FilRis*(*), Esten*3, data*10, ora*8
      character Numero*4, sino*1, DscImp(NumCur)*100
      logical crossX, prt, seFFT, GraLog, BasLin, NewPar, Msurat, CalCer
      common /Import/ NImp, iAscIm, Iprev
C
      if ( I_rip .eq. 1 .and. J_rip .eq. 1 ) return
      if ( NewPar ) then
        read ( 5, * ) 
        read ( 5, * ) 
        do k = 1, 8
          read ( 5, *, end = 10 ) R_vett(k), G_vett(k), B_vett(k)
        end do
   10   continue
        close ( unit = 5 )
        NewPar = .false.
      end if
C
      vcm_id = uis$create_color_map( vcm_size )
C
      if ( LunLiG .eq. 0 ) return
      ip = 1
  100 continue
C
       iOpzGr = LinLis(ip+1)
       if ( iOpzGr .eq. 5 ) goto 600
C
C        call cine_r( listG , LinLis, TabGr, DesFun, DesVar, nPunti, 
C     >               NumPun, NumVar, NumCur, LiLiDi, FilRis, LunLiG, 
C     >               TitFin, NumWin, Wid,Alt,FinTem, FinFre, FreqMx, 
C     >               data, ora, i_wind, ip, R_vett, G_vett, B_vett, 
C     >               AltCar, WidCar, Gradua, I_rip, J_rip )
C
       ncurve = LinLis(ip) - ip - 3
       n_asc  = LinLis(ip+2)
       seFFT = .false.
       if ( iOpzGr.ne.1.and.iOpzGr.ne.-1.and.n_asc.eq.0) seFFT = .true.
       BasLin = .false.
       if ( iOpzGr .eq. 2 .or. iOpzGr .eq. 4 ) BasLin = .true.
       GraLog = .false.
       if ( iOpzGr .eq. 0 .or. iOpzGr .eq. 2 ) GraLog = .true.
C
       k_asc = InLis( n_asc, listG, LunLiG, NumVar, NumCur )
       do i_ = 1, I_rip
       do j_ = 1, J_rip
         ijTG = N_TG ( I_rip, J_rip, i_, j_ )
         k_ = ijTG
         if ( k_asc .eq. 0 ) k_ = 0
         if ( i_.eq.1 .and. j_.eq.1 ) then
           Xmin = TabGr ( 1, k_asc, k_ )
           Xmax = Xmin
         end if
         do k = 2, nPunti
           if ( TabGr(k,k_asc,k_) .gt. Xmax ) Xmax=TabGr(k,k_asc,k_) 
           if ( TabGr(k,k_asc,k_) .lt. Xmin ) Xmin=TabGr(k,k_asc,k_) 
         end do
       end do
       end do
C
       nPun = nPunti - 1
C
       if ( seFFT ) then
        DFreq = 1.D0 / ( Xmax - Xmin )
        Xmax = min( FreqMx, FinFre )
        Xmin = max( 0.D00, FreIni )
        iPun = max( 2, int( Xmin/DFreq ) )
        nPun = int( (Xmax-Xmin) / DFreq )
       else if ( k_asc .eq. 0 .and. 
     >           (FinTem.lt.Xmax.or.TemIni.gt.Xmin) ) then
        iPun = 2
        nPun = 2
        do while ( TabGr(nPun,0,0) .lt. TemIni )
         iPun = iPun + 1
         nPun = nPun + 1
        end do
        Xmax = TabGr(nPun,0,0) 
        Xmin = Xmax
        do while ( TabGr(nPun,0,0) .lt. FinTem .and.
     >             nPun .le. nPunti-1 )
         if ( TabGr(nPun,0,0) .gt. Xmax ) Xmax = TabGr(nPun,0,0) 
         if ( TabGr(nPun,0,0) .lt. Xmin ) Xmin = TabGr(nPun,0,0) 
         nPun = nPun + 1
        end do
       end if
C
       if ( Msurat ) then
         do iPun = 1, NImp
           if ( TabGr(iPun,iAscIm,0).gt.Xmax ) Xmax=TabGr(iPun,iAscIm,0) 
           if ( TabGr(iPun,iAscIm,0).lt.Xmin ) Xmin=TabGr(iPun,iAscIm,0) 
         end do
       end if
C
       Xmarg = ( Xmax - Xmin ) / Dmarg
       Raggio = Gradua * ( Xmax - Xmin )
       if ( Msurat ) then
         Xmax = Xmax + Raggio 
         Xmin = Xmin - Raggio 
       end if
       write( XmaxA, '( 1PD10.3 )' ) Xmax
       write( XminA, '( 1PD10.3 )' ) Xmin
C
       if ( n_asc .eq. 0 ) then
         if ( seFFT ) then
           ascis = ' In ascissa le frequenze; da '//XminA//' a '//XmaxA
     >         //' Hz'
         else
           ascis = ' In ascissa il tempo; da '//XminA//' a '//XmaxA
     >         //' sec'
         end if
       else  
         if ( n_asc .gt. 0 ) then
           scritt = DesFun( n_asc)
         else
           scritt = DesVar(-n_asc)
         end if
         ascis = ' In ascissa : '//scritt(1:lung(scritt,70)) 
     >          //'; da '//XminA//' a '//XmaxA
       end if
C
       do i = ip + 2 + 1, ip + 2 + ncurve
C
         Amax = 0.D0
         Amin = 0.D0
         CurMax = 0.D0
         CurMin = 0.D0
         crossX = .FALSE.
C
         CalCer = .false.
C
         ind = LinLis(i)
         IndCur = InLis( ind, listG, LunLiG, NumVar, NumCur )
         if ( Msurat .and. ind.gt.0 ) then
           IndImp = iAbbin( ind )
         else
           IndImp = 0
         end if
C
         do i_ = 1, I_rip
         do j_ = 1, J_rip
C
           ijTG = N_TG ( I_rip, J_rip, i_, j_ )
C
           if ( I_rip .eq. 1 .or. J_rip .eq. 1 ) then
             write(Numero, '(i2.2,a2)' ) max( i_, j_ ), '  '
           else
             write(Numero, '(2i2.2)' ) i_, j_
           end if
C
           do j = max(2,iPun), nPun
             Aji = TabGr(j,IndCur,ijTG)
             if (Aji .gt. CurMax) CurMax = Aji
             if (Aji .lt. CurMin) CurMin = Aji
           end do
           if ( CurMax .gt. Amax ) Amax = CurMax
           if ( CurMin .lt. Amin ) Amin = CurMin
           CurMax = 0.D0
           CurMin = 0.D0
C
         end do ! ( j_ )
         end do ! ( i_ )
C
         if ( Msurat .and. IndImp.gt.0 ) then
           do iPun = 1, NImp
             Aji = TabGr( iPun, IndImp, 0 )
             if (Aji .gt. CurMax) CurMax = Aji
             if (Aji .lt. CurMin) CurMin = Aji
           end do
           if ( CurMax .gt. Amax ) Amax = CurMax
           if ( CurMin .lt. Amin ) Amin = CurMin
           Amarg = (Amax-Amin) / 30.D0
           Amax = Amax + Amarg
           Amin = Amin - Amarg
         end if
C
         crossX = Amax * Amin .lt. 0.D0
C
         scale = 0.D0
         elacs = 0.D0
         if (scale .lt. Amax) then
           scale = Amax
           elacs = Amin
         end if
         if (scale .lt. abs(Amin) ) then
           scale = abs(Amin)
           elacs = Amax
         end if
         if(Amax.le.0.D0.or.(crossX.and.abs(Amin).gt.Amax)) scale=-scale
         write( ScaNum, '( 1PD10.3 )' ) scale
C
         if ( scale .eq. 0.D0 ) then
           write( *, * ) ' Dati nulli '
           goto 600
         end if
C
         RiText = float( ncurve ) / 8.D0
         H1 = Alt - AltCar * ( 1.D0 - RiText )
         Ymin = 0.D0
         in_bas = 0
         if ( crossX ) then
           in_bas = abs(int( 10.D0*elacs/scale )) + 1
           Ymin = -0.1D0 * float( in_bas )
         end if
         if ( ind .gt. 0 ) then
           TitFin = FilRis(1:Lung(FilRis,LuNoFi)) // DesFun( ind)
         else
           TitFin = FilRis(1:Lung(FilRis,LuNoFi)) // DesVar(-ind)
         end if
C
         LunTit = lung( TitFin,80 )
C
         write (*,'('' Si vuole disegnare la finestra '', a, 
     >   '' (S=D/N) ? '', $ )' ) TitFin(Lung(FilRis,LuNoFi)+1:LunTit)
         read(*,'( a )' ) sino
         if( sino.eq.'N' .or. sino.eq.'n' ) goto 600
C
         vd_id = uis$create_display( Xmin-2*Xmarg, Ymin - 1.D0,
     >                       Xmax+Xmarg,  1.05D0, Wid, Alt, vcm_id )
         kb_id = uis$create_kb( 'sys$workstation' )
         wd_id = uis$create_window( vd_id, 'sys$workstation', 
     >           TitFin(1:LunTit) )
         call uis$enable_viewport_kb ( kb_id, wd_id )
         call uis$set_colors ( vd_id, 0, 8, R_vett, G_vett, B_vett )
         do k = 1, 8
           call uis$set_writing_index ( vd_id, 0, k, k )
         end do
C
         call uis$plot ( vd_id, 0, Xmin, Ymin, Xmax, Ymin )
         call uis$plot ( vd_id, 0, Xmax,  1.D0, Xmax, Ymin )
         call uis$plot ( vd_id, 0, Xmax,  1.D0, Xmin,  1.D0 )
         call uis$plot ( vd_id, 0, Xmin, Ymin, Xmin,   1.D0 )
         if (crossX) call uis$plot ( vd_id, 0, Xmin, 0.D0, Xmax, 0.D0 )
C
         WiCaWC = ( WidCar / Wid ) * ( Xmax - Xmin )
         n_grad = 9
         if ( WidCar * 4 * 8 .gt. 0.75D0 * Wid ) n_grad = 4
         if ( WidCar * 4 * 4 .lt. Wid ) then
           X_grad = ( Xmax - Xmin ) / ( n_grad + 1 )
           Xx = max( abs( Xmax ), abs( Xmin ) )
           Fatt = mantis( Xx ) / Xx
           do i_grad = 1, n_grad
             Xpl = Xmin + i_grad * X_grad
             call uis$plot ( vd_id, 0, Xpl, Ymin, Xpl, Ymin-Gradua )
             call uis$set_aligned_position(vd_id,0,Xpl-3*WiCaWC,
     >                                     Ymin-Gradua)
             write( GraTxt, '( f5.2 )' ) Fatt * Xpl
             call uis$text ( vd_id, 0, GraTxt )
           end do
         end if
C
         ScaOrd = 1.D0
         ScaOrd = mantis( abs( scale ) )
         AlCaWC = (AltCar/Alt) * (1.D0-Ymin) * (1.D0+0.3D0*abs(Ymin))
         if ( crossX ) then
           i_iniz = - in_bas
         else
           i_iniz = 1
         end if
         do i_grad = i_iniz, 9
           Ypl = float( i_grad ) / 10.D0
           Grad_Y = Gradua * ( Xmax - Xmin ) / 3.D0
           call uis$plot ( vd_id, 0, Xmin, Ypl, Xmin+Grad_Y, Ypl )
           call uis$plot ( vd_id, 0, Xmax, Ypl, Xmax-Grad_Y, Ypl )
           write( GraTxt, '( f5.2 )' ) Ypl * ScaOrd
           call uis$text ( vd_id, 0, GraTxt, Xmin-8*WiCaWC, Ypl+AlCaWC )
         end do
C
         call uis$set_aligned_position ( vd_id, 0, 0.D0, Ymin-Gradua )
C
         call uis$text ( vd_id, 0, ' ' )
         call uis$new_text_line ( vd_id, 0 )
         call uis$text ( vd_id, 0, ' ' )
         call uis$new_text_line ( vd_id, 0 )
         call uis$text ( vd_id, 0, ascis )
         call uis$new_text_line ( vd_id, 0 )
         if ( seFFT .and. GraLog ) then
           call uis$text ( vd_id, 0, 
     >                 ' FFT - Scala delle ordinate logaritmica' )
           call uis$new_text_line ( vd_id, 0 )
         else if ( seFFT .and. .not. GraLog ) then
           call uis$text ( vd_id, 0,
     >                 ' FFT - Scala delle ordinate lineare' )
           call uis$new_text_line ( vd_id, 0 )
         end if
         if ( seFFT .and. BasLin ) then
           call uis$text ( vd_id, 0, 
     >                 '       Correzione della linea di base' )
           call uis$new_text_line ( vd_id, 0 )
         end if
         call uis$text ( vd_id, 0, ' ' )
         call uis$new_text_line ( vd_id, 0 )
C
         do i_ = 1, I_rip
         do j_ = 1, J_rip
C
           ijTG = N_TG ( I_rip, J_rip, i_, j_ )
C
           if ( I_rip .eq. 1 .or. J_rip .eq. 1 ) then
             write(Numero, '(i2.2,a2)' ) max( i_, j_ ), '  '
           else
             write(Numero, '(2i2.2)' ) i_, j_
           end if
C
           icolor = mod( ijTG, 8 ) + 1
           if ( seFFT ) then
             X1 = Xmin
           else
             k_ = ijTG
             if ( k_asc .eq. 0 ) k_ = 0
             X1 = TabGr ( max(2,iPun), k_asc, k_ )
           end if
           Aji = TabGr( max(2,iPun), IndCur, ijTG )
           Y1 = Aji / scale
           do j = max(3,iPun+1), nPun, max(1,kPunti)
             if ( seFFT ) then
               X2 = (j-1) * Dfreq
             else
               k_ = ijTG
               if ( k_asc .eq. 0 ) k_ = 0
               X2 = TabGr( j, k_asc, k_ )
             end if
             Aji = TabGr( j, IndCur, ijTG )
             Y2 = Aji / scale
C
             call uis$plot ( vd_id, icolor, X1, Y1, X2, Y2 )
             X1 = X2
             Y1 = Y2
           end do
C
           scritt = ' Identificazione : ' // Numero
           if ( i_.eq.1 .and. j_.eq.1 ) 
     >               scritt = scritt(1:25)//' Scala : '//ScaNum
C
           call uis$text ( vd_id, icolor, scritt )
           call uis$new_text_line ( vd_id, icolor )
C
         end do ! ( j_ )
         end do ! ( i_ )
C
         if ( Msurat .and. IndImp.gt.0 ) then
C
           icolor = 2
           X1 = TabGr ( 1, iAscIm, 0 )
           Aji = TabGr( 1, IndImp, 0 )
           Y1 = Aji / scale
           if ( .not. CalCer ) then
             call uis$circle ( vd_id, icolor, X1, Y1, Raggio/9.D0)
             call uis$circle ( vd_id, icolor, X1, Y1, Raggio/13.D0)
             call uis$circle ( vd_id, icolor, X1, Y1, Raggio/18.D0)
           end if
           do iPun = 2, NImp
             X2 = TabGr ( iPun, iAscIm, 0 )
             Aji = TabGr( iPun, IndImp, 0 ) 
             Y2 = Aji / scale
             if ( CalCer ) then
               call uis$plot ( vd_id, icolor, X1, Y1, X2, Y2 )
               X1 = X2
               Y1 = Y2
             else
               call uis$circle ( vd_id, icolor, X2, Y2, Raggio/9.D0)
               call uis$circle ( vd_id, icolor, X2, Y2, Raggio/13.D0)
               call uis$circle ( vd_id, icolor, X2, Y2, Raggio/18.D0)
             end if
           end do
C
           LDscMi = lung( DscImp(IndImp), 100 )
           scritt = ' '//DscImp(IndImp)(1:LDscMi)
C
           call uis$text ( vd_id, icolor, scritt )
           call uis$new_text_line ( vd_id, icolor )
C
         end if
C
         write( scritt, '( 70a )' ) ( ' ', k = 1, 70 )
         write( scritt, '( 70a )' ) ( '-', k = 1, LunTit )
         call uis$new_text_line ( vd_id, 0 )
         call uis$text ( vd_id, 0, ' '//scritt )
         call uis$new_text_line ( vd_id, 0 )
         call uis$text ( vd_id, 0, ' '//TitFin(1:LunTit) )
         call uis$new_text_line ( vd_id, 0 )
         call uis$text ( vd_id, 0, ' '//scritt )
         call uis$new_text_line ( vd_id, 0 )
         call uis$text ( vd_id, 0, ' '//data//' '//ora )
         write( scritt, '( 70a )' ) ( ' ', k = 1, 70 )
         write( scritt, '( 70a )' ) ( '-', k = 1, 19 )
         call uis$new_text_line ( vd_id, 0 )
         call uis$text ( vd_id, 0, ' '//scritt )
C
         write( Esten, '( a, i2.2 )' ) '.', i
         call hcuis$write_display ( vd_id, 
     >     FilRis(1:Lung(FilRis,LuNoFi))//Esten )
         call uis$shrink_to_icon ( wd_id )
  600    continue
C
       end do ! ( i )
C
       ip = LinLis ( ip )
       if ( LinLis(ip) .eq. 0 ) return
      goto 100
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      integer function N_TG ( I_rip, J_rip, i_, j_ )
C
      if ( I_rip .eq. 1 .or.  J_rip .eq. 1 ) N_TG = max( i_, j_ )       
      if ( I_rip .gt. 1 .and. J_rip .gt .1 ) N_TG = J_rip * (i_-1) + j_
      return
      end      
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      double precision function mantis( x1 )
C
      double precision x, x1
C
      x = x1
      if ( abs( x ) .ge. 10.D0 ) then
  100  x = x / 10.D0
       if ( abs( x ) .ge. 10.D0 ) goto 100
      else if ( abs( x ) .lt. 1.D0 ) then
  200  x = x * 10.D0
       if ( abs( x ) .lt. 1.D0 ) goto 200
      end if
      mantis = sign( min( abs( x ), 9.95D0 ), x )
      return
      end

C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      integer function InLis ( ivf, listG, LunLiG, NumVar, NumCur )
C
      dimension listG ( NumVar+NumCur )
C
      InLis = 0
      do 100 i = 1, LunLiG
       if ( ivf .eq. listG(i) ) InLis = i
  100 continue
      return
      end    
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine OpzGra ( TemIni, FreIni, FinTem, FinFre, FreqMx, T_fi,
     >                    kPunti, U_Mscl, W_D, A_D, W, H, askU_M )
C
      implicit double precision ( a-h, o-z )
      character*1 sino
      logical U_Mscl, askU_M
      real W_D, A_D, W, H
C
  100 continue
      write(*, *) ' '
      write(*,
     > '( '' Tempo finale e frequenza massima di plottaggio :'')') 
      write(*,'( ''      Tempo finale = '', 1PD10.3 )' ) T_fi
      write(*,'( '' Frequenza massima = '', 1PD10.3 )' ) FreqMx
      write(*,
     > '( '' Tempo iniziale e frequenza minima sono nulli.'')') 
      write(*,'( '' Li si vuole cambiare ? ( S / N=D / A ) '', $ )')
      read(*,'( a )' ) sino
C
      FinTem = T_fi
      FinFre = FreqMx
      if ( sino .eq. 'S' .or. sino .eq. 's' ) then 
       write(*, *) ' '
       write(*,'(''    Tempo iniziale ? '', $ )')
       read(*,*) TemIni
       write(*,'(''  Frequenza minima ? '', $ )')
       read(*,*) FreIni
       write(*, *) ' '
       write(*,'(''      Tempo finale ? ( se 0 non si cambia ) '', $ )')
       read(*,*) FinTem
       if ( FinTem .le. 0.D0 ) FinTem = T_fi
       write(*,'('' Frequenza massima ? ( se 0 non si cambia ) '', $ )')
       read(*,*) FinFre
       if ( FinFre .le. 0.D0 ) FinFre = FreqMx
      else if ( sino .eq. 'A' .or. sino .eq. 'a' ) then 
       write(*,*) ' Se si vuole si puo" far disegnare solo parte delle'
       write(*,*) ' curve, da un valore iniziale a uno finale dei tempi'
       write(*,*) ' o delle frequenze.'
       goto 100
      end if
C
  200 continue
      write(*, *) ' '
      write(*,
     > '( '' Tracciare le curve con tutti i punti ? (S=D/N/A) '', $ )')
      read(*,'( a )' ) sino
C
      kPunti = 1
      if ( sino .eq. 'N' .or. sino .eq. 'n' ) then 
       write(*, *) ' '
       write(*,'(''  Ogni quanti punti ? '', $ )')
       read(*,*) kPunti
      else if ( sino .eq. 'A' .or. sino .eq. 'a' ) then 
       write(*,*) ' Se si vuole si possono tracciare le curve usando un'
       write(*,*) ' punto ogni due, o tre, ecc., in modo da limitare'
       write(*,*) ' le dimensioni dei file grafici.'
       goto 200
      end if
C
      U_Mscl = .false.
      if ( askU_M ) then
  300 continue
      write(*, *) ' '
      write(*,
     > '( '' Cambiare la prescrizione dei fattori di scala '')')
      write(*,
     > '( '' (da unico a multiplo e viceversa) (S/N=D/A) ? '', $ )')
      read(*,'( a )' ) sino
C
      if ( sino .eq. 'S' .or. sino .eq. 's' ) then 
       U_Mscl = .true.
      else if ( sino .eq. 'A' .or. sino .eq. 'a' ) then 
       write(*,*) ' Si puo" ottenere che i grafici che erano stati'
       write(*,*) ' predisposti con un fattore di scala unico siano'
       write(*,*) ' invece tracciati con fattori multipli, e viceversa.'
       goto 300
      end if
      end if
C
      W = W_D
      H = A_D
C
      write(*, *) ' '
      write(*,'( '' Le dimensioni predisposte delle finestre sono :'')') 
      write(*,'( '' Larghezza ='',i2,'' cm, altezza ='',i2,'' cm .'')') 
     >      ifix( W_D ), ifix( A_D )
      write(*,'( '' Le si vuole cambiare ? ( S / N=D ) '', $ )')
      read(*,'( a )' ) sino
C
      if ( sino .eq. 'S' .or. sino .eq. 's' ) then 
       write(*, *) ' '
       write( *, '( '' Larghezza = '' , $ )' )
       read ( *, * ) W
       write( *, '( ''   Altezza = '' , $ )' )
       read ( *, * ) H
      end if
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine WinSav( listG , LinLis, TabGr, DesFun, DesVar, nPunti, 
     >                  NumPun, NumVar, NumCur, LiLiDi, FilRs1, LunLiG, 
     >                  TitWin, NumWin, DtStam, T_fi, data, ora, 
     >                  nRipMx, ijTG )
      implicit double precision ( a-h, o-z )
C
      dimension LinLis(LiLiDi), listG(NumVar+NumCur)
      dimension TabGr(NumPun,0:NumVar+NumCur,0:nRipMx)
      character*50 DesFun(NumCur), DesVar(NumVar), TitWin(NumWin)*80
      character FilRs1*(*), data*10, ora*8
C
      open ( unit = 10, file = FilRs1//'.fin', status = 'new',
     >       form = 'unformatted' )
C
      write( 10 ) LunLiG, nPunti
      write( 10 ) DtStam, T_fi
      write( 10 ) data, ora
      write( 10 ) ( DesVar(i), i = 1, NumVar )
      write( 10 ) ( DesFun(i), i = 1, NumCur )
      write( 10 ) ( LinLis(i), i = 1, LiLiDi )
      write( 10 ) ( TitWin(i), i = 1, NumWin )
      write( 10 ) ( ListG(i),  i = 1, LunLiG )
      write( 10 ) ( ( TabGr(i,j,ijTG), i = 1, nPunti ), j = 1,LunLiG )
      write( 10 )   ( TabGr(i,0,0   ), i = 1, nPunti )
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine WinRet( listG , LinLis, TabGr, DesFun, DesVar, nPunti, 
     >                  NumPun, NumVar, NumCur, LiLiDi, FilRs1, LunLiG, 
     >                  TitWin, NumWin, DtStam, T_fi, data, ora, 
     >                  nRipMx, ijTG )
      implicit double precision ( a-h, o-z )
C
      dimension LinLis(LiLiDi), listG(NumVar+NumCur)
      dimension TabGr(NumPun,0:NumVar+NumCur,0:nRipMx)
      character*50 DesFun(NumCur), DesVar(NumVar), TitWin(NumWin)*80
      character FilRs1*(*), data*10, ora*8
C
      open ( unit = 10, file = FilRs1//'.fin', status = 'old',
     >       form = 'unformatted' )
C
      read( 10 ) LunLiG, nPunti
      read( 10 ) DtStam, T_fi
      read( 10 ) data, ora
      read( 10 ) ( DesVar(i), i = 1, NumVar )
      read( 10 ) ( DesFun(i), i = 1, NumCur )
      read( 10 ) ( LinLis(i), i = 1, LiLiDi )
      read( 10 ) ( TitWin(i), i = 1, NumWin )
      read( 10 ) ( ListG(i),  i = 1, LunLiG )
      read( 10 ) ( ( TabGr(i,j,ijTG), i = 1, nPunti ), j = 1, LunLiG )
      read( 10 )   ( TabGr(i,0,0   ), i = 1, nPunti )
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine VlPart (SMPLX, DelX, MaxErr, DesVar, restart, MxIter, 
     >                   nVar, NP, TolCen )
C
      implicit double precision ( a-h, o-z )
      double precision SMPLX(NP,NP), DelX(NP), MaxErr(NP)
      character DesVar(NP-1)*50, tit*50, UnMis*15, Numer*2
      logical restart, prt
C
      if ( .not. restart ) then
         type 31
   31    format( ' Numero massimo di iterazioni ( D = 100 ) ? ', $ )
         accept *, MxIter
         if ( MxIter .le. 0 ) MxIter = 100
         write( 2, 32 ) MxIter
   32    format( ' ', i4, ' iterazioni al massimo ')
      else
         read ( 2, '( bn, i5 )' ) MxIter
      end if
C
      write(1,*) ' '
      write(1,33) MxIter
      write(1,*) ' '
   33 format( ' Numero massimo di iterazioni : ', i4 )
      write(1,*) ' '
C
      UnMis = ' '
      do i = 1, nVar
       MaxErr(i) = TolCen
      end do
C
      return
C
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine SumRes ( TBL, nRig, nCol, iPun, RHS, f, nEqn, nVar, 
     >                    NumCur, x, dumm, n, t, T_in, T_fi, i_, j_
     >                    , TabGr, NumPun, NumVar, nRipMx )
C
      implicit double precision ( a-h, o-z )
      double precision TBL(nRig*nCol), RHS(nEqn), f(NumCur),
     >                 x(n), dumm(n)
      dimension TabGr(NumPun,0:NumVar+NumCur,0:nRipMx)
      logical risolvi
C
      k = nRig * ( iPun - 1 ) + 1
C
      call TerNot ( x, dumm, f, n, NumCur,t,0.D0,0.D0,T_in,T_fi,.false., 
     >              TBL(k), RHS, nEqn, nVar, .false., risolvi, i_, j_ 
     >              , TabGr, NumPun, NumVar, nRipMx )
C
      ErrFun = 0.D0
      do 100 j = 1, nEqn
  100  ErrFun = ErrFun + RHS(j)**2
      TBL( k + nVar ) = ErrFun 
C
      return
C
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine InSmpl ( SMPLX, P, Q, DelX, RHS, f, n, NP, nEqn, nVar, 
     >                   nEqnMx, NumCur, x, dumm, t, T_in, T_fi,
     >                   TabGr, NumPun, NumVar, nRipMx )
C
      implicit double precision ( a-h, o-z )
      double precision P(NP), Q(NP), DelX(NP), SMPLX(NP,NP), 
     >                 RHS(nEqnMx), f(NumCur), x(n), dumm(n)
      dimension TabGr(NumPun,0:NumVar+NumCur,0:nRipMx )

C
      parameter ( rad2 = 1.414214D0 )
C
      do i = 1, nVar
         P(i) = DelX(i) * (sqrt(float(nVar+1))+nVar-1) / (nVar*rad2)
         Q(i) = DelX(i) * (sqrt(float(nVar+1))     -1) / (nVar*rad2)
      end do
C
      call SumRes ( SMPLX, NP, NP, 1, RHS, f, nEqn, nVar, 
     >              NumCur, x, dumm, n, t, T_in, T_fi, 1, 1
     >              , TabGr, NumPun, NumVar, nRipMx )
      do i = 2, nVar + 1
         do j = 1, nVar
            SMPLX(j,i) = SMPLX(j,1) + Q(j)
         end do
         SMPLX(i-1,i) = SMPLX(i-1,1) + P(i-1)
         call SumRes (SMPLX, NP, NP, i, RHS, f, nEqn, nVar, 
     >                NumCur, x, dumm, n, t, T_in, T_fi, 1, 1
     >                , TabGr, NumPun, NumVar, nRipMx )
      end do
C
      return
C
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C 
      subroutine iteraz ( SMPLX, NEXT, BarCen, H, L, MEAN, Error, P, Q, 
     >                    DelX, MaxErr, NP, MxIter, nIter, nEqn, nVar,
     >                    nEqnMx, nVarMx, NumCur, f, RHS, x, dumm, 
     >                    n, t, T_in, T_fi, DelXNu, proseg, i_, j_
     >                    , TabGr, NumPun, NumVar, nRipMx )
C
      parameter ( alfa = 1.0D0 )  ! coefficiente di riflessione, > 0 
      parameter ( beta = 0.5D0 )  ! coefficiente di contrazione, da 0 a 1
      parameter ( gamma = 2.0D0)  ! coefficiente di espansione, > 1
C
      implicit double precision ( a-h, o-z )
      double precision SMPLX(NP,NP), NEXT(NP), BarCen(NP), MEAN(NP),
     >                 Error(NP), P(NP), Q(NP), DelX(NP), MaxErr(NP)
      double precision f(NumCur), RHS(nEqnMx), x(n), dumm(n)
      dimension TabGr(NumPun,0:NumVar+NumCur,0:nRipMx)
      integer H(NP), L(NP)
      logical fatto, PrVolt / .true. /, proseg
      save PrVolt
C
      m = nVar
      mp1 = m + 1
C
      if ( .not. PrVolt .or. proseg ) then
       do j = 1, m
        DelX(j) = DelXNu * SMPLX(j,1)
       end do
       call InSmpl ( SMPLX, P, Q, DelX, RHS, f, n, NP, nEqn, nVar, 
     >               nEqnMx, NumCur, x, dumm, t, T_in, T_fi,
     >               TabGr, NumPun, NumVar, nRipMx )
      end if
      PrVolt = .false.
C
      do i = 1, mp1
         L(i) = 1
         H(i) = 1
      end do
      call ORDER (SMPLX, L, H, mp1, NP)
C
      nIter = 0
      fatto = .false.
C   
      do while (.not.fatto .and. nIter.le.MxIter)
C
       nIter = nIter + 1
C
       do i = 1, mp1
        BarCen(i) = 0.D0
       end do
C
       do i = 1, mp1
        if ( i .ne. H(mp1) ) then
         do j = 1, m
          BarCen(j) = BarCen(j) + SMPLX(j,i)
         end do
        end if                             ! Calcolo del baricentro
       end do                              ! del simplesso.
!
       do i = 1, m                         ! Primo tentativo di riflettere.
        BarCen(i) = BarCen(i) / m
        NEXT(i) = (1.D0+alfa) * BarCen(i) - alfa * SMPLX(i,H(mp1))
       end do 
C                                              ! Nuovo vertice = Riflessione
C                                              ! speculare del peggiore.
       call SumRes (NEXT, NP, 1, 1, RHS, f, nEqn, nVar, 
     >              NumCur, x, dumm, n, t, T_in, T_fi, i_, j_
     >              , TabGr, NumPun, NumVar, nRipMx )
!
       if ( NEXT(mp1) .le. SMPLX(mp1,L(mp1)) ) then ! Meglio del migliore ?
!
        call NEWVER (SMPLX, NEXT, H, mp1, NP) ! Accettato,
C                                            ! ed espanso.
        do i = 1, m                              
         NEXT(i) = gamma * SMPLX(i,H(mp1)) + (1.D0-gamma) * BarCen(i)
        end do
!
        call SumRes (NEXT, NP, 1, 1, RHS, f, nEqn, nVar, 
     >               NumCur, x, dumm, n, t, T_in, T_fi, i_, j_
     >               , TabGr, NumPun, NumVar, nRipMx )
C                                              ! Ancora meglio ?
        if ( NEXT(mp1) .le. SMPLX(mp1,L(mp1)) )  
     >                    call NEWVER (SMPLX, NEXT, H, mp1, NP)
!
       else                                  ! Se non e' meglio del migliore:
!
        if ( NEXT(mp1) .le. SMPLX(mp1,H(mp1)) ) then
         call NEWVER ( SMPLX, NEXT,  H, mp1, NP)
C                                              ! (meglio del peggiore)
        else                           
C                                              ! Peggio del peggiore: contrarre.
         do i = 1, m
          NEXT(i) = beta * SMPLX(i,H(mp1)) + (1.D0-beta) * BarCen(i)
         end do
         call SumRes (NEXT, NP, 1, 1, RHS, f, nEqn, nVar, 
     >                NumCur, x, dumm, n, t, T_in, T_fi, i_, j_
     >                , TabGr, NumPun, NumVar, nRipMx )
!
         if ( NEXT(mp1) .le. SMPLX(mp1,H(mp1)) ) then
          call NEWVER ( SMPLX, NEXT,  H, mp1, NP)
C                                      ! Contrazione accettata;
         else                          ! altrimenti se va ancora male
          do i=1, mp1                  ! si stringono i vertici cattivi.
           do j=1, m
            SMPLX(j,i)=beta*( SMPLX(j,i)+SMPLX(j,L(mp1)) )
           end do
           call SumRes ( SMPLX, NP, NP, i, RHS, f, nEqn, nVar, 
     >                   NumCur, x, dumm, n, t, T_in, T_fi, i_, j_
     >                   , TabGr, NumPun, NumVar, nRipMx )
          end do
         end if
C
        end if
C
       end if 
C
       call ORDER ( SMPLX, L, H, mp1, NP)
C
C--> Controllo della convergenza :
C
       fatto = .true.
       do j=1, m
        Error(j)=(SMPLX(j,H(j))-SMPLX(j,L(j)))/SMPLX(j,H(j))
        if ( abs(Error(j)) .gt. MaxErr(j) ) fatto = .false.
       end do 
C
      end do ! while
C
      call SumRes ( SMPLX, NP, NP, 1, RHS, f, nEqn, nVar, 
     >              NumCur, x, dumm, n, t, T_in, T_fi, i_, j_
     >              , TabGr, NumPun, NumVar, nRipMx )
C
      return
C
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine NEWVER ( SMPLX, NEXT, H, n, NP)
C
      double precision SMPLX(NP,NP), NEXT(NP)
C
      integer H(n)
C
      do i = 1, n
         SMPLX(i,H(n)) = NEXT(i)
      end do
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine ORDER ( SMPLX, L, H,  n, NP)
C
      double precision SMPLX(NP,NP)
C
      integer L(NP), H(NP)
C
      do j = 1, n 
         do i = 1, n
            if ( SMPLX(j,i) .lt. SMPLX(j,L(j)) ) L(j) = i
            if ( SMPLX(j,i) .gt. SMPLX(j,H(j)) ) H(j) = i
         end do
      end do
C
      return
C
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
C--> Insieme di funzioni da usare con SIMULA;
C    sono quelle definite in ADAMS .
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      double precision function STEP ( x, x0, h0, x1, h1 )
C
      implicit double precision ( a-h, o-z )
C
      if ( x .le. x0 ) then
         STEP = h0
      else if ( x .ge. x1 ) then
         STEP = h1
      else
         STEP = h0 + (h1-h0) * ( (x-x0)/(x1-x0) )**2 *
     >              ( 3.D0 - 2 * ( (x-x0)/(x1-x0) ) )
      end if
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      double precision function StpSin ( x, x0, h0, x1, h1 )
C
      implicit double precision ( a-h, o-z )
C
      parameter ( pi = 3.141592654D0 )
C
      if ( x .le. x0 ) then
         StpSin = h0
      else if ( x .ge. x1 ) then
         StpSin = h1
      else
         StpSin = (h0+h1)/2 + 
     >            ( (h1-h0)/2 ) * sin ( pi * (x-x0)/(x1-x0) -pi/2 )
      end if
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      double precision function IMPACT ( x, xdot, x1, st, e, cmax, d )
C
      implicit double precision ( a-h, o-z )
C
C       ---|/|
C       ---|/|
C       ---|/|
C       ---|/|
C       ---|/|
C +----------+-----------> x
C 0   d -->| |
C            x1
C
C--> x    : spostamento da cui dipende la forza .
C--> xdot : derivata di x.
C--> x1   : limite inferiore per x; se x < x1, la
C           forza e' positiva.
C--> st   : rigidita' dell'interazione alla superficie 
C           di confine; positiva, se l'impatto e' verso
C           sinistra, altrimenti negativa.
C--> e    : esponente della caratteristica forza-deformazione
C           al confine.
C--> cmax : massimo coefficiente di smorzamento; mai negativo.
C--> d    : penetrazione del confine alla quale si applica 
C           l'intero smorzamento; positiva, se l'impatto e' verso
C           sinistra, altrimenti negativa; mai nulla.
C
      if ( x .lt. x1 ) then
      IMPACT = st*(x1-x)**e - STEP ( x, x1-d, cmax, x1, 0.D0) * xdot
      else
      IMPACT = 0.D0
      end if
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      double precision function BISTOP( x, xdot, x1, x2, st, e, cmax, d)
C
      implicit double precision ( a-h, o-z )
C
C       ---|/|               |\|---
C       ---|/|               |\|---
C       ---|/|               |\|---
C       ---|/|               |\|---
C       ---|/|               |\|---
C +----------+---------------+--------> x
C 0   d -->| |               | |<-- d
C            x1              x2
C
C--> x    : spostamento da cui dipende la forza .
C--> xdot : derivata di x.
C--> x1   : limite inferiore per x; se x < x1, la
C           forza e' positiva.
C--> x2   : limite superiore per x; se x > x2, la
C           forza e' negativa.
C--> st   : rigidita' dell'interazione alla superficie 
C           di confine.
C--> e    : esponente della caratteristica forza-deformazione
C           al confine.
C--> cmax : massimo coefficiente di smorzamento; mai negativo.
C--> d    : penetrazione del confine alla quale si applica 
C           l'intero smorzamento; mai nulla.
C
      if ( x .lt. x1 ) then
        BISTOP =  st*(x1-x)**e - STEP ( x, x1-d, cmax, x1, 0.D0) * xdot
      else if ( x .gt. x2 ) then
        BISTOP = -st*(x-x2)**e - STEP ( x, x2, 0.D0, x2+d, cmax) * xdot
      else
        BISTOP = 0.D0
      end if
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      double precision function GOBBA ( x, x1,x2,x3,x4, h1,h2,h3 )
C
      implicit double precision ( a-h, o-z )
C
      GOBBA = STEP( x, x1,h1, x2,h2 ) - STEP( x, x3,0.D0, x4,h2-h3 )
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      double precision function TRIANG ( t, a, f, t0 )
C
      implicit double precision ( a-h, o-z )
C
      per = 1.D0 / f
      per2 = 0.5D0 * per
      t1 = MOD ( t - t0 + 0.25D0 * per, per )
      if ( t1 .le. per2 ) then
       TRIANG = a * (  1.D0 - 2.D0 * t1 / per2 )
      else
       TRIANG = a * ( -3.D0 + 2.D0 * t1 / per2 )
      end if
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      double precision function PWM ( t, a, f, permod, d )
C
      implicit double precision ( a-h, o-z )
C
C--> Impulsi di larghezza modulata
C    -----------------------------
C
C--> a     : ampiezza degli impulsi
C--> f     : frequenza
C--> permod: percentuale di modulazione;
C            se = 1/2, si ha un'onda quadra.
C--> d     : larghezza dell'intervallino di transizione
C
C------------------------------------------------------
C
      per = 1.D0 / f
      t = MOD ( t, per )
      t0 = permod * per
C
      if ( t0 .le. d ) then
         PWM = 0.D0
      else if ( t0 .ge. per-d ) then
         PWM = a
      else if ( t .ge. per-d ) then
         PWM = STEP ( t, per-d, a, per, 0.D0 )
      else
         PWM = STEP ( t, t0-d, 0.D0, t0, a )
      end if
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      double precision function POLY ( x, x0, a, n )
C
C--> Poly = a(1) + a(2) * (x-x0) + ... + a(n) * (x-x0)**(n-1)
C
      implicit double precision ( a-h, o-z )
C
      dimension a ( 0 : n )
C
      y = x - x0
      POLY = a ( n )
      do 9943 i = n, 2, -1
 9943 POLY = POLY * y + a ( i-1 )
      return 
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      double precision function CHEBY ( x, x0, a, n )
C
C--> Valuta la combinazione lineare coi coefficienti a(i) dei primi
C    n polinomi di Chebyshev, calcolati in x-x0 .
C
      implicit double precision ( a-h, o-z )
C
      dimension a ( 0 : n )
C
      t0 = 1.D0
      t1 = x - x0
      CHEBY = a(1) * t0 + a(2) * t1
C
      do 9940 i = 3, n, 2
         tn0 = 2 * (x-x0) * t1 - t0
         tn1 = 2 * (x-x0) * tn0 - t1
         if ( i+1 .le. n ) then
            CHEBY = CHEBY + a(i) * tn0 + a(i+1) * tn1
         else
            CHEBY = CHEBY + a(i) * tn0
         end if
         t0 = tn0
         t1 = tn1
 9940 continue
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      double precision function SomCos ( x, x0, w, a, n )
C
      implicit double precision ( a-h, o-z )
C
      dimension a ( 0 : n )
C
      SomCos = 0.D0
      do 9941 i = 1, n
 9941 SomCos = SomCos + a(i) * cos ( i * w * (x-x0) )
C
      return 
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      double precision function SomSin ( x, x0, w, a, n )
C
      implicit double precision ( a-h, o-z )
C
      dimension a ( 0 : n )
C
      SomSin = 0.D0
      do 9942 i = 1, n
 9942 SomSin = SomSin + a(i) * sin ( i * w * (x-x0) )
C
      return 
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      double precision function SHF ( x, x0, w, a, phi, b )
C
      implicit double precision ( a-h, o-z )
      SHF = a * sin ( w * (x-x0) - phi ) + b
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
       DY = 0.D0
      else if ( X .ge. XX(N) ) then
       Y  = YY(N)
       DY = 0.D0
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
C--> Interpolazione circolare sulle tabelle XX e YY 
      double precision XX(0:*), YY(0:*), X, Xc, Y, DY, XN
C
      Nx = ifix( real( XX(0) ) )
      Ny = ifix( real( YY(0) ) )
      N  = min( Nx, Ny )
      XN = XX(N) - XX(1)
      Xc = XX(1) + mod( X, XN )
      i = 1
   10 i = i + 1
      if ( XX(i) .gt. Xc ) then
       DY = ( YY(i) - YY(i-1) ) / ( XX(i) - XX(i-1) )
       Y  = YY(i-1) + ( Xc - XX(i-1) ) * DY
      else
       goto 10
      end if
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12........
C
      function RANDOM ( x, y, z )
C
      integer x, y, z  
C  Devono essere compresi fra 1 e 30000
C
      x = 171 * MOD ( x, 177 ) -  2 * ( x / 177 )
      if ( x .lt. 0 ) x = x + 30269
      y = 172 * MOD ( y, 176 ) - 35 * ( y / 176 )
      if ( y .lt. 0 ) y = y + 30307
      z = 170 * MOD ( z, 178 ) - 63 * ( z / 178 )
      if ( z .lt. 0 ) z = z + 30323
C
      temp = x / 30269. + y / 30307. + z / 30323.
      RANDOM = temp - INT ( temp )
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine Semina
C
      common / semi / ix, iy, iz 
C
C---------------------------------------------------- Versione VMS
      is = INT ( SECNDS ( 0. ) ) 
C----------------------------------------------------
C---------------------------------------------------- Versione MS-DOS
C      integer*2 ihr,imin,isec,i100
C      call GETTIM( ihr,imin,isec,i100 )
C      is = i100 + 100*isec + 6000*imin + 360000*ihr
C---------------------------------------------------- 
C
      ix = MOD ( is, 33)
      iy = MOD ( is, 7000)
      iz = MOD ( is, 30000)
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine OpzImp( TabGr, NPImp, iAbbin, DscImp,FilImp, NumPun, 
     >                   NumVar, NumCur, nRipMx, Msurat )
C
C--> Questo sottoprogramma legge i risultati sperimentali, da
C    disegnare insieme a quelli teorici, e li carica nelle colonne
C    inutilizzate di TabGr; ossia quelle del tipo TabGr( i, j, 0 ).
C    Qui i.ne.0 e' l'indice della singola misura, e j.ne.0 e' l'indice
C    della quantita' misurata.
C
      implicit double precision ( a-h , o-z )
      dimension TabGr(NumPun,0:NumVar+NumCur,0:nRipMx)
      dimension iAbbin(NumCur), array(20)
      character DscImp(NumCur)*100, EI*1
      character FilImp*(*), Est*3, riga*132
      common /Import/ NImp, iAscIm, Iprev
      logical Msurat, InzImp, sTitCor, sTitPrc, RigCom, RigTab
C
      KolPnt = 1 ! Puntatore alla colonna iniziale
      KolAgg = 0 ! Numero di colonne da aggiungere
      NTitIm = 0 ! Numero dei titoli
      InzImp =  .true.
      sTitCor = .true.
C
    1 write ( *, * ) ' '
      write ( *, '('' Archivio delle misure ? '', $ )' ) 
      read  ( *, '(a)' ) FilImp
      write ( *, * ) ' '
      write ( *, '('' Tipo dell"archivio (D = Imp) ? '', $ )' ) 
      read  ( *, '(a)' ) Est
      if ( Est .eq. ' ' ) Est = 'imp'
C      
      open ( unit = 8, file = FilImp//'.imp', status = 'old', 
     >       iostat = IOS, readonly )
      Msurat = IOS .eq. 0
C
      if ( Msurat ) then
C
  100   read ( 8, '(a)', end = 200 ) riga
C
        if ( RigCom( riga ) ) goto 100
C
        sTitCor = .not. RigTab( riga )
        if ( InzImp .and. .not. sTitCor ) then
          write (*,'(a)') ' --> Errore :'
          write (*,'(a)') '     L''archivio ', FilImp//'.imp', ' deve'
          write (*,'(a)') '     iniziare con un titolo od un commento'
          goto 200
        end if
        InzImp = .false.
C
        if ( sTitCor ) then
          if ( .not. sTitPrc ) then
            KoAgPr = KolAgg
            KolAgg = 0
          end if
          call inc( KolAgg )
          call inc( NTitIm )
          DscImp( NTitIm ) = riga
          sTitPrc = .true.
        else if ( .not. sTitCor ) then
          if ( sTitPrc ) then
            Itab = 1
            KolPnt = KolPnt + KoAgPr
          end if
          call rig_num( riga, array, LunTab )
          do i = Itab, Itab + LunTab - 1 
            Irig = ( i - 1 ) / KolAgg + 1
            if ( KolAgg .lt. 2 ) then
              Icol = KolPnt 
            else
              Icol = mod( i-1, KolAgg ) + KolPnt 
            end if
            TabGr(Irig,Icol,0) = array(i-Itab+1)
          end do     
          Itab = Itab + LunTab
          sTitPrc = .false. 
        end if
        goto 100
  200   continue
        NImp = Irig
C
        write ( *, * ) ' '
        do i = 1, NTitIm
           write(*,'( '' #'', i2, '' : '', a )') 
     >        i, DscImp(i)(1:lung(DscImp(i),100))
        end do
C
        write(*, *) ' '
   90   write(*,'('' Indice (#) della misura in ascissa : '', $ )' )
        read (*, *) iAscIm
        if ( iAscIm.lt.1 .or. iAscIm.gt.NTitIm ) then
          write (*, '('' Sbagliato '')' )
          goto 90
        end if
        write ( *, * ) ' '
        do i = 1, NTitIm
          if ( i .eq. iAscIm ) then
            write(*,'( '' #'', i2, '' : '', a )') 
     >         i, DscImp(i)(1:lung(DscImp(i),100))//'; ---> in ascissa'
          else
            write(*,'( '' #'', i2, '' : '', a )') 
     >            i, DscImp(i)(1:lung(DscImp(i),100))
          end if
        end do
C
        do i = 1, NTabMx
          iAbbin(i) = 0
        end do
        write ( *, * ) ' '
        write ( *, * ) ' Abbinamenti fra risultati e misure :'
  101   write(*,'('' Indice del risultato (0 per finire): '', $ )' )
        read (*, *) n_abb
        if ( n_abb .eq. 0 ) goto 120
  110   write(*,'('' Indice (#) della misura : '', $ )' )
        read (*, *) m_abb
        if ( m_abb.lt.1 .or. m_abb.gt.NTitIm .or. m_abb.eq.iAscIm ) then
          write (*, '('' Sbagliato '')' )
          goto 110
        end if
        iAbbin( n_abb ) = m_abb
        goto 101
  120   continue     
C
      end if
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      double precision function MSR_( IndImp, X, TabGr, NumPun, NumVar, 
     >                               NumCur, nRipMx )
C--> Interpolazione delle curve misurate
      implicit double precision ( a-h , o-z )
      dimension TabGr(NumPun,0:NumVar+NumCur,0:nRipMx)
      common /Import/ NImp, iAscIm, Iprev
C
      if ( X .le. TabGr(1,iAscIm,0) ) then
       MSR_ = TabGr(1,IndImp,0) 
!      else if ( X .ge. TabGr(NImp,iAscIm,0) ) then
!       MSR_ = TabGr(NImp,IndImp,0) 
      else
       i = max(Iprev,2)
       do while ( .not. ( TabGr(i-1,iAscIm,0).le.X .and.
     >                    TabGr(i,  iAscIm,0).gt.X ) )
         if ( TabGr(i-1,iAscIm,0) .gt. X ) i = i - 1
         if ( TabGr(i  ,iAscIm,0) .le. X ) i = i + 1
       end do
       DY = ( TabGr(i,IndImp,0) - TabGr(i-1,IndImp,0) ) /
     >      ( TabGr(i,iAscIm,0) - TabGr(i-1,iAscIm,0) ) 
       MSR_ = TabGr(i-1,IndImp,0) + DY * ( X - TabGr(i-1,iAscIm,0) )
       Iprev = i
      end if
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      double precision function MSR_xy( ImpAsc,IndImp, X, TabGr, NumPun, 
     >                                  NumVar, NumCur, nRipMx )
C--> Interpolazione delle curve misurate
      implicit double precision ( a-h , o-z )
      dimension TabGr(NumPun,0:NumVar+NumCur,0:nRipMx)
      common /Import/ NImp, iAscIm, Iprev
C
      if ( X .le. TabGr(1,ImpAsc,0) ) then
       MSR_xy = TabGr(1,IndImp,0) 
!      else if ( X .ge. TabGr(NImp,ImpAsc,0) ) then
!       MSR_xy = TabGr(NImp,IndImp,0) 
      else
       i = max(Iprev,2)
       do while ( .not. ( TabGr(i-1,ImpAsc,0).le.X .and.
     >                    TabGr(i,  ImpAsc,0).gt.X ) )
         if ( TabGr(i-1,ImpAsc,0) .gt. X ) i = i - 1
         if ( TabGr(i  ,ImpAsc,0) .le. X ) i = i + 1
       end do
       DY = ( TabGr(i,IndImp,0) - TabGr(i-1,IndImp,0) ) /
     >      ( TabGr(i,ImpAsc,0) - TabGr(i-1,ImpAsc,0) ) 
       MSR_xy = TabGr(i-1,IndImp,0) + DY * ( X - TabGr(i-1,ImpAsc,0) )
       Iprev = i
      end if
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12........
C
      logical function RigTab ( riga )
      character riga*132
C... controlla se la riga contiene gli elementi di una tabella
      i = 0
      do while ( i.eq.0 .or. riga(i:i) .eq. ' ' )
        i = i + 1
      end do
      k = ichar( riga(i:i) )
      RigTab = ( k .ge. 48 .and. k .le. 57 )
     > .or. k .eq. 43 .or. k .eq. 45 .or. k .eq. 46
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      logical function RigCom ( riga )
      character riga*132
C... controlla se e' una riga di commento
      i = 0
      do while ( riga(i:i) .eq. ' ' )
        i = i + 1
      end do
      RigCom = riga(i:i) .eq. '!'
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine inc ( i )
        i = i + 1
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine rig_num( riga, array, LTab )
      implicit double precision ( a-h , o-z )
      double precision array(*)
      character riga*(*), accum*20, car*1
C
      logical se_Spz, sSpzPrc
C
      LunRig = lung( riga, 132 )
      kTab = 0
      se_Spz = .true.
      write ( accum, '( 20('' '') )' )
C
      do i = 1, LunRig
        car = riga(i:i)
        sSpzPrc = se_Spz
        se_Spz = car .eq. ' '
        if ( .not. se_Spz ) then
          if ( sSpzPrc ) then
            call inc( kTab )
            kont = 0
          end if
          call inc( kont )
          accum(kont:kont) = car 
          if ( i.eq.LunRig ) array(kTab) = AccNum( accum, kont )
        else if ( se_Spz .and. .not. sSpzPrc ) then
          if ( kont .gt. 0 ) array(kTab) = AccNum( accum, kont )
        end if
      end do
      LTab = kTab
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      double precision function AccNum( accum, kont )
C
      implicit double precision ( a-h , o-z )
      character accum*20, Frmt*7, FrmItm*2
C
      write ( FrmItm, '(i2)' ) kont
      Frmt = '(F' // FrmItm // '.0)'
      read ( accum(1:kont), Frmt ) AccNum
      write ( accum, '( 20('' '') )' )
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C

