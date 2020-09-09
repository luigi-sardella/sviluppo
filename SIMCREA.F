C
C       *********************************************************
C       *                                                       *
C       *                    S I M C R E A                      *
C       *                    -------------                      *
C       *                                                       *
C       *  Programma per generare automaticamente le procedure: *
C       *                                                       *
C       *  TerNot, che valuta i termini noti del sistema di     *
C       *  equazioni differenziali per l'integrazione numerica; *
C       *                                                       *
C       *  Costan, che precalcola le quantita' che non variano  *
C       *  nel tempo;                                           *
C       *                                                       *
C       *  LetDat, che gestisce la lettura e scrittura su file  *
C       *  dei parametri del problema;                          *
C       *                                                       *
C       *  e LetTab, che fa la stessa cosa per i dati forniti   *
C       *  in forma tabellare.                                  *
C       *                                                       *
C       *********************************************************
C
      parameter ( MaxPar = 100, MaxLen = 50, iDimTa = 50, LuNoFi = 30 )
      parameter ( nRipMx = 20 )
C
      character FilFor*(LuNoFi)
      character riga*132,rig1*132,opis*50, UnMis*15, NomPar*6, NomCos*6, 
     >        NomTab*6,NmTbDi*13, fact*10, spazio*6, sentin*1, 
     >        SN*1, vrg*2, apc*1, cnt*7, defaul*10, DimTab*3, LungTa*3,
     >        Formt*30, FrmIte*3, ValIni*50, ValTip*50, 
     >        NomVar*6, NumFun*10, ValInz*50, PasIni*50, NomInc*6,
     >        CalcFu*59
      character ParCom(MaxPar)*8, CosCom(MaxPar)*8, TabCom(MaxPar)*15,
     >          LisVar(MaxPar)*6, LisInc(MaxPar)*6
      character TabPar(MaxPar)*6
      dimension IndRip(MaxPar)      
C
      double precision deflt, AccNum, array ( MaxLen )
C
      logical NotAdj, PreCal, ContRi, ContXT, rilegi, RigTab, trovat,
     >        noskip, riserv, if_end, da_ora, sUgual, sEquaz, seFunz, 
     >        InFunz, TabMsr, seRipI, seRipJ
C
      NotAdj( i, j ) = abs ( i - j ) .ne. 1  
C --> Serve ad indicare se due ';' non sono adiacenti.
C
      spazio = '      '
      vrg    = ', '
      apc    = ''''
      cnt    = '     > '
C
    1 write(*,*) ' '
      write(*,2) 'TerNot'
    2 format ( ' Nome del file delle equazioni (D = ', a, ' ) ? ', $ )
      read(*, '(a)' ) FilFor
      if ( FilFor .eq. ' ' ) FilFor = 'TerNot'
C      
      open ( unit = 1, file = FilFor//'.eqn', status = 'old', 
     >       iostat = IOS, readonly )
      if ( IOS .ne. 0 ) then
      write(*,*) ' Il file non esiste, o il nome e'' sbagliato; riprova'
      goto 1
      end if
C
      open ( unit = 6, file = FilFor//'.for', status = 'new' )
C
      PreCal = .TRUE.
      write(*,*) ' '
      write(*,'( '' Si precalcolano le costanti ? ( S / [N] ) '', $ )')
      read(*, '( a )' ) SN
      if ( SN .eq. 'N' .or. SN .eq. 'n' ) PreCal = .FALSE.
C
      open ( unit = 2, status = 'SCRATCH' )
      open ( unit = 3, status = 'SCRATCH' )
      open ( unit = 7, status = 'SCRATCH' )
      open ( unit = 8, status = 'SCRATCH' )
      open ( unit = 9, status = 'SCRATCH' )
      open ( unit =10, status = 'SCRATCH' )
      open ( unit =11, status = 'SCRATCH' )
      open ( unit =12, status = 'SCRATCH' )
      open ( unit =13, status = 'SCRATCH' )
C
C... Unita' usate:
C    1 : File di ingresso, .EQN
C    6 : File di uscita,   .FOR
C... File provvisori :           Come vengono ricopiati :
C    2 : COMMON /COSTN /     !  
C    7 : COMMON /PARAM /     !   in TerNot  in Costan  in LetDat  in LetTab
C    9 : COMMON /TABEL /     !        2          2          2          9
C    3 : corpo di LetDat     !        7          7          3         10
C    8 : corpo di Costan     !        9          9                    12
C   10 : corpo di LetTab     !        1          8
C   11 : copia provvisoria del file d'ingresso
C   12 : elenco dei parametri dei calcoli ripetuti
C   13 : copia provvisoria del file d'ingresso
C
      NVar = 0
      NInc = 0
      NFnz = 0
      i_eqn = 0
      ImpCur = 0
      sUgual = .false.
      sEquaz = .false.
      if_end = .true.
      da_ora = .false. 
      TabMsr = .false.
      seRipI = .false.
      seRipJ = .false.
C
 2000 continue
       read (  1, '(a)', end = 2005 ) riga
       i = 0
       do while ( riga(1+i:1+i) .eq. ' ' )
        i = i + 1
       end do
       if ( i .gt. 0 ) then
        do j = i+1, 132
         riga(j-i:j-i) = riga(j:j)
        end do
       end if
       IndImp = index( riga, 'MSR_(' ) + index( riga, 'msr_(' )
       if ( IndImp .ne. 0 ) then
         rig1 = riga
         TabMsr = .true.
         IndPar = index( rig1(IndImp+5:), ')' ) + IndImp + 4
         riga = rig1(1:IndPar-1)//',TabGr,NumPun,NumVar,N_Cur,nRipMx'//
     >          rig1(IndPar:)
       end if
       IndImp = index( riga, 'MSR_xy(' ) + index( riga, 'msr_xy(' )
       if ( IndImp .ne. 0 ) then
         rig1 = riga
         TabMsr = .true.
         IndPar = index( rig1(IndImp+7:), ')' ) + IndImp + 6
         riga = rig1(1:IndPar-1)//',TabGr,NumPun,NumVar,N_Cur,nRipMx'//
     >          rig1(IndPar:)
       end if
C
       noskip = riga(1:1).ne.'!' .and. riga(1:1).ne.';'
       riserv = riga(1:1).eq.'@' .or. riga(1:1).eq.'#' .or.
     >          riga(1:1).eq.'%' .or. riga(1:4).eq.'save' .or.
     >          riga(1:1).eq.'>' .or. RigTab( riga )
C
       if ( sUgual .and. riga(1:1) .ne. '>' ) then
        write ( 13, '(a)' ) '> )'
        sUgual = .false.
        sEquaz = .false.
       end if
C
       IndxPI = index( riga, '?' )
       if ( IndxPI .eq. 0 .and. seFunz ) write (13,'(a)') 'end if'
       if ( riga(1:1) .ne. '?' .and. index(riga(2:),'?') .ne. 0 ) then
        write( rig1, '( a )' ) ' '
        call Xpand( riga, rig1, 0, Lung( riga, 132 ) )
        riga = rig1
       end if
       InFunz = IndxPi .ne. 0 .and. .not. seFunz
       if ( InFunz ) write (13,'(a)') 'if (Cal_Fnz) then'
       seFunz = IndxPi .ne. 0
C
       if ( riga(1:1) .eq. '_' ) then
C
        NVar = NVar + 1
        call AddPV ( riga, 4 )
        NomVar = ' '
        opis   = ' '
        ValIni = ' '
        ValTip = ' '
        ipv1 = index ( riga, ';' )
        ipv2 = ipv1 + index ( riga( ipv1+1 : ), ';' ) 
        ipv3 = ipv2 + index ( riga( ipv2+1 : ), ';' )
        ipv4 = ipv3 + index ( riga( ipv3+1 : ), ';' )
        NomVar = riga( 2 : ipv1-1 )
        LisVar( NVar ) = NomVar 
        if ( NotAdj( ipv1, ipv2 ) ) then
         opis = riga ( ipv1+1 : ipv2-1 )
        else
         opis = NomVar
        end if
        if ( NotAdj( ipv2, ipv3 ) ) ValIni = riga ( ipv2+1 : ipv3-1 )
        if ( NotAdj( ipv3, ipv4 ) ) ValTip = riga ( ipv3+1 : ipv4-1 )
        if ( ValIni .eq. ' ' ) ValIni = ' 0.D0'
        if ( ValTip .eq. ' ' ) ValTip = ' 1.D0'
        write ( 13, '(a,i2,a)' ) 'if(t_.eq.0.D0) x_(',
     >                                NVar, ') = ' // ValIni
        write ( 13, '(a,i2,a)' ) NomVar // '=x_(', NVar, ') !' // opis
        write ( 13, '(a,i2,a)' ) 'x_t(', NVar, ') =' // ValTip
C
       else if ( riga(1:1) .eq. '~' .or. riga(1:5) .eq. 'd/dt ' ) then
C
        iUgua = index ( riga, '=' ) 
        if ( iUgua .eq. 0 ) stop ' Manca un segno di = nelle equazioni'
        NomPar = riga(2:iUgua-1)
        if ( riga(1:5) .eq. 'd/dt ' ) NomPar = riga(6:iUgua-1)
        trovat = .false.
        do i = 1, NVar
         if ( NomPar .eq. LisVar(i) ) then
          i_var = i
          trovat = .true.
         end if
        end do
        if ( .not. trovat )then
         write( *, '(a)' ) ' Non definita la variabile '//NomPar
         stop
        end if
        write ( 13, '(a,i2,a)' ) 'dx_(', i_var, ') ' // 
     >                                riga(iUgua:lung(riga,132))
C
       else if ( riga(1:1) .eq. '\' ) then
C
        NInc = NInc + 1
        call AddPV ( riga, 4 )
        NomInc = ' '
        opis   = ' '
        ValInz = ' '
        PasIni = ' '
        ipv1 = index ( riga, ';' )
        ipv2 = ipv1 + index ( riga( ipv1+1 : ), ';' ) 
        ipv3 = ipv2 + index ( riga( ipv2+1 : ), ';' )
        ipv4 = ipv3 + index ( riga( ipv3+1 : ), ';' )
        NomInc = riga( 2 : ipv1-1 )
        LisInc( NInc ) = NomInc 
        if ( NotAdj( ipv1, ipv2 ) ) then
         opis = riga ( ipv1+1 : ipv2-1 )
        else
         opis = NomInc
        end if
        if ( NotAdj( ipv2, ipv3 ) ) ValInz = riga ( ipv2+1 : ipv3-1 )
        if ( NotAdj( ipv3, ipv4 ) ) PasIni = riga ( ipv3+1 : ipv4-1 )
        if ( ValInz .eq. ' ' ) ValInz = ' 0.D0'
        if ( PasIni .eq. ' ' ) PasIni = ' 1.D0'
        write ( 13, '(a,i2,a)' ) 'SMPLX(',NInc, ',1) = ' // ValInz
        write ( 13, '(a,i2,a)' ) NomInc // '=v_(', NInc, ') !' // opis
        write ( 13, '(a,i2,a)' ) 'DelX(', NInc, ') =' // PasIni
C
       else if ( riga(1:1) .eq. '&' ) then
C
        i_eqn = i_eqn + 1
        indPV = index ( riga, ';' )
        iUgua = index ( riga, '=' ) 
        sEquaz = .true.
        sUgual = iUgua .ne. 0
        if ( sUgual ) then
         riga(iUgua:iUgua) = '-'
         write ( 13, '(a,i2,a)' ) 's_(', i_eqn, ') = ' // 
     >      riga(indPV+1:iUgua)//'('//riga(iUgua+1:lung(riga,132))
        else
         write ( 13, '(a,i2,a)' ) 's_(', i_eqn, ') = ' // 
     >                            riga(indPV+1:lung(riga,132))
        end if
C
       else if ( sEquaz .and. .not.sUgual .and. riga(1:1).eq.'>' ) then
C
        iUgua = index ( riga, '=' ) 
        sUgual = iUgua .ne. 0
        if ( sUgual ) then
         riga(iUgua:iUgua) = '-'
         write ( 13,'(a)' ) riga(1:iUgua)//'('//
     >                      riga(iUgua+1:lung(riga,132))
        else
         write ( 13,'(a)' ) riga(1:lung(riga,132))
        end if
C
       else if ( riga(1:1) .eq. '?' ) then
C
        NFnz = NFnz + 1
        call AddPV ( riga, 3 )
        NumFun = ' '
        opis   = ' '
        CalcFu = ' '
        ipv1 = index ( riga, ';' )
        ipv2 = ipv1 + index ( riga( ipv1+1 : ), ';' ) 
        ipv3 = ipv2 + index ( riga( ipv2+1 : ), ';' )
        NumFun = riga( 2 : ipv1-1 )
        read( NumFun, '(bn,i10)' ) komp
        call Xpand( riga ( ipv1+1:ipv2-1 ), CalcFu, komp, ipv2-ipv1-1 )
        if ( NotAdj( ipv2, ipv3 ) ) opis = riga ( ipv2+1 : ipv3-1 )
        write ( 13, '(a,i2,a)' ) 'f_(', komp, ')='//CalcFu//'!'//opis
!        i_MSR = index(riga,'MSR_') 
!        if ( i_MSR .ne. 0 ) then
!          ImpCur = komp
!          i_ap = index( riga(i_MSR:), '(' )
!          i_vi = index( riga(i_vi: ), ',' )
!          read( riga(i_ap+1:i_vi-1), '(bn,i10)' ) indice
!        end if
C
       else if ( riga(1:7) .eq. 'RISOLVI' ) then
        write (13,'(a)') 'risolv_ = .true.'
C
       else if ( riga(1:3) .eq. 'NON' .and. 
     >           index( riga(4:),'RISOLVERE' ) .ne. 0 ) then
        write (13,'(a)') 'risolv_ = .false.'
C
       else if ( riga(1:6) .eq. 'return' ) then
        if ( da_ora ) write (13,'(a)') 'end if'          
        da_ora = .false.          
        write (13,'(a)') 'return'
C
       else if ( noskip .and. .not. riserv  ) then
C
        if ( PreCal ) then
         if ( riga(1:1) .eq. '$' ) then
          if ( da_ora .and. .not. if_end ) write (13,'(a)')  'end if '
          write (13,'(a)') riga(2:lung(riga,132))
          if_end = .true.
          da_ora = .true.
         else
          if ( da_ora .and. if_end ) write (13,'(a)') 'if(integr_)then' 
          write (13,'(a)') riga(:lung(riga,132))
          if_end = .false.
         end if
        else
         write (13,'(a)') riga(index(riga,'$')+1:lung(riga,132))
        end if
C
       else if ( noskip .and. riserv ) then
C
        write (13,'(a)') riga(1:lung(riga,132))
C
       end if
C
      goto 2000 
C
 2005 continue
C 
      if ( seFunz ) write (13,'(a)') 'end if'
      if ( da_ora .and. .not. if_end ) write (13,'(a)') 'end if'
      rewind 13
C
 1000 continue
       read ( 13, '(a)', end = 1005 ) riga
       LunRig = lung(riga,132)
       ipe = index( riga, '!' )
       if ( ipe .gt. 0 ) then 
        iFinRi = ipe - 1
       else
        iFinRi = LunRig
       end if
       if(iFinRi.le.66 .or. riga(1:1).eq.'@' .or. riga(1:1).eq.'#') then
        if ( iFinRi .gt. 0 ) write(11, '(a)' ) riga(:iFinRi)
       else
        write(11, '(a)' ) riga(:66)
        write(11, '(a)' ) '>' // riga(67:iFinRi)
       end if
       if ( index( riga, 'f_' ) .ne. 0 ) then 
        if ( ipe .gt. 0 ) then 
         iap = index( riga, '(' )
         icp = index( riga, ')' )
         LRi = min( LunRig, ipe + 48 )
         read( riga( iap+1 : icp-1 ), '(bn,i20)' ) komp
         write ( 11, '( a, i3, 3a )' ) 
     >         'Des_Fu(', komp, ')='' ', riga( ipe+1:LRi ) , apc
        end if
       end if
       if ( index( riga, 'x_' ) .ne. 0
     >.and. index( riga, 'x_t') .eq. 0 
     >.and. index( riga, 'dx_') .eq. 0 ) then 
        ipe = index( riga, '!' )
        if ( ipe .gt. 0 ) then 
         iap = index( riga, '(' )
         icp = index( riga, ')' )
         LRi = min( LunRig, ipe + 48 )
         read( riga( iap+1 : icp-1 ), '(bn,i20)' ) komp
         write ( 11, '( a, i3, 3a )' ) 
     >         'Des_Va(', komp, ')='' ', riga( ipe+1:LRi ), apc
        end if
       end if
      goto 1000
 1005 rewind 11
C
      NumPar = 0
      NumCos = 0
      NumTab = 0
      rilegi = .FALSE.
      write( DimTab, '( i3 )') iDimTa
C
  100 if ( .not. rilegi ) then
         read ( 11, '( a1, a )', end = 200 ) sentin, riga
         write( rig1, '( a )' ) ' '
      else
         read ( rig1, '( a1, a )' )         sentin, riga
         rilegi = .FALSE.
      end if
C
      if ( sentin .eq. '@' ) then
C
         NumPar = NumPar + 1
C
         if ( NumPar .gt. MaxPar ) then
            write(*,*) ' ***> ATTENZIONE <*** '
            write(*,*) ' Il programma e'' predisposto per un massimo '
            write(*,*) ' di ', MaxPar, ' variabili'
            goto 200
         end if
C
         call AddPV ( riga, 5 )
C
         NomPar = ' '
         opis   = ' '
         UnMis  = ' '
         defaul = ' '
         fact   = ' '
C
         ipv1 = index ( riga, ';' )
         ipv2 = ipv1 + index ( riga( ipv1+1 : ), ';' ) 
         ipv3 = ipv2 + index ( riga( ipv2+1 : ), ';' )
         ipv4 = ipv3 + index ( riga( ipv3+1 : ), ';' )
         ipv5 = ipv4 + index ( riga( ipv4+1 : ), ';' )
C
         NomPar = riga( 1 : ipv1-1 )
         ParCom( NumPar ) = NomPar // vrg
C
         if ( NotAdj( ipv1, ipv2 ) ) then
            opis = NomPar // ':' // riga ( ipv1+1 : ipv2-1 )
         else
            opis = NomPar
         end if
         if ( NotAdj( ipv2, ipv3 ) ) defaul = riga ( ipv2+1 : ipv3-1 )
         if ( NotAdj( ipv3, ipv4 ) )  UnMis = riga ( ipv3+1 : ipv4-1 )
         if ( NotAdj( ipv4, ipv5 ) )   fact = riga ( ipv4+1 : ipv5-1 )
C
         write ( defaul, '( 1pd10.2 )' ) AccNum(defaul,lung(defaul,10))
C
         write (3,20) spazio, 'tit   = ', apc, opis, apc
         write (3,20) spazio, 'UnMis = ', apc, UnMis, apc
         write (3,20) spazio, 'call rwdati ( ', NomPar, 
     >                ', tit, UnMis, restart, ' ,defaul, ' )'
C
         if ( fact .ne. ' ' ) write (3,20)
     >                spazio, NomPar, ' = ', NomPar, ' * ', fact
C
      end if
C
      if ( sentin .eq. '#' ) then
C
         NumTab = NumTab + 1
C
         if ( NumTab .gt. MaxPar ) then
            write(*,*) ' ***> ATTENZIONE <*** '
            write(*,*) ' Il programma e'' predisposto per un massimo '
            write(*,*) ' di ', MaxPar, ' variabili'
            goto 200
         end if
         call AddPV ( riga, 3 )
         opis = ' '
         fact = ' '
         ipv1 = index ( riga, ';' )
         ipv2 = ipv1 + index ( riga( ipv1+1 : ), ';' ) 
         ipv3 = ipv2 + index ( riga( ipv2+1 : ), ';' ) 
         NomTab = riga( 1 : ipv1-1 ) 
         NmTbDi = NomTab // '(0:' // DimTab // ')' 
         TabCom( NumTab ) = NmTbDi // vrg
         if ( NotAdj( ipv1, ipv2 ) ) then
            opis = NomTab // ':' // riga ( ipv1+1 : ipv2-1 )
         else
            opis = NomTab
         end if
         if ( NotAdj( ipv2, ipv3 ) ) fact = riga ( ipv2+1 : ipv3-1 )
C
         LunTb1 = 0
  105    read (11,20) riga
C... ho letto la riga successiva, contenente i valori tabellari
         if ( RigTab( riga ) ) then
           rilegi = .FALSE.
           LunTab = LunTb1
           call rig_num( riga, array(LunTab+1), LTab )
           LunTab = LunTab + Ltab
           LunTb1 = LunTab
           goto 105
         else
           rig1 = riga
           rilegi = .TRUE.
         end if
         if ( LunTab .gt. iDimTa ) LunTab = iDimTa
         write ( LungTa, '( i3 )' ) LunTab
C
         write (10,20) spazio, 'tit   = ', apc, opis, apc 
         write (10,20) spazio, 'fact  = ', apc, fact, apc
         write (10,20) spazio, 'LunTab = ', LungTa 
         write (10,20) spazio, 'TabInp = ' 
         k = 1
         do 115 nrig = 1, ( LunTab - 1 ) / 6 + 1
          if ( k + 5 .lt. LunTab ) then
           write (10,'( 2a, 6( 1x, 1pd9.2 ), 2a )' )  
     >            cnt, apc, (array(i),i=k,k+5), apc, '//'
          else
           write( FrmIte, '( i3 )' ) LunTab - k + 1
           Formt = '( 2a, ' // FrmIte // '( 1x, 1pd9.2 ), a )' 
           write (10, Formt ) cnt, apc, (array(i),i=k,LunTab), apc
          end if
          k = k + 6
  115    continue         
C           
         write (10,20) spazio, 'call RWtabl (',
     >      NomTab // ', tit, fact, restart, TabInp, LunTab )'
         if ( fact .ne. ' ' ) then
            write (10,20) spazio, 'do i = 1, LunTab'
            write (10,20) spazio, NomTab, '(i) = ', fact,  
     >                    ' * ', NomTab, '(i)'
            write (10,20) spazio,'end do'
         end if
         TabPar( NumTab ) = NomTab
         IndRip( NumTab ) = LunTab
      end if
C
      if ( sentin .eq. '%' ) then
C
         IndEq = index( riga, '=' )
         if ( IndEq .eq. 0 ) then
          write(*,*) riga, ' <******'
          goto 100
         else if ( index( riga(1:IndEq),'(' ) .ne. 0 ) then
          goto 100
         else
          NumCos = NumCos + 1
          if ( NumCos .gt. MaxPar ) then
           write(*,*) ' ***> ATTENZIONE <*** '
            write(*,*) ' Il programma e'' predisposto per un massimo '
            write(*,*) ' di ', MaxPar, ' variabili'
           goto 200
          end if
C
          i1 = 0
          i2 = 0
          do 9902 i = 1, IndEq
           if ( i1 .eq. 0 .and. riga(i:i) .ne. ' ' ) i1 = i
           if ( i1 .ne. 0 .and. i2 .eq. 0 .and. riga(i:i) .eq. ' '
     >         .or. riga(i:i) .eq. '=' ) i2 = i-1
 9902     continue
C
          NomCos = riga(i1:i2)         
          CosCom( NumCos ) = NomCos // vrg
C
         end if
      end if
C
      goto 100
C
  200 continue
C
      write (2,20) spazio, 'common  /param/ '
      write (7,20) spazio, 'common  /costn/ '
      write (9,20) spazio, 'common  /tabel/ '
C
      call WriCom ( ParCom, NumPar, NomPar, 7, 2, 'DummyP' )
      call WriCom ( CosCom, NumCos, NomCos, 7, 7, 'DummyC' )
      call WriCom ( TabCom, NumTab, NmTbDi, 4, 9, 'DummyT' )
C
      rewind 11
C
      write (6,20) spazio,
     > 'subroutine TerNot (x_,dx_,f_,n_,N_Cur,t_,dt_,dt_sta,T_in,T_fi,'
      write (6,20) cnt, 'Cal_Fnz, v_, s_, n_E,n_V, integr_, risolv_,' 
      write (6,20) cnt, 'i_, j_, TabGr, NumPun, NumVar, nRipMx ) '
      write (6,20) spazio, 'implicit double precision ( a - z )'
      write (6,20) spazio, 'integer n_, N_Cur, n_E, n_V, i_, j_'
      write (6,20) spazio, 'integer NumPun, NumVar, nRipMx'
      write (6,20) spazio, 'logical Cal_Fnz, integr_, risolv_'
      write (6,20) spazio, 'dimension x_(n_), dx_(n_), f_(N_Cur)'
      write (6,20) spazio, 'dimension v_(n_V), s_(n_E)'
      write (6,20) spazio, 'dimension TabGr(NumPun,0:NumVar+N_Cur,',
     >             '0:nRipMx)'
      write (6,20) spazio, 'common /Import/ NImp, iAscIm, Iprev'
C       
  500 read ( 2, 20, end = 505 ) riga
      write (6,20) riga(1:lung(riga,132))
      goto 500
C       
  505 read ( 7, 20, end = 510 ) riga
      write (6,20) riga(1:lung(riga,132))
      goto 505
C       
  510 read ( 9, 20, end = 515 ) riga
      write (6,20) riga(1:lung(riga,132))
      goto 510
C
  515 ContRi = .FALSE.
      ContXT = .FALSE.
      iunit = 6
      if ( PreCal ) iunit = 8  
C
C... ovvero, se si precalcolano le costanti
C
  600 if ( .not. rilegi ) then
         read ( 11, 20, end = 700 ) riga
         write( rig1, '( a )' ) ' '
      else
         read ( rig1, 20 )         riga
         rilegi = .FALSE.
      end if
      LunRig = lung(riga,132)
      if ( riga(1:1) .eq. '%' ) then
         write (iunit,20) spazio, riga(2:LunRig)
         ContRi = .TRUE.
      else if ( index( riga, 'Des_' ) .ne. 0 ) then
         write (8,20) spazio, riga(1:LunRig)
      else if ( riga(1:5).eq.'SMPLX' .or. riga(1:4).eq.'DelX') then
         write (iunit,20) spazio, riga(1:LunRig)
      else if ( index( riga, 'x_t'  ) .ne. 0 ) then
         write (8,20) spazio, riga(1:LunRig)
         ContXT = .TRUE.
      else if ( riga(1:1) .eq. '!' .or. riga(1:1) .eq. '@' ) then
         ContRi = .FALSE.
      else if ( riga(1:1) .eq. '#' ) then
         ContRi = .FALSE.
         rilegi = .FALSE.
  605    read ( 11, 20, end = 700 ) riga
         if ( RigTab( riga ) ) then
           goto 605
         else
           rig1 = riga
         rilegi = .TRUE.
         end if
      else if ( riga(1:1) .eq. '>' ) then
       if ( ContRi ) then
         write (iunit,20) cnt, riga(2:LunRig)
       else if ( ContXT ) then
         write (8,20) cnt, riga(2:LunRig)
       else
         write (6,20) cnt, riga(2:LunRig)
       end if
      else
         write (6,20) spazio, riga(1:LunRig)
         ContRi = .FALSE.
         ContXt = .FALSE.
      end if
C
      ind_ri = 1
  314 ind_i_ = index( riga(ind_ri:), 'i_' )
      if ( ind_i_ .gt. 0 ) then
        seRipI = .true.
        call AddRip( riga(ind_ri:ind_i_-1), ind_i_-ind_ri, 'i_' )
        ind_ri = ind_i_ + 2
        goto 314
      end if 
C
      ind_ri = 1
  315 ind_j_ = index( riga(ind_ri:), 'j_' )
      if ( ind_j_ .gt. 0 ) then
        seRipJ = .true.
        call AddRip( riga(ind_ri:ind_j_-1), ind_j_-ind_ri, 'j_' )
        ind_ri = ind_j_ + 2
        goto 315
      end if       
C
      goto 600
C
  700 write (6,20) spazio, 'return'
      write (6,20) spazio, 'end'
C
      write (6,20) spazio, 
     > 'subroutine Costan(N_P,N_V,N_C,Des_Va,Des_Fu,x_t,SMPLX,DelX,'
      write (6,20) cnt, ' i_, j_, Tab_Ms )'
!      write (6,20) cnt, ' i_, j_, Tab_Ms, Imp_Cu, indic_ )'
      write (6,20) spazio, 'implicit double precision ( a - z )'
      write (6,20) spazio, 'integer N_V, N_C, N_P, i_,j_ '
!      write (6,20) spazio, 'integer N_V, N_C, N_P, i_,j_, Imp_Cu, indic_'
      write (6,20) spazio, 'character*50 Des_Va(N_V), Des_Fu(N_C)'
      write (6,20) spazio, 'dimension x_t(N_V),SMPLX(N_P,N_P),DelX(N_P)'
      write (6,20) spazio, 'logical Tab_Ms'
C
      rewind 2
  800 read ( 2, 20, end = 805 ) riga
      write (6,20) riga(1:lung(riga,132))
      goto 800
  805 continue
      rewind 9
  900 read ( 9, 20, end = 905 ) riga
      write (6,20) riga(1:lung(riga,132))
      goto 900
  905 continue
C       
      iunit = 7
      rewind iunit
  305 read ( iunit, 20, end = 405 ) riga
      write (6,20) riga(1:lung(riga,132))
      goto 305
  405 continue
      write (6,20) spazio, 'do 100 i = 1, N_V'
      write (6,20) '  100  x_t(i) = 0.D0'
      iunit = 8
      rewind iunit
  307 read ( iunit, 20, end = 407 ) riga
      write (6,20) riga(1:lung(riga,132))
      goto 307
  407 continue
C
      write (6,20) spazio, 'do 200 i = 1, N_V'
      write (6,20) '  200 if( x_t(i) .eq. 0.D0 ) x_t(i) = 1.D0'
      write (6,20) spazio, 'Tab_Ms = .false.'
      if ( TabMsr ) write (6,20) spazio, 'Tab_Ms = .true.'
!      write (6,'(2a, i4)' ) spazio, 'Imp_Cu = ', ImpCur
!      write (6,'(2a, i4)' ) spazio, 'indic_ = ', indice
      write (6,20) spazio, 'return'
      write (6,20) spazio, 'end'
C
      write (6,20) spazio, 'subroutine LetDat( restart )'
      write (6,20) spazio, 'implicit double precision ( a - z )'
      write (6,20) spazio, 'logical restart'
      write (6,20) spazio, 'character tit*50, UnMis*15'
C
      do iunit = 2, 3
         rewind iunit
  300    read ( iunit, 20, end = 400 ) riga
         write (6,20) riga(1:lung(riga,132))
         goto 300
  400    continue
      end do
C
      write (6,20) spazio, 'return'
      write (6,20) spazio, 'end'
C
      write ( LungTa, '( i3 )' ) 10 * iDimTa
      write (6,20) spazio, 'subroutine LetTab( restart, I_rip,J_rip )'
      write (6,20) spazio, 'implicit double precision ( a - z )'
      write (6,20) spazio, 'integer I_rip, J_rip, LunTab'
      write (6,20) spazio, 'logical restart'
      write (6,20) spazio, 'character tit*50, TabInp*', 
     >                      LungTa, ', fact*10'
C
      do iunit = 9, 10
         rewind iunit
 1300    read ( iunit, 20, end = 1400 ) riga
         write (6,20) riga(1:lung(riga,132))
         goto 1300
 1400    continue
      end do 
      write( 6, '(a,i5)' ) spazio//'I_rip=', 1
      write( 6, '(a,i5)' ) spazio//'J_rip=', 1
      if ( seRipI .or. seRipJ ) then
        if ( seRipI ) write(6,'(a,i5)') spazio//'I_rip=', nRipMx
        if ( seRipJ ) write(6,'(a,i5)') spazio//'J_rip=', nRipMx
        rewind 12
 1305   read ( 12, 20, end = 1405 ) riga
        write (6,20) riga(1:lung(riga,132))
        goto 1305
 1405   continue
      end if
C
      write (6,20) spazio, 'return'
      write (6,20) spazio, 'end'
C
      stop
C
   20 format ( 20a )
C
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine AddPV ( riga, NumPV )
      character riga*132  
      npv = 0
      lun = 0
      do 9900 i = 132, 1, -1
      if ( riga(i:i) .ne. ' ' .and. lun .eq. 0 ) lun = i
      if ( riga(i:i) .eq. ';' ) npv = npv + 1
 9900 continue
      if ( npv .lt. NumPV ) then
      do 9901 i = 1, NumPV - npv
 9901 riga( lun+i : lun+i ) = ';'
      end if
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine WriCom ( LisCom, N_tot, ComVar, N_rig, iUnit, Dummy )
      character LisCom(*)*(*), ComVar*(*), cnt*7, Formt*5, Dummy*6
      cnt    = '     > '
      if ( N_tot .eq. 0 ) then
         N_tot = 1
         LisCom( 1 ) = Dummy 
      else
         LisCom( N_tot ) = ComVar // ' '
      end if
      write( Formt, '( ''('', i2, ''a)'' )' ) N_rig + 1
      k = 1
      do 10 nrig = 1, N_tot / N_rig + 1
       if ( k + N_rig - 1 .le. N_tot ) then
         write (iUnit,Formt) cnt, ( LisCom(i), i = k, k + N_rig - 1 )
       else
         write (iUnit,Formt) cnt, ( LisCom(i), i = k, N_tot )
       end if
       k = k + N_rig
   10 continue
      rewind iUnit
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      integer function lung ( riga, n )
      character riga*(*)
C
      i = n
      if ( .not. ( riga(i:i) .eq. ' ' .and. i .gt. 1 ) ) goto 9992
 9991 i = i - 1
      if ( riga(i:i) .eq. ' ' .and. i .gt. 1 ) goto 9991
 9992 continue
C
      lung = i
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      logical function RigTab ( riga )
      character riga*132
C . . . controlla se la riga contiene gli elementi di una tabella
      i = 0
   10 i = i + 1
      if ( riga(i:i) .eq. ' ' ) goto 10
      k = ichar( riga(i:i) )
      RigTab = ( k .ge. 48 .and. k .le. 57 )
     > .or. k .eq. 43 .or. k .eq. 45 .or. k .eq. 46
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine Xpand( Strng1, Strng2, komp, LunSt1 )
      character*(*) Strng1, Strng2, Xpk*1
      logical Parnts, Numero
C
      Parnts = .false.
      indSom = index( Strng1, '++' )
      indInt = index( Strng1, '$+' )
      if( indSom * indInt .gt. 0 ) then
       write (*,'(a)') ' --> Errore in ' // Strng1
       stop
      end if
      if ( indSom .gt. 0 ) then
       inizXp = indSom + 2
      else if ( indInt .gt. 0 ) then
       inizXp = indInt + 2
      else
       inizXp = 1
      end if
      if ( index(Strng1,'?').eq.0 .and. (indSom+indInt).eq.0 ) then
       Strng2 = Strng1( 1 : LunSt1 )
      else
       if ( indSom .gt. 0 ) then
        write( Strng2, '( ''f_('', i2, '')+'' )' ) komp
        kCF = 8
       else if ( indInt .gt. 0 ) then
        write( Strng2, '( ''f_('', i2, '')+dt_sta*('' )' ) komp
        kCF = 16
       else
        kCF = 1
       end if
       do k = inizXp, LunSt1
        Xpk = Strng1( k : k ) 
        if ( Xpk .ne. ' ' ) then
         if ( .not. Parnts .and. Xpk .ne. '?' ) then
          Strng2( kCF : kCF ) = Xpk
          kCF = kCF + 1
         else if ( .not. Parnts .and. Xpk .eq. '?' ) then
          Strng2( kCF : kCF+2)  = 'f_('
          kCF = kCF + 3
          Parnts = .true.
         else if ( Parnts ) then
          iXpk = ichar( Xpk )
          Numero = iXpk .ge. 48 .and. iXpk .le. 57
          if ( Numero ) then
           Strng2( kCF : kCF ) = Xpk
           kCF = kCF + 1
          else
           Strng2( kCF : kCF+1 ) = ')' // Xpk
           kCF = kCF + 2
           Parnts = .false.
          end if ! iXpk .ge. 48 .and. iXpk .le. 57
         end if ! .not. Parnts .and. Xpk .ne. '?'
        end if ! Xpk .ne. ' '
       end do ! k
      end if ! index( Strng1, '?' ) .eq. 0 .and. indSom .eq. 0 
      if ( Parnts ) then
       Strng2( kCF : kCF ) = ')'
       kCF = kCF + 1
      end if
      if ( indInt .gt. 0 ) Strng2( kCF : kCF ) = ')'
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine AddRip( sk, LungSK, k_ )
C
      character sk*(*), Param*6, k_*2, rigk*1
C
      k = LungSK
      kont = 0                  ! si trova, si esamina la riga a ritroso
      Param = '      '          ! per identificare il nome della lista.
  100 continue 
       k = k - 1
       rigk = sk(k:k)                
       if ( rigk .eq. ' ' .or. rigk .eq. '(' ) then
         goto 100
       else
         k = k + 1
       end if
  200 continue
        k = k - 1
        rigk = sk(k:k)                
        icar = ichar( rigk )
        if ( 48.le.icar.and.icar.le.57 .or. 65.le.icar.and.icar.le.90
     >  .or. 97.le.icar.and.icar.le.122 ) then
          kont = kont + 1
          k1 = 7 - kont
          Param(k1:k1) = rigk
          goto 200
        end if
C
      write( 12, '(7x,a)') k_//'rip=min('//k_//'rip,int('//
     >                  Param//'(0)))'
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine rig_num( riga, array, LTab )
C--> Legge una riga di numeri in formato libero
      implicit double precision ( a-h , o-z )
      double precision array(*)
      character riga*132, accum*20, car*1
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
      do i = 1, kont
        accum(i:i) = ' '
      end do
C
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
