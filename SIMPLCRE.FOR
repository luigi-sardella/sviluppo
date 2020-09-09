[K$ t simplex.forcre.for[Ka.for[K
C
C       *********************************************************
C       *                                                       *
C       *                  S I M P L C R E A                    *
C       *                  -----------------                    *
C       *                                                       *
C       *  Programma per generare automaticamente le procedure: *
C       *                                                       *
C       *  Equaz, che valuta le quantita' da minimizzare;       *
C       *                                                       *
C       *  Costan, che precalcola le quantita' costanti;        *
C       *                                                       *
C       *  LetDat, che gestisce la lettura e scrittura su file  *
C       *  dei parametri del problema;                          *
C       *                                                       *
C       *  e LetTab, che fa la stessa cosa per i dati forniti   *
C       *  in forma tabellare.                                  *
C       *                                                       *
C       *********************************************************
C
      parameter ( MaxPar = 50, MaxLen = 50, iDimTa = 50 )
C
      character riga*132,rig1*132,opis*50, UnMis*15, NomPar*6, NomCos*6, 
     >        NomTab*6,NmTbDi*13, fact*10, spazio*6, FilFor*9, sentin*1, 
     >        SN*1, vrg*2, apc*1, cnt*7, defaul*10, DimTab*3, LungTa*3,
     >        Formt*30, FrmIte*3, indDO*3, ValInz*50, PasIni*50,
     >        NomInc*6, NumFun*10
      character ParCom(MaxPar)*8, CosCom(MaxPar)*8, TabCom(MaxPar)*15,
     >          LisInc(MaxPar)*6
C
      dimension array ( MaxLen )
C
      logical NotAdj, PreCal, ContRi, ContXT, rilegi, RigTab, trovat
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
C    8 : corpo di Costan     !        9          9          
C   10 : corpo di LetTab     !        1          8
C   11 : copia provvisoria del file d'ingresso
C   12 : contiene la tabella che si sta leggendo
C   13 : copia provvisoria del file d'ingresso
C
      NInc = 0
      NFnz = 0
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
C
       if ( riga(1:1) .eq. '\' ) then
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
        if ( PasIni .eq. ' ' ) PasIni = ' 1. '
        if ( ValInz .ne. ' ' ) 
     >  write ( 13, '(a,i2,a)' ) 'SIMP(',NInc, ',1) = ' // ValInz
        write ( 13, '(a,i2,a)' ) NomInc // '=v_(', NInc, ') !' // opis
        write ( 13, '(a,i2,a)' ) 'STEP(', NInc, ') =' // PasIni
C
       else if ( riga(1:3) .eq. '&EQ' ) then
C
        iUgua = index ( riga, '=' ) 
        indPV = index ( riga, ';' )
        if ( iUgua .ne. 0 ) riga(iUgua:iUgua) = '-'
        read( riga(4:indPV-1), '(bn,i10)' ) i_eqn
        write ( 13, '(a,i2,a)' ) 's_(', i_eqn, ') = ' // 
     >                            riga(indPV+1:Lung( riga ))
C
       else if ( riga(1:1) .eq. '?' ) then
C
        NFnz = NFnz + 1
        if ( NFnz .eq. 1 ) write ( 13, '( a )' ) 'if (CalFnz) then'
        call AddPV ( riga, 3 )
        NumFun = ' '
        opis   = ' '
        ValInz = ' '
        ipv1 = index ( riga, ';' )
        ipv2 = ipv1 + index ( riga( ipv1+1 : ), ';' ) 
        ipv3 = ipv2 + index ( riga( ipv2+1 : ), ';' )
        NumFun = riga( 2 : ipv1-1 )
        ValInz = riga ( ipv1+1 : ipv2-1 )
        if ( NotAdj( ipv2, ipv3 ) ) opis = riga ( ipv2+1 : ipv3-1 )
        read( NumFun, '(bn,i10)' ) komp
        if ( NFnz .gt. 1 ) backspace ( 13 )
        write ( 13, '(a,i2,a)' ) 'f_(', komp, ') = '//ValInz//'!'//opis
        write ( 13, '( a )' ) 'end if'
C
       else if ( riga(1:1).ne.'!' .and. riga(1:1).ne.';' ) then
C
        write ( 13, '( a )' ) riga(:lung( riga ))
C
       end if
C
      goto 2000 
C 
 2005 rewind 13
C
 1000 continue
       read ( 13, '(a)', end = 1005 ) riga
       LunRig = Lung ( riga )
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
       if ( index( riga, 'v_' ) .ne. 0 ) then 
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
         if ( defaul .eq. ' ' ) defaul = ' 0. '
C
         write (3,20) spazio, 'tit   = ', apc, opis, apc
         write (3,20) spazio, 'UnMis = ', apc, UnMis, apc
         write (3,20) spazio, 'call rwdati ( ', NomPar, 
     >                ', tit, UnMis, restart, ' ,defaul, ', .true. )'
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
           do 110 i = 1, len( riga )
              if ( riga(i:i) .eq. '.' ) LunTab = LunTab + 1
  110      continue
           open  ( unit = 12, status = 'UNKNOWN' )
           write ( 12, 20) riga(1:Lung( riga ))
           close ( unit = 12 )
           open  ( unit = 12, status = 'UNKNOWN' )
           read  ( 12, * ) ( array(i), i = LunTb1+1, LunTab )
           close ( unit = 12, status = 'DELETE' )
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
         write (10,20) spazio, 'TabInp = ' 
         k = 1
         do 115 nrig = 1, ( LunTab - 1 ) / 6 + 1
          if ( k + 5 .lt. LunTab ) then
           write (10,'( 2a, 6( 1x, 1pe9.2 ), 2a )' )  
     >            cnt, apc, (array(i),i=k,k+5), apc, '//'
          else
           write( FrmIte, '( i3 )' ) LunTab - k + 1
           Formt = '( 2a, ' // FrmIte // '( 1x, 1pe9.2 ), a )' 
           write (10, Formt ) cnt, apc, (array(i),i=k,LunTab), apc
          end if
          k = k + 6
  115    continue         
C           
         write (10,20) spazio, 'call RWtabl (',
     >      NomTab // ', tit, fact, restart, TabInp, ' // LungTa // ' )'
         if ( fact .ne. ' ' ) then
            write ( indDO, '( i3 )' ) NumTab
            write (10,20) spazio, 'do ', indDO, ' i = 1, ', LungTa
            write (10,20) '  ', indDO, ' ', NomTab, '(i) = ', fact,  
     >                    ' * ', NomTab, '(i)'
         end if
C
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
     > 'subroutine Equaz ( v_, s_, f_, nE, nV, NumFun, CalFnz )'
      write (6,20) spazio, 'implicit double precision ( a - z )'
      write (6,20) spazio, 'integer nE, nV, NumFun'
      write (6,20) spazio, 'logical CalFnz'
      write (6,20) spazio, 'dimension v_(nV), s_(nE), f_(NumFun)'
C       
  500 read ( 2, 20, end = 505 ) riga
      write (6,20) riga(1:Lung( riga ))
      goto 500
C       
  505 read ( 7, 20, end = 510 ) riga
      write (6,20) riga(1:Lung( riga ))
      goto 505
C       
  510 read ( 9, 20, end = 515 ) riga
      write (6,20) riga(1:Lung( riga ))
      goto 510
C
  515 ContRi = .FALSE.
      iunit = 6
      if ( PreCal ) iunit = 8  
C... ovvero, se si precalcolano le costanti
C
C
  600 if ( .not. rilegi ) then
         read ( 11, 20, end = 700 ) riga
         write( rig1, '( a )' ) ' '
      else
         read ( rig1, 20 )         riga
         rilegi = .FALSE.
      end if
      LunRig = Lung ( riga )
      if ( riga(1:1) .eq. '%' ) then
         write (iunit,20) spazio, riga(2:LunRig)
         ContRi = .TRUE.
      else if ( index( riga, 'Des_' ) .ne. 0 ) then
         write (iunit,20) spazio, riga(1:LunRig)
      else if ( riga(1:4).eq.'SIMP' .or. riga(1:4).eq.'STEP' ) then
         write (iunit,20) spazio, riga(1:LunRig)
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
         write (iunit,20) '     >', riga(2:LunRig)
       else
         write (6,20) '     >', riga(2:LunRig)
       end if
      else
         write (6,20) spazio, riga(1:LunRig)
         ContRi = .FALSE.
      end if
      goto 600
C
  700 write (6,20) spazio, 'return'
      write (6,20) spazio, 'end'
C
      write (6,20) spazio, 
     > 'subroutine Costan( NP,N_V,N_C,Des_Va,Des_Fu,SIMP,STEP )'
      write (6,20) spazio, 'implicit double precision ( a - z )'
      write (6,20) spazio, 'integer N_V, N_C, NP'
      write (6,20) spazio, 'double precision SIMP(NP,NP), STEP(NP)'
      write (6,20) spazio, 'character*50 Des_Va(N_V), Des_Fu(N_C)'
C
      rewind 2
  800 read ( 2, 20, end = 805 ) riga
      write (6,20) riga(1:Lung( riga ))
      goto 800
  805 continue
      rewind 9
  900 read ( 9, 20, end = 905 ) riga
      write (6,20) riga(1:Lung( riga ))
      goto 900
  905 continue
C       
      do 9905 iunit = 7, 8
         rewind iunit
  305    read ( iunit, 20, end = 405 ) riga
         write (6,20) riga(1:Lung( riga ))
         goto 305
  405    continue
 9905 continue
C
      write (6,20) spazio, 'return'
      write (6,20) spazio, 'end'
C
      write (6,20) spazio, 'subroutine LetDat( restart )'
      write (6,20) spazio, 'implicit double precision ( a - z )'
      write (6,20) spazio, 'logical restart'
      write (6,20) spazio, 'character tit*50, UnMis*15'
C
      do 9906 iunit = 2, 3
         rewind iunit
  300    read ( iunit, 20, end = 400 ) riga
         write (6,20) riga(1:Lung( riga ))
         goto 300
  400    continue
 9906 continue
C
      write (6,20) spazio, 'return'
      write (6,20) spazio, 'end'
C
      write ( LungTa, '( i3 )' ) 10 * iDimTa
      write (6,20) spazio, 'subroutine LetTab( restart )'
      write (6,20) spazio, 'implicit double precision ( a - z )'
      write (6,20) spazio, 'logical restart'
      write (6,20) spazio, 'character tit*50, TabInp*', 
     >                      LungTa, ', fact*10'
C
      do 9907 iunit = 9, 10
         rewind iunit
 1300    read ( iunit, 20, end = 1400 ) riga
         write (6,20) riga(1:Lung( riga ))
         goto 1300
 1400    continue
 9907 continue
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
      integer function Lung ( riga )
      character riga*132
C
      i = 132
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
$ 