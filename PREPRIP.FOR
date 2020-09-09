      parameter ( LunAsci=20000, LuNoFi=30 )
      character FilInp*(LuNoFi), FilOut*(LuNoFi), asci*(LunAsci)
      character riga*132, opis*50, UnMis*15, defaul*10, fact*10,
     >          k_*2, SN*1, NomPar*6, EOL*5, Param*6, AtPar*7
C
      logical NotAdj, DueCicli, AlfNum
C
      NotAdj( i, j ) = abs ( i - j ) .ne. 1  
C --> Serve ad indicare se due ';' non sono adiacenti.
C
      EOL = '}'
      LunEOL = lung( EOL, 5 )
C
    1 write(*,*) ' '
      write(*,2) 'TerNot'
    2 format ( ' Nome del file delle equazioni (D = ', a, ' ) ? ', $ )
      read(*, '(a)' ) FilInp
      if ( FilInp .eq. ' ' ) FilInp = 'TerNot'
C      
      open ( unit = 1, file = FilInp//'.eqn', status = 'old', 
     >       iostat = IOS, readonly )
      if ( IOS .ne. 0 ) then
      write(*,*) ' Il file non esiste, o il nome e'' sbagliato; riprova'
      goto 1
      end if
C
      FilOut = FilInp
      DueCicli = .false.
      write (*,*) ' '
      write (*,'(''Un ciclo o Due cicli annidati ? (D=1/2) '',$)')
      read (*,'(a)' ) SN
      if ( SN .eq. '2' ) DueCicli = .true.
C
      write ( asci, '(a)' ) ' '
      kasci = 1
 2000 continue
        write ( riga, '(a)' ) ' '
        read ( 1, '(a)', end=2005 ) riga
        LunRig = lung( riga, 132 )
        riga = riga(:LunRig)//EOL(:LunEOL)
        asci = asci(:kasci)//riga(:LunRig+LunEOL)
        kasci = kasci + LunRig + LunEOL
        goto 2000
 2005 continue
c
 3000 write (*,*) ' '
      write (*,'(''Parametro da far variare? (Per finire: . ) '', $ )')
      read (*,'(a)' ) Param
      if ( Param .eq. '.' ) goto 4000
      k_ = 'j_'
      if ( DueCicli ) then
        write 
     >  (*,'(''Da variare nel ciclo Interno o Esterno? (D=I/E) '', $)')
        read (*, '(a)') SN
        if ( SN .eq. 'E' .or. SN .eq. 'e' ) k_ = 'i_'
      end if
      LunPar = lung( Param, 6 )
      AtPar = '@'//Param(:LunPar)
      kk = 1
C
 5000 Indx = index( asci(kk:kasci), AtPar(:LunPar+1) )
      if ( Indx .eq. 0 ) then
        write (*, '(''Il Parametro '', a, '' non esiste. '')' ) Param
        goto 3000
      end if
      IndPar = Indx + kk
      icar = ichar( asci(IndPar+LunPar:IndPar+LunPar) )
      if ( AlfNum( icar ) ) then
        kk = IndPar + LunPar
        goto 5000
      end if
      FilOut = FilOut(:lung(FilOut,LuNoFi))//'-'//Param
      IndEOL = index( asci(IndPar:), EOL(:LunEOL) ) + IndPar - 1
      riga = asci(IndPar:IndEOL)
C
      call AddPV( 5 )
      NomPar = ' '
      opis = ' '
      UnMis = ' '
      defaul = ' '
      fact = ' '
      ipv1 = index ( riga, ';' )
      ipv2 = ipv1 + index ( riga( ipv1+1 : ), ';' ) 
      ipv3 = ipv2 + index ( riga( ipv2+1 : ), ';' )
      ipv4 = ipv3 + index ( riga( ipv3+1 : ), ';' )
      ipv5 = ipv4 + index ( riga( ipv4+1 : ), ';' )
C
      NomPar = riga(:ipv1-1)
      if ( NotAdj( ipv1, ipv2 ) ) then
        opis = riga(ipv1+1:ipv2-1)
      else
        opis = NomPar
      end if
      if ( NotAdj( ipv2, ipv3 ) ) defaul = riga ( ipv2+1 : ipv3-1 )
      if ( NotAdj( ipv3, ipv4 ) )  UnMis = riga ( ipv3+1 : ipv4-1 )
      if ( NotAdj( ipv4, ipv5 ) )   fact = riga ( ipv4+1 : ipv5-1 )
C
      asci = asci(:IndPar-2)//'#'//NomPar(:lung(NomPar,6))
     >       //';'//opis(:lung(opis,50))//' ('//UnMis(:lung(UnMis,15))
     >       //') '//fact(:lung(fact,10)-LunEOL)//EOL//' '
     >       //defaul(:lung(defaul,10))//asci(IndEOL:)
      kk = 1
C
 6000 continue
        Indx = index1( asci(kk:), Param(:LunPar) )
        if ( Indx .eq. 0 ) goto 3000
        IndPar = abs( Indx ) + kk - 1
        if ( Indx .gt. 0 )
     >   asci = asci(:IndPar+LunPar-1)//'('//k_//')'//
     >          asci(IndPar+LunPar:)
        kk = IndPar + LunPar
        goto 6000
C
 4000 continue
      open ( unit=2, file=FilOut//'.eqn', status='new' )
C
      kk = 1
 7000 continue
        Indx = index( asci(kk:), EOL(:LunEOL) )
        if ( Indx .eq. 0 ) stop
        IndEOL = Indx + kk - 1
        riga = asci(kk:IndEOL-LunEOL)
        write ( 2, '(a)' ) riga(:lung(riga,132))
        kk = IndEOL + LunEOL
        goto 7000
C
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
      integer function index1 ( String, Str )
      character String*(*), Str*(*)
      logical AlfNum
      index1 = index( String, Str )
      LunStr = lung( Str, 6 )
      icar1 = ichar( String(index1-1:index1-1) )
      icar2 = ichar( String(index1+LunStr:index1+LunStr) )
      if ( String(index1-1:index1-1).eq.'#' .or.
     >     AlfNum(icar1) .or. AlfNum(icar2) ) index1 = - index1
      return
      end
C
C----------------------------------------------------------------------
C
      logical function AlfNum ( icar )
      AlfNum = 48.le.icar .and. icar.le.57 .or
     >         65.le.icar .and. icar.le.90 .or.
     >         97.le.icar .and. icar.le.122
      return
      end
C
