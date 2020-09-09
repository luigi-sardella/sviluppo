t sim-agg.for
C
C       *********************************************************
C       *                                                       *
C       *   Questo programma aggiorna i valori suggeriti e le   *
C       *   descrizioni delle variabili contenuti nel file      *
C       *   delle equazioni, in base a quanto e' correntemente  *
C       *   contenuto nel file dei dati ( che deve avere lo     *
C       *   stesso nome )                                       *
C       *                                                       *
C       *********************************************************
C
      character riga*100, tit*50, UnMis*15, NmPara*6, fact*10, 
     >          FilFor*9, sentin*1, defaul*10
C
      logical NotAdj
C
      NotAdj( i, j ) = abs ( i - j ) .ne. 1  
C --> Serve ad indicare se due ';' non sono adiacenti.
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
      open ( unit = 2, file = FilFor//'.dat', status = 'old', 
     >       iostat = IOS )
      if ( IOS .ne. 0 ) then
        write(*,*) ' Non esiste il file dei dati '
        stop
      end if
C
      open ( unit = 3, file = FilFor//'.eq1', status = 'new' )
C
  100 read ( 1, '( a1, a )', end = 200 ) sentin, riga
C
      if ( sentin .eq. '@' ) then
C
         npv = 0
         lun = 0
         do 9900 i = 80, 1, -1
            if ( riga(i:i) .ne. ' ' .and. lun .eq. 0 ) lun = i
            if ( riga(i:i) .eq. ';' ) npv = npv + 1
 9900    continue
C
         if ( npv .lt. 5 ) then
            do 9901 i = 1, 5 - npv
 9901       riga( lun+i : lun+i ) = ';'
         end if
C
         NmPara = ' '
         tit    = ' '
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
         NmPara = riga( 1 : ipv1-1 )
C
         if ( NotAdj( ipv1, ipv2 ) ) then
            tit = NmPara // ':' // riga ( ipv1+1 : ipv2-1 )
         else
            tit = NmPara
         end if
         if ( NotAdj( ipv2, ipv3 ) ) defaul = riga ( ipv2+1 : ipv3-1 )
         if ( NotAdj( ipv3, ipv4 ) )  UnMis = riga ( ipv3+1 : ipv4-1 )
         if ( NotAdj( ipv4, ipv5 ) )   fact = riga ( ipv4+1 : ipv5-1 )
C
         read( defaul, '( 1f )' ) q_prim
C
  300    read( 2, 210, err = 300, end = 400 ) q, tit
  210    format ( 1x, 1f10.0, 1x, a )
         if ( NmPara .eq. tit(1:6) ) then
            write (riga, 220) tit(1:6), ';', tit(8:50), ';',
     >             q, ';', UnMis, ';', fact
            if( q .ne. q_prim ) 
     >      write ( *, 220)   tit(1:6), ';', tit(8:50), ';',
     >             q, ';', UnMis, ';', fact
  220       format( 4a, 1pe10.3, 4a )
         else
            goto 300
         end if
C
      end if
C
      write ( 3, '( a1, a )' ) sentin, riga( 1 : Lung( riga ) )
C
      goto 100
C
  200 continue
C
      stop
C
  400 write(*,*) ' Fine del file dei dati; si cercava la variabile ', 
     >           NmPara
      stop
C
   20 format ( 20a )
C
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      integer function Lung ( riga )
C
      character riga*100
C
      i = 100
      if ( .not. ( riga(i:i) .eq. ' ' .and. i .gt. 1 ) ) goto 9992
 9991 i = i - 1
      if ( riga(i:i) .eq. ' ' .and. i .gt. 1 ) goto 9991
 9992 continue
C
      Lung = i
C
      return
      end
$ 