t importa.for
      parameter (NTabMx=10, NPunMx=10, LuNoFi=50)      
      implicit double precision ( a-h , o-z )
      double precision TabImp(NTabMx, NPunMx), array(20)
      character DscImp(NTabMx)*100, FilImp*(LuNoFi), riga*132
C
      logical InzImp, sTitCor, sTitPrc, RigCom, RigTab
C
      KolPnt = 1 ! Puntatore alla colonna iniziale
      KolAgg = 0 ! Numero di colonne da aggiungere
      NTitIm = 0 ! Numero dei titoli
      InzImp =  .true.
      sTitCor = .true.
C
      FilImp = 'Prova'
C
      open ( unit = 8, file = FilImp//'.Imp', status = 'old', 
     >       iostat = IOS, readonly )
C      
  100 read ( 8, '(a)', end = 200 ) riga
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
            TabImp(Irig,Icol) = array(i-Itab+1)
          end do     
          Itab = Itab + LunTab
          sTitPrc = .false. 
        end if
      goto 100
  200 continue
C
!      do i = 1, NTitIm
!        write ( 1, '(a)' ) DscImp(i)(1:lung(DscImp(i),100))
!      end do
!      do i = 1, NPunMx
!        write ( 1, '( 7(1x,f10.3) )' ) ( TabImp(i,j), j =1, NTitIm )
!      end do
C
      stop
!      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
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
      integer function lung ( riga, n )
      character riga*(*)
C
      i = n
      if ( .not. ( riga(i:i) .eq. ' ' .and. i .gt. 0 ) ) goto 9992
 9991 i = i - 1
      if ( riga(i:i) .eq. ' ' .and. i .gt. 0 ) goto 9991
 9992 continue
C
      lung = i
C
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine rig_num( riga, array, LunTab )
      implicit double precision ( a-h , o-z )
      double precision array(*)
      character riga*132, accum*20, car*1
C
      logical se_Spz, sSpzPrc
C
      LunRig = lung( riga, 132 )
      kTab = 0
      sSpzPrc = .true.
      write ( accum, '( 20('' '') )' )
C
      do i = 1, LunRig
        car = riga(i:i)
        sSpzPrc = se_Spz
        se_Spz = car .eq. ' '
        if ( .not. se_Spz ) then
          if ( sSpzPrc .or. i.eq.1 ) then
            call inc( kTab )
            kont = 0
          end if
          call inc( kont )
          accum(kont:kont) = car 
          if ( i.eq.LunRig ) array(kTab) = AccNum( accum, kont )
        else if ( se_Spz ) then
          if ( kont .gt. 0 ) array(kTab) = AccNum( accum, kont )
        end if
      end do
      LunTab = kTab
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
$ 