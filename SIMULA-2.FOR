C
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