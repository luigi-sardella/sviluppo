t curve-y.for
      parameter (NTabMx=40, I_max=10, NPunMx=40, LuNoFi=50)      
      implicit double precision ( a-h , o-z )
      double precision TabRis(NTabMx, I_max*I_max), 
     >                 TabImp(NTabMx, NPunMx)
      dimension NPImp(NTabMx), iAbbin(NTabMx), array(40) 
      character DscRis(NTabMx)*100, DscImp(NTabMx)*100, EI*1
      character FilRis*(LuNoFi), Est*3, SK*132, data*10, ora*8
      logical trovat, PrmVlt, asseZ, Msurat, AsZext, CalCer
      logical InzImp, sTitCor, sTitPrc, RigCom, RigTab
C
      real*4 Wid, Alt
      data Wid / 36. /, Alt / 26. /
C
      i_j_( Im, i, j ) = Im * ( j - 1 ) + i
C
      call DATE( data )
      call TIME( ora )
C
      do i = 1, NTabMx
        iAbbin(i) = 0
      end do
C
    1 write ( *, * ) ' '
      write ( *, '('' Archivio dei risultati ? '', $ )' ) 
      read  ( *, '(a)' ) FilRis
      write ( *, * ) ' '
      write ( *, '('' Tipo dell"archivio (D = ris) ? '', $ )' ) 
      read  ( *, '(a)' ) Est
      if ( Est .eq. ' ' ) Est = 'ris'
C      
      open ( unit = 1, file = FilRis//'.'//Est, status = 'old', 
     >       iostat = IOS, readonly )
      if ( IOS .ne. 0 ) then
      write(*,*) ' Il file non esiste, o il nome e'' sbagliato; riprova'
      goto 1
      end if
C
      open ( unit = 2, file = FilRis//'.Imp', status = 'old', 
     >       iostat = IOS, readonly )
      Msurat = IOS .eq. 0
C
      PrmVlt = .true.
      trovat = .false.
   20 continue
        read ( 1, '( a )', end = 30 ) SK
        LunRig = Lung( SK, 132 )
        Ident = index( SK(1:132), 'Identificazione :' )
        if ( Ident .gt. 0 ) then
          NRis = 0
          trovat = .true.
          read( SK(Ident+17:LunRig), '(bn,i5)' ) Idnt
          i_ = max( 1, Idnt / 100 )
          j_ = mod( Idnt, 100 )
          I_rip = max( I_rip, i_ )
          J_rip = max( J_rip, j_ )
        else if ( LunRig .gt. 0 .and. trovat .and. PrmVlt ) then
          NRis = NRis + 1
          I2P = index( SK(1:LunRig), ':' )
          DscRis( NRis ) = SK( I2P+1 : LunRig )
          read (SK(I2P-12:I2P-3),'(f10.0)') 
     >          TabRis(NRis,i_j_(I_max,i_,j_)) 
        else if ( LunRig .gt. 0 .and. trovat .and. .not. PrmVlt ) then
          NRis = NRis + 1
          I2P = index( SK(1:LunRig), ':' )
          read (SK(I2P-12:I2P-3),'(f10.0)') 
     >          TabRis(NRis,i_j_(I_max,i_,j_)) 
        else if ( LunRig .eq. 0 .and. trovat ) then
          PrmVlt = .false.
          trovat = .false.
          write ( *, '( '' Identificazione : '', i4 )' ) Idnt
          do i = 1, NRis
            write ( *, '( '' #'', i2, '' : '', 1pe11.4, a )' ) i, 
     >      TabRis(i,i_j_(I_max,i_,j_)),DscRis(i)(1:lung(DscRis(i),100))
          end do
        end if
      goto 20
C
   30 continue
C
      asseZ = I_rip .gt. 1 .and. J_rip .gt. 1
C
      write(*, *) ' '
   40 write(*,'('' Indice (#) della quantita" in ascissa : '', $ )' )
      read (*, *) iAscRi
      if ( iAscRi.lt.1 .or. iAscRi.gt.NRis ) then
        write (*, '('' Sbagliato '')' )
        goto 40
      end if
      if ( asseZ ) then
   50   write(*,'('' Indice (#) della quantita" in asse Z  : '', $ )' )
        read (*, *) iAsseZ
        if (iAsseZ.lt.1 .or. iAsseZ.gt.NRis .or. iAsseZ.eq.iAscRi) then
          write (*, '('' Sbagliato '')' )
          goto 50
        end if
        write(*,
     >       '('' La quantita" da porre in asse Z la si e" variata'')')
        write(*,
     >       '('' nel ciclo esterno (preferito; indice i_) '')' )
        write(*,
     >'('' oppure in quello interno (indice j_) ? ( E=D / I ) '', $ )' )
        read (*, '(a)' ) EI
        if ( EI.eq.'I' .or. EI.eq.'i' ) then
          AsZext = .false.
          NPris = I_rip
          NCris = J_rip
        else
          AsZext = .true.
          NPris = J_rip
          NCris = I_rip
        end if
      else
        AsZext = .true.
        NPris = max( I_rip, J_rip )
        NCris = 1
      end if
C
      write(*,*) ' '
      write(*,
     >     '('' Come mostrare le varie grandezze :'')')
      write(*, '( a )' )
     >' 1 = calcolate, con curva continua; importate, coi cerchietti '// 
     >' ( preferito ) ' 
      write(*, '( a, $ )' )
     >' 2 = calcolate, coi cerchietti; importate, con curva continua '//
     >'... ? ' 
      read (*, '(a)' ) EI
      CalCer = EI .eq. '2'
C 
      if ( Msurat ) then
C
        KolPnt = 1 ! Puntatore alla colonna iniziale
        KolAgg = 0 ! Numero di colonne da aggiungere
        NTitIm = 0 ! Numero dei titoli
        InzImp =  .true.
        sTitCor = .true.
C
  100   read ( 2, '(a)', end = 200 ) SK
C
        if ( RigCom( SK ) ) goto 100
C
        sTitCor = .not. RigTab( SK )
        if ( InzImp .and. .not. sTitCor ) then
          write (*,'(a)') ' --> Errore :'
          write (*,'(a)') '     L''archivio ', FilRis//'.imp', ' deve'
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
          DscImp( NTitIm ) = SK
          sTitPrc = .true.
        else if ( .not. sTitCor ) then
          if ( sTitPrc ) then
            Itab = 1
            KolPnt = KolPnt + KoAgPr
          end if
          call rig_num( SK, array, LunTab )
          do i = Itab, Itab + LunTab - 1 
            Irig = ( i - 1 ) / KolAgg + 1
            if ( KolAgg .lt. 2 ) then
              Icol = KolPnt 
            else
              Icol = mod( i-1, KolAgg ) + KolPnt 
            end if
            TabImp(Icol,Irig) = array(i-Itab+1)
          end do     
          Itab = Itab + LunTab
          sTitPrc = .false. 
        end if
        NImp = max( NImp, Irig )
        goto 100
200     continue
C
        write ( *, * ) ' '
        do i = 1, NRis
          if ( i .eq. iAscRi ) then
            write ( *, '( '' #'', i2, '' : '', a )' ) 
     >        i, DscRis(i)(1:lung(DscRis(i),100))//'; ---> in ascissa'
          else if ( asseZ .and. i .eq. iAsseZ ) then
            write ( *, '( '' #'', i2, '' : '', a )' ) 
     >        i, DscRis(i)(1:lung(DscRis(i),100))//'; ---> asse Z'
          else
            write ( *, '( '' #'', i2, '' : '', a )' ) 
     >        i, DscRis(i)(1:lung(DscRis(i),100))
          end if
        end do
C
        write ( *, * ) ' '
        do i = 1, NTitIm
           write(*,'( '' %'', i2, '' : '', a )') 
     >        i, DscImp(i)(1:lung(DscImp(i),100))
        end do
C
        write(*, *) ' '
   90   write(*,'('' Indice (%) della misura in ascissa : '', $ )' )
        read (*, *) iAscIm
        if ( iAscIm.lt.1 .or. iAscIm.gt.NTitIm ) then
          write (*, '('' Sbagliato '')' )
          goto 90
        end if
C
        write ( *, * ) ' '
        do i = 1, NRis
          if ( i .eq. iAscRi ) then
            write ( *, '( '' #'', i2, '' : '', a )' ) 
     >         i, DscRis(i)(1:lung(DscRis(i),100))//'; ---> in ascissa'
          else if ( asseZ .and. i .eq. iAsseZ ) then
            write ( *, '( '' #'', i2, '' : '', a )' ) 
     >         i, DscRis(i)(1:lung(DscRis(i),100))//'; ---> asse Z'
          else
            write ( *, '( '' #'', i2, '' : '', a )' ) 
     >            i, DscRis(i)(1:lung(DscRis(i),100))
          end if
        end do
C
        write ( *, * ) ' '
        do i = 1, NTitIm
          if ( i .eq. iAscIm ) then
            write(*,'( '' %'', i2, '' : '', a )') 
     >         i, DscImp(i)(1:lung(DscImp(i),100))//'; ---> in ascissa'
          else
            write(*,'( '' %'', i2, '' : '', a )') 
     >            i, DscImp(i)(1:lung(DscImp(i),100))
          end if
        end do
C
        write ( *, * ) ' '
        write ( *, * ) ' Abbinamenti fra risultati e misure :'
  101   write(*,'('' Indice (#) del risultato (0 per finire): '', $ )' )
        read (*, *) n_abb
        if ( n_abb .eq. 0 ) goto 120
        if ( n_abb.lt.1 .or. n_abb.gt.NRis .or. 
     >       n_abb.eq.iAscRi .or. n_abb.eq.iAsseZ ) then
          write (*, '('' Sbagliato '')' )
          goto 101
        end if
  110   write(*,'('' Indice (%) della misura : '', $ )' )
        read (*, *) m_abb
        if ( m_abb.lt.1.or.m_abb.gt.NTitIm.or.m_abb.eq.iAscIm ) then
          write (*, '('' Sbagliato '')' )
          goto 110
        end if
        iAbbin( n_abb ) = m_abb
        goto 101
  120   continue     
C
      end if
C
      call Risova( FilRis, Wid, Alt, data, ora, I_rip, J_rip, 
     >             nRipMx, LuNoFi,
     >             TabRis, TabImp, DscRis, DscImp, iAbbin, AsseZ,
     >             AsZext, Msurat, iAscRi, iAscIm, iAsseZ, NRis, 
     >             NCris, NPris, NTitIm, NImp, NtabMx, NPunMx, 
     >             I_max, CalCer )
C
      stop
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      subroutine Risova( FilRis, Wid, Alt, data, ora, I_rip, J_rip, 
     >                   nRipMx, LuNoFi,
     >                   TabRis, TabImp, DscRis, DscImp, iAbbin, AsseZ,
     >                   AsZext, Msurat, iAscRi, iAscIm, iAsseZ, NRis, 
     >                   NCris, NPris, NTImp, NImp, NtabMx, NPunMx, 
     >                   I_max, CalCer )
      implicit double precision ( a-h, o-z )
      double precision mantis
C
      include 'sys$library:uisentry'
      include 'sys$library:uisusrdef'
      integer vd_id, wd_id, vcm_id, vcm_size
      real R_vett(8), G_vett(8), B_vett(8), Wid, Alt
C
      data R_vett / 1.0, 0.0, 0.0 , 0.0, 1.0 , 0.8, 0.8 , 0.2 /
      data G_vett / 1.0, 0.0, 0.0 , 0.5, 0.0 , 0.0, 0.6 , 0.7 /
      data B_vett / 1.0, 0.0, 1.0 , 0.2, 0.0 , 0.9, 0.0 , 0.7 /
C     
      data vcm_size / 9 /, Dmarg / 20.D0 /
      data AltCar / 0.5D0 /, WidCar / 0.25D0 /, Gradua / 0.03D0 /
C
      double precision TabRis(NTabMx, * ), TabImp(NTabMx, NPunMx)
      dimension iAbbin(NTabMx) 
      character DscRis(NTabMx)*100, DscImp(NTabMx)*100, EI*1
      character FilRis*(*), Est*3, SK*132
      character scritt*150, ascis*150, XmaxA*10, XminA*10, GraTxt*5
      character TitFin*80, Esten*3, data*10, ora*8
      character sino*1, ScaNum*10, AsZnum*10
C
      logical crossX, prt, seFFT, GraLog, BasLin, NewPar
      logical AsseZ, Msurat, AsZext, CalCer
C
      vcm_id = uis$create_color_map( vcm_size )
C
       Xmin = TabRis ( iAscRi, iTabRi(I_max, 1, 1, AsZext) )
       Xmax = Xmin
       do iPun = 2, NPris
         do iCur = 1, NCris
           k_ = iTabRi(I_max, iCur, iPun, AsZext)
           if ( TabRis(iAscRi,k_) .gt. Xmax ) Xmax=TabRis(iAscRi,k_) 
           if ( TabRis(iAscRi,k_) .lt. Xmin ) Xmin=TabRis(iAscRi,k_) 
         end do
       end do
       if ( Msurat ) then
         do iPun = 1, NImp
           if ( TabImp(iAscIm,iPun).gt.Xmax ) Xmax=TabImp(iAscIm,iPun) 
           if ( TabImp(iAscIm,iPun).lt.Xmin ) Xmin=TabImp(iAscIm,iPun) 
         end do
       end if
C
       Xmarg = ( Xmax - Xmin ) / Dmarg
       Raggio = Gradua * ( Xmax - Xmin )
       Xmax = Xmax + Raggio 
       Xmin = Xmin - Raggio 
       write( XmaxA, '( 1PD10.3 )' ) Xmax
       write( XminA, '( 1PD10.3 )' ) Xmin
C
       scritt = DscRis (iAscRi)
       ascis = ' In ascissa : '//scritt(1:lung(scritt,150)) 
     >          //'; da '//XminA//' a '//XmaxA
C
       do i = 1, NRis
C
         if ( i.eq.iAscRi .or. i.eq.iAsseZ ) goto 600
C
         if ( Msurat ) IndImp = iAbbin( i )
         Amax = 0.D0
         Amin = 0.D0
         CurMax = 0.D0
         CurMin = 0.D0
         crossX = .FALSE.
C
         do iCur = 1, NCris
           do iPun = 1, NPris
             k_ = iTabRi(I_max, iCur, iPun, AsZext)
             Aji = TabRis( i, k_ )
             if (Aji .gt. CurMax) CurMax = Aji
             if (Aji .lt. CurMin) CurMin = Aji
           end do
           if ( CurMax .gt. Amax ) Amax = CurMax
           if ( CurMin .lt. Amin ) Amin = CurMin
           CurMax = 0.D0
           CurMin = 0.D0
         end do ! ( iCur )
C
         if ( Msurat ) then
           do iPun = 1, NImp
             Aji = TabImp( IndImp, iPun )
             if (Aji .gt. CurMax) CurMax = Aji
             if (Aji .lt. CurMin) CurMin = Aji
           end do
           if ( CurMax .gt. Amax ) Amax = CurMax
           if ( CurMin .lt. Amin ) Amin = CurMin
         end if
         Amarg = (Amax-Amin) / 30.D0
         Amax = Amax + Amarg
         Amin = Amin - Amarg
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
         Ymin = 0.D0
         in_bas = 0
         if ( crossX ) then
           in_bas = abs(int( 10.D0*elacs/scale )) + 1
           Ymin = -0.1D0 * float( in_bas )
         end if
         TitFin = FilRis(1:Lung(FilRis,LuNoFi)) // DscRis(i)
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
         call uis$text ( vd_id, 0, ' ' )
         call uis$new_text_line ( vd_id, 0 )
C
         do iCur = 1, NCris
C
           icolor = mod( iCur, 8 ) + 1
           k_ = iTabRi(I_max, iCur, 1, AsZext)
           X1 = TabRis ( iAscRi, k_ )
           Aji = TabRis( i, k_ )
           Y1 = Aji / scale
           if ( CalCer ) then
             call uis$circle ( vd_id, icolor, X1, Y1, Raggio/9.D0)
             call uis$circle ( vd_id, icolor, X1, Y1, Raggio/13.D0)
             call uis$circle ( vd_id, icolor, X1, Y1, Raggio/18.D0)
           end if
           do iPun = 2, NPris
             k_ = iTabRi(I_max, iCur, iPun, AsZext)
             X2 = TabRis( iAscRi, k_ )
             Aji = TabRis( i, k_ )
             Y2 = Aji / scale
             if ( .not. CalCer ) then
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
           LDscRi = lung( DscRis(i), 100 )
           if ( AsseZ ) then
             LDscZ  = lung( DscRis(iAsseZ), 100 )
             write( AsZnum, '(1PD10.3)' ) 
     >         TabRis(iAsseZ,iTabRi(I_max,iCur,1,AsZext))
             if ( iCur.eq.1 ) then
C              scritt = DscRis(i)(1:LDscRi)//' @ '// 
               scritt = DscRis(i)(1:  4   )//' @ '// 
     > DscRis(iAsseZ)(1:LDscZ)//' = '//AsZnum//' Scala : '//ScaNum
             else
C              do ii = 1, LDscRi
               do ii = 1, 4
                 write ( scritt(ii:ii), '(a)' ) ' '
               end do
               ii = LDscRi / 3
               write ( scritt(ii:ii), '(a)' ) '"'
               ii = LDscRi / 3 * 2
               write ( scritt(ii:ii), '(a)' ) '"'
C              scritt = scritt(1:LDscRi)//' @ '//DscRis(iAsseZ)(1:LDscZ)
               scritt = scritt(1:  4   )//' @ '//DscRis(iAsseZ)(1:LDscZ)
     >         //' = '//AsZnum
             end if
           else
             LDscRi = lung( DscRis(i), 100 )
             scritt = DscRis(i)(1:LDscRi)
             if ( iCur.eq.1 ) 
     >         scritt = scritt(1:lung(scritt,150))//' Scala : '//ScaNum
           end if
C
           call uis$text ( vd_id, icolor, scritt )
           call uis$new_text_line ( vd_id, icolor )
C
         end do ! ( iCur )
C
         if ( Msurat ) then
C
           icolor = 2
           X1 = TabImp ( iAscIm, 1 )
           Aji = TabImp( IndImp, 1 )
           Y1 = Aji / scale
           if ( .not. CalCer ) then
             call uis$circle ( vd_id, icolor, X1, Y1, Raggio/9.D0)
             call uis$circle ( vd_id, icolor, X1, Y1, Raggio/13.D0)
             call uis$circle ( vd_id, icolor, X1, Y1, Raggio/18.D0)
           end if
           do iPun = 2, NImp
             X2 = TabImp ( iAscIm, iPun )
             Aji = TabImp( IndImp, iPun ) 
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
           LDscIm = lung( DscImp(IndImp), 100 )
           scritt = ' '//DscImp(IndImp)(1:LDscIm)
C
           call uis$text ( vd_id, icolor, scritt )
           call uis$new_text_line ( vd_id, icolor )
C
         end if
C
         write( scritt, '( 150a )' ) ( ' ', k = 1, 150 )
         write( scritt, '( 150a )' ) ( '-', k = 1, LunTit )
         call uis$new_text_line ( vd_id, 0 )
         call uis$text ( vd_id, 0, ' '//scritt )
         call uis$new_text_line ( vd_id, 0 )
         call uis$text ( vd_id, 0, ' '//TitFin(1:LunTit) )
         call uis$new_text_line ( vd_id, 0 )
         call uis$text ( vd_id, 0, ' '//scritt )
         call uis$new_text_line ( vd_id, 0 )
         call uis$text ( vd_id, 0, ' '//data//' '//ora )
         write( scritt, '( 150a )' ) ( ' ', k = 1, 150 )
         write( scritt, '( 150a )' ) ( '-', k = 1, 19 )
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
       write( *, '( '' Premere RETURN per uscire '' )' )
       read ( *, '( a )' ) 
       stop
C
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      integer function iTabRi(I_max, iCur, iPun, AsZext)
      logical AsZext
      if ( AsZext ) then
        iTabRi = I_max * ( iPun - 1 ) + iCur
      else
        iTabRi = I_max * ( iCur - 1 ) + iPun
      end if
      return
      end
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C
      double precision function mantis( x1 )
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
      subroutine rig_num( riga, array, LunTab )
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