!===============================================================================
!   ACTUALIZADO AL 29/OCT/2007.............21/dic/08  JAP
!   se modifico el atmosfera.f ............26/may/09  
!     se sacaron PAUSE 23/jun/2014  JAP
!     2019/mar/04: se programo para que calcule los filtros sobre la secuencia
!                  mientras cambia la masa estelar.
!-------------------------------------------------------------------------------
!                                  04/mar/2019
!-------------------------------------------------------------------------------
!     GURKA-PROGRAMMING (lease como aclaracion para correr el programa):   
!
!     SENTENCIAS QUE HAY QUE MODIFICAR (BUSCAR JAP-mod)
!
!     se le agrego el calculo del radio y la gravedad como FUNCTION. Si se las 
!     necesita compilarlas con el resto de las rutinas.
!
!     leo desde filtros.dat!!!
!
!      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
!                          ------------
!                            filtros.f 
!                          ------------
!
!
!
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (NFILT= 200)
!
      CHARACTER * 10  BAND_NAME,   AUXBAND
      CHARACTER * 15  COLOR_NAME,  AUXCHAR
      CHARACTER * 500 LONG_STRING, FORMAT_COLOR
      CHARACTER * 50  TRACK_NAME, LABEL
      CHARACTER * 10 EXTENSION
      CHARACTER * 60 ARCHIVOS_FILTROS
      DIMENSION ARCHIVOS_FILTROS(30)
      DIMENSION COLOR_NAME(NFILT), BAND_NAME(NFILT)
!
      COMMON /ABUN_He/Xe_top, tau_He
      COMMON /EVOL_AT/ TEMPE, P, DMATM, RADI, FINENV, RADIRAD, BEGENV,
     &     KATM
      COMMON /TRACK_Bin/ Tbin, Pbin, Abin, Escala_Hpg   ! 17 julio 2003
      COMMON /XEDAD/ Xedad, Gsup, Xlumi, xhy
      COMMON /COLOR_IDX/ COLOR_NAME, FORMAT_COLOR, BAND_NAME, NIND, 
     &     NBAND
      COMMON /ARCH_FILTROS/ ARCHIVOS_FILTROS, NARCH
!
      COMMON /time_0/ time0, icodetime0 ! JAP
      DIMENSION FCL(8000),w(8000)
!
      DIMENSION Xz(30)
      DATA Xz/ 0.0d0, 0.0d0 ,0.0d0, 0.0d0, 0.0d0,
     &     0.0d0, 0.0d0 ,0.0d0, 0.0d0, 0.0d0,
     &     0.0d0, 0.0d0 ,0.0d0, 0.0d0, 0.0d0,
     &     0.0d0, 0.0d0 ,0.0d0, 0.0d0, 0.0d0,
     &     0.0d0, 0.0d0 ,0.0d0, 0.0d0, 0.0d0,
     &     0.0d0, 0.0d0 ,0.0d0, 0.0d0, 0.0d0/
      REAL*8 CLUZ
      PARAMETER(CLUZ=2.99792458d10)
!
      IERROR= 0
      ICODFILTRO=0
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
      OPEN(UNIT=10, FILE='filtros.dat')
      READ(10,*) TRACK_NAME
      READ(10,*) EXTENSION
      READ(10,*) icodetime0, time0
      READ(10,*) ICODFILTRO
      CLOSE(UNIT=10)
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
c      go to 613
C      CALL GETARG(1,TRACK_NAME)
c      CALL GETARG(2,EXTENSION)
      CALL LEE_FILTROS(ARCHIVOS_FILTROS,NARCH,ICODFILTRO)
!
      DO I=1, NFILT
         COLOR_NAME(I)= '              '
         BAND_NAME(I)= '      '
      ENDDO
!-------------------------------------------------------------------------------
!     Cambiar NBAND al numero total de magnitudes que se quieren.
!     comentar los filtros que no se usan
!-------------------------------------------------------------------------------
!     NBAND= #  ! JAP-mod, cambiar este numero segun el numero
!                 de colores que se empleen, ojo seguir la numeracion
!                 correlativa
!-------------------------------------------------------------------------------
!                                       f_LANDOLT_UBVRI.dat
!-------------------------------------------------------------------------------
      IF(ICODFILTRO.EQ.1) THEN
         NBAND= 5
         BAND_NAME( 1) = 'U'    ! Landolt
         BAND_NAME( 2) = 'B'    ! Landolt
         BAND_NAME( 3) = 'V'    ! Landolt
         BAND_NAME( 4) = 'R'    ! Landolt           
         BAND_NAME( 5) = 'I'    ! Landolt           
!-------------------------------------------------------------------------------
!     continuar con la numeracion       f_SDSS_DR5.dat
!-------------------------------------------------------------------------------
      ELSEIF(ICODFILTRO.EQ.2) THEN
         NBAND= 5
         BAND_NAME( 1) = 'u'    ! SDSS  DR5
         BAND_NAME( 2) = 'g'    ! SDSS  DR5
         BAND_NAME( 3) = 'r'    ! SDSS  DR5
         BAND_NAME( 4) = 'i'    ! SDSS  DR5
         BAND_NAME( 5) = 'z'    ! SDSS  DR5
!-------------------------------------------------------------------------------
!     continuar con la numeracion        f_2MASS.dat 
!-------------------------------------------------------------------------------
      ELSEIF(ICODFILTRO.EQ.3) THEN
         NBAND= 3
         BAND_NAME( 1) = 'J_2M' ! 2MASS
         BAND_NAME( 2) = 'H_2M' ! 2MASS          
         BAND_NAME( 3) = 'Ks_2M' ! 2MASS          
!-------------------------------------------------------------------------------
!     continuar con la numeracion        HRC: High Resolution Channel
!                                        f_HST_ACS_HRC_VEGAMAG.dat
!                                        ACS: Advanced Camera for Survey
!-------------------------------------------------------------------------------
      ELSEIF(ICODFILTRO.EQ.4) THEN
         NBAND= 17
         BAND_NAME( 1) = 'F220W' ! HST ACS HRC
         BAND_NAME( 2) = 'F250W' ! HST ACS HRC
         BAND_NAME( 3) = 'F330W' ! HST ACS HRC
         BAND_NAME( 4) = 'F344N' ! HST ACS HRC
         BAND_NAME( 5) = 'F435W' ! HST ACS HRC
         BAND_NAME( 6) = 'F475W' ! HST ACS HRC
         BAND_NAME( 7) = 'F502N' ! HST ACS HRC
         BAND_NAME( 8) = 'F550M' ! HST ACS HRC
         BAND_NAME( 9) = 'F555W' ! HST ACS HRC
         BAND_NAME(10) = 'F606W' ! HST ACS HRC
         BAND_NAME(11) = 'F625W' ! HST ACS HRC
         BAND_NAME(12) = 'F658N' ! HST ACS HRC
         BAND_NAME(13) = 'F660N' ! HST ACS HRC
         BAND_NAME(14) = 'F775W' ! HST ACS HRC
         BAND_NAME(15) = 'F814W' ! HST ACS HRC
         BAND_NAME(16) = 'F850LP'! HST ACS HRC
         BAND_NAME(17) = 'F892N' ! HST ACS HRC  
!-------------------------------------------------------------------------------
!     continuar con la numeracion        WFC: Wide Field Channel
!                                        f_HST_ACS_WFC_VEGAMAG.dat
!                                        ACS: Advanced Camera for Survey
!-------------------------------------------------------------------------------
      ELSEIF(ICODFILTRO.EQ.5) THEN
         NBAND= 13
         BAND_NAME( 1) = 'F435W' ! HST ACS WFC
         BAND_NAME( 2) = 'F475W' ! HST ACS WFC
         BAND_NAME( 3) = 'F502N' ! HST ACS WFC
         BAND_NAME( 4) = 'F550M' ! HST ACS WFC
         BAND_NAME( 5) = 'F555W' ! HST ACS WFC
         BAND_NAME( 6) = 'F606W' ! HST ACS WFC
         BAND_NAME( 7) = 'F625W' ! HST ACS WFC
         BAND_NAME( 8) = 'F658N' ! HST ACS WFC
         BAND_NAME( 9) = 'F660N' ! HST ACS WFC
         BAND_NAME(10) = 'F775W' ! HST ACS WFC
         BAND_NAME(11) = 'F814W' ! HST ACS WFC
         BAND_NAME(12) = 'F850LP'! HST ACS WFC
         BAND_NAME(13) = 'F892N' ! HST ACS WFC
!-------------------------------------------------------------------------------
!     continuar con la numeracion        f_STROMGREN.dat
!-------------------------------------------------------------------------------
      ELSEIF(ICODFILTRO.EQ.6) THEN
         NBAND= 4
         BAND_NAME( 1) = 'u_st' ! Stromgren
         BAND_NAME( 2) = 'v_st' ! Stromgren
         BAND_NAME( 3) = 'b_st' ! Stromgren
         BAND_NAME( 4) = 'y_st' ! Stromgren
!-------------------------------------------------------------------------------
!     continuar con la numeracion        f_UBVRIJHKL_ber.dat
!-------------------------------------------------------------------------------
      ELSEIF(ICODFILTRO.EQ.7) THEN
         NBAND= 9
         BAND_NAME( 1) = 'U'    ! Bergeron
         BAND_NAME( 2) = 'B'    ! Bergeron
         BAND_NAME( 3) = 'V'    ! Bergeron
         BAND_NAME( 4) = 'R'    ! Bergeron
         BAND_NAME( 5) = 'I'    ! Bergeron
         BAND_NAME( 6) = 'J'    ! Bergeron
         BAND_NAME( 7) = 'H'    ! Bergeron
         BAND_NAME( 8) = 'K'    ! Bergeron
         BAND_NAME( 9) = 'L'    ! Bergeron
!-------------------------------------------------------------------------------
!     continuar con la numeracion        f_UBVRIJHKL_kur.dat
!-------------------------------------------------------------------------------
      ELSEIF(ICODFILTRO.EQ.8) THEN
         NBAND= 9
         BAND_NAME( 1) = 'U'    ! Kurucs
         BAND_NAME( 2) = 'B'    ! Kurucs
         BAND_NAME( 3) = 'V'    ! Kurucs
         BAND_NAME( 4) = 'R'    ! Kurucs
         BAND_NAME( 5) = 'I'    ! Kurucs
         BAND_NAME( 6) = 'J'    ! Kurucs
         BAND_NAME( 7) = 'H'    ! Kurucs
         BAND_NAME( 8) = 'K'    ! Kurucs
         BAND_NAME( 9) = 'L'    ! Kurucs
!-------------------------------------------------------------------------------
!     continuar con la numeracion        f_UBVRIJHKsKL.dat
!-------------------------------------------------------------------------------
      ELSEIF(ICODFILTRO.EQ.9) THEN
         NBAND= 10
         BAND_NAME( 1) = 'U'    ! Landolt
         BAND_NAME( 2) = 'B'    ! Landolt
         BAND_NAME( 3) = 'V'    ! Landolt
         BAND_NAME( 4) = 'R'    ! Landolt
         BAND_NAME( 5) = 'I'    ! Landolt
         BAND_NAME( 6) = 'J'    ! O-C magnitude
         BAND_NAME( 7) = 'H'    ! O-C magnitude
         BAND_NAME( 8) = 'Ks'   ! O-C magnitude
         BAND_NAME( 9) = 'K'    ! Bergeron
         BAND_NAME(10) = 'L'    ! Kurucs
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
      ELSE
         STOP
      ENDIF
!-------------------------------------------------------------------------
!     JAP-mod
!     Cambiar NIND al numero total de indices de color que se quieren
!     seguir la numeracion correlativa, si es =0 no calcula los indices 
!     de color (diferencias dadas en COLOR_NAME(#)) y solo calculo filtros
!-------------------------------------------------------------------------
      NIND= 7     ! NIND= 0   
!-------------------------------------------------------------------------
c      COLOR_NAME(1)= 'F435W-F814W'
c      COLOR_NAME(2)= 'F606W-F814W'
!-------------------------------------------------------------------------
!     Landolt
!
!      COLOR_NAME( 1)= 'U-B'           
!      COLOR_NAME( 2)= 'B-V'           
!      COLOR_NAME( 3)= 'V-R'           
!      COLOR_NAME( 4)= 'V-I'
!      COLOR_NAME( 5)= 'R-I'
!
!     Bergeron
!
c      COLOR_NAME( 1)= 'U-B'           
c      COLOR_NAME( 2)= 'B-V'           
c      COLOR_NAME( 3)= 'V-R'           
c      COLOR_NAME( 4)= 'V-K'
c      COLOR_NAME( 5)= 'V-I'
c      COLOR_NAME( 6)= 'R-I'
c      COLOR_NAME( 7)= 'J-H'
c      COLOR_NAME( 8)= 'H-K'
c      COLOR_NAME( 9)= 'K-L'
!
!
!     HST-HRC:
!
      COLOR_NAME( 1)= 'F250W-F435W'           
      COLOR_NAME( 2)= 'F435W-F555W'           
      COLOR_NAME( 3)= 'F435W-F606W'           
      COLOR_NAME( 4)= 'F435W-F814W'
      COLOR_NAME( 5)= 'F475W-F814W'
      COLOR_NAME( 6)= 'F555W-F814W'
      COLOR_NAME( 7)= 'F606W-F814W'
!     
!     HST-WFC:     
!     
c      COLOR_NAME( 1)= 'F435W-F555W'           
c      COLOR_NAME( 2)= 'F435W-F606W'           
c      COLOR_NAME( 3)= 'F435W-F814W'           
c      COLOR_NAME( 4)= 'F475W-F814W'
c      COLOR_NAME( 5)= 'F555W-F814W'
c      COLOR_NAME( 6)= 'F606W-F814W'
!     
!     
!      Mezcla de varios filtros y sistemas
!
C
c      COLOR_NAME( 1)= 'U-B'
c      COLOR_NAME( 2)= 'B-V'
c      COLOR_NAME( 3)= 'V-R'
c      COLOR_NAME( 4)= 'V-K'
c      COLOR_NAME( 5)= 'R-I'
c      COLOR_NAME( 6)= 'J-H'
c      COLOR_NAME( 7)= 'H-K'
c      COLOR_NAME( 8)= 'K-L'
c      COLOR_NAME( 5)= 'V-Ks'
c      COLOR_NAME( 6)= 'J-Hs'
c      COLOR_NAME( 7)= 'H-Ks'
C     COLOR_NAME(4)= 'F435W-F625W'

C
!     JAP-mod: ver aca que indices convienen poner
C================
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
      KALL   = 1     ! acerca de los resultados a guardar (con 0 no guarda)
      IDEAL  = 0     ! gas ideal (1) no-ideal (0) (p' eos.f, eoz.f)
      IBIN   = 0     ! estrella aislada (0), binaria (1)
      FDIL   = 0.0d0 ! cero para estrella no irradiada FDIL: factor de dilucion
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
C================
C
      IF(KALL.gt.0) then
C
         open(unit= 27, file='w_col.dat') ! fotometria UBVRI-JHKL
         open(unit= 29, file='w_fco.dat') ! flujo monocromatico continuo + Ly
         open(unit= 31, file='w_atc.dat') ! zonas convectivas segun tau
         open(unit= 32, file='w_amc.dat') ! zonas convectivas segun masa/cm
         open(unit= 33, file='w_lin.dat') ! anchos equivalentes de lineas
         open(unit= 34, file='w_pru.dat') ! datos de iteraciones
         open(unit= 35, file='w_fli.dat') ! flujo monocromatico con lineas
         open(unit= 37, file='w_atm.dat') ! tau, T, P, rho, Fr, Fc, grad.adiab.
         open(unit= 39, file='w_atx.dat') ! convergencia del modelo
         open(unit= 40, file='w_pob.dat') ! abundancias atomos, molec., e- 
         open(unit= 44, file='w_wwx.dat') ! prob.ocupacion H, H2, He y He+
         open(unit= 50, file='w_TPR.dat') ! tau, T, P, rho, x, m
c     open(unit= 54, file='w_pul.dat')
         open(unit=  5, file='w_err.dat') ! errores de calculo
C
         write(33,59) 'Tef ', 'Halfa','Hbeta','He3889','He5876',
     &        'He6678','He7065','He7281','+ 4688','Palfa'
c      write(33,59) 'Tef ', 'Halfa','Hbeta','Hgamma','H4 ','H5 ',
c     <                     'Palfa','Pbeta','Pgamma','P4 ','P5 '
         write(27,55)'Tef ','log g','U-B ','B-V ','V-R ','V-K ','R-I ',
     <        'J-H ','H-K ','K-L ','BC(V)' !,'M_V '
C      write(27,55)'Tef ','U-B ','B-V ','V-R ','V-K ','R-I ','J-H ',
C     <                   'H-K ','K-L ','BC(V)','M_V '
         write(39,*)'Algunos datos importantes'
         write(39,51) 'Tef  ','lg g','Y  ','gas','DF/F(%)','DT/T(%)',
     <        'Equ.Rad.','T(1) ','DT(1) ','DTer(1) ','ite'
         write(*,51) 'Tef  ','lg g','Y  ','gas','DF/F(%)','DT/T(%)',
     <        'Equ.Rad.','T(1) ','DT(1) ','DTer(1) ','ite'
         write(31,60) 'Tef  ','conv ','1%  ','20%  ','50% ','80% ',
     <        '80% ','50% ','20% ','1% ','conv '
         write(32,61) 'Tef ','log g','Y_bot ','Y_sup ','tauHe',
     <        'm(N) ','lg m_c','lg m_c','TL_c','TL_c',
     <        'q i=2','qtop ','qbot ','q i=N'

c      WRITE(54,66) '   Teff  ','   log g  ',,'   M_V  '

C      write(54,65)'Tef ','log g','edad(Gyr) ','M_V ','U-B ','B-V ',
C     <            'V-R ','V-K ','R-I ','J-H ','H-K ','K-L ','BC(V)'
 65      format(2(a9),a12,12(a7))
 51      format(a7,a7,a6,a3,a9,a9,a9,a9,2(a8),a3)
 54      format(2X,5(A9))
 55      format(a7,a6,8(a7),2(a7)) ! con log g
c     55    format(a7,8(a7),2(a7))   ! sin log g
 57      format(A7,8(A8,1X))
c     59    format(11(a7))
 59      format(a7,9(a7),2(a11),a8)
 60      format(12(a7))
 61      format(a7,a6,2(a10),a7,3(a8),2(a6),4(a8))
C     ----------------------  impresiones  ----------------------
      ENDIF
C     
      LONG_STRING= '#  Teff  '//'  log g  '//' log L/Lo '//
     &     '   Age(Gyr)   '//'    Y    '//'  BC(V) '//'   M_V  *'  ! JAP
c      FORMAT_COLOR= '(T2,F8.1,TR1,F8.5,TR1,F9.6,E13.6,'//
c     &     'TR1,E8.2,TR1,2(F7.3,TR1)*'
c      FORMAT_COLOR= '(T2,F8.0,TR1,F8.5,TR1,F9.6,E13.6,'//
c     &     'TR1,E8.2,TR1,2(F7.3,TR1)*'  ! JAP formato de escritura
      FORMAT_COLOR= '(T2,F8.0,TR1,F8.5,TR1,F9.6,TR1,F13.9,'//
     &     'TR1,E8.2,TR1,2(F7.3,TR1)*'  ! JAP formato de escritura
      LENGTH_BASE= 67

      DO I=1, NBAND
         AUXCHAR= BAND_NAME(I)
         J=1
         DO WHILE(AUXCHAR(J:J).EQ.' ')
            J= J+1
            IF (J.EQ.11) THEN
               WRITE(*,*) BAND_NAME(I),I,NBAND
               STOP 'ERROR EN INDICE COLOR (filtros.f)'
            ENDIF
         ENDDO
         IZERO= ICHAR('0')
         I0= J
         I9= INDEX(AUXCHAR(J:10),' ')-1
         IF (I9.LT.1) THEN
            I9= 15
         ELSE
            I9= I9 + I0 - 1
         ENDIF
         LENGTH= I9-I0+1
         IF (LENGTH.EQ.1) THEN
            LONG_STRING= LONG_STRING(1:INDEX(LONG_STRING,'*')-1)//
     &           '    '//AUXCHAR(I0:I9)//'   *'
         ELSEIF (LENGTH.EQ.2) THEN
            LONG_STRING= LONG_STRING(1:INDEX(LONG_STRING,'*')-1)//
     &           '   '//AUXCHAR(I0:I9)//'   *'
         ELSEIF (LENGTH.EQ.3) THEN
            LONG_STRING= LONG_STRING(1:INDEX(LONG_STRING,'*')-1)//
     &           '   '//AUXCHAR(I0:I9)//'  *'
         ELSEIF (LENGTH.EQ.4) THEN
            LONG_STRING= LONG_STRING(1:INDEX(LONG_STRING,'*')-1)//
     &           '  '//AUXCHAR(I0:I9)//'  *'
         ELSEIF (LENGTH.EQ.5) THEN
            LONG_STRING= LONG_STRING(1:INDEX(LONG_STRING,'*')-1)//
     &           '  '//AUXCHAR(I0:I9)//' *'
         ELSEIF (LENGTH.EQ.6) THEN
            LONG_STRING= LONG_STRING(1:INDEX(LONG_STRING,'*')-1)//
     &           ' '//AUXCHAR(I0:I9)//' *'
         ELSE 
            LONG_STRING= LONG_STRING(1:INDEX(LONG_STRING,'*')-1)//
     &           ' '//AUXCHAR(I0:I9)//'*'
         ENDIF
         LDL= 7 - LENGTH
         IRES= MOD(-LDL,2)
         IF (LDL.GE.0) THEN
            FORMAT_COLOR= FORMAT_COLOR(1:INDEX(FORMAT_COLOR,'*')-1)//
     &           ',F7.3,TR1*'
            LENGTH_BASE= LENGTH_BASE + 8
         ELSE
            LDL= -LDL
            FORMAT_COLOR= FORMAT_COLOR(1:INDEX(FORMAT_COLOR,'*')-1)//
     &           ',TR'//CHAR(IZERO+LDL/2+IRES)//',F7.3,TR'//
     &           CHAR(IZERO+LDL/2)//'*'
            LENGTH_BASE= LENGTH_BASE + LDL/2 + IRES + 7 + LDL/2
         ENDIF
      ENDDO

      DO I=1, NIND
         AUXCHAR= COLOR_NAME(I)
         J=1
         DO WHILE(AUXCHAR(J:J).EQ.' ')
            J= J+1
            IF (J.EQ.16) STOP 'ERROR EN INDICE COLOR (filtros.f)'
         ENDDO
         IZERO= ICHAR('0')
         I0= J
         I9= INDEX(AUXCHAR(J:15),' ')-1
         IF (I9.LT.1) THEN
            I9= 15
         ELSE
            I9= I9 + I0 - 1
         ENDIF
         LENGTH= I9-I0+1
         IF (LENGTH.EQ.3) THEN
            LONG_STRING= LONG_STRING(1:INDEX(LONG_STRING,'*')-1)//
     &           '   '//AUXCHAR(I0:I9)//'  *'
         ELSEIF (LENGTH.EQ.4) THEN
            LONG_STRING= LONG_STRING(1:INDEX(LONG_STRING,'*')-1)//
     &           '  '//AUXCHAR(I0:I9)//'  *'
         ELSEIF (LENGTH.EQ.5) THEN
            LONG_STRING= LONG_STRING(1:INDEX(LONG_STRING,'*')-1)//
     &           '  '//AUXCHAR(I0:I9)//' *'
         ELSEIF (LENGTH.EQ.6) THEN
            LONG_STRING= LONG_STRING(1:INDEX(LONG_STRING,'*')-1)//
     &           ' '//AUXCHAR(I0:I9)//' *'
         ELSE 
            LONG_STRING= LONG_STRING(1:INDEX(LONG_STRING,'*')-1)//
     &           ' '//AUXCHAR(I0:I9)//'*'
         ENDIF
         LDL= 7 - LENGTH
         IRES= MOD(-LDL,2)
         IF (LDL.GE.0) THEN
            FORMAT_COLOR= FORMAT_COLOR(1:INDEX(FORMAT_COLOR,'*')-1)//
     &           ',F7.3,TR1*'
            LENGTH_BASE= LENGTH_BASE + 8
         ELSE
            LDL= -LDL
            FORMAT_COLOR= FORMAT_COLOR(1:INDEX(FORMAT_COLOR,'*')-1)//
     &           ',TR'//CHAR(IZERO+LDL/2+IRES+1)//',F7.3,TR'//
     &           CHAR(IZERO+LDL/2)//'*'
            LENGTH_BASE= LENGTH_BASE + LDL/2 + IRES + 8 + LDL/2
         ENDIF
      ENDDO

      LONG_STRING= LONG_STRING(1:INDEX(LONG_STRING,'*')-1)
      FORMAT_COLOR= FORMAT_COLOR(1:INDEX(FORMAT_COLOR,'*')-5)//')'

      LABEL= '(T1,A*'
      IF (LENGTH_BASE .GT. 100) THEN
         LABEL= LABEL(1:INDEX(LABEL,'*')-1)//
     &        CHAR(ICHAR('0')+LENGTH_BASE/100)//'*'
      ENDIF
      L10= LENGTH_BASE - 100*(LENGTH_BASE/100)
      LABEL= LABEL(1:INDEX(LABEL,'*')-1)//
     &     CHAR(ICHAR('0')+L10/10)//'*'
      L1= L10 - 10*(L10/10)
      LABEL= LABEL(1:INDEX(LABEL,'*')-1)//
     &     CHAR(ICHAR('0')+L1)//')'


C      NO = 0
C      IF(NO.eq.1) goto 101
      Gcte = 6.6725985d-8    ! cm^3/g/s^2, constante de GRAVITACION
      R_Sol  = 6.95508d+10   ! cm, radio del Sol
      A_Sol  = 1.9891d+33   ! gs, masa del Sol
      X_Sol  = 3.8458d+33   ! erg/s, luminosidad
C --
      PI4  = 3.141592653589793d0 * 4.d0
      XLx   = -3.75915d0            ! log L/L_Sol  (dato)
      TLx   =  3.64000d0            ! log Tef      (dato)
      Amasa =  0.2389341d0*A_Sol    ! M/M_Sol      (dato)
      TE    = 10.d0**TLx            ! K, Tef
      X_Lu  = X_Sol*10.d0**XLx      ! erg/s, luminosidad
      Radio = dsqrt( X_Lu / (PI4*5.669d-5*TE**4))  ! cm
      EXO   = Radio*1.d-10          ! radio estelar en 10^10 cm
      GSUP  = Gcte*Amasa/Radio**2                 ! cm/s^2, gravedad superf.
C      RLund = dsqrt( X_Sol*10.d0**(-3.8)/(PI4*5.669d-5*3700.d0**4))  ! cm
      Xe_top = .9d0    
      tau_He = 1.d-10   
C
      OPEN(unit=52, file = track_name ,status='OLD')    
      IDOT= INDEX(TRACK_NAME,' ')-1
      NDOT= 0
      DO I=1, IDOT
         IF (TRACK_NAME(I:I) .EQ. '.') NDOT= I
      ENDDO
      IF (NDOT.EQ.0) NDOT= INDEX(TRACK_NAME,' ')-1
      OPEN(UNIT=54, FILE= TRACK_NAME(1:NDOT)//EXTENSION)
      DO I=1, NARCH
         WRITE(54,'(T1,A,A)') '#',ARCHIVOS_FILTROS(I)  ! JAP
      ENDDO

      WRITE(54,LABEL) LONG_STRING
c      READ(52,*)
c      READ(52,*)
C
cx      write(54,82) 'Teff ','log g ','log L/Lo','M/Mo ','100R/Ro',
cx     <              'Xhidrog ','edad Gyr','t(WD)Gyr'
c      read(52,'(12(/))')
      jiv = 2
      te_0= 3.d6
c      DO j = 1, 15000 
      DO I=1, 5 ! salteo las 5 lineas del encabezado del track JAP
         READ(52,*,END=232,ERR=232)
      ENDDO

c      DO WHILE(.TRUE.)  ! para que lea hasta terminar el archivo JAP
C     LEO LAS CANTIDADES DEL TRACK .TEFF
         READ(52,*,END=232,ERR=232) TE, TELOG, GLOG, EDAD6LOG, XMASS,
     &        xlumi, X1H_sup, X2H_sup, X3He_sup, X4He_sup
c
         gsup= D10X(glog) 
         radio= dsqrt(gcte*1.989d33*xmass / gsup)
!
         EX0   = Radio*1.d-10   ! radio estelar en 10^10 cm
         Xz(1)  = X1H_sup + X2H_sup ! abundancia hidrogeno
!     no uso el He3 He4 leido
         Xz(2)  = 1.d0-XHy      ! abundancia helio  (rhoHe/rho)
         Xz(6)  = 0.d0          ! abundancia carbono 
         Xz(7)  = 0.d0          ! abundancia nitrogeno 
         Xz(8)  = 0.d0          ! abundancia oxigeno 
         Xedad  = EDAD6LOG      ! en Myr
!
         xedad= xedad/1.0d3     ! lo paso a Gyr
c
C======================================================================         
 613     write(*,*) "EN 613"
c         NK=4000      
c      CALL BLACKBODY(NK,FCL,TE,W)
c         do i=1,100
c            WRITE(*,*) fcl(i)
c     enddo
         OPEN(UNIT=70, FILE="ob.txt", status="OLD")
         nk=0
         do while(.true.)
            read(70,*,end=270) w(nk), fcl(nk)
            w(nk)=10.d0**w(nk)
            fcl(nk)=10.d0**fcl(nk) !/3.1415926535d0
!            fcl(nk)= 1.d-8*Fcl(NK) !*W(NK)*W(NK)/CLUZ !Paso a frecuencia
            write(*,*) w(nk),fcl(nk)
            nk=nk+1
         enddo
c   
 270  CALL COLORES_NUEVO(Fcl,W,NK,EX0,TE,G,To,IHAKU)
      CALL FLUSH(54)
c      ENDDO                     !WHILE
c      
 232  CLOSE(54)
      CLOSE(52)
 888  FORMAT(i5,f8.0,f9.3,f12.6,1e12.3,3(f10.5),f7.2)
 233  CLOSE(53)
      STOP
      END
C======================================================================
      SUBROUTINE COLORES_NUEVO(Fcl,W,NK,EX0,TE,G,To,IHAKU)
      IMPLICIT NONE
      INTEGER NFILT, NFREQ, NCOL, NC, NF, NK, NMAXFREQ, NTOT
      INTEGER I, J, L, J1, I0, I9, IHAKU, IC
      INTEGER IPAR, IV, ITYPE
      CHARACTER *6  NAME_FILT
      CHARACTER *20 AUXCHAR
      CHARACTER *1 AUX

      REAL FILT_TABULADO, EFF_AREA, FILT_WVL, FILT_TRAN
      DOUBLE PRECISION FLUX_INTERPOL, FLUX_COLOR
      REAL CALIB_CONST
      REAL MAG_FILT
      REAL AA, DW
      REAL DILUCION, CORR
      DOUBLE PRECISION W, Fcl, EX0, TE, G, To, Y, CLUZ, SS
      PARAMETER (NFILT= 200, NF= 5000, NMAXFREQ= 8000, NTOT= NF*NFILT*2)
      PARAMETER (CLUZ = 2.99792458d10)

      DIMENSION FLUX_COLOR(NF)
      DIMENSION NAME_FILT(NFILT), NFREQ(NFILT)
      DIMENSION CALIB_CONST(NFILT), ITYPE(NFILT)
      DIMENSION W(NMAXFREQ), Fcl(NMAXFREQ), MAG_FILT(NFILT)
C     Tercer indice en TABULA_FILT. 1: Freq. 2: Transmit.
      DIMENSION FILT_TABULADO(NF,NFILT,2), FLUX_INTERPOL(NF,NFILT)
      DIMENSION FILT_WVL(NF), FILT_TRAN(NF)
      DIMENSION Y(NMAXFREQ)

      REAL COLOR, BANDA
      CHARACTER * 15 COLOR_NAME, AUXCOLOR
      CHARACTER * 10 BAND_NAME
      CHARACTER * 500 FORMAT_COLOR
      DIMENSION COLOR_NAME(NFILT), COLOR(NFILT), BAND_NAME(NFILT)
      DIMENSION BANDA(NFILT)
      INTEGER NIND, NBAND, icodetime0
      CHARACTER * 6 COL1, COL2 

      DOUBLE PRECISION DMAG_V, XEDAD, XGSUP, XLUMI, XHY, time0
      REAL BC, DMBOL

      common /time_0/ time0, icodetime0   ! JAP

C      common/colors/ u_v,b_v,v_r,r_i,v_k,h_k,dindj_h,v_i,dmag_v,bc
      COMMON /XEDAD/ Xedad, Xgsup, Xlumi, Xhy
      COMMON /COLOR_IDX/ COLOR_NAME,FORMAT_COLOR,BAND_NAME,NIND,NBAND

      COMMON /FILTROS/ FILT_TABULADO, CALIB_CONST, NAME_FILT, ITYPE, 
     &     NFREQ, NCOL, IV

      SAVE 

C ********
C Calculo las magnitudes en las distintas bandas
C ********

C
C   Rutina para calcular los colores.
C   Se utilizan los pasabandas y calibraciones de Bessell y Bessell
C   & Brett (PASP,1990,102,1181; 1988,100,1134).
C   W es la longitud de onda en Angstroms y Fcl es el flujo radiativo
C   emergente (divido por pi) con unidades  erg /(cm^2 seg Hz)
C   EX0 es el radio de la estrella en 10^10 cm.
C
C
C  Se transforma el flujo de 1/Hz a 1/A
C
C Ajuste de colores? Para single o binaria de componentes equivalentes?
      Ipar = 0
      IF(Ipar.EQ.1) EX0=1.4142D0*EX0 ! radio efectivo del sistema binario
      IF(Ipar.EQ.1) write(*,*) 'ajuste para binaria'
C      DO I=1,NK
C         Y(I)= CLUZ*1.d8*Fcl(I)/(W(I)*W(I)) ! flujo superf: erg /(cm^2 seg A)
C         AA = .25d0*CLUZ*1.d8/W(I)/W(I) ! Y=flujo astrofisico (H=Y/4.)
C      ENDDO
C
C     Calculo todas las magnitudes
C
c     DILUCION= EX0 / 3.085678D9 ! radio estelar(10^10cm)/10parsec(10^10 cm)
c      DILUCION=1.D0
c      CORR = - 2.5D0 * DLOG10(3.14159D0 * DILUCION * DILUCION)
      corr=0. ! la correcion es 0 porque ya tengo el flujo a d=10 kpc
      DO IC=1, NCOL
         DO I=1, NFREQ(IC)
c     Longitud de onda de los filtros
            FILT_WVL(I)= FILT_TABULADO(I,IC,1)
         ENDDO
         WRITE(*,*) "ITYPE(IC)",ITYPE(IC)
         IF (ITYPE(IC) .LT. 2) THEN
c no le pido que pase a longitud de onda porque ya viene asi            
            DO I=1,NK
C               Y(I)= CLUZ*1.d8*Fcl(I)/(W(I)*W(I)) ! flujo sup.:erg/(cm^2 seg A)
               Y(I)= FCL(I)
            ENDDO
            IF (ITYPE(IC) .EQ. 0) THEN
               DO I=1, NFREQ(IC)
                  FILT_TRAN(I)= FILT_TABULADO(I,IC,2)
               ENDDO
            ELSEIF (ITYPE(IC) .EQ. 1) THEN
               DO I=1, NFREQ(IC)
                  FILT_TRAN(I)= FILT_TABULADO(I,IC,2) * FILT_WVL(I)
               ENDDO
            ENDIF
         ELSEIF (ITYPE(IC) .EQ. 2) THEN
            DO I=1,NK
               Y(I)= Fcl(I)     ! flujo sup.:erg/(cm^2 seg A)
            ENDDO
            DO I=1, NFREQ(IC)
               FILT_TRAN(I)= FILT_TABULADO(I,IC,2) / FILT_WVL(I)
            ENDDO
         ELSEIF (ITYPE(IC).EQ. 3) THEN !ESTO ES POR SI LE MANDO F_LAMBDA
            DO I=1,NK
               Y(I)= Fcl(I)     ! flujo sup.:erg/(cm^2 seg A)
            ENDDO
            DO I=1, NFREQ(IC)
               FILT_TRAN(I)= FILT_TABULADO(I,IC,2) * FILT_WVL(I)
            ENDDO
         ENDIF
C      ENDDO
C         ENDIF
C
C Calculo de la magnitud
         CALL INT_FIL2(NK,W,Y,FILT_WVL,FILT_TRAN,NFREQ(IC),SS,EFF_AREA)
         MAG_FILT(IC)= -2.5*DLOG10(SS/EFF_AREA) + CORR
         MAG_FILT(IC)= MAG_FILT(IC) + CALIB_CONST(IC)
         IF (IV.EQ.0) BC= -99.9
         IF (IC.EQ.IV) THEN
            DMAG_V= MAG_FILT(IC)
            DMBOL= -2.5*XLUMI+4.75
            BC = DMBOL - DMAG_V
         ENDIF
      ENDDO

C                           *************
C ****************  ESCRITURA MAGNITUDES Y COLORES  ********************
C                           *************
      DO I=1, NBAND
         AUXCOLOR= BAND_NAME(I)
         J=1 
         DO WHILE(AUXCOLOR(J:J).EQ.' ')
            J= J+1
            IF (J.EQ.7) STOP 'ERROR NOMBRE BANDA'
         ENDDO
         I0= INDEX(AUXCOLOR(J:15),' ')+J-1
         COL1= AUXCOLOR(J:I0-1)
         I0= 0
         DO IC=1, NCOL
            IF (COL1.EQ.NAME_FILT(IC)(1:6)) I0= IC
         ENDDO
         IF (I0.NE.0) THEN
            BANDA(I)= MAG_FILT(I0)
         ELSE
            BANDA(I)= -99.9
         ENDIF
      ENDDO
      
      DO I= 1, NIND
         AUXCOLOR= COLOR_NAME(I)
         J=1 
         DO WHILE(AUXCOLOR(J:J).EQ.' ')
            J= J+1
            IF (J.EQ.16) STOP 'ERROR INDICE COLOR'
         ENDDO
         I0= INDEX(AUXCOLOR,'-')
         COL1= AUXCOLOR(J:I0-1)
         COL2= AUXCOLOR(I0+1:I0+6)
         I0= 0
         I9= 0
         DO IC=1, NCOL
            L= INDEX(NAME_FILT(IC),' ')-1
            IF (COL1(1:L).EQ.NAME_FILT(IC)(1:L)) I0= IC
            IF (COL2(1:L).EQ.NAME_FILT(IC)(1:L)) I9= IC
         ENDDO
         IF (I0.NE.0 .AND. I9.NE.0) THEN
            COLOR(I)= MAG_FILT(I0) - MAG_FILT(I9)
         ELSE
            COLOR(I)= -99.9
         ENDIF
      ENDDO
!
!     [time0]= Myr lo paso a Gyr [xedad]= Gyr
!
      if(icodetime0.eq.1)then    ! calculo la edad respecto a un t_0  JAP
         WRITE(54,FORMAT_COLOR) TE,LOG10(G),Xlumi,
     &        XEDAD-10.d0**(time0)/1.d3,1.D0-XHY,
     &        BC,DMAG_V,(BANDA(I),I=1,NBAND),(COLOR(I),I=1,NIND)
      else                     ! imprimo como viene
         WRITE(54,FORMAT_COLOR) TE,LOG10(G),Xlumi,XEDAD,1.D0-XHY,
     &        BC,DMAG_V,(BANDA(I),I=1,NBAND),(COLOR(I),I=1,NIND)
         WRITE(*,*) "U B V R",(BANDA(I),I=1,NBAND)

      endif      
 60   RETURN
c      
      END
c
C *****************************************************************
C     Lectura filtros
C *****************************************************************
      SUBROUTINE LEE_FILTROS(ARCHIVOS_FILTROS,NARCH,ICODFILTRO)
      IMPLICIT NONE
      INTEGER NC, IV, ICODFILTRO
      INTEGER I, J, L, I0, I9, K, KKL
      INTEGER NFILT, NF, NTOT
      PARAMETER (NFILT= 200, NF= 5000, NTOT= 2*NFILT*NF)

      INTEGER ITYPE, NFREQ, NCOL, NARCH
      REAL FILT_TABULADO, CALIB_CONST, ccc
      CHARACTER *6 NAME_FILT
C     Tercer indice en TABULA_FILT. 1: Freq. 2: Transmit.
      DIMENSION NAME_FILT(NFILT), FILT_TABULADO(NF,NFILT,2)
      DIMENSION ITYPE(NFILT), NFREQ(NFILT), CALIB_CONST(NFILT)

      CHARACTER *60 ARCHIVOS_FILTROS
      DIMENSION ARCHIVOS_FILTROS(30)
      CHARACTER *20 AUXCHAR
      CHARACTER *1  AUX
      CHARACTER *5  AUX5
      LOGICAL LOG_FILT
      COMMON /FILTROS/ FILT_TABULADO, CALIB_CONST, NAME_FILT, ITYPE, 
     &     NFREQ, NCOL, IV

      DO I=1, 30
         ARCHIVOS_FILTROS(I)= '   '
      ENDDO

      DO I=1, NFILT
         CALIB_CONST(I)= 0.D0
         ITYPE(I)= 0
         NAME_FILT(I)= '      '
         NFREQ(I)= 0
         DO J=1, NF
            DO L=1, 2
               FILT_TABULADO(J,I,L)= 0.D0
            ENDDO
         ENDDO
      ENDDO
      NCOL= 0
      IV= 0
      NC= 0
C************************************************************************
      NARCH= 1  ! JAP-mod       cambiar este numero segun el numero
C                 de archivos de filtros que se empleen, ojo seguir la 
C                 numeracion correlativa 
C************************************************************************
C      ARCHIVOS_FILTROS(1)= 'f_UBVRIJHKL_berccd.dat'  ! NO ESTAN
C      ARCHIVOS_FILTROS(2)= 'f_WFPC2.dat'             ! NO ESTAN
C
      IF(ICODFILTRO.EQ.1) THEN
         ARCHIVOS_FILTROS(1)= 'f_LANDOLT_UBVRI.dat' ! Landolt
      ELSEIF(ICODFILTRO.EQ.2) THEN
         ARCHIVOS_FILTROS(1)= 'f_SDSS_DR5.dat' ! SDSS  DR5
      ELSEIF(ICODFILTRO.EQ.3) THEN
         ARCHIVOS_FILTROS(1)= 'f_2MASS.dat' ! 2MASS
      ELSEIF(ICODFILTRO.EQ.4) THEN
C----------------
C         ACS: Advanced Camera for Surveys
C         WFC: Wide Field Channel
C         HRC: High Resolution Channel
C----------------
         ARCHIVOS_FILTROS(1)= 'f_HST_ACS_HRC_VEGAMAG.dat' ! HST HRC
      ELSEIF(ICODFILTRO.EQ.5) THEN
         ARCHIVOS_FILTROS(1)= 'f_HST_ACS_WFC_VEGAMAG.dat' ! HST WFC
      ELSEIF(ICODFILTRO.EQ.6) THEN
         ARCHIVOS_FILTROS(1)= 'f_STROMGREN.dat' ! Stromgren
      ELSEIF(ICODFILTRO.EQ.7) THEN
         ARCHIVOS_FILTROS(1)= 'f_UBVRIJHKL_ber.dat' ! Bergeron
      ELSEIF(ICODFILTRO.EQ.8) THEN
         ARCHIVOS_FILTROS(1)= 'f_UBVRIJHKL_kur.dat' ! Kurucs
      ELSEIF(ICODFILTRO.EQ.9) THEN
         ARCHIVOS_FILTROS(1)= 'f_UBVRIJHKsKL.dat' ! Landolt O-C Berg Kur
      ELSE
         STOP
      ENDIF
C
C
      DO K=1, NARCH
         OPEN(78,FILE='./FILTROS/'//ARCHIVOS_FILTROS(K),
     &        STATUS='OLD',ERR=112)
         DO WHILE (.TRUE.)
 23         READ(78,'(T1,A1)',ERR=110,END=110) AUX
            IF (AUX.EQ.'*') GOTO 23
            IF (AUX.EQ.'#') THEN
               NC= NC + 1
               BACKSPACE(78)
               READ(78,'(T3,A20)',ERR=110,END=110) AUXCHAR
               J= 1
               DO WHILE (AUXCHAR(J:J).EQ.' ')
                  J= J+1
                  IF (J.EQ.7) STOP 'MAL NOMBRE COLOR'
               ENDDO
               L= INDEX(AUXCHAR(J:J+7),' ')-1
               NAME_FILT(NC)= AUXCHAR(J:J+L)
               J= J+L
               DO WHILE ((AUXCHAR(J:J+2).NE.' 2 ')
     &              .AND. (AUXCHAR(J:J+2).NE.' 1 ') 
     &              .AND. (AUXCHAR(J:J+2).NE.' 0 '))
                  J=J+1
                  IF (J.EQ.21) THEN 
                     WRITE(*,*) 'FALTA TIPO FILTRO (0,1,2): ',
     &                    NAME_FILT(NC)
                     STOP 
                  ENDIF
               ENDDO
               ITYPE(NC)= ICHAR(AUXCHAR(J+1:J+1))-ICHAR('0')
C     
C     Busco indice de color V para calcular correccion bolometrica
C     
               IF (IV.EQ.0 .AND. AUXCHAR(1:2) .EQ. 'V ') THEN
                  IV= NC
               ENDIF
               
               READ(78,*,ERR=111,END=111) CALIB_CONST(NC)
               NFREQ(NC)= 0
               I0= ICHAR('0')
               I9= I0 + 9
               L= I0
               I= 0
               DO WHILE (.TRUE.)
                  READ(78,'(T1,A)',ERR=110,END=110) AUX5
                  IF (AUX5 .EQ. '     ') GOTO 113
                  BACKSPACE(78)
                  READ(78,*,ERR=111,END=111) 
     &                 FILT_TABULADO(I,NC,1),FILT_TABULADO(I,NC,2)
                  NFREQ(NC)= I
                  I= I+1
               ENDDO
 113           IF (I.GT.NF) THEN
                  WRITE(*,*) 'Too many frequencies in filter ',
     &                 NAME_FILT(NC)
                  STOP
               ENDIF
            ENDIF 
         ENDDO
 110     CLOSE(78)
      ENDDO
      NCOL= NC
      RETURN
      
 111  WRITE(*,*) 'ERROR AL LEER FILTRO: ',NAME_FILT(NC),nc
      STOP
 112  WRITE(*,*) 'ARCHIVO AUSENTE: ',ARCHIVOS_FILTROS(K)
      STOP
      END
C======================================================================      
C======================================================================
      SUBROUTINE INT_FIL2(NK,X,Y,FILT_WVL,FILT_TRAN,IMAX,SS,EFF_AREA)
C======================================================================      
      IMPLICIT NONE
      INTEGER IMAX, NFILT, NF, NMAXFREQ
      INTEGER KC, KF, NK, K
      PARAMETER (NF= 5000, NFILT= 200, NMAXFREQ= 8000)

      DOUBLE PRECISION X, Y, SS, YA, YB, AR, FF1, FF2, YFQ, DW
      REAL FILT_WVL, FILT_TRAN, XLOW, EFF_AREA

      DIMENSION X(NMAXFREQ), Y(NMAXFREQ), FILT_WVL(IMAX)
      DIMENSION FILT_TRAN(IMAX)

      Kc = 1
 1    if ( X(Kc).LT.FILT_WVL(1)*1.000001d0) then
         Kc = Kc + 1
         goto 1     ! busqueda del primer punto-flujo sobre el pasabanda
      endif
c----------
      Ya  =  dlog(Y(Kc-1))
      Yb  =  dlog(Y(Kc))
      AR  =  Ya +(Yb-Ya) * 
     &     (FILT_WVL(1)-X(Kc-1)) / (X(Kc)-X(Kc-1))
      FF2 =  DEXP(AR) * FILT_TRAN(1)
      Xlow=  FILT_WVL(1)
c----------
      SS = 0.d0
      Kf = 2
      DO K = Kc, NK
 3       if ( FILT_WVL(Kf).LT.X(K)) then
            Ya   =  dlog(Y(K-1)+1.d-199)
            Yb   =  dlog(Y(K)+1.d-199)
            AR   =  Ya +(Yb-Ya) * 
     &           (FILT_WVL(Kf)-X(K-1)) /(X(K)-X(K-1))
            FF1  =  FF2
            FF2  =  FILT_TRAN(Kf) * DEXP(AR)
            SS   =  SS + (FILT_WVL(Kf)-Xlow)*(FF1+FF2)
            Xlow =  FILT_WVL(Kf)
            Kf   = Kf + 1
            if (Kf.GT.IMAX) goto 4
            goto 3
         else
            AR   =  (X(K)-FILT_WVL(Kf-1)) 
     &           / (FILT_WVL(Kf)-FILT_WVL(Kf-1))
            YFQ  =  FILT_TRAN(Kf-1) + AR  
     &           * (FILT_TRAN(Kf)-FILT_TRAN(Kf-1)) 
            FF1  =  FF2
            FF2  =  YFQ * Y(K)
            SS   =  SS + (X(K)-Xlow)*(FF1+FF2)
          Xlow =  X(K)
         endif
      ENDDO
 4    SS= SS / 2.D0

      EFF_AREA= 0.D0
      DO K=2, IMAX
         DW= FILT_WVL(K) - FILT_WVL(K-1)
         EFF_AREA= EFF_AREA + DW * (FILT_TRAN(K) + 
     &        FILT_TRAN(K-1))
      ENDDO
      EFF_AREA= EFF_AREA / 2.D0

      RETURN
      END
C************************************************************************
C* PROCEDIMIENTO FUNCTION PARA 10^X
C************************************************************************
      DOUBLE PRECISION FUNCTION D10X(X)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      D10X=10.D0**X
      RETURN
      END
C************************************************************************
      SUBROUTINE BLACKBODY (NK,FCL,TE,W)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      dimension V(8000),W(8000),Fcl(8000)
      PARAMETER(CLUZ = 2.99792458d10) ! cm/s
      WRITE(*,*) NK,W(1)
      DO K=1,NK
         W(K)=0.D0+50.D0*DFLOAT(K)
      ENDDO
c
      open(unit=72, file="BB.DAT")
      DO K=1,NK
         V(K) = CLUZ/W(K)*1.d8
         BPP =  14.745007D0*(V(K)*1.d-16)**3
         AEW = -143878775.9d0/W(K)/TE
         EWT = DEXP(AEW)
         Fcl(K) = BPP*EWT/(1.d0-EWT)
         write(72,*) w(k),fcl(k)
      ENDDO
      close(72)
       RETURN
       END
