      PROGRAM MAGNITUDES
      IMPLICIT NONE
! PROGRAMA TOMADO DE atmosfera.f del codigo de Rene Rohrmann.
C     Calculo todas las magnitudes
C
C     DEFINO CANTIDADES UTILES PARA CALCULAR LAS MAG.
      gsup= D10X(glog) 
      radio= dsqrt(gcte*1.989d33*xmass / gsup)
      EX0   = Radio*1.d-10      ! radio estelar en 10^10 cm
c
      ICODFILTRO=0
c     Para leer los pasabanda de los filtros
      CALL LEE_FILTROS(ARCHIVOS_FILTROS,NARCH,ICODFILTRO)
c     FCL= FLUJO EMERGENTE CONTINUO + LINEAS
C     
      DILUCION= EX0 / 3.085678D9 ! radio estelar(10^10cm)/10parsec(10^10 cm)
      CORR = - 2.5D0 * DLOG10(3.14159D0 * DILUCION * DILUCION)
      DO IC=1, NCOL
         DO I=1, NFREQ(IC)
c     Longitud de onda de los filtros
            FILT_WVL(I)= FILT_TABULADO(I,IC,1)
         ENDDO
         IF (ITYPE(IC) .LT. 2) THEN
            DO I=1,NK
!CREO QUE ESTE DEBERIA SER EL FLUJO QUE LEO               
!               Y(I)= CLUZ*1.d8*Fcl(I)/(W(I)*W(I)) ! flujo sup.:erg/(cm^2 seg A)
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
         ENDIF
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
      endif


c      WRITE(54,FORMAT_COLOR) TE,LOG10(G),Xlumi,XEDAD,1.D0-XHY,
c     &     BC,DMAG_V,(BANDA(I),I=1,NBAND),(COLOR(I),I=1,NIND)


c      WRITE(54,*) TE,LOG10(G),Xlumi,XEDAD,1.D0-XHY,

c      WRITE(*,FORMAT_COLOR) TE,LOG10(G),Xlumi,XEDAD,1.D0-XHY,
c     &     BC,DMAG_V,(BANDA(I),I=1,NBAND),(COLOR(I),I=1,NIND)
      
 60   RETURN
      
      END

C                           *************
C***************************   INF_FIL   ****************************      
C                           *************
      SUBROUTINE INT_FIL2(NK,X,Y,FILT_WVL,FILT_TRAN,IMAX,SS,EFF_AREA)
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
c     Integra por trapecios
c     
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
c     FILT_WVL()K ES LA FUNCION DE TRANSMISION S_lambda
      EFF_AREA= 0.D0
      DO K=2, IMAX
         DW= FILT_WVL(K) - FILT_WVL(K-1)
         EFF_AREA= EFF_AREA + DW * (FILT_TRAN(K) + 
     &        FILT_TRAN(K-1))
      ENDDO
      EFF_AREA= EFF_AREA / 2.D0

      RETURN
      END
C============================================================
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
