!*******************
!     Define Lado Derecho
!*******************
      SUBROUTINE PDMatLD(IEta, PDNFila,                                 &
     &     NBarra, NLinea, NCenEmb, NCenSer,                            &
     &     NBloque, BloInd, BloPot,                                     &
     &     CenPMin, CenPMax, ISimul,                                    &
     &     FVertReb, NEmbVReb, EmbVRebInd, EmbVReb,                     &
     &     NEmbVMinH, EmbVMinHInd, EmbVMinH, EmbCMinH,                  &
     &     FFIltVar, FiltNCen,                                          &
     &     FConvLaja, ParLaja, ParLajaM,                                &
     &     FConvMaule, ParMaule,                                        &
     &     FRestRalco, ParRalco,                                        &
     &     FRestGnl, ParGnl,                                            &
     &     FRestReserva, ParRes,                                    &
     &     FBaterias, ParBaterias,                                      &
     &     EmbVIni, ScaleVol, LD,Sentido, Dim)
      USE PLP

      TYPE(PAR_DIMS), INTENT(IN) :: Dim

!     Variables Globales
!******************
      CHARACTER*1 Sentido(PDNFila)
      INTEGER IEta
      INTEGER PDNFila
      INTEGER NBarra
      INTEGER NLinea
      INTEGER NCenEmb
      INTEGER NCenSer
      INTEGER NBloque
      INTEGER BloInd(Dim%IBlo)
      DOUBLE PRECISION BloPot(Dim%Bar, Dim%Blo)
      DOUBLE PRECISION EmbVIni(Dim%Emb)
      DOUBLE PRECISION EmbVReb(Dim%EmbVReb)

      DOUBLE PRECISION CenPMax(Dim%Cen, Dim%Blo)
      DOUBLE PRECISION CenPMin(Dim%Cen, Dim%Blo)
      INTEGER ISimul

      INTEGER NEmbVMinH
      INTEGER EmbVMinHInd(Dim%Emb)
      DOUBLE PRECISION EmbVMinH(Dim%Emb, Dim%Eta)
      DOUBLE PRECISION EmbCMinH(Dim%Emb, Dim%Eta)

      DOUBLE PRECISION ScaleVol(Dim%Emb)
      DOUBLE PRECISION LD(PDNFila)
      LOGICAL FVertReb
      INTEGER NEmbVReb
      INTEGER EmbVRebInd(Dim%EmbVReb)

      LOGICAL FFiltvar
      INTEGER FiltNCen

      INTEGER FConvLaja
      TYPE(PAR_LAJA) ParLaja
      TYPE(PAR_LAJAM) ParLajaM

      INTEGER FConvMaule
      TYPE(PAR_MAULE) ParMaule

      LOGICAL FRestRalco
      TYPE(PAR_RALCO) ParRalco

      LOGICAL FRestGnl
      TYPE(PAR_GNL) ParGnl

      LOGICAL FRestReserva
      TYPE(PAR_RESERVA) ParRes

      LOGICAL FBaterias
      TYPE(PAR_BATERIAS) ParBaterias
!     Variables Locales
!*****************
      INTEGER FOffset
      INTEGER IBar
      INTEGER ICen
      INTEGER ILin
      INTEGER IBlo
      INTEGER IReb
      INTEGER IMinH

      INTEGER IBat, IIny
      INTEGER I, IBInd, ICentral, IFil, IZona

      DOUBLE PRECISION gmin
      INTEGER DoEta

      FOffset = 0
      DO IBlo = 1, NBloque

         IBInd = BloInd(IBlo)

!*********
!     Demanda
!*********
         DO IBar = 1, NBarra
            LD(FOffset + IBar) = BloPot(IBar, IBInd)
         ENDDO
         FOffset = FOffset + NBarra

!********************
!     Balance Caudal Embalse
!********************
         DO ICen = 1, NCenEmb
            LD(FOffset + ICen) = 0.0d0
         ENDDO
         FOffset = FOffset + NCenEmb

!********************
!     Balance Hidro Serie
!********************
         DO ICen = 1, NCenSer
            LD(FOffset + ICen) = 0.0d0
         ENDDO
         FOffset = FOffset + NCenSer

!***************
!     Flujo DC
!***************
         DO ILin = 1, NLinea
            LD(FOffset + ILin) = 0.d0
         ENDDO
         FOffset = FOffset + NLinea

!***************
!     Convenio Laja
!***************
         IF (FConvLaja .EQ. 1) THEN
            FOffset = FOffset + ParLaja%NumFilBlo
         ENDIF
         IF (FConvLaja .EQ. 2) THEN
            FOffset = FOffset + ParLajaM%NumFilBlo
         ENDIF

!***************
!     Convenio Maule
!***************
         IF (FConvMaule .NE. 0) THEN
            FOffset = FOffset + ParMaule%NumFilBlo
         ENDIF

!***************
!     Restriccion Ralco
!***************
         IF (FRestRalco) THEN
            FOffset = FOffset + ParRalco%NumFilBlo
         ENDIF

!***************
!     Restriccion Gnl
!***************
         IF (FRestGnl) THEN
            Do I = 1, ParGnl%NumTGNL
               FOffset = FOffset + ParGnl%TGNL(I)%NumFilBlo
            ENDDO
         ENDIF

!***************
!     Restriccion Reserva
!***************
         IF (FRestReserva) THEN
            LD(FOffset + 1: FOffset + ParRes%NumFilBlo) = 0.0d0

            DoEta = 1
            IF (ParRes%EtaIni .gt. 0 .and. IEta .lt. ParRes%EtaIni) THEN
              DoEta = 0
            ENDIF
            IF (ParRes%EtaFin .gt. 0 .and. IEta .gt. ParRes%EtaFin) THEN
               DoEta = 0
            ENDIF

            
            IFil = ParRes%NumZonas*9*DoEta !Restricciones de satisfaccion de requerimiento de zonas
            
            !Restricciones del estilo 
            !     pmax - RC*gc >= RPp_z + RSp_z + RTp_z
            !     RC*gc - pmin >= RPn_z + RSn_z + RTn_z
            DO IZona=1, ParRes%NumZonas*DoEta ! sea grega porque son restricciones por zona ahora
               DO ICen = 1, ParRes%NumCen * DoEta
                  ICentral = ParRes%Cen(ICen)
                  IFil = IFil + 1
                  LD(FOffset + IFil) = -CenPMax(ICentral, IBind)

                  gmin = ParRes%CenGMin(ICen, IBind, ISimul)
                  !IF (gmin .gt. 0) THEN
                  !   write(*,*) 'LD', ICen, IBind, ISimul, gmin
                  !ENDIF
                  gmin = MAX(gmin, CenPMin(ICentral, IBind))
                  gmin = MIN(gmin, CenPMax(ICentral, IBind))
                  IFil = IFil + 1
                  LD(FOffset + IFil) = gmin
               ENDDO
            ENDDO
            ! Restricciones: 
            ! RC_c P_c^b - \sum_{c,z} R_{-,c,z}^b >= 0 \quad \forall c  \in G \forall b \in BL
            DO ICen=1, ParRes%NumCen *DoEta
               IFil= IFil + 1
            ENDDO
            DO IZona = 1, ParRes%NumZonas * DoEta
               IFil = IFil + 1
               LD(FOffset + IFil) = ParRes%REQ10s(IZona, IBind)
               IFil = IFil + 1
               LD(FOffset + IFil) = ParRes%REQ5mp(IZona, IBind)
               IFil = IFil + 1
               LD(FOffset + IFil) = ParRes%REQ5mn(IZona, IBind)
               IFil = IFil + 1
               LD(FOffset + IFil) = ParRes%REQpp(IZona, IBind)
               IFil = IFil + 1
               LD(FOffset + IFil) = ParRes%REQpn(IZona, IBind)
               IFil = IFil + 1
               LD(FOffset + IFil) = ParRes%REQsp(IZona, IBind)
               IFil = IFil + 1
               LD(FOffset + IFil) = ParRes%REQsn(IZona, IBind)
               IFil = IFil + 1
               LD(FOffset + IFil) = ParRes%REQtp(IZona, IBind)
               IFil = IFil + 1
               LD(FOffset + IFil) = ParRes%REQtn(IZona, IBind)
            ENDDO
            !  RC_c qrp_c^b = \sum_{z \in Zona, s \in RES^+} FACTOR_z*H_{b}rp_{z,s}^b
            DO ICen=1, ParRes%NumCen * DoEta
               IFil = IFil  + 1
            ENDDO
            ! Restricción:
            ! qrpr + qrpe + qrpm  - qrp = 0
            IF (FConvLaja .eq. 2) THEN
               IFil = IFil + 1
            ENDIF
            FOffset= FOffset + ParRes%NumFilBlo
         ENDIF
         IF (FBaterias) THEN

            IFil= 0
            DO IBat=1, ParBaterias%NBaterias
               ! restricción ED_BESS^b= 1 / FPD egen_BESS^b
               IFil = IFil + 1
               LD(FOffset + IFil) = 0
               DO IIny=1, ParBaterias%NIny(IBat)
                  ICentral= ParBaterias%NumInyBat(IBat,IIny)
                  ! Restriccion g_FV + EC \leq GMAX
                  IFil = IFil + 1
                  LD(FOffset + IFil)= CenPMax(ICentral, IBind)
                  !Sentido de la restricción cambia si el Pmax es 0 y el Pmin es negativo, porque estariamos en una standalone, 
                  ! y queremos que solamente extraiga la energía inyecta a la bateria.
                  IF ((CenPMax(ICentral, IBind) .LE. 0) .AND. (CenPMin(ICentral, IBind) .LT. 0)) THEN
                     Sentido(FOffset + IFil)='E'
                  ENDIF
                  ! Restriccion g_FV + EC \geq GMIN 
                  IFil = IFil + 1
                  LD(FOffset + IFil)= CenPMin(ICentral, IBind)
               ENDDO
            ENDDO
            DO IBat=1, ParBaterias%NBaterias
               IFil = IFil + 1
               LD(FOffset + IFil)=0
            ENDDO
            FOffset = FOffset + ParBaterias%NumFilBlo
         ENDIF
      ENDDO

!********************
!     Balance Vol Embalse
!********************
      DO ICen = 1, NCenEmb
         IF (IEta .EQ. 1) THEN
            LD(FOffset + ICen) = EmbVIni(ICen)/ScaleVol(ICen)
         ELSE
            LD(FOffset + ICen) = 0.0d0
         ENDIF
      ENDDO
      FOffset = FOffset + NCenEmb

      DO ICen = 1, NCenEmb
         LD(FOffset + ICen) = 0.0d0
      ENDDO
      FOffset = FOffset + NCenEmb

!********************
!     Balance Vertimientos de Rebalse Embalse
!********************
      IF (FVertReb) THEN
         DO IReb = 1, NEmbVReb
            ICen = EmbVRebInd(IReb)
            IF (IEta .EQ. 1) THEN
               LD(FOffset + IReb) = -(EmbVIni(ICen)-EmbVReb(IReb))       &
     &              / ScaleVol(ICen)

!               LD(FOffset + IReb) = MAX(LD(FOffset + IReb), 0.0d0)
            ENDIF
         ENDDO
         FOffset = FOffset + 2*NEmbVReb
      ENDIF

!********************
!     Cota minima con holgura
!********************
      IF (NEmbVMinH .gt. 0) THEN
         DO IMinH = 1, NEmbVMinH
            ICen = EmbVMinHInd(IMinH)
            IF (EmbCMinH(ICen, IEta) .gt. 0) THEN
               LD(FOffset + IMinH) = - EmbVMinH(ICen, IEta) / ScaleVol(ICen)
            ENDIF
         ENDDO
         FOffset = FOffset + NEmbVMinH
      ENDIF

!********************
!     Variables de filtracion
!********************
      IF (FFiltvar) THEN
         LD(FOffset + 1: FOffset + FiltNCen) = 0.0d0
         FOffset = FOffset + FiltNCen
      ENDIF

!********************
!     Convenio Laja
!********************
      IF (FConvLaja .EQ. 1) THEN
         LD(FOffset + 1: FOffset + ParLaja%NumFilEta) = 0.0d0
         FOffset = FOffset + ParLaja%NumFilEta
      ENDIF
      IF (FConvLaja .EQ. 2) THEN
         LD(FOffset + 1: FOffset + ParLajaM%NumFilEta) = 0.0d0
         FOffset = FOffset + ParLajaM%NumFilEta
         IF (FRestReserva) THEN
            ! Se agregan las 4 restricciones del estilo
            ! para: qrpr, qrpe, qrpm
            ! - \sum_{b \in BL} H_{tb} l_qrpr + l_tqrpr = 0
            LD(FOffset +1 : FOffset + ParRes%NumFilEta) = 0.0d0
            FOffset= FOffset + ParRes%NumFilEta
         ENDIF
      ENDIF

!********************
!     Convenio Maule
!********************
      IF (FConvMaule .NE. 0) THEN
         LD(FOffset + 1: FOffset + ParMaule%NumFilEta) = 0.0d0
         FOffset = FOffset + ParMaule%NumFilEta
      ENDIF

!********************
!     Restriccion Ralco
!********************
      IF (FRestRalco) THEN
         LD(FOffset + ParRalco%IQEV_F) = ParRalco%BRalco(1)
         LD(FOffset + ParRalco%IQEH_F) = 0.0d0

         FOffset = FOffset + ParRalco%NumFilEta
      ENDIF

!********************
!     Restriccion Gnl
!********************
      IF (FRestGnl) THEN
         Do I = 1, ParGnl%NumTGNL
            LD(FOffset + ParGnl%TGNL(I)%IVGNLF_F) = ParGnl%TGNL(I)%Vini
            LD(FOffset + ParGnl%TGNL(I)%IVGNLE_F) = 0.0d0

            FOffset = FOffset + ParGnl%TGNL(I)%NumFilEta
         ENDDO
      ENDIF


      RETURN
      END
