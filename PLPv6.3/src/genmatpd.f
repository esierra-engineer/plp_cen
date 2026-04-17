!************************************
!>     Subrutina Define Matrices P.D.D
!************************************
      SUBROUTINE GenMatPD(FAfluFict,                                    &
     &     FVertReb, NEmbVReb, EmbVRebFilInd,                           &
     &     NEmbVMinH,                                                   &
     &     FConvLaja, ParLaja, ParLajaM,                                &
     &     FConvMaule, ParMaule,                                        &
     &     FRestRalco, ParRalco,                                        &
     &     FRestGnl, ParGnl,                                            &
     &     FRestReserva, ParRes,                                        &
     &     FBaterias, ParBaterias,                                      &
     &     FOldLaja, ParLajaC, LajaIPar,                                &
     &     FOldMaule, ParMauleC, MauleIPar,                             &
     &     NEtapa, NBarra, NLinea,                                      &
     &     NFlujo, NCentral, NCenEmb, NCenSer, NCenPas,                 &
     &     NBloque,                                                     &
     &     CenGBar,                                                     &
     &     PDNFila, PDNCol,                                             &
     &     PDLDAcNFila, PDLDAcNCol, PDLDAcFilaInd, PDLDAcColInd,        &
     &     FiltCenInd,                                                  &
     &     FiltColInd, FiltNCen,                                        &
     &     FFiltVar, FiltVarColInd, FiltVarFilInd,                      &
     &     FExtrac, ExtrNCen, ExtrCenInd, ExtrColInd, ExtrMax,          &
     &     RendCenInd, RendEmbInd,                                      &
     &     RendColInd, RendFilaInd, RendVolColInd, RendNCen,            &
     &     PmaxCenInd, PmaxEmbInd,                                      &
     &     PmaxColInd, PmaxVolColInd, PmaxNCen,                         &
     &     EstocNCol, EstocNFila, EstocColInd, EstocFilaInd,            &
     &     NSimul, NVarPhi,                                             &
     &     Dim)
!     filtrendbeg
!     filtrend
!     comun a todas las rutinas:
      USE PLP

      TYPE(PAR_DIMS), INTENT(IN):: Dim

!     variables
!**************
      LOGICAL FAfluFict
      LOGICAL FVertReb
      INTEGER NEmbVReb
      INTEGER EmbVRebFilInd(Dim%EmbVReb, Dim%Eta)

      INTEGER NEmbVMinH


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
      LOGICAL FOldLaja
      TYPE(PAR_LAJAC) ParLajaC
      INTEGER LajaIPar(DimILaja)

      LOGICAL FOldMaule
      TYPE(PAR_MAULEC) ParMauleC
      INTEGER MauleIPar(DimIMaule)

      LOGICAL FFiltVar
      INTEGER FiltVarColInd(Dim%EmbFilt, Dim%Eta)
      INTEGER FiltVarFilInd(Dim%EmbFilt, Dim%Eta)
      LOGICAL FExtrac
      INTEGER ExtrNCen
      INTEGER ExtrCenInd(Dim%Extr)
      INTEGER ExtrColInd(Dim%Extr, Dim%IBlo, Dim%Eta)
      DOUBLE PRECISION ExtrMax(Dim%Extr)


      INTEGER CenGBar(Dim%Cen)
      INTEGER FiltCenInd(Dim%EmbFilt)
      INTEGER FiltColInd(Dim%EmbFilt, Dim%IBlo, Dim%Eta)
      INTEGER FiltNCen
!     filtrendbeg
      INTEGER RendCenInd(Dim%EmbRend)
      INTEGER RendColInd(Dim%EmbRend, Dim%IBlo, Dim%Eta)
      INTEGER RendFilaInd(Dim%EmbRend, Dim%IBlo, Dim%Eta)
      INTEGER RendEmbInd(Dim%EmbRend)
      INTEGER RendNCen
      INTEGER RendVolColInd(Dim%EmbRend, Dim%Eta)
!     filtrend
!     filtpmaxbeg
      INTEGER PmaxCenInd(Dim%EmbPmax)
      INTEGER PmaxColInd(Dim%EmbPmax, Dim%IBlo, Dim%Eta)
      INTEGER PmaxEmbInd(Dim%EmbPmax)
      INTEGER PmaxNCen
      INTEGER PmaxVolColInd(Dim%EmbPmax, Dim%Eta)
!     filtpmax

      INTEGER IColAcop
      INTEGER IFilaAcop
      INTEGER IFiltCen
      INTEGER IExtrCen
!     filtrendbeg
      INTEGER IRendCen
      INTEGER IPmaxCen
!     filtrend
      INTEGER NAfluFict
      INTEGER NAng
      INTEGER NBarra
      INTEGER NEtapa
      INTEGER NCenEmb
      INTEGER NCenPas
      INTEGER NCenSer
      INTEGER NCentral
      INTEGER NFlujo
      INTEGER NLinea
      INTEGER NSimul
      INTEGER NVarPhi
      INTEGER NVert
      INTEGER NVol
      INTEGER PDLDAcColInd(Dim%PDLDAcCol, Dim%Eta)
      INTEGER PDLDAcFilaInd(Dim%PDLDAcFila, Dim%Eta)
      INTEGER PDLDAcNCol
      INTEGER PDLDAcNFila
      INTEGER PDNCol(Dim%Eta)
      INTEGER PDNFila(Dim%Eta)

      INTEGER NBloque(Dim%Eta)
      INTEGER IEta
      INTEGER IBlo, COffset, FOffset, CBloque, FBloque
      INTEGER CAcOffset, FAcOffset

      INTEGER CBloque0

      INTEGER EstocNCol
      INTEGER EstocNFila
      INTEGER IEstocCol
      INTEGER IEstocFila
      INTEGER EstocColInd(Dim%EstocCol, Dim%IBlo, Dim%Eta)
      INTEGER EstocFilaInd(Dim%EstocFila, Dim%IBlo, Dim%Eta)
      INTEGER Idx, Ind
      INTEGER VolOffset

      INTEGER I

!
!     Codigo
!***********
      NVert = NCenEmb + NCenSer
      IF (FAfluFict) THEN
         NAfluFict = NCenEmb
      ELSE
         NAfluFict = 0
      ENDIF
      NVol = NCenEmb
      NAng = NBarra
      CBloque0 = NCentral + 2*NFlujo + NVert + NAfluFict + NVol + NAng
      CBloque = CBloque0
      FBloque = NBarra + NCenEmb + NCenSer + NLinea
      IF (FExtrac) THEN
!        agregamos las extracciones como variable del bloque
         CBloque = CBloque + ExtrNCen
      ENDIF
      IF (FConvLaja .EQ. 1) THEN
         CBloque = CBloque + ParLaja%NumColBlo
         FBloque = FBloque + ParLaja%NumFilBlo
      ENDIF
      IF (FConvLaja .EQ. 2) THEN
         CBloque = CBloque + ParLajaM%NumColBlo
         FBloque = FBloque + ParLajaM%NumFilBlo
      ENDIF
      IF (FConvMaule .NE. 0) THEN
         CBloque = CBloque + ParMaule%NumColBlo
         FBloque = FBloque + ParMaule%NumFilBlo
      ENDIF
      IF (FRestRalco) THEN
         CBloque = CBloque + ParRalco%NumColBlo
         FBloque = FBloque + ParRalco%NumFilBlo
      ENDIF
      IF (FRestGnl) THEN
         Do I = 1, ParGnl%NumTGNL
            CBloque = CBloque + ParGnl%TGNL(I)%NumColBlo
            FBloque = FBloque + ParGnl%TGNL(I)%NumFilBlo
         ENDDO
      ENDIF
      IF (FRestReserva) THEN
         CBloque = CBloque + ParRes%NumColBlo
         FBloque = FBloque + ParRes%NumFilBlo
      ENDIF
      IF (FBaterias) THEN
         CBloque= CBloque + ParBaterias%NumColBlo
         FBloque= FBloque + ParBaterias%NumFilBlo
      ENDIF
      ExtrColInd = 0
      NVarPhi = NSimul
      PDLDAcNFila = NCenEmb
      PDLDAcNCol = NVol
      EstocNCol = NCenPas
      EstocNFila = NCenEmb + NCenSer

      IF (FConvLaja .EQ. 1) THEN
         PDLDAcNFila = PDLDAcNFila + ParLaja%PDLDAcNFila
         PDLDAcNCol = PDLDAcNCol + ParLaja%PDLDAcNCol
      ENDIF
      IF (FConvLaja .EQ. 2) THEN
         PDLDAcNFila = PDLDAcNFila + ParLajaM%PDLDAcNFila
         PDLDAcNCol = PDLDAcNCol + ParLajaM%PDLDAcNCol
      ENDIF
      IF (FConvMaule .NE. 0) THEN
         PDLDAcNFila = PDLDAcNFila + ParMaule%PDLDAcNFila
         PDLDAcNCol = PDLDAcNCol + ParMaule%PDLDAcNCol
      ENDIF
      IF (FRestGnl) THEN
         Do I = 1, ParGnl%NumTGNL
            PDLDAcNFila = PDLDAcNFila + ParGnl%TGNL(I)%PDLDAcNFila
            PDLDAcNCol = PDLDAcNCol + ParGnl%TGNL(I)%PDLDAcNCol
         ENDDO
      ENDIF

      PDLDAcColInd = 0
      PDLDAcFilaInd = 0

      DO IEta = 1, NEtapa
         PDNCol(IEta) = NBloque(IEta)*CBloque
         PDNFila(IEta) = NBloque(IEta)*FBloque
!        ecuaciones de volumen 2*NVol
         PDNCol(IEta) = PDNCol(IEta) + 2*NVol
         PDNFila(IEta) = PDNFila(IEta) + 2*NVol
         IF (FVertReb) THEN
!           agregamos variables/restricciones de rebalse para vertimiento
            PDNCol(IEta) = PDNCol(IEta) + 3*NEmbVReb
            PDNFila(IEta) = PDNFila(IEta) + 2*NEmbVReb
         ENDIF
         IF (NEmbVMinH .gt. 0) THEN
!           agregamos variables/restricciones de cota minima con holgura
            PDNCol(IEta) = PDNCol(IEta) + NEmbVMinH
            PDNFila(IEta) = PDNFila(IEta) + NEmbVMinH
         ENDIF
         IF (FFiltVar) THEN
!           agregamos la filtracion como variable de la etapa
            PDNCol(IEta) = PDNCol(IEta) + FiltNCen
            PDNFila(IEta) = PDNFila(IEta) + FiltNCen
         ENDIF
         IF (FBaterias) THEN
            PDNCol(IEta)= PDNCol(IEta) + ParBaterias%NumColEta
            PDNFila(IEta)= PDNFila(IEta) + ParBaterias%NumFilEta
         ENDIF
         IF (FConvLaja .EQ. 1) THEN
            PDNCol(IEta) = PDNCol(IEta) + ParLaja%NumColEta
            PDNFila(IEta) = PDNFila(IEta) + ParLaja%NumFilEta
         ENDIF
         IF (FConvLaja .EQ. 2) THEN
            PDNCol(IEta) = PDNCol(IEta) + ParLajaM%NumColEta
            PDNFila(IEta) = PDNFila(IEta) + ParLajaM%NumFilEta
            IF (FRestReserva) THEN
               PDNCol(IEta) = PDNCol(IEta) + ParRes%NumColEta
               PDNFila(IEta) = PDNFila(IEta) + ParRes%NumFilEta
            ENDIF
         ENDIF
         IF (FConvMaule .NE. 0) THEN
            PDNCol(IEta) = PDNCol(IEta) + ParMaule%NumColEta
            PDNFila(IEta) = PDNFila(IEta) + ParMaule%NumFilEta
         ENDIF
         IF (FRestRalco) THEN
            PDNCol(IEta) = PDNCol(IEta) + ParRalco%NumColEta
            PDNFila(IEta) = PDNFila(IEta) + ParRalco%NumFilEta
         ENDIF
         IF (FRestGnl) THEN
            Do I = 1, ParGnl%NumTGNL
               PDNCol(IEta) = PDNCol(IEta) + ParGnl%TGNL(I)%NumColEta
               PDNFila(IEta) = PDNFila(IEta) + ParGnl%TGNL(I)%NumFilEta
            ENDDO
         ENDIF

         COffset = 0
         FOffset = 0
         CAcOffset = 0
         FAcOffset = 0

         DO IBlo =1, NBloque(IEta)
            IF (.NOT. FFiltVar) THEN
               DO IFiltCen = 1, FiltNCen
                  FiltColInd(IFiltCen, IBlo, IEta) = COffset +          &
     &                 FiltCenInd(IFiltCen)
               ENDDO
            ENDIF
            IF (FExtrac) THEN
!              extracciones de centrales
               DO IExtrCen = 1, ExtrNCen
                  ExtrColInd(IExtrCen, IBlo, IEta) = COffset +      &
     &                 ExtrCenInd(IExtrCen)
               ENDDO
            ENDIF

            DO IRendCen = 1, RendNCen
               RendColInd(IRendCen, IBlo, IEta) = COffset +             &
     &              RendCenInd(IRendCen)
               RendFilaInd(IRendCen, IBlo, IEta) =  FOffset +           &
     &              CenGBar(RendCenInd(IRendCen))
            ENDDO

            DO IPmaxCen = 1, PmaxNCen
               PmaxColInd(IPmaxCen, IBlo, IEta) = COffset +             &
     &              PmaxCenInd(IPmaxCen)
            ENDDO


!           Centrales de pasada
            DO IEstocCol = 1, EstocNCol
               EstocColInd(IEstocCol, IBlo, IEta) = COffset +           &
     &              NCenEmb + NCenSer + IEstocCol
            ENDDO
!           embalses y centrales series
            DO IEstocFila = 1, EstocNFila
               EstocFilaInd(IEstocFila, IBlo, IEta) = FOffset +         &
     &              NBarra + IEstocFila
            ENDDO

            FOffset = FOffset + FBloque
            COffset = COffset + CBloque
         ENDDO
         ! Indice de rendimientos
         DO IRendCen = 1, RendNCen
            RendVolColInd(IRendCen, IEta) = COffset +                   &
     &           RendEmbInd(IRendCen)
         ENDDO

         DO IPmaxCen = 1, PmaxNCen
            PmaxVolColInd(IPmaxCen, IEta) = COffset +                   &
     &           PmaxEmbInd(IPmaxCen)
         ENDDO

         ! Indice de volumenes
         DO IColAcop = 1, NVol
            PDLDAcColInd(IColAcop, IEta) =  COffset + IColAcop
         ENDDO
         CAcOffset = CAcOffset + NVol
         DO IFilaAcop = 1, NCenEmb
            PDLDAcFilaInd(IFilaAcop, IEta) = FOffset + IFilaAcop
         ENDDO
         FAcOffset = FAcOffset + NCenEmb


         ! Vol Embalses
         VolOffset = COffset
         COffset = COffset + 2*NCenEmb
         FOffset = FOffset + 2*NCenEmb

         ! Rebalses
         IF (FVertReb) THEN
            DO Idx = 1, NEmbVReb
               EmbVRebFilInd(Idx, IEta) = FOffset + Idx
            ENDDO
            COffset = COffset + 3*NEmbVReb
            FOffset = FOffset + 2*NEmbVReb
         ENDIF

         ! cota minima con holgura
         IF (NEmbVMinH .gt. 0) THEN
            COffset = COffset + NEmbVMinH
            FOffset = FOffset + NEmbVMinH
         ENDIF

         ! Indice de variables de filtracion
         IF (FFiltVar) THEN
            DO IFiltCen = 1, FiltNCen
               FiltVarColInd(IFiltCen, IEta) = COffset + IFiltCen
               FiltVarFilInd(IFiltCen, IEta) = FOffset + IFiltCen
            ENDDO
            COffset = COffset + FiltNCen
            FOffset = FOffset + FiltNCen
         ENDIF
         ! Indice variables de etapa de baterias
         IF (FBaterias) THEN
            DO Idx=1, ParBaterias%NumColEta
               ParBaterias%ColIndEta(IEta,Idx)= COffset + Idx
            ENDDO
            DO Idx=1, ParBaterias%NumFilEta
               ParBaterias%FilIndEta(IEta,Idx)= FOffset + Idx
            ENDDO
            COffset= COffset + ParBaterias%NumColEta
            FOffset= FOffset + ParBaterias%NumFilEta
         ENDIF
         ! Indice variables de Laja
         IF (FConvLaja .EQ. 1) THEN
            DO Idx = 1, ParLaja%NumColEta
               ParLaja%ColIndEta(Idx, IEta) = COffset + Idx
            ENDDO
            DO Idx = 1, ParLaja%NumFilEta
               ParLaja%FIlIndEta(Idx, IEta) = FOffset + Idx
            ENDDO

            IF (ParLaja%UsaCorteOptim) THEN
               DO Idx=1, ParLaja%PDLDAcNCol
                  PDLDAcColInd(CAcOffset + Idx, IEta) = COffset + Idx
               ENDDO
               CAcOffset = CAcOffset + ParLaja%PDLDAcNCol
               DO Idx=1, ParLaja%PDLDAcNFila
                  PDLDAcFilaInd(FAcOffset + Idx, IEta) = FOffset + Idx
               ENDDO
               FAcOffset = FAcOffset + ParLaja%PDLDAcNFila
            ENDIF

            COffset = COffset + ParLaja%NumColEta
            FOffset = FOffset + ParLaja%NumFilEta
         ENDIF
         IF (FConvLaja .EQ. 2) THEN
            DO Idx = 1, ParLajaM%NumColEta
               ParLajaM%ColIndEta(Idx, IEta) = COffset + Idx
            ENDDO
            DO Idx = 1, ParLajaM%NumFilEta
               ParLajaM%FIlIndEta(Idx, IEta) = FOffset + Idx
            ENDDO

            DO Idx=1, ParLajaM%PDLDAcNCol
               PDLDAcColInd(CAcOffset + Idx, IEta) = COffset + Idx
            ENDDO
            CAcOffset = CAcOffset + ParLajaM%PDLDAcNCol
            DO Idx=1, ParLajaM%PDLDAcNFila
               PDLDAcFilaInd(FAcOffset + Idx, IEta) = FOffset + Idx
            ENDDO
            FAcOffset = FAcOffset + ParLajaM%PDLDAcNFila
            COffset = COffset + ParLajaM%NumColEta
            FOffset = FOffset + ParLajaM%NumFilEta
            IF (FRestReserva) THEN
            ! Son 4 variables por etapa, con 4 restricciones (definiciones) de estas
               DO Idx=1, ParRes%NumFilEta !tqrpr_fil,tqrpe_fil,tqrpm_fil
                  ParRes%FilIndEta(IEta,Idx)= FOffset + Idx
               ENDDO
               DO Idx=1, ParRes%NumColEta
                  ParRes%ColIndEta(IEta,Idx)= COffset + Idx !tqrpr_col,tqrpe_col,tqrpm_col
               ENDDO
               FOffset = FOffset + ParRes%NumFilEta
               COffset = COffset + ParRes%NumColEta
            ENDIF
         ENDIF


         ! Indice variables de Maule
         IF (FConvMaule .NE. 0) THEN
            ParMaule%ExtrMax425 = 0.0d0
            IF (FExtrac .AND. (ParMaule%IExtr425 .GT. 0)) THEN
               ParMaule%ExtrMax425 = ExtrMax(ParMaule%IExtr425)
               DO IBlo = 1, NBloque(IEta)
                  Idx = CBloque*(IBlo-1) + CBloque0 + ParMaule%IExtr425
                  ParMaule%Extr425ColInd(IBlo, IEta) = Idx
               ENDDO
            ENDIF

            Idx = ParMaule%IEmbColbun
            ParMaule%VColbunColInd(IEta) = PDLDAcColInd(Idx, IEta)

            DO Idx = 1, ParMaule%NumColEta
               ParMaule%ColIndEta(Idx, IEta) = COffset + Idx
            ENDDO
            DO Idx = 1, ParMaule%NumFilEta
               ParMaule%FilIndEta(Idx, IEta) = FOffset + Idx
            ENDDO

            IF (ParMaule%UsaCorteOptim) THEN
               DO Idx=1, ParMaule%PDLDAcNCol
                  PDLDAcColInd(CAcOffset + Idx, IEta) = COffset + Idx
               ENDDO
               CAcOffset = CAcOffset + ParMaule%PDLDAcNCol
               DO Idx=1, ParMaule%PDLDAcNFila
                  PDLDAcFilaInd(FAcOffset + Idx, IEta) = FOffset + Idx
               ENDDO
               FAcOffset = FAcOffset + ParMaule%PDLDAcNFila
            ENDIF


            COffset = COffset + ParMaule%NumColEta
            FOffset = FOffset + ParMaule%NumFilEta
         ENDIF
         !
         IF (FOldLaja) THEN
            DO IBlo = 1, NBloque(IEta)
               DO Idx = LajaBloQVarBeg, LajaBloQVarEnd
                  IF (LajaIPar(Idx) .EQ. 0) THEN
                     CYCLE
                  ENDIF
                  Ind = CBloque*(IBlo-1) + LajaIPar(Idx)
                  ParLajaC%IBloInd(Idx, IBlo, IEta) =  Ind
               ENDDO
               !
               Ind = CBloque*(IBlo-1) + LajaIPar(IIVertLaja)
               ParLajaC%IBloInd(IIVertLaja, IBlo, IEta) =  Ind
            ENDDO
            Ind = VolOffset + LajaIPar(IICenElToro)
            ParLajaC%IEtaInd(IIVolLaja, IEta) = Ind
         ENDIF

         !
         IF (FOldMaule) THEN
            DO IBlo = 1, NBloque(IEta)
               DO Idx = MauleBloQVarBeg, MauleBloVVarEnd
                  IF (MauleIPar(Idx) .EQ. 0) THEN
                     CYCLE
                  ENDIF
                  Ind = CBloque*(IBlo-1) + MauleIPar(Idx)
                  ParMauleC%IBloInd(Idx, IBlo, IEta) =  Ind
               ENDDO
            ENDDO
         ENDIF


         ! Indice de variables de ralco
         IF (FRestRalco) THEN

            Idx = ParRalco%IEmbRalco
            ParRalco%VRalcoColInd(IEta) = PDLDAcColInd(Idx, IEta)

            DO Idx = 1, ParRalco%NumColEta
               ParRalco%ColIndEta(Idx, IEta) = COffset + Idx
            ENDDO
            DO Idx = 1, ParRalco%NumFilEta
               ParRalco%FilIndEta(Idx, IEta) = FOffset + Idx
            ENDDO

            COffset = COffset + ParRalco%NumColEta
            FOffset = FOffset + ParRalco%NumFilEta
         ENDIF

         ! Indice de variables de gnl
         IF (FRestGnl) THEN
            Do I = 1, ParGnl%NumTGNL
               DO Idx = 1, ParGnl%TGNL(I)%NumColEta
                  ParGnl%TGNL(I)%ColIndEta(Idx, IEta) = COffset + Idx
            ENDDO
               DO Idx = 1, ParGnl%TGNL(I)%NumFilEta
                  ParGnl%TGNL(I)%FIlIndEta(Idx, IEta) = FOffset + Idx
            ENDDO

               IF (ParGnl%TGNL(I)%UsaCorteOptim) THEN
                  DO Idx=1, ParGnl%TGNL(I)%PDLDAcNCol
                  PDLDAcColInd(CAcOffset + Idx, IEta) = COffset + Idx
               ENDDO
                  CAcOffset = CAcOffset + ParGnl%TGNL(I)%PDLDAcNCol

                  DO Idx=1, ParGnl%TGNL(I)%PDLDAcNFila
                  PDLDAcFilaInd(FAcOffset + Idx, IEta) = FOffset + Idx
               ENDDO
                  FAcOffset = FAcOffset + ParGnl%TGNL(I)%PDLDAcNFila
            ENDIF

               COffset = COffset + ParGnl%TGNL(I)%NumColEta
               FOffset = FOffset + ParGnl%TGNL(I)%NumFilEta
            ENDDO
         ENDIF


      ENDDO

      RETURN
      END
