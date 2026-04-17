!*********************************
!     Matriz Invariante A Vol Embalses
!*********************************
      SUBROUTINE GenPDRalcoBloA(IEta, blodur, etadur,                   &
     &     ParRalco,                                                    &
     &     QEmbOffset, COffset, FOffset,                                &
     &     A)
      USE PLP, ONLY : PAR_DIMS, PAR_RALCO
      USE A_MATRIX

!     Variables Globales
!******************
      INTEGER IEta
      INTEGER COffset
      INTEGER FOffset
      TYPE(PAR_RALCO) ParRalco

      INTEGER QEmbOffset
      INTEGER IQERalco

      DOUBLE PRECISION blodur
      DOUBLE PRECISION etadur
      TYPE(AMatrix) A
      
!     Variables Locales
!****************
      INTEGER FOffseti
      
      FOffseti = FOffset
      
!***************
!     Ralco
!***************
!     Calculo del Qe Horario 
      IQERalco = QEmbOffset + ParRalco%IEmbRalco 
      CALL Am_set(A, IQERalco, ParRalco%FilIndEta(ParRalco%IQEH_F, IEta), -blodur/etadur)
      
      COffset = COffset + ParRalco%NumColBlo
      FOffset = FOffset + ParRalco%NumFilBlo

      RETURN
      END


!******************
      SUBROUTINE GenPDRalcoEtaFO(ParRalco,                              &
     &     COffset,                                                     &
     &     PDNCol, FO, LowBnd, UppBnd)
      USE PLP, ONLY : PAR_DIMS, PAR_RALCO
      USE OSI
!     Variables Globales
!******************
      INTEGER PDNCol
      TYPE(PAR_RALCO) ParRalco

      INTEGER COffset
      DOUBLE PRECISION FO(PDNCol)
      DOUBLE PRECISION LowBnd(PDNCol)
      DOUBLE PRECISION UppBnd(PDNCol)


      DOUBLE PRECISION DINFTY
      DINFTY = osi_getinfty()

!     Funcion Objetivo
!****************
      FO(COffset + ParRalco%IQEH) = 0.0d0

!     Restricciones de tipo 'x < = ' y 'x > = '
!***********************************
      UppBnd(COffset + ParRalco%IQEH) = DINFTY
      LowBnd(COffset + ParRalco%IQEH) = -DINFTY

      RETURN
      END
!!!!

      SUBROUTINE GenPDRalcoEtaA(IEta, ParRalco,                         &
     &     VolOffSet, COffset, FOffset,                                 &
     &     A, PDNCol, PDNFila, PDNombre, Sentido)
      USE PLP, ONLY : PAR_DIMS, PAR_RALCO
      USE A_MATRIX

!     Variables Globales
!******************
      INTEGER PDNCol, PDNFila
      TYPE(PAR_RALCO) ParRalco
      CHARACTER*24 PDNombre (PDNCol)
      CHARACTER*1 Sentido(PDNFila)
      INTEGER IEta
      INTEGER COffset
      INTEGER FOffset
      INTEGER VolOffset
      
      TYPE(AMatrix) A
      DOUBLE PRECISION ARalco

!     Variables Locales
!****************
      INTEGER IVol
      INTEGER Idx
      INTEGER VRalcoColInd
      CHARACTER*12 Nombre
      CHARACTER*80 fconcat

      IVol = VolOffset + ParRalco%IEmbRalco
     
      Nombre = 'qeh'
      Nombre = fconcat('r_', Nombre)
      PDNombre(COffset + ParRalco%IQEH) = Nombre

      DO Idx = 1, ParRalco%NumColEta
         ParRalco%ColIndEta(Idx, IEta) = COffset + Idx
      ENDDO
      
      DO Idx = 1, ParRalco%NumFilEta
         ParRalco%FIlIndEta(Idx, IEta) = FOffset + Idx
      ENDDO

      ! Gasto horario
      CALL Am_set(A, COffset + ParRalco%IQEH, FOffset + ParRalco%IQEH_F, 1.0d0)

      ! Caudal de deficit de riego

      VRalcoColInd = ParRalco%VRalcoColInd(IEta)

      CALL Am_set(A, COffset + ParRalco%IQEH, FOffset + ParRalco%IQEV_F, -1.0d0)

      ARalco = -ParRalco%ARalco(1)*ParRalco%ScaleVol
      CALL Am_set(A, IVol, FOffset + ParRalco%IQEV_F, ARalco)

      Sentido(FOffset + ParRalco%IQEV_F) = 'L'

      RETURN
      END

!!!!!!!


      SUBROUTINE FijaRalco(IEta, VolIni, NumEmb, ParRalco, lp)

      USE PLP, ONLY : PAR_DIMS, PAR_RALCO, C_SIZE_T
      USE OSI

      INCLUDE 'machcons.fpp'

      INTEGER, INTENT(IN):: IEta
      INTEGER, INTENT(IN):: NumEmb

      TYPE(PAR_RALCO), INTENT(IN):: ParRalco
      DOUBLE PRECISION, INTENT(IN):: VolIni(NumEmb)

!     outs
      INTEGER(C_SIZE_T), INTENT(INOUT):: lp

!     locals
      DOUBLE PRECISION VolRalco

      INTEGER Idx
      INTEGER IFil
      INTEGER ICol

      DOUBLE PRECISION value
      INTEGER index
      INTEGER ISeg

      DOUBLE PRECISION ARalco


      IF (ParRalco%NSegRalco .LT. 2) THEN
         RETURN
      ENDIF

      VolRalco = VolIni(ParRalco%IEmbRalco)
      ISeg = 1
      DO Idx = 1, ParRalco%NSegRalco
         IF (VolRalco .LT. ParRalco%VCotRalco(Idx)) THEN
            EXIT
         ENDIF
         ISeg = Idx
      ENDDO

      IFil = ParRalco%FilIndEta(ParRalco%IQEV_F, IEta) - 1
      ICol = ParRalco%VRalcoColInd(IEta) - 1

      ARalco = -ParRalco%ARalco(ISeg)*ParRalco%ScaleVol
      CALL osi_lp_setcoefficient(lp, IFIl, ICol, ARalco)

      index = IFil 
      value = ParRalco%BRalco(ISeg)
      CALL osi_lp_setrowrhs(lp, index, value)

      RETURN 

      END
!

      SUBROUTINE InitParRalco(ParRalco, Dim)
      USE PLP, ONLY : PAR_RALCO, PAR_DIMS

      TYPE(PAR_RALCO) ParRalco
      TYPE(PAR_DIMS), INTENT(IN) :: Dim
!
      ALLOCATE(ParRalco%VRalcoColInd(Dim%Eta))
      ALLOCATE(ParRalco%ColIndEta(ParRalco%NumColEta, Dim%Eta))
      ALLOCATE(ParRalco%FilIndEta(ParRalco%NumFilEta, Dim%Eta))

      RETURN
      END

      SUBROUTINE LeeRalco(NArcRalcoN,                                   &
     &     NCenEmb, CenNom, ScaleVol, ParRalco, ULog, Dim)
      USE PLP, ONLY : PAR_DIMS, PAR_RALCO

      TYPE(PAR_DIMS), INTENT(IN) :: Dim
      

      INTEGER NCenEmb
      CHARACTER*(*) NArcRalcoN
      CHARACTER*48 CenNom(Dim%Cen)
      TYPE(PAR_RALCO) ParRalco
      INTEGER ULog

      DOUBLE PRECISION ScaleVol(Dim%Emb)

!     variables locales
      CHARACTER*12 AuxVar
      CHARACTER*48 NomCentral
      CHARACTER*42 Objeto

      INTEGER URead
      LOGICAL FStop

      INTEGER NumCen
      INTEGER Idx
      
      EXTERNAL Abrir
      INTEGER Abrir

!

      CALL InitParRalco(ParRalco, Dim)

      FStop = .FALSE.

      ! Apertura de archivo
      URead = Abrir(NArcRalcoN, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leeralco: Error, no existe archivo ',           &
     &        NArcRalcoN, '.'
         WRITE(ULog, '(3A)') 'leeralco: Error, no existe archivo ',        &
     &        NArcRalcoN, '.'
         STOP 1
      ENDIF


!     lago ralco
      
      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NomCentral
      Objeto ='embalse'
      CALL NomCen2NumCen(NumCen, FStop, NomCentral,                  &
     &     CenNom, NCenEmb, Objeto, ULog)
      IF (FStop) THEN
         STOP 1
      ENDIF

      ParRalco%ScaleVol = ScaleVol(NumCen)
      ParRalco%IEmbRalco = NumCen

      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParRalco%NSegRalco

      READ(URead, '(A1)') AuxVar
      DO Idx = 1, ParRalco%NSegRalco
         READ(URead, *) ParRalco%VCotRalco(Idx),                         &
     &        ParRalco%BRalco(Idx), ParRalco%ARalco(Idx)
      ENDDO
      
      RETURN

      END 
