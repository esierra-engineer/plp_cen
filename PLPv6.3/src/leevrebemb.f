      SUBROUTINE LeeVRebEmbDim(ULog, Dim)
      USE PLP, ONLY : PAR_DIMS, NArcVRebEmb

      TYPE(PAR_DIMS) Dim

      INTEGER ULog

      CHARACTER*80 AuxVar
      EXTERNAL Abrir
      INTEGER Abrir
      INTEGER URead
      INTEGER EmbNum
!
      URead = Abrir(NArcVRebEmb, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(ULog, '(3A)') 'leevrebemb: No existe archivo ',           &
     &        NArcVRebEmb, '.'         
         RETURN
      ENDIF

      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) EmbNum

      Dim%EmbVReb = EmbNum

      CALL Cerrar(URead)

      RETURN
      END



!**************************************
!     Subrutina Datos de volumenes de rebalse de embalses
!**************************************
      SUBROUTINE LeeVRebEmb(NEmbVReb, EmbVRebInd, NCenEmb, CenNom,      &
     &     EmbVReb, EmbCReb, ULog, Dim)
      USE PLP, ONLY : PAR_DIMS, NArcVRebEmb

      TYPE(PAR_DIMS), INTENT(IN) :: Dim
!
!     Variables Globales
!******************
      CHARACTER*48 CenNom(Dim%Cen)
      INTEGER NCenEmb
      INTEGER ULog
      DOUBLE PRECISION EmbVReb(Dim%EmbVReb)
      DOUBLE PRECISION EmbCReb(Dim%EmbVReb)
!     Variables Locales
!******************
      CHARACTER*48 NomEmb
      CHARACTER*80 AuxVar
      INTEGER Abrir
      INTEGER IReb
      INTEGER I
      INTEGER EmbNum
      INTEGER NumEmb
      INTEGER URead

      INTEGER EmbVRebInd(Dim%EmbVReb)
      INTEGER NEmbVReb

      DOUBLE PRECISION VRebEmb
      DOUBLE PRECISION CQRebEmb
!************************
!     Lee datos plpfilemb.dat
!************************
      LOGICAL FWarning
      CHARACTER*42 Objeto

      NEmbVReb = 0

      URead = Abrir(NArcVRebEmb, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(ULog, '(3A)') 'leerebemb: No existe archivo ',           &
     &        NArcVRebEmb, '.'         
         RETURN
      ENDIF

      WRITE(ULog, '(3A)') 'leerebemb: archivo ',                        &
     &     NArcVRebEmb, ' encontrado.'

      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) EmbNum

      EmbVReb = 0d0
      EmbCReb = 0d0

      IReb = 0
      DO I = 1, EmbNum
         READ(URead, '(A1)') AuxVar
         READ(URead, *) NomEmb
         READ(URead, '(A1)') AuxVar
         READ(URead, *) VRebEmb
         READ(URead, '(A1)') AuxVar
!        READ(URead, *) CQRebEmb, CVRebEmb
         READ(URead, *) CQRebEmb
         Objeto ='embalse'
         CALL NomCen2NumCen(NumEmb, FWarning, NomEmb,                  &
     &        CenNom, NCenEmb, Objeto, ULog)

         IF (NumEmb .GT. 0) THEN
            IReb = IReb + 1
            EmbVRebInd(IReb) = NumEmb
            EmbVReb(IReb) = VRebEmb
            EmbCReb(IReb) = CQRebEmb
         ENDIF
      ENDDO
      NEmbVReb = IReb

      CALL Cerrar(URead)
      RETURN
      END
