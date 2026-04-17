!**************************************
!     Subrutina Datos de volumenes de rebalse de embalses
!**************************************
      SUBROUTINE LeeQeBnd(NCenEmb, CenNom,      &
     &     EMbQeLow, EmbQeUpp, ULog, Dim)
      USE PLP
      USE OSI

      TYPE(PAR_DIMS), INTENT(IN) :: Dim

!
!     Variables Globales
!******************
      CHARACTER*48 CenNom(Dim%Cen)
      INTEGER NCenEmb
      INTEGER ULog
      DOUBLE PRECISION EmbQeLow(Dim%Emb)
      DOUBLE PRECISION EmbQeUpp(Dim%Emb)
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

      INTEGER NEmbVReb

      DOUBLE PRECISION QeLow, QeUpp

!************************
!     Lee datos plpfilemb.dat
!************************
      LOGICAL FWarning
      CHARACTER*42 Objeto

      DOUBLE PRECISION DINFTY
      DINFTY = osi_getinfty()

      NEmbVReb = 0

      EmbQeLow(1:NCenEmb) = -DINFTY
      EmbQeUpp(1:NCenEmb) = DINFTY

      URead = Abrir(NArcQeEmb, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         RETURN
      ENDIF

      WRITE(6, '(3A)') 'leeqebnd: archivo ',              &
     &     NArcVRebEmb, ' encontrado.'
      WRITE(ULog, '(3A)') 'leeqebnd: archivo ',           &
     &     NArcVRebEmb, ' encontrado.'

      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) EmbNum
      IReb = 0
      DO I = 1, EmbNum
         READ(URead, '(A1)') AuxVar
         READ(URead, *) NomEmb
         READ(URead, '(A1)') AuxVar
         READ(URead, *) QeLow, QeUpp
         Objeto ='embalse'
         CALL NomCen2NumCen(NumEmb, FWarning, NomEmb,                  &
     &        CenNom, NCenEmb, Objeto, ULog)

         IF (NumEmb .GT. 0) THEN
            EmbQeLow(NumEmb) = QeLow
            EmbQeUpp(NumEmb) = QeUpp
         ENDIF
      ENDDO

      CALL Cerrar(URead)
      RETURN
      END
