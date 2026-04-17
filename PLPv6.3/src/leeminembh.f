!******************************************************************
      SUBROUTINE LeeMinEmbH(NEtapa, NCenEmb, CenNom, EmbFEsc,  &
     &     NEmbVMinH, EmbVMinHInd, EmbVMinH, EmbCMinH, ULog, Dim)
      USE PLP

      TYPE(PAR_DIMS), INTENT(IN) :: Dim

!******************************************************************
!     lee mantenimientos de embalses
      EXTERNAL Abrir
      INTEGER Abrir
      CHARACTER*12 AuxVar
      CHARACTER*48 CenNom(Dim%Cen)
      INTEGER NEtapa
      INTEGER NCenEmb
      INTEGER ULog
      INTEGER URead
      LOGICAL FStop
      LOGICAL FWarning
      INTEGER NEmbVMinH
      INTEGER EmbVMinHInd(Dim%Emb)
      DOUBLE PRECISION EmbCMinH(Dim%Emb, Dim%Eta)
      DOUBLE PRECISION EmbVMinH(Dim%Emb, Dim%Eta)
      DOUBLE PRECISION EmbFEsc(Dim%Emb)
!
      NEmbVMinH = 0
      
      FStop = .FALSE.
      FWarning = .FALSE.
      URead = Abrir(NArcMinEmbH, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(ULog, '(3A)') 'leeminembh: Error, no existe archivo ',    &
     &        NArcMinEmbH, '.'
         RETURN
      ENDIF
      READ(URead, '(A1)') AuxVar

      CALL LeeMinEmbHi(NEtapa, NCenEmb, CenNom, EmbFEsc,  &
     &     NEmbVMinH, EmbVMinHInd, EmbVMinH, EmbCMinH, URead, ULog, Dim)

      CALL Cerrar(URead)

      RETURN
      END




!******************************************************************
      SUBROUTINE LeeMinEmbHi(NEtapa, NCenEmb, CenNom, EmbFEsc,  &
     &     NEmbVMinH, EmbVMinHInd, EmbVMinH, EmbCMinH, URead, ULog, Dim)

      USE PLP

      TYPE(PAR_DIMS), INTENT(IN) :: Dim

!******************************************************************
!     lee mantenimientos de embalses
      EXTERNAL Abrir
      INTEGER Abrir
      CHARACTER*12 AuxVar
      CHARACTER*48 CenNom(Dim%Cen)
      CHARACTER*48 NomEmb
      INTEGER IEmb
      INTEGER IEmbMan
      INTEGER IEta
      INTEGER NEtapa
      INTEGER NCenEmb
      INTEGER NEmbMan
      INTEGER NEtaMan
      INTEGER NumEmb
      INTEGER NumEta
      INTEGER ULog
      INTEGER URead
      LOGICAL FWarning
      INTEGER NEmbVMinH
      INTEGER EmbVMinHInd(Dim%Emb)
      DOUBLE PRECISION EmbCMinH(Dim%Emb, Dim%Eta)
      DOUBLE PRECISION EmbVMinH(Dim%Emb, Dim%Eta)
      DOUBLE PRECISION EmbFEsc(Dim%Emb)
      DOUBLE PRECISION EmbCMinEsc
      DOUBLE PRECISION EmbCostoH
      CHARACTER*42 Objeto

!
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NEmbMan
      IEmb = 0
      DO IEmbMan = 1, NEmbMan
         READ(URead, '(A1)') AuxVar
         READ(URead, *) NomEmb
         
         NumEmb = 0
         Objeto ='embalse'
         CALL NomCen2NumCen(NumEmb, FWarning, NomEmb,                  &
     &        CenNom, NCenEmb, Objeto, ULog)
         IF (NumEmb .GT. 0) THEN
            IEmb = IEmb + 1
            EmbVminHInd(IEmb) = NumEmb
         ENDIF
         
         READ(URead, '(A1)') AuxVar
         READ(URead, *) NEtaMan
         READ(URead, '(A1)') AuxVar
         DO IEta = 1, NEtaMan
            READ(URead, *) NumEta, EmbCMinEsc, EmbCostoH
            IF ((NumEmb .gt. 0) .AND. &
     &           (NumEta .GE. 1) .AND. (NumEta .LE. NEtapa)) THEN
               EmbVMinH(NumEmb, NumEta) = EmbCMinEsc*EmbFEsc(NumEmb)/1D3
               EmbCMinH(NumEmb, NumEta) = EmbCostoH
            ENDIF
         ENDDO
      ENDDO

      NEmbVMinH = IEmb
      
      RETURN
      END
