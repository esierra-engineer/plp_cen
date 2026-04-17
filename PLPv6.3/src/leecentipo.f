      SUBROUTINE LeeCenTipo(ULog,CenTipo,CenNom,Dim)

      USE PLP, ONLY : PAR_DIMS, NArcCenTipo,PCenTipMod
      TYPE(PAR_DIMS) Dim
      INTEGER ULog
      CHARACTER*48 CenNom(Dim%Cen)
      CHARACTER*1 CenTipo(Dim%Cen)     

      CHARACTER*12 AuxVar
      INTEGER Abrir
      INTEGER URead
      INTEGER ICen
      LOGICAL FWarning
      CHARACTER*48 NomCen
      CHARACTER*3 CenLab


      FWarning=.False.
      URead = Abrir(NArcCenTipo, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leecentipo: No existe archivo ',       &
     &        NArcCenTipo, ', se utilizará CenTipo de plpcnfce.dat.'
         RETURN
      ENDIF

      READ(URead, *, END=1000) AuxVar
      DO WHILE (.TRUE.)
         READ(URead, *, END=1000) NomCen, CenLab
         
         CALL NomCen2NumCen(ICen, FWarning, NomCen,  &
      &        CenNom, Dim%Cen, NArcCenTipo, ULog)
         CenTipo(ICen)=PCenTipMod
         Dim%CenLabel(ICen)= CenLab
      ENDDO
    
 1000 CALL Cerrar(URead)


      END