!******************************************
!     Graba Archivo Datos Extracciones
!******************************************
      SUBROUTINE GraDatBDExtrac(NArcNom, ISimul,                         & 
     &     NBloque, BloEta, TipoEtapa,                                   &
     &     CenInd, ExtrNCen, ExtrCenInd, ExtrDat, Dim, ULog)
      USE PLP

      TYPE(PAR_DIMS), INTENT(IN) :: Dim
      INTEGER ULog


!
      CHARACTER*8 STipoEmb
      CHARACTER*8 STipoSer
      CHARACTER*24 NArcNom
      CHARACTER*12 TipoEtapa(Dim%Eta)
      CHARACTER*8 NomSimul
      INTEGER Abrir
      INTEGER IBlo
      INTEGER ISimul
      INTEGER NBloque
      INTEGER BloEta(Dim%Blo)
      INTEGER UWrite
      INTEGER Idx
      INTEGER ExtrNCen
      DOUBLE PRECISION ExtrDat(Dim%Extr, Dim%Blo)
      INTEGER ExtrCenInd(Dim%Extr)
      INTEGER CenInd(Dim%Cen)
!
      CHARACTER*12 Nombre
      CHARACTER*(DimLargo) CExtr
      CHARACTER*80 fconcat
      INTEGER ICentral

!
      
      STipoEmb = PCenTipEmb//PCenTipEmbAux
      STipoEmb(3:3) = Char(0)
      STipoSer = PCenTipRie//PCenTipSer
      STipoSer(3:3) = Char(0)
      IF (ISimul .EQ. 1) THEN
         UWrite = Abrir(NArcNom, 'UNKNOWN', 'SEQUENTIAL', ULog)
         WRITE(UWrite, '(A, $)') 'Hidro,Bloque,TipoEtapa'
         
         DO Idx = 1, ExtrNCen            
            ICentral = ExtrCenInd(Idx)
            
            Nombre = 'qx'
            CALL Num2Char(CenInd(ICentral), CExtr, No, DimLargo)
            Nombre = fconcat(Nombre, CExtr)
            
            Nombre = fconcat(Nombre, '@')
            CALL Num2Char(Idx, CExtr, No, DimLargo)
            Nombre = fconcat(Nombre, CExtr)               
               
            WRITE(UWrite, '('', '', A, $)') Nombre
         ENDDO

         WRITE(UWrite, *)
      ELSE
         UWrite = Abrir(NArcNom, 'OLD', 'APPEND', ULog)
      END IF
      
      DO IBlo = 1, NBloque
         IF (ISimul .EQ. 0) THEN
            NomSimul = 'MEDIA'
         ELSE
            WRITE(NomSimul,'("Sim", I3)') ISimul            
         ENDIF      
         

         WRITE(UWrite, '(2A,I4,A,A, $)') NomSimul,',',IBlo,',',         &
     &        TipoEtapa(BloEta(IBlo))
         
         DO Idx = 1, ExtrNCen
            WRITE(UWrite, '('', '', F10.2 $)') ExtrDat(Idx, IBlo)
         ENDDO

         WRITE(UWrite, *)
      ENDDO
      
      CALL Cerrar(UWrite)
      RETURN
      END

