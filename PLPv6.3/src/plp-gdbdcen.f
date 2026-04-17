!***************************************
!     Graba Archivo Generacion Centrales
!***************************************
      SUBROUTINE GraDatBDCen(NArcNom, ISimul, NBloques, BloEta, BloDur, &
     &     TipoEtapa, NCentral, CenNom, CenTipo,                        &
     &     CenGBar, CenPGen, NBarra, BarNom, CMg,                       &
     &     RenCen, PmaxCen, CenCVar, FWarningFalla, Dim, ULog)
      USE PLP

      TYPE(PAR_DIMS), INTENT(IN) :: Dim
      INTEGER ULog

!
      CHARACTER*12 STipoCen
      CHARACTER*1 CenTipo(Dim%Cen)
      CHARACTER*48 BarNom(Dim%Bar)
      CHARACTER*48 CenNom(Dim%Cen)
      CHARACTER*24 NArcNom
      CHARACTER*12 TipoEtapa(Dim%Eta)
      CHARACTER*3 ExtClave
      CHARACTER*48 BarNom2
      CHARACTER*8 NomSimul
      DOUBLE PRECISION BloDur(Dim%Blo)
      DOUBLE PRECISION CenPGen(Dim%Cen, Dim%Blo)
      DOUBLE PRECISION CMg(Dim%Bar, Dim%Blo)
      DOUBLE PRECISION RenCen(Dim%Cen, Dim%Blo)
      DOUBLE PRECISION PmaxCen(Dim%Cen, Dim%Blo)
      DOUBLE PRECISION CenCVar(Dim%Cen, Dim%Eta)
      INTEGER Abrir
      INTEGER CenGBar(Dim%Cen)
      INTEGER IBlo
      INTEGER ICen
      INTEGER ISimul
      INTEGER NBarra
      INTEGER NBloques
      INTEGER BloEta(Dim%Blo)
      INTEGER NCentral
      INTEGER UWrite
      INTEGER UWrite2
      INTEGER fPosChar
      LOGICAL FWarningFalla

 100  FORMAT((A6,",",I4,",",A,",",I4,",",A,",",A,",",I4,",",A,9(",",F12.2)))
 101  FORMAT((A6,",",I4,",",A,",",I4,",",A,",",A,",",I4,",",A,2(",",F12.2)))
      
      FWarningFalla = .FALSE.

      STipoCen = PCenTipEmb//PCenTipTer//PCenTipPas//PCenTipSer//PCenTipFal//PCenTipMod//PCenTipBat
      STipoCen(8:8) = Char(0)
      IF (ISimul .EQ. 1) THEN
!        Archivo de salida Centrales
         UWrite = Abrir(NArcNom, 'UNKNOWN', 'SEQUENTIAL', ULog)
         WRITE(UWrite, '(5A)') 'Hidro,Bloque,TipoEtapa,',               &
     &        'CenNum,CenNom,CenTip,CenBar,BarNom,',                    &
     &        'CenQgen,CenPgen,CenEgen,',                               &
     &        'CenInyP,CenInyE,',                                       &
     &        'CenRen,CenCVar,CenCostOp,CenPMax'
         
!        Archivo de salida Central de Falla
         UWrite2 = Abrir('plpfal.csv', 'UNKNOWN', 'SEQUENTIAL', ULog)
         WRITE(UWrite2, '(5A)') 'Hidro,Bloque,TipoEtapa,',              &
     &        'CenNum,CenNom,CenTip,CenBar,BarNom,',                    &
     &        'CenPgen,CenEgen'
      ELSE
!        Archivo de salida Centrales
         UWrite = Abrir(NArcNom, 'OLD', 'APPEND', ULog)
!        Archivo de salida Central de Falla
         UWrite2 = Abrir('plpfal.csv', 'OLD', 'APPEND', ULog)
      END IF
      DO ICen = 1, NCentral
         IF(fPosChar(CenTipo(ICen), STipoCen) .GT. 0) THEN
            IF (CenTipo(ICen) .NE. PCenTipFal) THEN
               IF (ISimul .EQ. 0) THEN
                  NomSimul = 'MEDIA'
               ELSE
                  WRITE(NomSimul,'("Sim", I3)') ISimul
               ENDIF
               IF (NBarra .GT. 1) THEN
                  BarNom2 = BarNom(CenGBar(ICen))
               ELSE
                  BarNom2 = 'Uninodal    '
               ENDIF

               WRITE(UWrite, 100) &
     &              (NomSimul, &
     &              IBlo, &
     &              TipoEtapa(BloEta(IBlo)), &
     &              ICen, &
     &              CenNom(ICen), &
     &              ExtClave(CenTipo(ICen),Dim%CenLabel(ICen)), &
     &              CenGBar(ICen), &
     &              BarNom2, &
     &              CenPGen(ICen, IBlo), &
     &              CenPGen(ICen, IBlo)*RenCen(ICen, IBlo), &
     &              CenPGen(ICen, IBlo)*RenCen(ICen, IBlo)*BloDur(IBlo)*1d-3, &
     &              CenPGen(ICen, IBlo)*RenCen(ICen, IBlo)*CMg(CenGBar(ICen), IBlo), &
     &              CenPGen(ICen, IBlo)*RenCen(ICen, IBlo)*CMg(CenGBar(ICen), IBlo)*BloDur(IBlo)*1d-3, &
     &              RenCen(ICen, IBlo), &
     &              CenCVar(ICen, BloEta(IBlo)), &
     &              CenPGen(ICen, IBlo)*RenCen(ICen, IBlo)*CenCVar(ICen, BloEta(IBlo))*BloDur(IBlo)*1d-3, &
     &              PmaxCen(ICen, IBlo), &
     &              IBlo = 1, NBloques)

!     Central de Falla solo si generacion mayor que cero
            ELSE
            DO IBlo = 1, NBloques
                  IF (CenPGen(ICen, IBlo) .eq. 0) THEN
                     CYCLE
                  ENDIF
                  FWarningFalla = .TRUE.
               IF (ISimul .EQ. 0) THEN
                     NomSimul = 'MEDIA'
               ELSE
                     WRITE(NomSimul,'("Sim", I3)') ISimul
               ENDIF
               IF (NBarra .GT. 1) THEN
                     BarNom2 = BarNom(CenGBar(ICen))
               ELSE
                     BarNom2 = 'Uninodal    '
               ENDIF

                  WRITE(UWrite2, 101) &
     &                 NomSimul, &
     &                 IBlo, &
     &                 TipoEtapa(BloEta(IBlo)), &
     &                 ICen, &
     &                 CenNom(ICen), &
     &                 ExtClave(CenTipo(ICen),Dim%CenLabel(ICen)), &
     &                 CenGBar(ICen), &
     &                 BarNom2, &
     &                 CenPGen(ICen, IBlo)*RenCen(ICen, IBlo), &
     &                 CenPGen(ICen, IBlo)*RenCen(ICen, IBlo)*BloDur(IBlo)*1d-3
            ENDDO
         ENDIF
         ENDIF
      ENDDO
      CALL Cerrar(UWrite)
      CALL Cerrar(UWrite2)
      RETURN
      END

!***************************************
!     Graba Archivo Costos Operacionales
!***************************************
      SUBROUTINE GraDatBDCop(NArcNom, ISimul, NBloques, BloEta, BloDur, &
     &     NCentral, CenPGen, CenCVar, FPhi, Dim, ULog)
      USE PLP, ONLY : PAR_DIMS

      TYPE(PAR_DIMS), INTENT(IN) :: Dim
      INTEGER ULog


!
      CHARACTER*24 NArcNom
      CHARACTER*8 NomSimul
      DOUBLE PRECISION BloDur(Dim%Blo)
      DOUBLE PRECISION CenPGen(Dim%Cen, Dim%Blo)
      DOUBLE PRECISION CenCVar(Dim%Cen, Dim%Eta)
      DOUBLE PRECISION FPhi(Dim%Eta)
      INTEGER Abrir
      INTEGER IBlo
      INTEGER IEta
      INTEGER ICen
      INTEGER ISimul
      INTEGER NBloques
      INTEGER BloEta(Dim%Blo)
      INTEGER NCentral
      INTEGER UWrite2
      DOUBLE PRECISION suma

      IF (ISimul .EQ. 1) THEN
         UWrite2 = Abrir(NArcNom, 'UNKNOWN', 'SEQUENTIAL', ULog)
         WRITE(UWrite2, '(A)') 'Hidro, IBlo, CostoOperActual'
      ELSE
         UWrite2 = Abrir(NArcNom, 'OLD', 'APPEND', ULog)
      ENDIF

      IF (ISimul .EQ. 0) THEN
         NomSimul = 'MEDIA'
      ELSE
         WRITE(NomSimul,'("Sim", I3)') ISimul
      ENDIF

      DO IBlo = 1, NBloques
         IEta = BloEta(IBlo)
         suma = 0.0d0
         DO ICen = 1, NCentral
            suma = suma + CenPGen(ICen, IBlo)*CenCVar(ICen, IEta)        &
     &           *BloDur(IBlo)/FPhi(IEta)
         ENDDO
         WRITE(UWrite2, '(A,A,I4,A,F18.3)') NomSimul,',',IBlo,',',suma
      ENDDO
      CALL Cerrar(UWrite2)

      RETURN
      END
