      SUBROUTINE GenPDTGnlBloA(IEta, blodur,                            &
     &     TGnl,                                                     &
     &     CenOffset, COffset, FOffset,                                 &
     &     A)
      USE PLP
      USE A_MATRIX

!     Variables Globales
!******************
      INTEGER IEta
      INTEGER CenOffset
      INTEGER COffset
      INTEGER FOffset
      TYPE(TermGNL) TGnl


      INTEGER IQEGnl

      DOUBLE PRECISION blodur
      TYPE(AMatrix) A

!     locales
      DOUBLE PRECISION fdt
      DOUBLE PRECISION FactRendim
      INTEGER ICen

!***************
!     Gnl
!***************
!     Aporte de qg horario

      ! Vgnle + \sum dt_i Qgnl_i = Vinp

      Do ICen = 1, TGNL%NumCen
         IQEGnl = CenOffset + TGNL%CenInd(ICen)
         FactRendim = TGNL%GnlRen*TGNL%CenRen(ICen)*3.6d3
         fdt = blodur/FactRendim

         CALL Am_set(A, IQEGnl,                                      &
     &        TGNL%FilIndEta(TGNL%IVGNLE_F, IEta), fdt)
      ENDDO

      COffset = COffset + TGNL%NumColBlo
      FOffset = FOffset + TGNL%NumFilBlo

      RETURN
      END


!*********************************
!     Matriz Invariante A Vol Embalses
!*********************************
      SUBROUTINE GenPDTGnlEtaA(IEta,                                     &
     &     COffset, FOffset,                                            &
     &     A, PDNCol, PDNombre, TGnl)
      USE PLP
      USE A_MATRIX

      TYPE(TermGNL), INTENT(INOUT) :: TGnl

!     Variables Globales
!******************
      INTEGER PDNCol
      CHARACTER*12 Nombre
      CHARACTER*24 PDNombre (PDNCol)
      CHARACTER*80 fconcat
      INTEGER COffset
      INTEGER FOffset
      TYPE(AMatrix) A
      INTEGER IEta

!     Variables Locales
!*****************
      INTEGER Idx

      DO Idx = 1, TGNL%NumColEta
         Nombre = TGNL%VarEtaNames(Idx)
         Nombre = fconcat('gnl_', Nombre)
         PDNombre(COffset + Idx) = Nombre
      ENDDO

      DO Idx = 1, TGNL%NumColEta
         TGNL%ColIndEta(Idx, IEta) = COffset + Idx
      ENDDO

      DO Idx = 1, TGNL%NumFilEta
         TGNL%FilIndEta(Idx, IEta) = FOffset + Idx
      ENDDO


      ! Vgnlf - Vgnle + Vgnlv = Vgnli
      CALL Am_set(A, COffset + TGNL%IVGNLF, FOffset + TGNL%IVGNLF_F, 1.d0)
      CALL Am_set(A, COffset + TGNL%IVGNLE, FOffset + TGNL%IVGNLF_F,-1.d0)
      CALL Am_set(A, COffset + TGNL%IVGNLV, FOffset + TGNL%IVGNLF_F, 1.d0)

      ! Vgnle + \sum dt_i Qgnl_i = Vinp
      CALL Am_set(A, COffset + TGNL%IVGNLE, FOffset + TGNL%IVGNLE_F, 1.d0)

      RETURN
      END
!**************************
!     FO y Limites Vol Gnlalses
!**************************
      SUBROUTINE GenPDTGnlEtaFO(TGnl, edur, FPhi,                       &
     &     COffset, PDNCol, FO, LowBnd, UppBnd)

      USE OSI
      USE PLP

      TYPE(TermGNL), INTENT(IN)::  TGnl

!     Variables Globales
!******************
      INTEGER PDNCol
      INTEGER COffset
      DOUBLE PRECISION edur
      DOUBLE PRECISION FPhi
      DOUBLE PRECISION FO(PDNCol)
      DOUBLE PRECISION LowBnd(PDNCol)
      DOUBLE PRECISION UppBnd(PDNCol)
!     Variables Locales
!*****************

      DOUBLE PRECISION DINFTY
      DINFTY = osi_getinfty()


!     Funcion Objetivo
!****************
      FO(COffset + 1 : COffset + TGNL%NumColEta) = 0.0d0

      FO(COffset + TGNL%IVGNLF) = TGNL%CAlm * 1d3 * edur / FPhi
      FO(COffset + TGNL%IVGNLV) = TGNL%CVer * 1d3 / FPhi



!     Restricciones de tipo 'x < = ' y 'x > = '
!***********************************
      UppBnd(COffset + TGNL%IVGNLF) = TGNL%VMax
      LowBnd(COffset + TGNL%IVGNLF) = 0.0d0

      UppBnd(COffset + TGNL%IVGNLE) = DINFTY
      LowBnd(COffset + TGNL%IVGNLE) = -DINFTY

      UppBnd(COffset + TGNL%IVGNLV) = DINFTY
      LowBnd(COffset + TGNL%IVGNLV) = 0.0d0

      RETURN
      END



!*******************************************


      SUBROUTINE FijaTermGnl(IEta, ISimul, VolIni, NumEmb, TGnl, lp)

      USE PLP
      USE OSI

      INCLUDE 'machcons.fpp'

      INTEGER, INTENT(IN):: IEta
      INTEGER, INTENT(IN):: ISimul
      INTEGER, INTENT(IN):: NumEmb

      TYPE(TermGNL), INTENT(IN):: TGnl

      DOUBLE PRECISION, INTENT(OUT)::  VolIni(NumEmb + 1)

!     outs
      INTEGER(C_SIZE_T), INTENT(INOUT):: lp

!     locals
      DOUBLE PRECISION VolGnlPrev
      DOUBLE PRECISION LDResGnl(TGnl%NumFilEta)
      INTEGER Idx
!
      VolGnlPrev = TGNL%VolEtaPrev(ISimul, IEta)

      IF (TGNL%UsaCorteOptim) THEN
         VolIni(NumEmb + 1) = VolGnlPrev
      ENDIF
!
!     Las restricciones de economias, gastos y derechos asumen en valor
!     de las variables de la etapa previa, que se consideran como valor
!     inicial
!
      LDResGnl(TGNL%IVGNLF_F) = VolGnlPrev
      LDResGnl(TGNL%IVGNLE_F) = TGNL%VolInpEta(IEta)

!
!     Llama a cplex para setear los lados derechos de las restricciones

!     Escala las restricciones de volumenes
      DO Idx = 1, TGNL%NumFilEta
         LDResGnl(Idx) = LDResGnl(Idx)
      ENDDO

      CALL ModifBrdRhs(TGNL%NumFilEta,                 &
     &     TGNL%FilIndEta(1, IEta), LDResGnl,          &
     &     lp)


      RETURN
      END


      SUBROUTINE FijaGnl(IEta, ISimul, VolIni, NumEmb, ParGnl, lp)

      USE PLP
      USE OSI

      INCLUDE 'machcons.fpp'

      INTEGER, INTENT(IN):: IEta
      INTEGER, INTENT(IN):: ISimul
      INTEGER, INTENT(IN):: NumEmb

      TYPE(Par_GNL), INTENT(IN):: ParGnl

      DOUBLE PRECISION, INTENT(OUT)::  VolIni(NumEmb + 1)
!     outs
      INTEGER(C_SIZE_T), INTENT(INOUT):: lp

!     locals
      INTEGER I

      Do I = 1, ParGnl%NumTGNL
         CALL FijaTermGNL(IEta, ISimul, VolIni, NumEmb, ParGnl%TGNL(I), lp)
      ENDDO


      RETURN
      END

!************************


      SUBROUTINE InitTermGnl(NCen, TGnl, Dim)
      USE PLP

      INTEGER NCen

      CHARACTER*12 CId
      CHARACTER*80 fconcat


      TYPE(TermGNL) TGnl
      TYPE(PAR_DIMS), INTENT(IN) :: Dim
!

      ALLOCATE(TGnl%VolInpEta(Dim%Eta))
      ALLOCATE(TGnl%VarEtaNames(TGnl%DimColEta))

      ALLOCATE(TGnl%VolEtaPrev(Dim%Simul, Dim%Eta))

      ALLOCATE(TGnl%ColIndEta(TGnl%DimColEta, Dim%Eta))
      ALLOCATE(TGnl%FilIndEta(TGnl%DimFilEta, Dim%Eta))

      ALLOCATE(TGnl%DataEta(TGnl%DimColEta, Dim%Eta))
      ALLOCATE(TGnl%DualEta(TGnl%DimFilEta, Dim%Eta))


      ALLOCATE(TGnl%CenInd(NCen))
      ALLOCATE(TGnl%CenRen(NCen))

      CId = ' '
      CALL Num2Char(TGnl%Id, CId, No, DimLargo)

      TGnl%VarEtaNames(TGnl%IVGNLF) = fconcat('vgnlf_', CId)
      TGnl%VarEtaNames(TGnl%IVGNLE) = fconcat('vgnle_', CId)
      TGnl%VarEtaNames(TGnl%IVGNLV) = fconcat('vgnlv_', CId)
      TGnl%CenInd = 0


      RETURN
      END

!*******************

      SUBROUTINE LeeTermGnl(URead, NEtapa,                              &
     &     NCentral, CenNom, CenRen, CenCVar,                           &
     &     TGnl, ULog, Dim)
      USE PLP

      TYPE(PAR_DIMS), INTENT(IN) :: Dim


      INTEGER NEtapa
      INTEGER NCentral
      CHARACTER*48 CenNom(Dim%Cen)
      DOUBLE PRECISION CenRen(Dim%Cen)
      DOUBLE PRECISION CenCVar(Dim%Cen, Dim%Eta)

      TYPE(TermGNL) TGnl
      INTEGER Id
      INTEGER ULog

!     variables locales
      CHARACTER*12 AuxVar
      CHARACTER*48 NomCentral
      CHARACTER*42 Objeto

      INTEGER URead
      LOGICAL FStop

      INTEGER NumCen

      EXTERNAL Abrir
      INTEGER Abrir

      INTEGER I, NumVolInp, IEtapa
      DOUBLE PRECISION VolInp
      DOUBLE PRECISION Rendim

      INTEGER ICen
      INTEGER NCen

      CHARACTER*48 Nombre
      REAL(8) :: VolMax, VolIni, CGNL, CVer, CReg, CAlm, GnlRen
!


      FStop = .FALSE.


!     rest gnl

      READ(URead, '(A1)') AuxVar
      READ(URead, *) Id, Nombre, VolMax, VolIni, CGNL, CVer, CReg, CAlm, GnlRen

      TGnl%Id = Id
      TGnl%Nombre = Nombre
      TGnl%GnlRen = GnlRen
      TGnl%VMax = VolMax/1d3
      TGnl%Vini = VolIni/1d3
      TGnl%CReg = CReg
      TGnl%CAlm = CAlm/24.0d0
      TGnl%CVer = CVer

      TGnl%UsaCorteOptim = .TRUE.

     ! centrales asociadas

      READ(URead, '(A1)') AuxVar
      READ(URead, *) NCen

      CALL InitTermGnl(NCen, TGnl, Dim)

      READ(URead, '(A1)') AuxVar
      ICen = 0
      DO I = 1, NCen
         READ(URead, *) NomCentral, Rendim
         Objeto ='central'
         CALL NomCen2NumCen(NumCen, FStop, NomCentral,                  &
     &        CenNom, NCentral, Objeto, ULog)
         IF (NumCen .NE. 0) THEN
            ICen = ICen + 1
            TGnl%CenInd(ICen) = NumCen
            TGnl%CenRen(ICen) = Rendim
            CenRen(NumCen) = 1.0d0
            CenCVar(NumCen, 1:NEtapa) = TGnl%CReg/Rendim
         ENDIF
      ENDDO
      TGnl%NumCen = ICen

      TGnl%VolEtaPrev(1:Dim%Simul, 1) = TGnl%Vini


      IF (TGnl%UsaCorteOptim) THEN
         TGnl%PDLDAcNFila = TGnl%DimPDLDAcFila
         TGnl%PDLDAcNCol = TGnl%DimPDLDAcCol
      ELSE
         TGnl%PDLDAcNFila = 0
         TGnl%PDLDAcNCol = 0
      ENDIF

      READ(URead, '(A1)') AuxVar
      READ(URead, *) NumVolInp

      TGnl%VolInpEta(1:NEtapa) = 0.0d0

      READ(URead, '(A1)') AuxVar
      DO I = 1, NumVolInp
         READ(URead, *) IEtapa, VolInp
         IF (IEtapa .LE. NEtapa) THEN
            TGnl%VolInpEta(IEtapa) = VolInp/1d3
         ENDIF
      ENDDO

      RETURN

      END


      SUBROUTINE LeeGnl(NEtapa,                               &
     &     NCentral, CenNom, CenRen, CenCVar,                           &
     &     ParGnl, ULog, Dim)
      USE PLP, ONLY : PAR_DIMS, PAR_GNL

      TYPE(PAR_DIMS), INTENT(IN) :: Dim


      INTEGER NEtapa
      INTEGER NCentral
      CHARACTER*48 CenNom(Dim%Cen)
      DOUBLE PRECISION CenRen(Dim%Cen)
      DOUBLE PRECISION CenCVar(Dim%Cen, Dim%Eta)

      TYPE(PAR_GNL) ParGnl
      INTEGER ULog

!     variables locales
      CHARACTER*12 AuxVar

      INTEGER URead
      EXTERNAL Abrir
      INTEGER Abrir

      INTEGER I

      INTEGER NTGNL

      CHARACTER*48 NArcGnlN
!

      NArcGnlN = 'plpcnfgnl.dat'
      ! Apertura de archivo
      URead = Abrir('plpcnfgnl.dat', 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leegnl: Error, no existe archivo ',           &
     &        NArcGnlN, '.'
         WRITE(ULog, '(3A)') 'leegnl: Error, no existe archivo ',        &
     &        NArcGnlN, '.'
         STOP 1
      ENDIF


!     rest gnl
      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NTGNL

      ParGNL%NumTGNL = NTGNL
      ALLOCATE(ParGNL%TGNL(NTGNL))

      DO I=1, ParGNL%NumTGNL
         CALL LeeTermGnl(URead, NEtapa,                                 &
     &        NCentral, CenNom, CenRen, CenCVar,                        &
     &        ParGNL%TGnl(I), ULog, Dim)

      ENDDO

      RETURN

      END


!******************************************
!     Graba Archivo Datos Centrales Embalse
!******************************************
      SUBROUTINE GraDatBDGnlN(NArcNom, ISimul, NSimul,                  &
     &     NBloque, BloDur, BloEta, TipoEtapa,                          &
     &     FPhi, FactTiempo, CenPGen,                                   &
     &     ParGnl, Dim, ULog)
!
      USE PLP

      TYPE(PAR_DIMS), INTENT(IN) :: Dim
      INTEGER ULog


!
      TYPE(PAR_GNL) ParGnl
      CHARACTER*48 NArcNom
      CHARACTER*12 TipoEtapa(Dim%Eta)
      DOUBLE PRECISION BloDur(Dim%Blo)
      INTEGER Abrir
      INTEGER IEta
      INTEGER IBlo
      INTEGER ISimul
      INTEGER NSimul
      INTEGER NBloque
      INTEGER BloEta(Dim%Blo)
      INTEGER UWrite
      DOUBLE PRECISION Time
      INTEGER Idx

      DOUBLE PRECISION FactRendim
      DOUBLE PRECISION FactTiempo
      DOUBLE PRECISION FPhi(Dim%Eta)
      DOUBLE PRECISION CenPGen(Dim%Cen, Dim%Blo)
      DOUBLE PRECISION Vini
      INTEGER ICen

      DOUBLE PRECISION TotalRendim
      DOUBLE PRECISION TotalPGen
      DOUBLE PRECISION PGen

      INTEGER I
!

      IF (ISimul .EQ. 1) THEN
         UWrite = Abrir(NArcNom, 'UNKNOWN', 'SEQUENTIAL', ULog)
         WRITE(UWrite, '(A, '','', $)') 'Hidro'
         WRITE(UWrite, '(A, '','', $)') 'Bloque'
         WRITE(UWrite, '(A, '','', $)') 'TipoEtapa'
         WRITE(UWrite, '(A, '','', $)') 'HorasAcum'
         DO I=1, ParGNL%NumTGNL
            WRITE(UWrite, '(A, '','', $)') 'TermNom'
            WRITE(UWrite, '(A, '','', $)') 'VinpGnl'
         WRITE(UWrite, '(A, '','', $)') 'ViniGnl'
            DO Idx = 1, ParGnl%TGNL(I)%NumColEta
               WRITE(UWrite, '(A, '', '', $)') ParGnl%TGNL(I)%VarEtaNames(Idx)
         ENDDO
         WRITE(UWrite, '(A, '','', $)') 'ViniPSomb1'
         WRITE(UWrite, '(A, '','', $)') 'ViniPSomb2'
            DO Idx = 1, ParGnl%TGNL(I)%NumCen
               WRITE(UWrite, '(A,I3,'', '', $)') 'PGen', ParGnl%TGNL(I)%CenInd(Idx)
               WRITE(UWrite, '(A,I3,'', '', $)') 'QGen', ParGnl%TGNL(I)%CenInd(Idx)
               WRITE(UWrite, '(A,I3,'', '', $)') 'QGnl', ParGnl%TGNL(I)%CenInd(Idx)
               WRITE(UWrite, '(A,I3,'', '', $)') 'ConsGnl', ParGnl%TGNL(I)%CenInd(Idx)
            ENDDO
         ENDDO

         WRITE(UWrite, *)
      ELSE
         UWrite = Abrir(NArcNom, 'OLD', 'APPEND', ULog)
      END IF

      Time = 0.0d0
      DO IBlo = 1, NBloque
         Time = Time + BloDur(IBlo)
         IEta = BloEta(IBlo)
         IF (ISimul .EQ. 0) THEN
            WRITE(UWrite, '(A, $)') 'MEDIA, '
         ELSE
            WRITE(UWrite, '(A, I3, '', '', $)') 'Sim', ISimul
         ENDIF
         WRITE(UWrite, '(I4, '', '', $)') IBlo
         WRITE(UWrite, '(A, '', '', $)')  TipoEtapa(IEta)
         WRITE(UWrite, '(F7.1, '', '', $)') Time

         DO I=1, ParGNL%NumTGNL
            WRITE(UWrite, '(A, '', '', $)')  ParGnl%TGNL(I)%Nombre
         TotalRendim = 0d0
         TotalPGen = 0d0
            DO Idx = 1, ParGnl%TGNL(I)%NumCen
               ICen = ParGnl%TGNL(I)%CenInd(Idx)
               FactRendim = ParGnl%TGNL(I)%GnlRen*ParGnl%TGNL(I)%CenRen(Idx)*3.6D3
            PGen = CenPGen(ICen, IBlo)
            PGen = PGen + 1.0d-8
            TotalRendim = TotalRendim + FactRendim*PGen
            TotalPGen = TotalPGen + PGen
         ENDDO
         TotalRendim = TotalRendim/TotalPGen

            WRITE(UWrite, '(F11.2, '', '', $)') ParGnl%TGNL(I)%VolInpEta(IEta)*1D3

         IF (ISimul .eq. 0) THEN
               Vini = SUM(ParGnl%TGNL(I)%VolEtaPrev(1:NSimul, IEta))/NSimul
         ELSE
               Vini = ParGnl%TGNL(I)%VolEtaPrev(ISimul, IEta)
         ENDIF

         WRITE(UWrite, '(F11.2, '', '', $)') Vini*1D3
            DO Idx = 1, ParGnl%TGNL(I)%NumColEta
               WRITE(UWrite, '(F11.2, '', '', $)')                         &
     &              ParGnl%TGNL(I)%DataEta(Idx, IEta)*1D3
         ENDDO

         WRITE(UWrite, '(F9.2, '', '', $)')                             &
     &           ParGnl%TGNL(I)%DualEta(ParGnl%TGNL(I)%IVGNLF, IEta)*      &
     &        FPhi(IEta)*FactTiempo/TotalRendim

         WRITE(UWrite, '(F9.2, '', '', $)')                             &
     &           ParGnl%TGNL(I)%DualEta(ParGnl%TGNL(I)%IVGNLF, IEta)*      &
     &        FPhi(IEta)/1D3

            DO Idx = 1, ParGnl%TGNL(I)%NumCen
               ICen = ParGnl%TGNL(I)%CenInd(Idx)
               FactRendim = ParGnl%TGNL(I)%GnlRen*ParGnl%TGNL(I)%CenRen(Idx)*3.6D3
            PGen = CenPGen(ICen, IBlo)
            WRITE(UWrite, '(F7.2, '', '', $)') PGen
               WRITE(UWrite, '(F7.2, '', '', $)') PGen/ParGnl%TGNL(I)%CenRen(Idx)
            WRITE(UWrite, '(F7.2, '', '', $)') PGen/FactRendim*3.6d3
               WRITE(UWrite, '(F7.2, '', '', $)') PGen/FactRendim*3.6d3*BloDur(IBlo)/1d3
         ENDDO

         ENDDO
         WRITE(UWrite, *)
      ENDDO

      CALL Cerrar(UWrite)
      RETURN
      END
