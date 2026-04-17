#if 0
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
      
      FO(COffset + TGNL%IVGNLF) = TGNL%CAlm  * 1d3 * edur / FPhi
      FO(COffset + TGNL%IVGNLV) = TGNL%CVer  * 1d3 / FPhi
      
         

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

#endif
!*******************

      SUBROUTINE LeeNodoGn(URead, Nodo)
      USE PLP

      INTEGER URead
      TYPE(NodoGN) nodo

!     variables locales
      CHARACTER*48 nombre
      INTEGER Id

      REAL(8) PMin, PMax

      READ(URead, *) Id, nombre, PMin, PMax

      nodo%Id = Id
      nodo%nombre = nombre
      nodo%PMin = PMin
      nodo%PMax = PMax
      
      RETURN
      END 

      SUBROUTINE LeeDuctoGn(URead, Ducto)
      USE PLP

      INTEGER URead
      TYPE(DuctoGN) ducto

!     variables locales
      CHARACTER*48 nombre
      INTEGER Id
      INTEGER nodo_a_id, nodo_b_id
      REAL(8) C, FMin, FMax

      READ(URead, *) Id, nombre, nodo_a_id, nodo_b_id, C, FMin, FMax

      ducto%Id = Id
      ducto%nombre = nombre
      ducto%C = C
      ducto%nodo_a_id = nodo_a_id
      ducto%nodo_b_id = nodo_b_id
      ducto%FMin = FMin
      ducto%FMax = FMax      

      RETURN

      END 


      SUBROUTINE LeeBombaGn(URead, Bomba)
      USE PLP

      INTEGER URead
      TYPE(BombaGN) bomba

!     variables locales
      CHARACTER*48 nombre
      INTEGER Id
      INTEGER nodo_a_id, nodo_b_id
      REAL(8) CDel, CFlu, DMax, FMax

      READ(URead, *) Id, nombre, nodo_a_id, nodo_b_id, CDel, CFlu, DMax, FMax

      bomba%Id = Id
      bomba%nombre = nombre
      bomba%nodo_a_id = nodo_a_id
      bomba%nodo_b_id = nodo_b_id
      bomba%CDel = CDel
      bomba%CFlu = CFlu
      bomba%DMax = DMax
      bomba%FMax = FMax

      RETURN

      END 


      SUBROUTINE LeeRegasGn(URead, Regas)
      USE PLP

      INTEGER URead
      TYPE(RegasGN) regas

!     variables locales
      CHARACTER*48 nombre
      INTEGER Id
      INTEGER term_id, nodo_id
      REAL(8) CReg, FMax, FReg, POut

      READ(URead, *) Id, nombre, term_id, nodo_id, CReg, FMax, FReg, POut

      regas%Id = Id
      regas%nombre = nombre
      regas%term_id = term_id
      regas%nodo_id = nodo_id
      regas%CReg = CReg
      regas%POut = POut
      regas%FMax = FMax
      regas%FReg = FReg

      RETURN

      END 

      !
      !
      
      SUBROUTINE LeeDemandaGn(URead, Demanda)
      USE PLP

      INTEGER URead
      TYPE(DemandaGN) demanda

!     variables locales
      CHARACTER*48 nombre
      INTEGER Id
      INTEGER nodo_id, bloque, nbloques, I
      REAL(8) valor
      CHARACTER*12 AuxVar

      READ(URead, *) Id, nombre, nodo_id
      
      demanda%Id = Id
      demanda%nombre = nombre
      demanda%nodo_id = nodo_id

      READ(URead, '(A1)') AuxVar

      READ(URead, *)  nbloques
      demanda%nbloques = nbloques
      allocate(demanda%bloque(nbloques))
      allocate(demanda%valor(nbloques))
      
      READ(URead, '(A1)') AuxVar
      do I = 1, nbloques
         READ(URead, *) bloque, valor
         demanda%bloque(I) = bloque
         demanda%valor(I) = valor
      enddo      
      
      RETURN

      END 

      !
      !
      
      SUBROUTINE LeeGeneraGn(URead, Genera,                             &
     &     NEtapa, NCentral, CenNom, CenRen, CenCVar,                   &
     &     ULog, Dim)

      USE PLP

      TYPE(PAR_DIMS), INTENT(IN) :: Dim

      INTEGER URead
      INTEGER NEtapa

      TYPE(GeneraGN) Genera
      INTEGER NCentral
      CHARACTER*48 CenNom(Dim%Cen)
      DOUBLE PRECISION CenRen(Dim%Cen)
      DOUBLE PRECISION CenCVar(Dim%Cen, Dim%Eta)
      INTEGER ULog

!     variables locales
      CHARACTER*48 nombre
      INTEGER Id
      INTEGER nodo_id
      REAL(8) rend, pmin
      LOGICAL FStop
      INTEGER NumCen
      CHARACTER*42 Objeto

      READ(URead, *) Id, nombre, nodo_id, rend, pmin
      
      genera%Id = Id
      genera%nombre = nombre
      genera%nodo_id = nodo_id
      genera%rend = rend
      genera%pmin = pmin

      FStop = .true.
      CALL NomCen2NumCen(NumCen, FStop, nombre,                  &
     &     CenNom, NCentral, Objeto, ULog)
      IF (NumCen .NE. 0) THEN
         CenRen(NumCen) = 1.0d0
         CenCVar(NumCen, 1:NEtapa) = 0.0d0
      ENDIF

      
      RETURN

      END 

      !
      ! lectura Gn 
      !
    
      SUBROUTINE LeeGn(NEtapa,                                          &
     &     NCentral, CenNom, CenRen, CenCVar,                           &
     &     ParGn, ULog, Dim)
      USE PLP, ONLY : PAR_DIMS, PAR_GN

      TYPE(PAR_DIMS), INTENT(IN) :: Dim
      

      INTEGER NEtapa
      INTEGER NCentral
      CHARACTER*48 CenNom(Dim%Cen)
      DOUBLE PRECISION CenRen(Dim%Cen)
      DOUBLE PRECISION CenCVar(Dim%Cen, Dim%Eta)

      TYPE(PAR_GN) ParGn
      INTEGER ULog

!     variables locales
      CHARACTER*12 AuxVar

      INTEGER URead
      EXTERNAL Abrir
      INTEGER Abrir
      INTEGER I
      INTEGER NumNodos
      INTEGER NumDuctos, NumSegmentos
      INTEGER NumBombas
      INTEGER NumRegas
      INTEGER NumDemandas
      INTEGER NumGeneras

      CHARACTER*48 NArcGnN
!

      NArcGnN = 'plpcnfgn.dat'
      ! Apertura de archivo
      URead = Abrir(NArcGnN, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leegnl: Error, no existe archivo ',           &
     &        NArcGnN, '.'
         WRITE(ULog, '(3A)') 'leegnl: Error, no existe archivo ',        &
     &        NArcGnN, '.'
         STOP 1
      ENDIF


      READ(URead, '(A1)') AuxVar
!     nodos
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NumNodos

      ParGN%NumNodos = NumNodos
      ALLOCATE(ParGN%Nodo(NumNodos))

      READ(URead, '(A1)') AuxVar
      DO I=1, ParGN%NumNodos     
         CALL LeeNodoGN(URead, ParGN%Nodo(I))
      ENDDO

!     ductos
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NumDuctos, NumSegmentos

      ParGN%NumSegmentos = NumSegmentos
      ParGN%NumDuctos = NumDuctos
      ALLOCATE(ParGN%Ducto(NumDuctos))

      READ(URead, '(A1)') AuxVar
      DO I=1, ParGN%NumDuctos     
         CALL LeeDuctoGN(URead, ParGN%Ducto(I))
      ENDDO

!     bombas
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NumBombas

      ParGN%NumBombas = NumBombas
      ALLOCATE(ParGN%Bomba(NumBombas))

      READ(URead, '(A1)') AuxVar
      DO I=1, ParGN%NumBombas     
         CALL LeeBombaGN(URead, ParGN%Bomba(I))
      ENDDO

!     regas
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NumRegas

      ParGN%NumRegas = NumRegas
      ALLOCATE(ParGN%Regas(NumRegas))

      READ(URead, '(A1)') AuxVar
      DO I=1, ParGN%NumRegas
         CALL LeeRegasGN(URead, ParGN%Regas(I))
      ENDDO

!     demandas
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NumDemandas

      ParGN%NumDemandas = NumDemandas
      ALLOCATE(ParGN%Demanda(NumDemandas))

      READ(URead, '(A1)') AuxVar
      DO I=1, ParGN%NumDemandas     
         CALL LeeDemandaGN(URead, ParGN%Demanda(I))
      ENDDO

!     generas
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NumGeneras

      ParGN%NumGeneras = NumGeneras
      ALLOCATE(ParGN%Genera(NumGeneras))

      READ(URead, '(A1)') AuxVar
      DO I=1, ParGN%NumGeneras     
         CALL LeeGeneraGN(URead, ParGN%Genera(I),                       &
     &        NEtapa, NCentral, CenNom, CenRen, CenCVar,                &
     &        ULog, Dim)
      ENDDO


      RETURN

      END 


#if 0
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
         WRITE(UWrite, '(A, '','', $)') 'Etapa'
         WRITE(UWrite, '(A, '','', $)') 'TipoEtapa'
         WRITE(UWrite, '(A, '','', $)') 'HorasAcum'
         WRITE(UWrite, '(A, '','', $)') 'ViniGnl'
         DO I=1, ParGNL%NumTGNL     
            DO Idx = 1, ParGnl%TGNL(I)%NumColEta
               WRITE(UWrite, '(A, '', '', $)') ParGnl%TGNL(I)%VarEtaNames(Idx)
            ENDDO
            WRITE(UWrite, '(A, '','', $)') 'ViniPSomb1'
            WRITE(UWrite, '(A, '','', $)') 'ViniPSomb2'
            DO Idx = 1, ParGnl%TGNL(I)%NumCen
               WRITE(UWrite, '(A,I3,'', '', $)') 'PGen', ParGnl%TGNL(I)%CenInd(Idx)
               WRITE(UWrite, '(A,I3,'', '', $)') 'QGen', ParGnl%TGNL(I)%CenInd(Idx)
               WRITE(UWrite, '(A,I3,'', '', $)') 'QGnl', ParGnl%TGNL(I)%CenInd(Idx)
            ENDDO
         ENDDO

         WRITE(UWrite, *)
      ELSE
         UWrite = Abrir(NArcNom, 'OLD', 'APPEND', ULog)
      END IF

      DO I=1, ParGNL%NumTGNL     
         Time = 0.0d0
         DO IBlo = 1, NBloque
            IEta = BloEta(IBlo)
            IF (ISimul .EQ. 0) THEN
               WRITE(UWrite, '(A, $)') 'MEDIA, '
            ELSE
               WRITE(UWrite, '(A, I3, '', '', $)') 'Sim', ISimul
            ENDIF
            WRITE(UWrite, '(I4, '', '', $)') I
            WRITE(UWrite, '(I4, '', '', $)') IBlo
            WRITE(UWrite, '(A, '', '', $)')  TipoEtapa(IEta)
            WRITE(UWrite, '(F7.1, '', '', $)') Time
            
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
     &           FPhi(IEta)*FactTiempo/TotalRendim
            
            WRITE(UWrite, '(F9.2, '', '', $)')                             &
     &           ParGnl%TGNL(I)%DualEta(ParGnl%TGNL(I)%IVGNLF, IEta)*      &
     &           FPhi(IEta)/1D3
            
            DO Idx = 1, ParGnl%TGNL(I)%NumCen
               ICen = ParGnl%TGNL(I)%CenInd(Idx)
               FactRendim = ParGnl%TGNL(I)%GnlRen*ParGnl%TGNL(I)%CenRen(Idx)*3.6D3
               PGen = CenPGen(ICen, IBlo)
               WRITE(UWrite, '(F7.2, '', '', $)') PGen
               WRITE(UWrite, '(F7.2, '', '', $)') PGen/ParGnl%TGNL(I)%CenRen(Idx)
               WRITE(UWrite, '(F7.2, '', '', $)') PGen/FactRendim*3.6d3
            ENDDO
            
            WRITE(UWrite, *)
            Time = Time + BloDur(IBlo)
         ENDDO
      ENDDO

      CALL Cerrar(UWrite)
      RETURN
      END
#endif
