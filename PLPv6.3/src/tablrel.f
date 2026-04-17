      SUBROUTINE TablRel(NEtapa, NBloques,                              &
     &     NBarra, NCenEmb,                                             &
     &     NCentral, NLinea,                                            &
     &     CenGBar, LinNBar, AIncid, CenRen,                            &
     &     BarNom, CenNom, LinNom, LinVNom, NumEtaCF,                   &
     &     CenTipo, CenCVar, EmbVMin, EmbVMax, EmbFEsc,                 &
     &     NSimul,                                                      &
     &     BloDur, BloEta, Year, Mes, Dim, ULog)
      USE PLP

      TYPE(PAR_DIMS), INTENT(IN)::  Dim
      INTEGER ULog

!
      CHARACTER*1 CenTipo(Dim%Cen)
      CHARACTER*48 BarNom(Dim%Bar)
      CHARACTER*48 CenNom(Dim%Cen)
      CHARACTER*48 LinNom(Dim%Lin)
      DOUBLE PRECISION BloDur(Dim%Blo)
      DOUBLE PRECISION CenRen(Dim%Cen)
      DOUBLE PRECISION CenCVar(Dim%Cen, Dim%Eta)
      DOUBLE PRECISION EmbVMax(Dim%Emb, Dim%Eta)
      DOUBLE PRECISION EmbVMin(Dim%Emb, Dim%Eta)
      DOUBLE PRECISION EmbFEsc(Dim%Emb)
      DOUBLE PRECISION LinVNom(Dim%Lin)
      INTEGER AIncid(Dim%HidSPP, 0:Dim%HidSPP)
      INTEGER BloEta(Dim%Blo)
      INTEGER CenGBar(Dim%Cen)
      INTEGER LinNBar(2, Dim%Lin)
      INTEGER Mes(Dim%Eta)
      INTEGER NBarra
      INTEGER NEtapa
      INTEGER NBloques
      INTEGER NCenEmb
      INTEGER NCentral
      INTEGER NLinea
      INTEGER NSimul
      INTEGER NumEtaCF
      INTEGER Year(Dim%Eta)
!     locales
      CHARACTER*7 NombreEtapa
      CHARACTER*7 NombreBloque
      CHARACTER*6 NombreSimul
      CHARACTER*8 STipoCen
      CHARACTER*8 STipoEmb
      CHARACTER*8 STipoSer
      DOUBLE PRECISION CosVar
      DOUBLE PRECISION FactRendim
      DOUBLE PRECISION VolMax
      DOUBLE PRECISION VolMin
      INTEGER Abrir
      INTEGER fPosChar
      INTEGER IEta
      INTEGER I
      INTEGER IMil
      INTEGER ICen
      INTEGER ICenJ
      INTEGER IDec
      INTEGER IUni
      INTEGER J
      INTEGER UWrite

      LOGICAL DxAEQy

!
      NombreEtapa = 'Eta0000'
      NombreBloque = 'Blo0000'
      NombreSimul = 'Sim000'
      STipoEmb = PCenTipEmb//PCenTipEmbAux
      STipoEmb(3:3) = Char(0)
      STipoSer = PCenTipRie//PCenTipSer
      STipoSer(3:3) = Char(0)
      UWrite = Abrir(NArcBarras, 'UNKNOWN', 'SEQUENTIAL', ULog)
      WRITE(UWrite, '(A)') '#Numero, Nombre, Latitud'
      DO I = 1, NBarra
         WRITE(UWrite, '(I4, '','', $)') I
         WRITE(UWrite, '( A, '','', $)') BarNom(I)
         WRITE(UWrite, '(F8.4, '','', $)') 0.0d0
         WRITE(UWrite, *)
      ENDDO
      CALL Cerrar(UWrite)
      STipoCen =                                                        &
     &     PCenTipEmb//                                                 &
     &     PCenTipTer//                                                 &
     &     PCenTipPas//                                                 &
     &     PCenTipSer//                                                 &
     &     PCenTipFal
      STipoCen(6:6) = Char(0)
      UWrite = Abrir(NArcCentrales, 'UNKNOWN', 'SEQUENTIAL', ULog)
      WRITE(UWrite, '(A)') '#Numero, Nombre, Tipo, Empresa, Barra'
      DO I = 1, NCentral
         IF(fPosChar(CenTipo(I), STipoCen) .GT. 0) THEN
            WRITE(UWrite, '(I4, '','', $)') I
            WRITE(UWrite, '(A, '','', $)') CenNom(I)
            WRITE(UWrite, '(A, '','', $)') CenTipo(I)
            WRITE(UWrite, '(I4, '','', $)') 0
            WRITE(UWrite, '(I4, '','', $)') CenGBar(I)
            WRITE(UWrite, *)
         ENDIF
      ENDDO
      CALL Cerrar(UWrite)
      UWrite = Abrir(NArcEmbalses, 'UNKNOWN', 'SEQUENTIAL', ULog)
      WRITE(UWrite, '(A, A)')                                           &
     &     '#Numero, Nombre, Tipo, VolMin, VolMax, ',                   &
     &     'VolMinNECF, VolMaxNECF, FEscala, FactRendim'
      DO I = 1, NCenEmb
         FactRendim = 0.0d0
         DO J = 1, AIncid(I, 0)
            ICenJ = AIncid(I, J)
            IF (CenGBar(ICenJ) .GT. 0) THEN
               FactRendim = FactRendim + CenRen(ICenJ)
            ENDIF
         ENDDO
         VolMin = EmbVMin(I, 1)
         VolMax = EmbVMax(I, 1)
         DO J = 2, NEtapa
            IF (EmbVMin(I, J) .LT. VolMin) VolMin = EmbVMin(I, J)
            IF (EmbVMax(I, J) .GT. VolMax) VolMax = EmbVMax(I, J)
         ENDDO
         WRITE(UWrite, '(I4, '','', $)') I
         WRITE(UWrite, '(A, '','', $)') CenNom(I)
         WRITE(UWrite, '(A, '','', $)') CenTipo(I)
         WRITE(UWrite, '(F15.2, '','', $)') VolMin
         WRITE(UWrite, '(F15.2, '','', $)') VolMax
         IF (NumEtaCF .GT. 0) THEN
            WRITE(UWrite, '(F15.2, '','', $)') EmbVMin(I, NumEtaCF)
            WRITE(UWrite, '(F15.2, '','', $)') EmbVMax(I, NumEtaCF)
         ELSE
            WRITE(UWrite, '(F15.2, '','', $)') 0.0d0
            WRITE(UWrite, '(F15.2, '','', $)') 0.0d0
         ENDIF
         WRITE(UWrite, '(I4, '','', $)') NINT(LOG10(EmbFEsc(I)))
         WRITE(UWrite, '(F9.3, '','', $)') FactRendim
         WRITE(UWrite, *)
      ENDDO
      CALL Cerrar(UWrite)
      UWrite = Abrir(NArcSeries, 'UNKNOWN', 'SEQUENTIAL', ULog)
      WRITE(UWrite, '(A)') '#Numero, Nombre, Tipo'
      DO I = 1, NCentral
         IF(fPosChar(CenTipo(I), STipoSer) .GT. 0) THEN
            WRITE(UWrite, '(I4, '','', $)') I
            WRITE(UWrite, '(A, '','', $)') CenNom(I)
            WRITE(UWrite, '(A, '','', $)') CenTipo(I)
            WRITE(UWrite, *)
         ENDIF
      ENDDO
      CALL Cerrar(UWrite)
      UWrite = Abrir(NArcEtapas, 'UNKNOWN', 'SEQUENTIAL', ULog)
      WRITE(UWrite, '(2A)') '#Bloque, Nombre, Tipo, Anno, Mes, ',       &
     &     'Duracion, FactEner, PctjMes'
      DO I = 1, NBloques
         WRITE(UWrite, '(I4, '','', $)') I
         IMil = I/1000
         ICen = I/100
         IDec = I/10 - 10*ICen
         IUni = I - 10*IDec  - 100*ICen - 1000*IMil
         NombreBloque(4:4) = CHAR(48 + INT(IMil))         
         NombreBloque(5:5) = CHAR(48 + INT(ICen))
         NombreBloque(6:6) = CHAR(48 + INT(IDec))
         NombreBloque(7:7) = CHAR(48 + INT(IUni))

         IMil = BloEta(I)/1000
         ICen = BloEta(I)/100
         IDec = BloEta(I)/10 - 10*ICen
         IUni = BloEta(I) - 10*IDec  - 100*ICen - 1000*IMil
         NombreEtapa(4:4) = CHAR(48 + INT(IMil))
         NombreEtapa(5:5) = CHAR(48 + INT(ICen))
         NombreEtapa(6:6) = CHAR(48 + INT(IDec))
         NombreEtapa(7:7) = CHAR(48 + INT(IUni))

         WRITE(UWrite, '( A, '','', $)') NombreBloque
         WRITE(UWrite, '( A, '','', $)') NombreEtapa
         WRITE(UWrite, '(I4, '','', $)') Year(BloEta(I))
         WRITE(UWrite, '(I4, '','', $)') Mes(BloEta(I))
         WRITE(UWrite, '(F6.0, '','', $)') BloDur(I)
         WRITE(UWrite, '(F9.5, '','', $)') 0.0d0
         WRITE(UWrite, '(I4, '','', $)') 0
         WRITE(UWrite, *)
      ENDDO
      CALL Cerrar(UWrite)
      UWrite = Abrir(NArcSimuls, 'UNKNOWN', 'SEQUENTIAL', ULog)
      WRITE(UWrite, '(A)') '#Numero, Nombre'
      DO I = 1, NSimul + 1
         WRITE(UWrite, '(I4, '','', $)') I
         IF (I .LT. NSimul + 1) THEN
            ICen = I/100
            IDec = I/10 - 10*ICen
            IUni = I - 10*IDec  - 100*ICen
            NomBreSimul(4:4) = CHAR(48 + INT(ICen))
            NomBreSimul(5:5) = CHAR(48 + INT(IDec))
            NomBreSimul(6:6) = CHAR(48 + INT(IUni))
         ELSE
            NombreSimul = "SimMed"
         ENDIF
         WRITE(UWrite, '( A, '','', $)') NombreSimul
         WRITE(UWrite, *)
      ENDDO
      CALL Cerrar(UWrite)
      UWrite = Abrir(NArcLineas, 'UNKNOWN', 'SEQUENTIAL', ULog)
      WRITE(UWrite, '(2A)') '#Numero, Nombre, Voltaje, Empresa, ',      &
     &     'BarraA, BarraB, VATT'
      DO I = 1, NLinea
         WRITE(UWrite, '(I4, '','', $)') I
         WRITE(UWrite, '(A, '','', $)') LinNom(I)
         WRITE(UWrite, '(I4, '','', $)') INT(LinVNom(I))
         WRITE(UWrite, '(I4, '','', $)') 0
         WRITE(UWrite, '(I4, '','', $)') LinNBar(1, I)
         WRITE(UWrite, '(I4, '','', $)') LinNBar(2, I)
         WRITE(UWrite, '(F10.2, '','', $)') 0.0d0
         WRITE(UWrite, *)
      ENDDO
      CALL Cerrar(UWrite)
      UWrite = Abrir(NArcCostoOp, 'UNKNOWN', 'SEQUENTIAL', ULog)
      WRITE(UWrite, '(A)') '# ICen, IEta, CostoOp'
      DO ICen = 1, NCentral
         IF (CenTipo(ICen) .EQ. PCenTipTer) THEN
            WRITE(UWrite, '(I4, '','', $)') ICen
            WRITE(UWrite, '(I4, '','', $)') 1
            WRITE(UWrite, '(F8.3, '','', $)') CenCVar(ICen, 1)
            WRITE(UWrite, *)
            CosVar = CenCVar(ICen, 1)
            DO IEta = 1, NEtapa - 1
               IF (.NOT. DxAEQy(CenCVar(ICen, IEta + 1), CosVar, 0.0d0)) THEN
                  WRITE(UWrite, '(I4, '','', $)') ICen
                  WRITE(UWrite, '(I4, '','', $)') IEta
                  WRITE(UWrite, '(F8.3, '','', $)') CenCVar(ICen, IEta)
                  WRITE(UWrite, *)
                  CosVar = CenCVar(ICen, IEta + 1)
               ENDIF
            ENDDO
         ENDIF
      ENDDO
      CALL Cerrar(UWrite)
      END
