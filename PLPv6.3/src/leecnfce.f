      SUBROUTINE LeeCnfCenDim(ULog, Dim)

      USE PLP, ONLY : PAR_DIMS, NArcCnfCen

      TYPE(PAR_DIMS) Dim

!
!
!     parametros y variables locales:
!

      INTEGER NCenEmb
      INTEGER NCenPas
      INTEGER NCenSer
      INTEGER NCentral
      INTEGER NCenFalla
      INTEGER NCenTer
      INTEGER NCenBat
      INTEGER ULog
      INTEGER URead
!
      CHARACTER*12 AuxVar
      INTEGER Abrir

!************************
      URead = Abrir(NArcCnfCen, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(2A)') 'leecnfcen: Error, no existe archivo ',       &
     &        NArcCnfCen
         WRITE(ULog, '(2A)') 'leecnfcen: Error, no existe archivo ',    &
     &        NArcCnfCen
         STOP 1
      ENDIF
!
!***************************************
!     Numero de Centrales, Embalses y Pasadas
!***************************************
      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NCentral, NCenEmb, NCenSer, NCenFalla, NCenPas,&
      & NCenBat
      NCenTer = NCentral - NCenEmb - NCenSer - NCenPas - NCenFalla &
      & - NCenBat

      Dim%Emb = NCenEmb
      Dim%Ser = NCenSer
      Dim%Falla = NCenFalla
      Dim%Pas = NCenPas
      Dim%Ter = NCenTer
      Dim%Bat= NCenBat
      Dim%Vert = Dim%Ser + Dim%Emb
      Dim%Hid = Dim%Emb + Dim%Ser + Dim%Pas
      Dim%HidSPP = Dim%Emb + Dim%Ser
      Dim%Cen = Dim%Ter + Dim%Hid + Dim%Falla +Dim%Bat
      Dim%EstocVar = Dim%Emb

      Dim%PDLDAcCol = Dim%Emb
      Dim%PDLDAcFila = Dim%Emb
      Dim%EstocCol = Dim%Pas
      Dim%EstocFila = Dim%HidSPP


      CALL Cerrar(URead)

      RETURN
      END


!*************************************
!     Subrutina Lee Configuracion Centrales
!*************************************
      SUBROUTINE LeeCnfCen(NEtapa, NBloques,                            &
     &     NLinea, NCentral, NCenFalla,                                 &
     &     NCenEmb, NCenSer, NCenPas,                                   &
     &     CenNom, CenTipo, CenPMin, CenPMax, CenVMin, CenVMax,         &
     &     CenCVar, EmbCFUE,                                            &
     &     CenRen, CenGBar, CenGHid, CenVHid,                           &
     &     HidSPPQAfl, PasQAfl,                                         &
     &     EmbVIni, EmbVFin, EmbVMin, EmbVMax, EmbFEsc,                 &
     &     FactTiempo,                                                  &
     &     EstocFIndep, CenInd, FInterfaz, ULog, Dim)

      USE PLP

      TYPE(PAR_DIMS) Dim
!
!
!     parametros y variables locales:
      CHARACTER*1 CenTipo(Dim%Cen)
      CHARACTER*12 AuxVar
      CHARACTER*48 CenNom(Dim%Cen)
      INTEGER Abrir
      INTEGER CenGBar(Dim%Cen)
      INTEGER CenGHid(Dim%Cen, 2)
      INTEGER CenVAux(Dim%Cen, 2)
      INTEGER CenGAux(Dim%Cen, 2)
      INTEGER CenInd(Dim%Cen)
      INTEGER CenIPot
      INTEGER CenVHid(Dim%Cen, 2)
      INTEGER EtaIniSinMT
      INTEGER ICen
      INTEGER ICenPP
      INTEGER IEta
      INTEGER IBlo
      INTEGER Ind1
      INTEGER Ind2
      INTEGER NEtapa
      INTEGER NBloques
      INTEGER NCenEmb
      INTEGER NCenPas
      INTEGER NCenRie
      INTEGER NCenSer
      INTEGER NCentral
      INTEGER NCenFalla
      INTEGER NCenTer
      INTEGER NCenBat
      INTEGER ContadorCenDesde
      INTEGER ContadorCenHasta
      INTEGER NLinea
      INTEGER ULog
      INTEGER URead
      LOGICAL CenFCAD
      LOGICAL CenInter
      LOGICAL CenMinTec
      LOGICAL CenMTTdHrz
      LOGICAL CenOn
      LOGICAL EmbCFUE(Dim%Emb)
      LOGICAL FCenCAD
      LOGICAL FCenInter
      LOGICAL FFaseSinMT
      LOGICAL FMinTec
      LOGICAL EstocFIndep(Dim%Hid)
      LOGICAL FStop
      LOGICAL FWarning
      INTEGER FInterfaz
      DOUBLE PRECISION CenCArr
      DOUBLE PRECISION CenCDet
      DOUBLE PRECISION CenCosVar(Dim%Cen)
      DOUBLE PRECISION CenCVar(Dim%Cen, Dim%Eta)
      DOUBLE PRECISION CenPIni
      DOUBLE PRECISION CenPMax(Dim%Cen, Dim%Blo)
      DOUBLE PRECISION CenPMin(Dim%Cen, Dim%Blo)
      DOUBLE PRECISION CenVMax(Dim%Vert, Dim%Blo)
      DOUBLE PRECISION CenVMin(Dim%Vert, Dim%Blo)
      DOUBLE PRECISION CenRen(Dim%Cen)
      DOUBLE PRECISION CenCauAfl
      DOUBLE PRECISION CenPotMax
      DOUBLE PRECISION CenPotMin
      DOUBLE PRECISION CenVertMax
      DOUBLE PRECISION CenVertMin
      DOUBLE PRECISION EmbVIni(Dim%Emb)
      DOUBLE PRECISION EmbVFin(Dim%Emb)
      DOUBLE PRECISION EmbVMax(Dim%Emb, Dim%Eta)
      DOUBLE PRECISION EmbVMin(Dim%Emb, Dim%Eta)
      DOUBLE PRECISION EmbFEsc(Dim%Emb)
      DOUBLE PRECISION EmbVolFin
      DOUBLE PRECISION EmbVolIni
      DOUBLE PRECISION EmbVolMax
      DOUBLE PRECISION EmbVolMin
      DOUBLE PRECISION EtapasPorHora
      DOUBLE PRECISION FactTiempo
      DOUBLE PRECISION HidSPPQAfl(Dim%HidSPP, Dim%Blo)
      DOUBLE PRECISION PasQAfl(Dim%Pas, Dim%Blo)


!
!     codigo:
      FWarning = .FALSE.
      NCenRie = 0
      IEta = 1
      ALLOCATE(Dim%CenLabel(Dim%Cen))
!
!************************
!     Lee datos plphid.dat
!************************
      URead = Abrir(NArcCnfCen, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(2A)') 'leecnfcen: Error, no existe archivo ',       &
     &        NArcCnfCen
         WRITE(ULog, '(2A)') 'leecnfcen: Error, no existe archivo ',    &
     &        NArcCnfCen
         STOP 1
      ENDIF
!
!***************************************
!     Numero de Centrales, Embalses y Pasadas
!***************************************
      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NCentral, NCenEmb, NCenSer, NCenFalla, NCenPas,&
      &  NCenBat
      NCenTer = NCentral - NCenEmb - NCenSer - NCenPas - NCenFalla &
      & - NCenBat
!
!**********************************
!     chequeo de la validez de los datos
!**********************************
      FStop = .FALSE.
      IF (NCentral .GT. Dim%Cen) THEN
         WRITE(6, '(A, I4, A, I4, A)')                                  &
     &        'leecnfcen: Error, numero de centrales = ',               &
     &        NCentral, ' > ',                                          &
     &        Dim%Cen, ', fin.'
         WRITE(ULog, '(A, I4, A, I4, A)')                               &
     &        'leecnfcen: Error, numero de centrales = ',               &
     &        NCentral, ' > ',                                          &
     &        Dim%Cen, ', fin.'
         FStop = .TRUE.
      ENDIF
      IF (NCenEmb .GT. Dim%Emb) THEN
         WRITE(6, '(A, I4, A, I4, A)')                                  &
     &        'leecnfcen: Error, numero de embalses = ', NCenEmb, ' > ',&
     &        Dim%Emb, ', fin.'
         WRITE(ULog, '(A, I4, A, I4, A)')                               &
     &        'leecnfcen: Error, numero de embalses = ', NCenEmb, ' > ',&
     &        Dim%Emb, ', fin.'
         FStop = .TRUE.
      ENDIF
      IF (NCenSer .GT. Dim%Ser) THEN
         WRITE(6, '(A, I4, A, I4, A)')                                  &
     &        'leecnfcen: Error, numero de series = ', NCenSer, ' > ',  &
     &        Dim%Ser, ', fin.'
         WRITE(ULog, '(A, I4, A, I4, A)')                               &
     &        'leecnfcen: Error, numero de series = ', NCenSer, ' > ',  &
     &        Dim%Ser, ', fin.'
         FStop = .TRUE.
      ENDIF
      IF (NCenPas .GT. Dim%Pas) THEN
         WRITE(6, '(A, I3, A, I4, A)')                                  &
     &        'leecnfcen: Error, numero de pasadas = ', NCenPas, ' > ', &
     &        Dim%Pas, ', fin.'
         WRITE(ULog, '(A, I3, A, I4, A)')                               &
     &        'leecnfcen: Error, numero de pasadas = ', NCenPas, ' > ', &
     &        Dim%Pas, ', fin.'
         FStop = .TRUE.
      ENDIF
      IF (FStop) THEN
         STOP 1
      ENDIF
!
!*********************************************
!     Flags de Intermitencia, Minimo Tecnicos Costo de Arranque y
!     Detencion, Fase Sin Minimo Tecnico, Mas
!     Etapa de Eliminacion de Minimos Tecnicos
!*********************************************
      READ(URead, '(A1)') AuxVar
      READ(URead, *) FCenInter, FMinTec, FCenCAD, FFaseSinMT, EtaIniSinMT
!
!******************************
!     Lee Datos de Centrales Embalse
!******************************
      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      DO ICen = 1, NCenEmb
         READ(URead, '(A1)') AuxVar
         READ(URead, *)                                                 &
     &        CenInd(ICen), CenNom(ICen),                               &
     &        CenIPot,                                                  &
     &        CenMinTec, CenInter,                                      &
     &        CenFCAD, CenMTTdHrz, EstocFIndep(ICen)

!
!
!*****************************
!     Lectura intervalos generacion
!*****************************
         READ(URead, '(A1)') AuxVar
         READ(URead, *) CenPotMin,                             &
     &        CenPotMax, CenVertMin, CenVertMax
         IF (CenPotMin .GT. CenPotMax) THEN
            WRITE(6, '(2A)')                                         &
     &           'leecnfcen: Error en los datos central ',           &
     &           CenNom(ICen)
            WRITE(6, '(A, I1, A)')                                   &
     &           'leecnfcen: El intervalo de generacion ', 1,  &
     &           ' esta mal definido.'
            WRITE(ULog, '(2A)')                                      &
     &           'leecnfcen: Error en los datos central ',           &
     &           CenNom(ICen)
            WRITE(ULog, '(A, I1, A)')                                &
     &           'leecnfcen: El intervalo de generacion ', 1,  &
     &           ' esta mal definido.'
            FStop = .TRUE.
         ENDIF
!
!***********************************
!     Lectura costos arranque y detencion
!***********************************
         READ(URead, '(A1)') AuxVar
         READ(URead, *) CenCArr, CenCDet, CenOn
!
!     Lectura otros datos
!*******************
         READ(URead, '(A1)') AuxVar
         READ(URead, *) CenCosVar(ICen), CenRen(ICen),                  &
     &        CenGBar(ICen), CenGHid(ICen, 1),                          &
     &        CenVHid(ICen, 1), CenPIni,                          &
     &        CenCauAfl, EmbVolIni, EmbVolFin,                          &
     &        EmbVolMin, EmbVolMax,                                     &
     &        EmbFEsc(ICen), EmbCFUE(ICen)
         IF (CenGBar(ICen) .EQ. 0) THEN
            CenTipo(ICen) = PCenTipEmbAux
            Dim%CenLabel(ICen)= PCenTipEmbAux
         ELSE
            CenTipo(ICen) = PCenTipEmb
            Dim%CenLabel(ICen) =PCenTipEmb
         ENDIF
         ! CenCosVar(ICen) = 0.d0
         DO IEta = 1, NEtapa
            EmbVMin(ICen, IEta) = EmbVolMin*EmbFEsc(ICen)/1D3
            EmbVMax(ICen, IEta) = EmbVolMax*EmbFEsc(ICen)/1D3
         ENDDO
         DO IBlo = 1, NBloques
            HidSPPQAfl(ICen, IBlo) = CenCauAfl
         ENDDO
         DO IBlo = 1, NBloques
            CenPMin(ICen, IBlo) = CenPotMin
            CenPMax(ICen, IBlo) = CenPotMax
            CenVMin(ICen, IBlo) = CenVertMin
            CenVMax(ICen, IBlo) = CenVertMax
         ENDDO
         EmbVIni(ICen) = EmbVolIni*EmbFEsc(ICen)/1D3
         EmbVFin(ICen) = EmbVolFin*EmbFEsc(ICen)/1D3
      ENDDO
!
!********************************
!     Lee Datos Centrales Pasada Serie
!********************************
      READ(URead, '(A1)') AuxVar
      DO ICen = NCenEmb + 1, NCenEmb + NCenSer
         READ(URead, '(A1)') AuxVar
         READ(URead, *)                                                 &
     &        CenInd(ICen), CenNom(ICen),                               &
     &        CenIPot,                                            &
     &        CenMinTec, CenInter,                                &
     &        CenFCAD, CenMTTdHrz, EstocFIndep(ICen)
!
!     Lectura intervalos generacion
!*****************************
         READ(URead, '(A1)') AuxVar
         READ(URead, *) CenPotMin,                             &
     &        CenPotMax, CenVertMin, CenVertMax
         IF (CenPotMin .GT. CenPotMax) THEN
            WRITE(6, '(3A)')                                         &
     &           'leecnfcen: Error en los datos central ',           &
     &           CenNom(ICen), '.'
            WRITE(6, '(A, I1, A)')                                   &
     &           'leecnfcen: El intervalo de generacion ',           &
     &           1, ' esta mal definido.'
            WRITE(ULog, '(3A)')                                      &
     &           'leecnfcen: Error en los datos central ',           &
     &           CenNom(ICen), '.'
            WRITE(ULog, '(A, I1, A)')                                &
     &           'leecnfcen: El intervalo de generacion ',           &
     &           1, ' esta mal definido.'
            FStop = .TRUE.
         ENDIF
!
!     Lectura costos arranque y detencion
!***********************************
         READ(URead, '(A1)') AuxVar
         READ(URead, *) CenCArr, CenCDet, CenOn

         READ(URead, '(A1)') AuxVar
         READ(URead, *) CenCosVar(ICen), CenRen(ICen),                  &
     &        CenGBar(ICen),                                            &
     &        CenGHid(ICen, 1), CenVHid(ICen, 1), CenPIni,        &
     &        CenCauAfl
         ! CenCosVar(ICen) = 0.d0
         IF (CenGBar(ICen) .EQ. 0) THEN
            CenTipo(ICen) = PCenTipRie
            Dim%CenLabel(ICen) = PCenTipRie
            NCenRie = NCenRie + 1
         ELSE
            CenTipo(ICen) = PCenTipSer
            Dim%CenLabel(ICen) = PCenTipSer
         ENDIF
!
!     Inicializacion de los arreglos con los caudales afluentes de
!     las centrales: Por defecto, se suponen constantes por todo el hori
!     La posibilidad de hacerlos variar por etapa y/o de hacerlo aleator
!     encuentra en la rutina _leecaudal_.
         DO IBlo = 1, NBloques
            HidSPPQAfl(ICen, IBlo) = CenCauAfl
         ENDDO
         DO IBlo = 1, NBloques
            CenPMin(ICen, IBlo) = CenPotMin
            CenPMax(ICen, IBlo) = CenPotMax
            CenVMin(ICen, IBlo) = CenVertMin
            CenVMax(ICen, IBlo) = CenVertMax
         ENDDO
      ENDDO
!
!*******************************
!     Lee Datos Centrales Pasada Pura
!*******************************
      READ(URead, '(A1)') AuxVar
      DO ICen = NCenEmb + NCenSer + 1, NCenEmb + NCenSer + NCenPas
!
!     Indice del comienzo de las pasadas puras
         ICenPP = ICen - NCenEmb - NCenSer
         READ(URead, '(A1)') AuxVar
         READ(URead, *)                                                 &
     &        CenInd(ICen), CenNom(ICen),                               &
     &        CenIPot,                                            &
     &        CenMinTec, CenInter,                                &
     &        CenFCAD, CenMTTdHrz, EstocFIndep(ICen)
!
!     Lectura intervalos generacion
!*****************************
         READ(URead, '(A1)') AuxVar
         READ(URead, *) CenPotMin,                             &
     &        CenPotMax
         IF (CenPotMin .GT. CenPotMax) THEN
            WRITE(6, '(3A)')                                         &
     &           'leecnfcen: Error en los datos central ',           &
     &           CenNom(ICen), '.'
            WRITE(6, '(A, I1, A)')                                   &
     &           'leecnfcen: El intervalo de generacion ',           &
     &           1, ' esta mal definido.'
            WRITE(ULog, '(3A)')                                      &
     &           'leecnfcen: Error en los datos central ',           &
     &           CenNom(ICen), '.'
            WRITE(ULog, '(A, I1, A)')                                &
     &           'leecnfcen: El intervalo de generacion ',           &
     &           1, ' esta mal definido.'
            FStop = .TRUE.
         ENDIF
!
!     Lectura costos arranque y detencion
!***********************************
         READ(URead, '(A1)') AuxVar
         READ(URead, *) CenCArr, CenCDet, CenOn

         READ(URead, '(A1)') AuxVar
         READ(URead, *) CenCosVar(ICen), CenRen(ICen),                  &
     &        CenGBar(ICen),                                            &
     &        CenGHid(ICen, 1), CenVHid(ICen, 1), CenPIni,        &
     &        CenCauAfl
         ! CenCosVar(ICen) = 0.d0
         IF (CenGBar(ICen) .EQ. 0) THEN
            WRITE(6, '(3A)') 'leecnfcen: Error en los datos central ',  &
     &           CenNom(ICen), '.'
            WRITE(6, '(2A)') 'leecnfcen: No puede estar conectado ',    &
     &           'a la barra 0.'
            WRITE(ULog, '(3A)')                                         &
     &           'leecnfcen: Error en los datos central ',              &
     &           CenNom(ICen), '.'
            WRITE(ULog, '(2A)') 'leecnfcen: No puede estar conectado ', &
     &           'a la barra 0.'
            FStop = .TRUE.
         ENDIF
         CenTipo(ICen) = PCenTipPas
         Dim%CenLabel(ICen) = PCenTipPas
         DO IBlo = 1, NBloques
            PasQAfl(ICenPP, IBlo) = CenCauAfl
         ENDDO
         DO IBlo = 1, NBloques
            CenPMin(ICen, IBlo) = CenPotMin
            CenPMax(ICen, IBlo) = CenPotMax
         ENDDO
      ENDDO
!
!******************************************
!     Lee Datos de Centrales Termicas, FV, EO y CS
!******************************************
      READ(URead, '(A1)') AuxVar
      ContadorCenDesde=NCenEmb + NCenSer + NCenPas
      ContadorCenHasta=ContadorCenDesde + NCenTer
      DO ICen = ContadorCenDesde + 1, ContadorCenHasta
         READ(URead, '(A1)') AuxVar
         READ(URead, *)                                                 &
     &        CenInd(ICen), CenNom(ICen),                               &
     &        CenIPot,                                            &
     &        CenMinTec, CenInter,                                &
     &        CenFCAD, CenMTTdHrz
!
!
!     Lectura intervalos generacion
!*****************************
         READ(URead, '(A1)') AuxVar
         READ(URead, *) CenPotMin,                             &
     &        CenPotMax
         IF (CenPotMin .GT. CenPotMax) THEN
            WRITE(6, '(3A)')                                         &
     &           'leecnfcen: Error en los datos central ',           &
     &           CenNom(ICen), '.'
            WRITE(6, '(A, I1, A)')                                   &
     &           'leecnfcen: El intervalo de generacion ',           &
     &           1, ' esta mal definido.'
            WRITE(ULog, '(3A)')                                      &
     &           'leecnfcen: Error en los datos central ',           &
     &           CenNom(ICen), '.'
            WRITE(ULog, '(A, I1, A)')                                &
     &           'leecnfcen: El intervalo de generacion ',           &
     &           1, ' esta mal definido.'
            FStop = .TRUE.
         ENDIF
!
!     Lectura costos arranque y detencion
!***********************************
         READ(URead, '(A1)') AuxVar
         READ(URead, *) CenCArr, CenCDet, CenOn

         READ(URead, '(A1)') AuxVar
         READ(URead, *) CenCosVar(ICen), CenRen(ICen),                  &
     &        CenGBar(ICen),                                            &
     &        CenGHid(ICen, 1), CenVHid(ICen, 1), CenPIni
         CenTipo(ICen) = PCenTipTer
         Dim%CenLabel(ICen) = PCenTipTer
         DO IBlo = 1, NBloques
            CenPMin(ICen, IBlo) = CenPotMin
            CenPMax(ICen, IBlo) = CenPotMax
         ENDDO
      ENDDO
!======================
!     BATERIAS Y FALLAS
!======================
      ContadorCenDesde= ContadorCenHasta
      ContadorCenHasta= NCentral
      READ(URead, '(A1)') AuxVar
      DO ICen = ContadorCenDesde + 1, ContadorCenHasta
         READ(URead, '(A1)') AuxVar
         READ(URead, *)                                                 &
     &        CenInd(ICen), CenNom(ICen),                               &
     &        CenIPot,                                            &
     &        CenMinTec, CenInter,                                &
     &        CenFCAD, CenMTTdHrz
!
!
!     Lectura intervalos generacion
!*****************************
         READ(URead, '(A1)') AuxVar
         READ(URead, *) CenPotMin,                             &
     &        CenPotMax
         IF (CenPotMin .GT. CenPotMax) THEN
            WRITE(6, '(3A)')                                         &
     &           'leecnfcen: Error en los datos central ',           &
     &           CenNom(ICen), '.'
            WRITE(6, '(A, I1, A)')                                   &
     &           'leecnfcen: El intervalo de generacion ',           &
     &           1, ' esta mal definido.'
            WRITE(ULog, '(3A)')                                      &
     &           'leecnfcen: Error en los datos central ',           &
     &           CenNom(ICen), '.'
            WRITE(ULog, '(A, I1, A)')                                &
     &           'leecnfcen: El intervalo de generacion ',           &
     &           1, ' esta mal definido.'
            FStop = .TRUE.
         ENDIF
!
!     Lectura costos arranque y detencion
!***********************************
         READ(URead, '(A1)') AuxVar
         READ(URead, *) CenCArr, CenCDet, CenOn

         READ(URead, '(A1)') AuxVar
         READ(URead, *) CenCosVar(ICen), CenRen(ICen),                  &
     &        CenGBar(ICen),                                            &
     &        CenGHid(ICen, 1), CenVHid(ICen, 1), CenPIni
         IF (ICen .LE. NCentral - NCenFalla) THEN
            CenTipo(ICen) = PCenTipBat
            Dim%CenLabel(ICen) = PCenTipBat
         ELSE
            CenTipo(ICen) = PCenTipFal
            Dim%CenLabel(ICen) = PCenTipFal
         ENDIF
         DO IBlo = 1, NBloques
            CenPMin(ICen, IBlo) = CenPotMin
            CenPMax(ICen, IBlo) = CenPotMax
         ENDDO
      ENDDO
      CALL Cerrar(URead)

      
!
!**********************************
!     chequeo de la validez de los datos
!**********************************
      EtapasPorHora = 3.6d0/FactTiempo

      IF(FWarning) THEN
         WRITE(ULog, '(A)') 'leecnfcen: Hay warnings.'
         IF (FInterfaz .gt. 1) THEN
            WRITE(6, '(A)') 'leecnfcen: Hay warnings.'
            WRITE(6, '(A)') 'leecnfcen: Continuo?'
            READ(5, *)
            WRITE(6, '(A)') 'Continuando...'
         ENDIF
      ENDIF

      DO ICen = 1, NCentral
!
!     Ajustes por la unidad de medida de tiempo
!******************************************
         DO IEta = 1, NEtapa
            CenCVar(ICen, IEta) = CenCosVar(ICen)*                      &
     &           FactTiempo/3.6d0
         ENDDO
      ENDDO
!
!     Sistema Uninodal
      IF (NLinea .EQ. 0) THEN
         DO ICen = 1, NCentral
            IF(CenGBar(ICen) .NE. 0) THEN
               CenGBar(ICen) = 1
            ENDIF
         ENDDO
      ENDIF
      DO Ind1 = 1, NCentral
         CenGAux(Ind1, 1) = 0
         CenVAux(Ind1, 1) = 0
         Ind2 = 0
         DO WHILE ((Ind2 .LT. NCentral) .AND.                           &
     &        (CenGAux(Ind1, 1) .EQ. 0))
            Ind2 = Ind2 + 1
            IF (CenInd(Ind2) .EQ. CenGHid(Ind1, 1)) THEN
               CenGAux(Ind1, 1) = Ind2
            ENDIF
         ENDDO
         Ind2 = 0
         DO WHILE ((Ind2 .LT. NCentral) .AND.                           &
     &        (CenVAux(Ind1, 1) .EQ. 0))
            Ind2 = Ind2 + 1
            IF (CenInd(Ind2) .EQ. CenVHid(Ind1, 1)) THEN
               CenVAux(Ind1, 1) = Ind2
            ENDIF
         ENDDO
      ENDDO
      DO Ind1 = 1, NCentral
         CenGHid(Ind1, 1) = CenGAux(Ind1, 1)
         CenVHid(Ind1, 1) = CenVAux(Ind1, 1)
         CenGHid(Ind1, 2) = 0
         CenVHid(Ind1, 2) = 0
      END DO
      IF (FStop) THEN
         STOP 1
      ENDIF
      RETURN
      END
