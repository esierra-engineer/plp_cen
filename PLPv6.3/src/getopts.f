      SUBROUTINE GET_OPTS( &
     &     eprhs, &
     &     epopt, &
     &     ScaleObj, &
     &     ScalePhi, &
     &     ScaleAng, &
     &     ScaleVolG, &
     &     FScaleQs, &
     &     FScaleVdi, &
     &     FAngZero, &
     &     FConvLaja, &
     &     FConvMaule, &
     &     FRestRalco, &
     &     FRestGnl, &
     &     FRestReserva, &
     &     FBaterias, &
     &     FMemMode, &
     &     FVertReb, &
     &     FSeparaLP, &
     &     FSeparaCF, &
     &     FZSPFBest, &
     &     FOnePhi, &
     &     FInterfaz, &
     &     FDepHid, &
     &     FlagFilt, &
     &     FRendProm, &
     &     FRendBdrs, &
     &     FAfluFict, &
     &     FactEPS, &
     &     FactMLD, &
     &     OptiEPS, &
     &     OptiMLD, &
     &     FactMXC, &
     &     FactDBL, &
     &     FSeparaCFA, &
     &     FOneFeasRay, &
     &     DualFactIter, &
     &     FInfactSol, &
     &     idual_pmode, &
     &     iprim_pmode, &
     &     nthreads, &
     &     Dim)

#ifdef _OPENMP
      USE OMP_LIB
#endif

      USE PLP

      TYPE(PAR_DIMS) Dim

      DOUBLE PRECISION eprhs
      DOUBLE PRECISION epopt
      DOUBLE PRECISION ScaleObj
      DOUBLE PRECISION ScalePhi
      DOUBLE PRECISION ScaleAng
      DOUBLE PRECISION ScaleVolG

      LOGICAL FScaleQs
      LOGICAL FScaleVdi
      LOGICAL FAngZero
      INTEGER FConvLaja
      INTEGER FConvMaule
      LOGICAL FRestRalco
      LOGICAL FRestGnl
      LOGICAL FRestReserva
      LOGICAL FBaterias
      INTEGER FMemMode
      LOGICAL FVertReb
      LOGICAL FSeparaLP
      LOGICAL FSeparaCF
      LOGICAL FZSPFBest
      LOGICAL FOnePhi
      INTEGER FInterfaz
      LOGICAL FDepHid
      INTEGER FlagFilt
      LOGICAL FRendProm
      LOGICAL FRendBdrs
      LOGICAL FAfluFict
      DOUBLE PRECISION FactEPS
      DOUBLE PRECISION FactMLD
      DOUBLE PRECISION OptiEPS
      DOUBLE PRECISION OptiMLD
      INTEGER FactMXC
      INTEGER FactDBL
      LOGICAL FSeparaCFA
      LOGICAL FOneFeasRay
      INTEGER DualFactIter
      LOGICAL FInfactSol
      INTEGER idual_pmode
      INTEGER iprim_pmode
      INTEGER nthreads
      INTEGER read_stat

!     locals

      CHARACTER(len=255) :: optieps_value
      CHARACTER(len=255) :: onephi_mode
      CHARACTER(len=255) :: zspfbest_mode
      CHARACTER(len=255) :: aflufict_mode
      CHARACTER(len=255) :: separalp_mode
      CHARACTER(len=255) :: filtrac_mode
      CHARACTER(len=255) :: rendprom_mode
      CHARACTER(len=255) :: rendbdrs_mode
      CHARACTER(len=255) :: separacf_mode
      CHARACTER(len=255) :: angzero_mode
      CHARACTER(len=255) :: vertreb_mode
      CHARACTER(len=255) :: fact_mode
      CHARACTER(len=255) :: convlaja_mode
      CHARACTER(len=255) :: convmaule_mode
      CHARACTER(len=255) :: restralco_mode
      CHARACTER(len=255) :: restgnl_mode
      CHARACTER(len=255) :: restreserva_mode
      CHARACTER(len=255) :: baterias_mode
      CHARACTER(len=255) :: memory_mode
      CHARACTER(len=255) :: scaleqs_mode
      CHARACTER(len=255) :: scaleobj_mode
      CHARACTER(len=255) :: scalevol_mode
      CHARACTER(len=255) :: scalevdi_mode
      CHARACTER(len=255) :: scalephi_mode
      CHARACTER(len=255) :: scaleang_mode
      CHARACTER(len=255) :: scale_mode
      CHARACTER(len=255) :: dephid_mode
      CHARACTER(len=255) :: factdbl_value
      CHARACTER(len=255) :: interfaz_mode
      CHARACTER(len=255) :: facteps_value
      CHARACTER(len=255) :: factmld_value
      CHARACTER(len=255) :: optimld_value
      CHARACTER(len=255) :: separacfa_mode
      CHARACTER(len=255) :: onefeasray_mode
      CHARACTER(len=255) :: infactsol_mode
      CHARACTER(len=255) :: factmxc_value
      CHARACTER(len=255) :: epopt_value
      CHARACTER(len=255) :: eprhs_value
      CHARACTER(len=255) :: value
      CHARACTER(len=255) :: pmode

!

      FDepHid = .TRUE.
      CALL get_environment_variable("PLP_DEPHID_MODE", dephid_mode)
      IF (dephid_mode .eq. 'no') THEN
         FDepHid = .FALSE.
      ENDIF
!
      FAfluFict = .TRUE.
      CALL get_environment_variable("PLP_AFLUFICT_MODE", aflufict_mode)
      IF (aflufict_mode .eq. 'no') THEN
         FAfluFict = .FALSE.
      ENDIF
!
      FSeparaLP = .FALSE.
#ifdef _OPENMP
      FSeparaLP = .TRUE.
#endif

      CALL get_environment_variable("PLP_SEPARALP_MODE", separalp_mode)
      IF (separalp_mode .eq. 'si') THEN
         FSeparaLP = .TRUE.
      ENDIF
      IF (separalp_mode .eq. 'no') THEN
         FSeparaLP = .FALSE.
      ENDIF
!
      FlagFilt = FILT_LINE
      CALL get_environment_variable("PLP_FILTRAC_MODE", filtrac_mode)
      IF (filtrac_mode .eq. 'prom') THEN
         FlagFilt = FILT_PROM
      ENDIF

      IF (filtrac_mode .eq. 'lineal') THEN
         FlagFilt = FILT_LINE
      ENDIF

      IF (filtrac_mode .eq. 'const') THEN
         FlagFilt = FILT_CONS
      ENDIF


!
      FRendProm = .FALSE.
      CALL get_environment_variable("PLP_RENDPROM_MODE", rendprom_mode)
      IF (rendprom_mode .eq. 'si') THEN
         FRendProm = .TRUE.
      ENDIF
!
      FRendBdrs = .FALSE.
      CALL get_environment_variable("PLP_RENDBDRS_MODE", rendbdrs_mode)
      IF (rendbdrs_mode .eq. 'si') THEN
         FRendBdrs = .TRUE.
      ENDIF
!
      FSeparaCF = .FALSE.
      CALL get_environment_variable("PLP_SEPARACF_MODE", separacf_mode)
      IF (separacf_mode .eq. 'si') THEN
         FSeparaCF = .TRUE.
      ENDIF
!
      FZSPFBest = .TRUE.
      CALL get_environment_variable("PLP_ZSPFBEST_MODE", zspfbest_mode)
      IF (zspfbest_mode .eq.'no') THEN
         FZSPFBest = .FALSE.
      ENDIF
!
      FOnePhi = .FALSE.
      CALL get_environment_variable("PLP_ONEPHI_MODE", onephi_mode)
      IF (onephi_mode .eq.'si') THEN
         FOnePhi = .TRUE.
      ENDIF
!
      FAngZero = .FALSE.
      CALL get_environment_variable("PLP_ANGZERO_MODE", angzero_mode)
      IF (angzero_mode .eq.'si') THEN
         FAngZero = .TRUE.
      ENDIF
!     Escalamiento de restricciones de flujos, activar con 'si'
      FScaleQs = .FALSE.
      CALL get_environment_variable("PLP_SCALEQS_MODE", scaleqs_mode)
      IF (scaleqs_mode .eq. 'si') THEN
         FScaleQs = .TRUE.
      ENDIF
!     Tolerancia numerica superior agregada a los cortes de factibilidad en
!     el lado derecho

      FactEPS = 1.0d-8
      CALL get_environment_variable("PLP_FACTEPS_VALUE", facteps_value)
      IF (facteps_value .ne. '') THEN
         READ(facteps_value,*) FactEPS
      ENDIF

      FactMLD = 1.0d3
      CALL get_environment_variable("PLP_FACTMLD_VALUE", factmld_value)
      IF (factmld_value .ne. '') THEN
         READ(factmld_value,*) FactMLD
      ENDIF

      OptiEPS = 1.0d-8
      CALL get_environment_variable("PLP_OPTIEPS_VALUE", optieps_value)
      IF (optieps_value .ne. '') THEN
         READ(optieps_value,*) OptiEPS
      ENDIF

      OptiMLD = 1.0d3
      CALL get_environment_variable("PLP_OPTIMLD_VALUE", optimld_value)
      IF (optimld_value .ne. '') THEN
         READ(optimld_value,*) OptiMLD
      ENDIF


!     Maximo numureo de ciclos de factibilidad
      FactMXC = 5000
      CALL get_environment_variable("PLP_FACTMXC_VALUE", factmxc_value)
      IF (factmxc_value .ne. '') THEN
         READ(factmxc_value,*) FactMXC
      ENDIF

!
      FactDBL = 0
      CALL get_environment_variable("PLP_FACTDBL_VALUE", factdbl_value)
      IF (factdbl_value .ne. '') THEN
         READ(factdbl_value,*) FactDBL
      ENDIF

!
      FSeparaCFA = .TRUE.
      CALL get_environment_variable("PLP_SEPARACFA_MODE", separacfa_mode)
      IF (separacfa_mode .eq. 'no') THEN
         FSeparaCFA = .FALSE.
      ENDIF

!
      FOneFeasRay = .FALSE.
      CALL get_environment_variable("PLP_FONEFEASRAY_MODE", onefeasray_mode)
      IF (onefeasray_mode .eq. 'si') THEN
         FOneFeasRay = .TRUE.
      ENDIF

!
      DualFactIter = 10
      CALL get_environment_variable("PLP_DUALFACTITER_VALUE", value)
      IF (value .ne. '') THEN
         READ(value,*) DualFactIter
      ENDIF

!
      FInfactSol = .FALSE.
      CALL get_environment_variable("PLP_INFACTSOL_MODE", infactsol_mode)
      IF (infactsol_mode .eq. 'si') THEN
         FInfactSol = .TRUE.
      ENDIF


!     Activacion de vertimiento por rebalse de embalses
      FVertReb = .TRUE.
      CALL get_environment_variable("PLP_VERTREB_MODE", vertreb_mode)
      IF (vertreb_mode .eq. 'no') THEN
         FVertReb = .FALSE.
      ENDIF

!
!     Activacion de Convenio el Laja
      FConvLaja = 0
      CALL get_environment_variable("PLP_CONVLAJA_MODE", convlaja_mode)
      IF (convlaja_mode .eq. 'si') THEN
         FConvLaja = 1
      ELSE IF (convlaja_mode .eq. 'no') THEN
         FConvLaja = 0
      ELSE IF (convlaja_mode .ne. '') THEN
         READ(convlaja_mode, *) FConvLaja
      ENDIF
!     Activacion de Convenio el Maule
      FConvMaule = 0
      CALL get_environment_variable("PLP_CONVMAULE_MODE", convmaule_mode)
      IF (convmaule_mode .eq. 'si') THEN
         FConvMaule = 1
      ELSE IF (convmaule_mode .eq. 'no') THEN
         FConvMaule = 0
      ELSE IF (convmaule_mode .ne. '') THEN
         READ(convmaule_mode, *) FConvMaule
      ENDIF

      IF (FConvLaja + FConvMaule .ne. 0) THEN
         FScaleQs = .TRUE.
      ENDIF

!     Activacion de restriccion de Ralco
      FRestRalco = .FALSE.
      CALL get_environment_variable("PLP_RESTRALCO_MODE", restralco_mode)
      IF (restralco_mode .eq. 'si') THEN
         FRestRalco = .TRUE.
      ENDIF
!     Activacion de restriccion de Gnl
      FRestGnl = .FALSE.
      CALL get_environment_variable("PLP_RESTGNL_MODE", restgnl_mode)
      IF (restgnl_mode .eq. 'si') THEN
         FRestGnl = .TRUE.
      ENDIF
!     Activacion de restriccion de Reserva
      FRestReserva = .TRUE.
      CALL get_environment_variable("PLP_RESTRESERVA_MODE", restreserva_mode)
      IF (restreserva_mode .eq. 'no') THEN
         FRestReserva = .FALSE.
      ENDIF
      FBaterias = .TRUE.
      CALL get_environment_variable("PLP_BATERIAS_MODE", baterias_mode)
      if (baterias_mode .eq. 'no') THEN
         FBaterias = .False.
      ENDIF
!     Activacion de modo de memoria
      FMemMode = -1
      CALL get_environment_variable("PLP_MEMORY_MODE", memory_mode)
      IF (memory_mode .ne. '') THEN
         READ(memory_mode,*) FMemMode
      ENDIF
!     Escalamiento de la funcion objetivo, activar con 'si' o pasar un valor
      ScaleObj = 1.0d0
      CALL get_environment_variable("PLP_SCALEOBJ_MODE", scaleobj_mode)
      IF (scaleobj_mode .eq. 'si') THEN
         ScaleObj = 1.0d7
      ELSE IF ((scaleobj_mode .ne. 'no').and.(scaleobj_mode .ne. '')) THEN
         READ(scaleobj_mode,*) ScaleObj
      ENDIF
!     Escalamiento dinamico de los volumenes de los embalses,
!     se activa con 'si', y hace que las variables de volumenese de  embalses
!     esten todoas entre [0,10]
      FScaleVdi = .FALSE.
      CALL get_environment_variable("PLP_SCALEVDI_MODE", scalevdi_mode)
      IF (scalevdi_mode .eq. 'si') THEN
         FScaleVdi = .TRUE.
      ENDIF
!     Escalamiento global de volumenes de embalse, se activa con 'si' o valor
      ScaleVolG = 1.0d0
      CALL get_environment_variable("PLP_SCALEVOL_MODE", scalevol_mode)
      IF (scalevol_mode .eq. 'si') THEN
         ScaleVolG = 1.0d3
      ELSE IF ((scalevol_mode .ne. 'no').and.(scalevol_mode .ne. '')) THEN
         READ(scalevol_mode,*) ScaleVolG
      ENDIF
!     Escala las varibles 'varphi' que refieren al valor del agua,
!     activa con 'si' o valor
      ScalePhi = 1.0d0
      CALL get_environment_variable("PLP_SCALEPHI_MODE", scalephi_mode)
      IF (scalephi_mode.eq.'si') THEN
         ScalePhi = 1.0d7
      ELSE IF ((scalephi_mode .ne. 'no').and.(scalephi_mode .ne. '')) THEN
         READ(scalephi_mode,*) ScalePhi
      ENDIF
!     Escala las varibles 'varang' que refieren al valor del agua,
!     activa con 'si' o valor
      ScaleAng = 1.0d0
      CALL get_environment_variable("PLP_SCALEANG_MODE", scaleang_mode)
      IF (scaleang_mode.eq.'si') THEN
         ScaleAng = 1.0d4
      ELSE IF ((scaleang_mode .ne. 'no').and.(scaleang_mode .ne. '')) THEN
         READ(scaleang_mode,*) ScaleAng
      ENDIF

!     Scale Mode
      CALL get_environment_variable("PLP_SCALE_MODE", scale_mode)
      IF (scale_mode .eq. 'si') THEN
         FactEPS = 1.0d-8
         FactMLD = 1.0d3
         OptiEPS = 1.0d-8
         OptiMLD = 1.0d3
         FScaleQs = .TRUE.
         ScaleVolG = 1d0
         ScaleObj = 1d7
         ScalePhi = 1d7
         ScaleAng = 1d4
         FAngZero = .TRUE.
         FScaleVdi = .TRUE.
         IF (scalevdi_mode .eq. 'no') THEN
            FScaleVdi = .FALSE.
         ENDIF
         IF (scalevol_mode .ne. '') THEN
            READ(scalevol_mode,*) ScaleVolG
         ENDIF
         IF (scaleobj_mode .ne. '') THEN
            READ(scaleobj_mode,*) ScaleObj
         ENDIF
         IF (scalephi_mode .ne. '') THEN
            READ(scalephi_mode,*) ScalePhi
         ENDIF
         IF (scaleang_mode .ne. '') THEN
            READ(scaleang_mode,*) ScaleAng
         ENDIF
         IF (facteps_value .ne. '') THEN
            READ(facteps_value,*) FactEPS
         ENDIF
         IF (factmld_value .ne. '') THEN
            READ(factmld_value,*) FactMLD
         ENDIF
         IF (optieps_value .ne. '') THEN
            READ(optieps_value,*) OptiEPS
         ENDIF
         IF (optimld_value .ne. '') THEN
            READ(optimld_value,*) OptiMLD
         ENDIF

         IF (scalevdi_mode .eq. 'no') THEN
            FScaleVdi = .FALSE.
         ENDIF
         IF (scalevol_mode .ne. '') THEN
            READ(scalevol_mode,*) ScaleVolG
         ENDIF
      ENDIF

      IF (FInterfaz .ge. 0) THEN
         CALL get_environment_variable("PLP_INTERFAZ_MODE", interfaz_mode)
         IF (interfaz_mode .eq. 'si') THEN
            FInterfaz = 1
         ELSE IF (interfaz_mode .eq. 'no') THEN
            FInterfaz = 0
         ELSE IF (interfaz_mode .ne. '') THEN
            READ(interfaz_mode,*, iostat=read_stat) FInterfaz
            IF (read_stat .ne. 0) THEN
               FInterfaz = 1
            ENDIF
         ENDIF
      ENDIF

      IF (.NOT. FAfluFict) THEN
         FSeparaLP = .TRUE.
      ENDIF

      IF (FOnePhi) THEN
         FSeparaLP = .TRUE.
      ENDIF

      epopt = 1.0d-9
      CALL get_environment_variable("PLP_EPOPT_VALUE", epopt_value)
      IF (epopt_value .ne. '') THEN
         READ(epopt_value,*) epopt
      ENDIF
      eprhs = 1.0d-9
      CALL get_environment_variable("PLP_EPRHS_VALUE", eprhs_value)
      IF (eprhs_value .ne. '') THEN
         READ(eprhs_value,*) eprhs
      ENDIF

      idual_pmode = 0
      iprim_pmode = 0
      nthreads = 1

      CALL get_environment_variable("PLP_PDUAL_MODE", pmode)
      IF (pmode.eq.'no') THEN
         idual_pmode = 0
      ELSEIF (pmode.eq.'si') THEN
         idual_pmode = 1
      ENDIF

      CALL get_environment_variable("PLP_PPRIM_MODE", pmode)
      IF (pmode.eq.'no') THEN
         iprim_pmode = 0
      ELSEIF (pmode.eq.'si') THEN
         iprim_pmode = 1
      ENDIF

      CALL get_environment_variable("PLP_PARAL_MODE", pmode)
      IF (pmode .eq. 'no') THEN
         iprim_pmode = 0
         idual_pmode = 0
      ELSEIF (pmode.eq.'si') THEN
         iprim_pmode = 1
         idual_pmode = 1
      ENDIF

      IF ((idual_pmode .NE. 0) .OR. (iprim_pmode .NE. 0)) THEN
#ifdef _OPENMP
         nthreads = OMP_get_max_threads()
         IF (nthreads .GT. 1) THEN
            idual_pmode = 1
            iprim_pmode = 1
         ENDIF
#else
         nthreads = 1
         idual_pmode = 0
         iprim_pmode = 0
         WRITE(6,'(2A)') 'Uso de paralelismo requiere ', &
     &        ' recompilar con soporte OpenMP.'
#endif
      ENDIF


!     Modo de cortes de factibilidad
      Dim%FactMode = FACT_ELASTIC
      CALL get_environment_variable("PLP_FACT_MODE", fact_mode)
      IF (fact_mode .ne. '') THEN
         READ(fact_mode,*) Dim%FactMode
      ENDIF

      RETURN
      END


      SUBROUTINE PRINT_OPTSI(ULog, &
     &     eprhs, &
     &     epopt, &
     &     ScaleObj, &
     &     ScalePhi, &
     &     ScaleAng, &
     &     ScaleVolG, &
     &     FScaleQs, &
     &     FScaleVdi, &
     &     FAngZero, &
     &     FConvLaja, &
     &     FConvMaule, &
     &     FRestRalco, &
     &     FRestGnl, &
     &     FRestReserva, &
     &     FBaterias, &
     &     FMemMode, &
     &     FOldLaja, &
     &     FOldMaule, &
     &     FVertReb, &
     &     FSeparaLP, &
     &     FSeparaCF, &
     &     FZSPFBest, &
     &     FOnePhi, &
     &     FInterfaz, &
     &     FDepHid, &
     &     FlagFilt, &
     &     FFiltVar, &
     &     FExtrac, &
     &     FRendProm, &
     &     FRendBdrs, &
     &     FAfluFict, &
     &     FactEPS, &
     &     FactMLD, &
     &     OptiEPS, &
     &     OptiMLD, &
     &     FactMXC, &
     &     FactDBL, &
     &     FSeparaCFA, &
     &     FOneFeasRay, &
     &     DualFactIter, &
     &     FInfactSol, &
     &     idual_pmode, &
     &     iprim_pmode, &
     &     nthreads, &
     &     CenNom, ScaleVol, &
     &     Dim)
      USE PLP, ONLY : PAR_DIMS
      TYPE(PAR_DIMS), INTENT(IN)::  Dim

      INTEGER ULOG

      DOUBLE PRECISION eprhs
      DOUBLE PRECISION epopt
      DOUBLE PRECISION ScaleObj
      DOUBLE PRECISION ScalePhi
      DOUBLE PRECISION ScaleAng
      DOUBLE PRECISION ScaleVolG
      LOGICAL FScaleQs
      LOGICAL FScaleVdi
      LOGICAL FAngZero
      INTEGER FConvLaja
      INTEGER FConvMaule
      LOGICAL FRestRalco
      LOGICAL FRestGnl
      LOGICAL FRestReserva
      LOGICAL FBaterias
      INTEGER FMemMode
      LOGICAL FOldLaja
      LOGICAL FOldMaule
      LOGICAL FVertReb
      LOGICAL FSeparaLP
      LOGICAL FSeparaCF
      LOGICAL FZSPFBest
      LOGICAL FOnePhi
      INTEGER FInterfaz
      LOGICAL FDepHid
      INTEGER FlagFilt
      LOGICAL FFiltVar
      LOGICAL FExtrac
      LOGICAL FRendProm
      LOGICAL FRendBdrs
      LOGICAL FAfluFict
      DOUBLE PRECISION FactEPS
      DOUBLE PRECISION FactMLD
      DOUBLE PRECISION OptiEPS
      DOUBLE PRECISION OptiMLD
      INTEGER FactMXC
      INTEGER FactDBL
      LOGICAL FSeparaCFA
      LOGICAL FOneFeasRay
      INTEGER DualFactIter

      LOGICAL FInfactSol
      INTEGER idual_pmode
      INTEGER iprim_pmode
      INTEGER nthreads

      CHARACTER*48 CenNom(Dim%Emb)
      DOUBLE PRECISION ScaleVol(Dim%Emb)
!
      INTEGER IEmb
!

      WRITE (ULog, '(A)') '===== General Params ======'
      WRITE (ULOG, '(A10, G14.3)') 'EPOpt', epopt
      WRITE (ULOG, '(A10, G14.3)') 'Eprhs', eprhs
      WRITE (ULog, '(A10, G14.3)') 'ScaleObj', ScaleObj
      WRITE (ULog, '(A10, G14.3)') 'ScalePhi', ScalePhi
      WRITE (ULog, '(A10, G14.3)') 'ScaleAng', ScaleAng
      WRITE (ULog, '(A10, G14.3)') 'ScaleVol', ScaleVolG
      WRITE (ULog, '(A10, L5)') 'FScaleQs', FScaleQs
      WRITE (ULog, '(A10, L5)') 'FScaleVdi', FScaleVdi
      WRITE (ULog, '(A10, L5)') 'FAngZero', FAngZero
      WRITE (ULog, '(A10, I5)') 'FConvLaja', FConvLaja
      WRITE (ULog, '(A10, I5)') 'FConvMaule', FConvMaule
      WRITE (ULog, '(A10, L5)') 'FRestRalco', FRestRalco
      WRITE (ULog, '(A10, L5)') 'FRestGnl', FRestGnl
      WRITE (ULog, '(A10, L5)') 'FRestReserva', FRestReserva
      WRITE (ULog, '(A10, L5)') 'FBaterias', FBaterias
      WRITE (ULog, '(A10, I5)') 'FMemMode', FMemMode
      WRITE (ULog, '(A10, L5)') 'FOldLaja', FOldLaja
      WRITE (ULog, '(A10, L5)') 'FOldMaule', FOldMaule
      WRITE (ULog, '(A10, L5)') 'FVertReb', FVertReb
      WRITE (ULog, '(A10, L5)') 'FSeparaLP', FSeparaLP
      WRITE (ULog, '(A10, L5)') 'FSeparaCF', FSeparaCF
      WRITE (ULog, '(A10, L5)') 'FZSPFBest', FZSPFBest
      WRITE (ULog, '(A10, L5)') 'FOnePhi', FOnePhi
      WRITE (ULog, '(A10, I5)') 'FInterfaz', FInterfaz
      WRITE (ULog, '(A10, L5)') 'FDepHid', FDepHid
      WRITE (ULog, '(A10, I5)') 'FlagFilt', FlagFilt
      WRITE (ULog, '(A10, L5)') 'FFiltVar', FFiltVar
      WRITE (ULog, '(A10, L5)') 'FExtrac', FExtrac
      WRITE (ULog, '(A10, L5)') 'FRendProm', FRendProm
      WRITE (ULog, '(A10, L5)') 'FRendBdrs', FRendBdrs
      WRITE (ULog, '(A10, L5)') 'FAfluFict', FAfluFict
      WRITE (ULog, '(A10, G14.3)') 'FactEPS', FactEPS
      WRITE (ULog, '(A10, G14.3)') 'FactMLD', FactMLD
      WRITE (ULog, '(A10, G14.3)') 'OptiEPS', OptiEPS
      WRITE (ULog, '(A10, G14.3)') 'OptiMLD', OptiMLD
      WRITE (ULog, '(A10, I5)') 'FactMXC', FactMXC
      WRITE (ULog, '(A10, I5)') 'FactDBL', FactDBL
      WRITE (ULog, '(A10, L5)') 'FSeparaCFA', FSeparaCFA
      WRITE (ULog, '(A10, L5)') 'FOneFeasRay', FOneFeasRay
      WRITE (ULog, '(A10, I5)') 'DualFactIter', DualFactIter
      WRITE (ULog, '(A10, L5)') 'FInfactSol', FInfactSol
      WRITE (ULog, '(A10, I5)') 'IDualPMode', idual_pmode
      WRITE (ULog, '(A10, I5)') 'IPrimPMode', iprim_pmode
      WRITE (ULog, '(A10, I5)') 'nthreads', nthreads


      WRITE (ULog, '(A)') '===== Scale Emb ======'
      DO IEmb=1, Dim%Emb
         WRITE(ULog,'(A, A12, A, G14.3)') 'Embalse = ', CenNom(IEmb), &
     &        ', escala = ', ScaleVol(IEmb)
      ENDDO

      WRITE (ULog, '(A)') '===== Dim Params ======'
      WRITE (ULog, '(A10, I5)') 'Simul' , Dim%Simul
      WRITE (ULog, '(A10, I5)') 'Eta', Dim%Eta
      WRITE (ULog, '(A10, I5)') 'Blo', Dim%Blo
      WRITE (ULog, '(A10, I5)') 'IBlo', Dim%IBlo
      WRITE (ULog, '(A10, I5)') 'Year', Dim%Year
      WRITE (ULog, '(A10, I5)') 'Kit', Dim%Kit
      WRITE (ULog, '(A10, I5)') 'PDLDAcFila', Dim%PDLDAcFila
      WRITE (ULog, '(A10, I5)') 'PDLDAcCol', Dim%PDLDAcCol
      WRITE (ULog, '(A10, I5)') 'EstocCol', Dim%EstocCol
      WRITE (ULog, '(A10, I5)') 'EstocFila', Dim%EstocFila
      WRITE (ULog, '(A10, I5)') 'EstocVar', Dim%EstocVar
      WRITE (ULog, '(A10, I5)') 'PDIter', Dim%PDIter
      WRITE (ULog, '(A10, I5)') 'Apert', Dim%Apert
      WRITE (ULog, '(A10, I5)') 'Clase', Dim%Clase
      WRITE (ULog, '(A10, I5)') 'XCol', Dim%XCol
      WRITE (ULog, '(A10, I5)') 'XFila', Dim%XFila
      WRITE (ULog, '(A10, I5)') 'Cen', Dim%Cen
      WRITE (ULog, '(A10, I5)') 'Emb', Dim%Emb
      WRITE (ULog, '(A10, I5)') 'Lin', Dim%Lin
      WRITE (ULog, '(A10, I5)') 'Extr', Dim%Extr
      WRITE (ULog, '(A10, I5)') 'Bar', Dim%Bar
      WRITE (ULog, '(A10, I5)') 'Vert', Dim%Vert
      WRITE (ULog, '(A10, I5)') 'Ser', Dim%Ser
      WRITE (ULog, '(A10, I5)') 'Pas', Dim%Pas
      WRITE (ULog, '(A10, I5)') 'Ter', Dim%Ter
      WRITE (ULog, '(A10, I5)') 'Hid', Dim%Hid
      WRITE (ULog, '(A10, I5)') 'HidSPP', Dim%HidSPP
      WRITE (ULog, '(A10, I5)') 'Falla', Dim%Falla
      WRITE (ULog, '(A10, I5)') 'Flu', Dim%Flu
      WRITE (ULog, '(A10, I5)') 'EmbVReb', Dim%EmbVReb
      WRITE (ULog, '(A10, I5)') 'EmbFilt', Dim%EmbFilt
      WRITE (ULog, '(A10, I5)') 'FiltParam', Dim%FiltParam
      WRITE (ULog, '(A10, I5)') 'FiltTramo', Dim%FiltTramo
      WRITE (ULog, '(A10, I5)') 'EmbRend', Dim%EmbRend
      WRITE (ULog, '(A10, I5)') 'RendParam', Dim%RendParam
      WRITE (ULog, '(A10, I5)') 'RendTramo', Dim%RendTramo

      WRITE (ULog, '(A10, I5)') 'CenManS', Dim%CenManS
      WRITE (ULog, '(A10, I5)') 'LinManS', Dim%LinManS
      WRITE (ULog, '(A10, I5)') 'EmbManS', Dim%EmbManS

      WRITE (ULog, '(A10, I5)') 'FactMode', Dim%FactMode

      RETURN
      END


      SUBROUTINE PRINT_OPTS(ULog, &
     &     eprhs, &
     &     epopt, &
     &     ScaleObj, &
     &     ScalePhi, &
     &     ScaleAng, &
     &     ScaleVolG, &
     &     FScaleQs, &
     &     FScaleVdi, &
     &     FAngZero, &
     &     FConvLaja, &
     &     FConvMaule, &
     &     FRestRalco, &
     &     FRestGnl, &
     &     FRestReserva, &
     &     FBaterias, &
     &     FMemMode, &
     &     FOldLaja, &
     &     FOldMaule, &
     &     FVertReb, &
     &     FSeparaLP, &
     &     FSeparaCF, &
     &     FZSPFBest, &
     &     FOnePhi, &
     &     FInterfaz, &
     &     FDepHid, &
     &     FlagFilt, &
     &     FFiltVar, &
     &     FExtrac, &
     &     FRendProm, &
     &     FRendBdrs, &
     &     FAfluFict, &
     &     FactEPS, &
     &     FactMLD, &
     &     OptiEPS, &
     &     OptiMLD, &
     &     FactMXC, &
     &     FactDBL, &
     &     FSeparaCFA, &
     &     FOneFeasRay, &
     &     DualFactIter, &
     &     FInfactSol, &
     &     idual_pmode, &
     &     iprim_pmode, &
     &     nthreads, &
     &     CenNom, ScaleVol, &
     &     Dim)
      USE PLP, ONLY : PAR_DIMS
      TYPE(PAR_DIMS), INTENT(IN)::  Dim

      INTEGER ULOG

      DOUBLE PRECISION eprhs
      DOUBLE PRECISION epopt
      DOUBLE PRECISION ScaleObj
      DOUBLE PRECISION ScalePhi
      DOUBLE PRECISION ScaleAng
      DOUBLE PRECISION ScaleVolG
      LOGICAL FScaleQs
      LOGICAL FScaleVdi
      LOGICAL FAngZero
      INTEGER FConvLaja
      INTEGER FConvMaule
      LOGICAL FRestRalco
      LOGICAL FRestGnl
      LOGICAL FRestReserva
      LOGICAL FBaterias
      INTEGER FMemMode
      LOGICAL FOldLaja
      LOGICAL FOldMaule
      LOGICAL FVertReb
      LOGICAL FSeparaLP
      LOGICAL FSeparaCF
      LOGICAL FZSPFBest
      LOGICAL FOnePhi
      INTEGER FInterfaz
      LOGICAL FDepHid
      INTEGER FlagFilt
      LOGICAL FFiltVar
      LOGICAL FExtrac
      LOGICAL FRendProm
      LOGICAL FRendBdrs
      LOGICAL FAfluFict
      DOUBLE PRECISION FactEPS
      DOUBLE PRECISION FactMLD
      DOUBLE PRECISION OptiEPS
      DOUBLE PRECISION OptiMLD
      INTEGER FactMXC
      INTEGER FactDBL
      LOGICAL FSeparaCFA
      LOGICAL FOneFeasRay
      INTEGER DualFactIter
      LOGICAL FInfactSol
      INTEGER idual_pmode
      INTEGER iprim_pmode
      INTEGER nthreads

      CHARACTER*48 CenNom(Dim%Emb)
      DOUBLE PRECISION ScaleVol(Dim%Emb)


      CALL PRINT_OPTSI(ULog, &
     &     eprhs, &
     &     epopt, &
     &     ScaleObj, &
     &     ScalePhi, &
     &     ScaleAng, &
     &     ScaleVolG, &
     &     FScaleQs, &
     &     FScaleVdi, &
     &     FAngZero, &
     &     FConvLaja, &
     &     FConvMaule, &
     &     FRestRalco, &
     &     FRestGnl, &
     &     FRestReserva, &
     &     FBaterias, &
     &     FMemMode, &
     &     FOldLaja, &
     &     FOldMaule, &
     &     FVertReb, &
     &     FSeparaLP, &
     &     FSeparaCF, &
     &     FZSPFBest, &
     &     FOnePhi, &
     &     FInterfaz, &
     &     FDepHid, &
     &     FlagFilt, &
     &     FFiltVar, &
     &     FExtrac, &
     &     FRendProm, &
     &     FRendBdrs, &
     &     FAfluFict, &
     &     FactEPS, &
     &     FactMLD, &
     &     OptiEPS, &
     &     OptiMLD, &
     &     FactMXC, &
     &     FactDBL, &
     &     FSeparaCFA, &
     &     FOneFeasRay, &
     &     DualFactIter, &
     &     FInfactSol, &
     &     idual_pmode, &
     &     iprim_pmode, &
     &     nthreads, &
     &     CenNom, ScaleVol, &
     &     Dim)


      RETURN
      END
