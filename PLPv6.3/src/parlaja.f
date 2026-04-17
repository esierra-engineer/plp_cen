

!
!     Estructura de parametros del Laja
!
      TYPE PAR_LAJA

      CHARACTER*24 :: NArcLajaO  = 'plplaja.csv'

      INTEGER :: DimColBlo = 10
      INTEGER :: DimFilaBlo = 5
      INTEGER :: DimColEta = 35
      INTEGER :: DimFilaEta = 35
      INTEGER :: DimAflHI = 10
      INTEGER :: DimCenRetRiego = 10

      ! Indices relevantes
      INTEGER IEmbLaja
      INTEGER IQRebLaja

      ! Indice de afluentes
      INTEGER IAflAltoPolcura
      ! Indice de afluentes Hoya Intermedia
      INTEGER NumAflHoyaInt
      INTEGER, ALLOCATABLE :: IAflHoyaInt(:)
      ! Indice de variable de filtracion de El Toro
      INTEGER IFiltLaja

      ! Ecuaciones/variables de bloque
      INTEGER :: NumColBlo = 6
      INTEGER :: NumFilBlo = 2

      INTEGER :: IQGDG = 1 ! Caudal Generado a cuenta de Derechos de Gastos 
      INTEGER :: IQGES = 2 ! Caudal Generado a cuenta de Economias Superiores
      INTEGER :: IQGER = 3 ! Caudal Generado a cuenta de Economias de Reserva
      INTEGER :: IQGAP = 4 ! Caudal Generado a cuenta de Alto Polcura
      INTEGER :: IQG50 = 5 ! Caudal Generado a cuenta de regla de 50cm
      
      INTEGER :: IQDGN = 6 ! Caudal gasto medio neto ( qgdg + qfilt )

      DOUBLE PRECISION CQVar(5)

      CHARACTER*8, ALLOCATABLE :: VarBloNames(:)

      ! Ecuaciones/variables de etapa
      INTEGER :: NumColEta = 27
      INTEGER :: NumFilEta = 23
      INTEGER :: NumVarEst = 7
      INTEGER :: NumResVol = 9
      INTEGER :: NumVarVol = 14

      INTEGER :: IVDGMF = 1  ! Volumen Derechos Gasto Mensual
      INTEGER :: IVDGAF = 2  ! Volumen Derechos Gasto Anual
      INTEGER :: IVAPF  = 3  ! Volumen de Economias Alto Polcura totales
      INTEGER :: IVESF  = 4  ! Volumen Economias de endesa anuales
      INTEGER :: IVERF  = 5  ! Volumen Economias de reserva totales
      INTEGER :: IVDAF  = 6  ! Volumen derechos anuales      
      INTEGER :: IVDMF  = 7  ! Volumen derechos mensuales
      INTEGER :: IVESN  = 8  ! Volumen de Economias Endesa nuevas      
      INTEGER :: IVAPN  = 9  ! Volumen de Economias Alto Polcura nuevas
      INTEGER :: IVERN  = 10 ! Volumen de Economias de Reserva nuevas
      INTEGER :: IVDAD  = 11 ! Volumen de descuentos de derechos anuales
      INTEGER :: IVDMD  = 12 ! Volumen de descuentos de derechos mensuales
      INTEGER :: IVU    = 13 ! Volumen util
      INTEGER :: IVRB   = 14 ! Volumen de vertimiento de rebalse 
      INTEGER :: IQDGNH = 15 ! Caudal Gasto Medio Horario
      INTEGER :: IQGESH = 16 ! Caudal Economias de endesa horarias
      INTEGER :: IQGDGH = 17 ! Caudal Derechos de endesa horarias
      INTEGER :: IQGERH = 18 ! Caudal Economias de reserva horarias
      INTEGER :: IQGAPH = 19 ! Caudal a cargo de Alto Polcura horario
      INTEGER :: IQG50H = 20 ! Caudal a cargo de regla de 50cm horario
      INTEGER :: IQGH   = 21 ! Caudal turbinado horario
      INTEGER :: IQDEFM = 22 ! Caudal deficit minimo
      INTEGER :: IQRS   = 23 ! Caudal de riego total asegurado
      INTEGER :: IQHI   = 24  ! Caudal hoya intermedia
      INTEGER :: IQPR   = 25  ! Caudal de riego primeros regantes
      INTEGER :: IQNR   = 26  ! Caudal de riego nuevos regantes
      INTEGER :: IQLAJA = 27  ! Caudal turbinado mas filtraciones
      

!     variables opcionales
      INTEGER :: IVRBES  ! Volumen de vertimiento de economias de endesa
      INTEGER :: IVUN  ! Volumen util superior negativo (bajo colchon inferior)
      INTEGER :: IVUP  ! Volumen util superior positivo (sobre colchon inferior)
      INTEGER :: IQEP  ! Caudal economizable
      INTEGER :: IQEN  ! Caudal no economizable
      INTEGER :: IQEA  ! Caudal de economias anticipadas
      INTEGER :: IQDR  ! Caudal deficit de riego

!     restricciones
      INTEGER :: IVDGMF_F = 1  ! Volumen Derechos Gasto Mensual
      INTEGER :: IVDGAF_F = 2  ! Volumen Derechos Gasto Anual
      INTEGER :: IVAPF_F  = 3  ! Volumen de Economias Alto Polcura totales
      INTEGER :: IVESF_F  = 4  ! Volumen Economias de endesa anuales
      INTEGER :: IVERF_F  = 5  ! Volumen Economias de reserva totales
      INTEGER :: IVDAF_F  = 6  ! Volumen derechos anuales      
      INTEGER :: IVDMF_F  = 7  ! Volumen derechos mensuales
      INTEGER :: IVU_F    = 8
      INTEGER :: IVRB_F   = 9 
      INTEGER :: IQDGNH_F = 10
      INTEGER :: IQGESH_F = 11
      INTEGER :: IQGDGH_F = 12
      INTEGER :: IQGERH_F = 13
      INTEGER :: IQGAPH_F = 14
      INTEGER :: IQG50H_F = 15
      INTEGER :: IQGH_F   = 16
      INTEGER :: IQDR_F   = 17
      INTEGER :: IGDA_F   = 18
      INTEGER :: IGDM_F   = 19
      INTEGER :: IQRS_F   = 20
      INTEGER :: IQPR_F   = 21
      INTEGER :: IQNR_F   = 22
      INTEGER :: IQLAJA_F = 23

!     restriccioes opcionales
      INTEGER :: IVUP_F
      INTEGER :: IVUN_F
      INTEGER :: IQEA_F
      INTEGER :: IQEN_F
      INTEGER :: IQEP_F

      LOGICAL, ALLOCATABLE :: VarEtaVol(:)
      CHARACTER*8, ALLOCATABLE :: VarEtaNames(:)

      INTEGER, ALLOCATABLE :: ColIndEta(:, :)
      INTEGER, ALLOCATABLE :: FilIndEta(:, :)

      ! Gastos y Economias
      DOUBLE PRECISION GastoMenIni
      DOUBLE PRECISION GastoAnuIni
      DOUBLE PRECISION EcoEndAnuIni
      DOUBLE PRECISION EcoResTotIni
      DOUBLE PRECISION EcoAltoPolTotIni
      DOUBLE PRECISION DiasAnuCInfIni
      DOUBLE PRECISION DiasMesCInfIni
      DOUBLE PRECISION VDerechosAnuIni
      DOUBLE PRECISION VDerechosMenIni
      DOUBLE PRECISION VolUtilMin
      DOUBLE PRECISION VOlUtilColInf
      DOUBLE PRECISION VolUtilColMed
      DOUBLE PRECISION VolAcumA50cmReb
      
      DOUBLE PRECISION GastoHorMax
      DOUBLE PRECISION GastoMenMax
      DOUBLE PRECISION GastoAnuMax
      DOUBLE PRECISION GastoResMax
      DOUBLE PRECISION, ALLOCATABLE :: VGastoMenMax(:)
      DOUBLE PRECISION, ALLOCATABLE :: VGastoAnuMax(:)

      INTEGER, ALLOCATABLE :: TipoEtaGM(:)

      DOUBLE PRECISION, ALLOCATABLE :: DataEta(:, :)
      DOUBLE PRECISION, ALLOCATABLE :: DataBlo(:, :)
      
      DOUBLE PRECISION QFiltHist
      DOUBLE PRECISION QRiegoHist
      DOUBLE PRECISION QNRiegoCSup
      DOUBLE PRECISION QNRiegoCMed
      DOUBLE PRECISION QNRiegoCInf
      DOUBLE PRECISION PGastosMesHid(12)
      DOUBLE PRECISION CVertEcon
      DOUBLE PRECISION CRiegoNS
      DOUBLE PRECISION CVolUtilNeg
      DOUBLE PRECISION FactVolUtilAct
      DOUBLE PRECISION CSubEconAnu

      LOGICAL FHolgTurRiego
      LOGICAL FHolgRetRiego
      LOGICAL FVertEcoAct
      LOGICAL FVolUtilEcoEnd
      LOGICAL FVolUtilEcoAP
      LOGICAL FVolUtilSupAct
      LOGICAL FQEconEndAct

      INTEGER, ALLOCATABLE :: IQRI(:) ! Caudal de riego debloque
      INTEGER, ALLOCATABLE :: IQRIHC(:) ! Caudal de riego retirado horario
      INTEGER, ALLOCATABLE :: IQRDHC(:) ! Caudal de demanda de riego horario
      INTEGER, ALLOCATABLE :: IQRFHC(:) ! Caudal de riego falla horario           

      INTEGER, ALLOCATABLE :: IQRIHF(:) ! Restriccion de riego retirado horario
      INTEGER, ALLOCATABLE :: IQRFHF(:) ! Restriccion de riego falla horario
      INTEGER, ALLOCATABLE :: IQRDHF(:) ! Caudal de demanda de riego horario
      
      INTEGER NumRetRiego
      INTEGER, ALLOCATABLE :: ICenRetRiego(:)
      DOUBLE PRECISION, ALLOCATABLE :: PRetPRiego(:, :)
      DOUBLE PRECISION, ALLOCATABLE :: PRetNRiego(:, :)
      DOUBLE PRECISION, ALLOCATABLE :: RiegoExtra(:, :)


      DOUBLE PRECISION, ALLOCATABLE :: CPRiego(:, :)
      DOUBLE PRECISION, ALLOCATABLE :: CNRiego(:, :)
      DOUBLE PRECISION, ALLOCATABLE :: DRExtra(:, :)

      DOUBLE PRECISION ScaleVol
      DOUBLE PRECISION CostoVAP

      DOUBLE PRECISION, ALLOCATABLE :: VarEtaPrev(:, :, :)

      LOGICAL, ALLOCATABLE :: ColchonInfActivo(:, :)

      INTEGER MetodoCalcDef

      !  Acoplamiento
      INTEGER :: DimPDLDAcCol = 5
      INTEGER :: DimPDLDAcFila = 5
      INTEGER :: PDLDAcNFila = 0
      INTEGER :: PDLDAcNCol = 0
      LOGICAL :: UsaCorteOptim = .FALSE.

      END TYPE

