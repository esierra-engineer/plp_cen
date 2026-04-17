      TYPE PAR_MAULE

      CHARACTER*24 :: NArcMauleO  = 'plpmaule.csv'
      
      INTEGER :: DimColBlo = 10
      INTEGER :: DimFilaBlo = 5
      INTEGER :: DimColEta = 47
      INTEGER :: DimFilaEta = 47
      INTEGER :: DimAflHI = 10
      
      ! Indices relevantes
      INTEGER IEmbMaule
      INTEGER ILagInvern
      INTEGER IEmbMelado
      INTEGER IEmbColbun

      INTEGER IFiltInvern      
      INTEGER IFiltColbun
      

      INTEGER IQRebInvern
      INTEGER IQVerInvern
      INTEGER IQRebMaule
      INTEGER IQVerMaule
      INTEGER IAflInvern
      INTEGER IAflMaule
      INTEGER IQCanelon

      ! Indice de afluentes Hoya Intermedia
      INTEGER NumAflHoyaInt
      INTEGER, ALLOCATABLE :: IAflHoyaInt(:)
      
      ! Ecuaciones/variables de bloque
      INTEGER :: NumColBlo = 9
      INTEGER :: NumFilBlo = 3

      INTEGER :: IQMNE  = 1 ! Caudal Maule R.Normal Electrico
      INTEGER :: IQMNR  = 2 ! Caudal Maule R.Normal Riego
      INTEGER :: IQMOE  = 3 ! Caudal Maule R.Ordinario Electrico
      INTEGER :: IQMOR  = 4 ! Caudal Maule R.Ordinario Riego
      INTEGER :: IQMCE  = 5 ! Caudal Maule Compensacion de Endesa
      INTEGER :: IQMEI  = 6 ! Caudal Maule Economias de Invernada
      INTEGER :: IQIDN  = 7 ! Caudal Laguna Invernada en deficit de nivel
      INTEGER :: IQISD  = 8 ! Caudal Laguna Invernada sin deficit de riego
      INTEGER :: IQTER  = 9 ! Caudal Laguna Invernada entregado a riego

      INTEGER :: IQMAULE_F = 1 ! Caudal embalse maula
      INTEGER :: IQLINVE_F = 2 ! Caudal laguna invernada
      INTEGER :: IQTER_F   = 3 ! Caudal total entregado a riego
      CHARACTER*8, ALLOCATABLE :: VarBloNames(:)

      ! Ecuaciones/variables de etapa
      INTEGER :: NumColEta = 38
      INTEGER :: NumFilEta = 34
      INTEGER :: NumVarEst = 9
      INTEGER :: NumResVol = 15
      INTEGER :: NumVarVol = 16

      INTEGER :: IVMGEMF = 1 ! Volumen Maule Gasto Derechos Electricos Mensual
      INTEGER :: IVMGEAF = 2 ! Volumen Maule Gasto Derechos Electricos Anual
      INTEGER :: IVMGRTF = 3 ! Volumen Maule Gasto Derechos Riego de temporada
      INTEGER :: IVMGOEF = 4 ! Volumen Maule Gasto R. Ordinaria Electricas
      INTEGER :: IVMGORF = 5 ! Volumen Maule Gasto R. Ordinaria Riego
      INTEGER :: IVMDCEF = 6 ! Volumen Maule Derechos de Compensacion Electrica
      INTEGER :: IVMDEIF = 7 ! Volumen Maule Derechos de Economias de Invernada
      INTEGER :: IVMDOEF = 8 ! Volumen Maule Derecho R. Ordinaria Electrico
      INTEGER :: IVMDORF = 9 ! Volumen Maule Derecho R. Ordinaria Riego
      INTEGER :: IVMUTIL =10 ! Volumen Maule Util
      INTEGER :: IVMDCEN =11 ! Volumen Maule Derechos de Compensacion Electrica nuevos
      INTEGER :: IVMREB  =12 ! Volumen Maule de Rebalse
      INTEGER :: IVMDEMT =13 ! Volumen Maule Derechos Electricos Mensuales totales
      INTEGER :: IVMDEAT =14 ! Volumen Maule Derechos Electricos Anuales totales
      INTEGER :: IVMDRTT =15 ! Volumen Maule Derechos Riego Temporada totales
      INTEGER :: IVMDEIN =16 ! Volumen Maule Derechos de Economias de Invernada nuevos
      INTEGER :: IQMNIH = 17 ! Caudal Maule neto entrante
      INTEGER :: IQMNEH = 18 ! Caudal Maule a cuenta de Derechos de Electricos
      INTEGER :: IQMNRH = 19 ! Caudal Maule a cuenta de Derechos de Riego
      INTEGER :: IQMOEH = 20 ! Caudal Maule a cuenta de Reservas de Electricas
      INTEGER :: IQMORH = 21 ! Caudal Maule a cuenta de Reservas de Riego
      INTEGER :: IQMCEH = 22 ! Caudal Maule a cuenta de Compensacion de Electrica
      INTEGER :: IQMEIH = 23 ! Caudal Maule a cuenta de Economias de Invernada
      INTEGER :: IQIDNH = 24 ! Caudal Laguna Invernada de deficit de riego que mantiene nivel
      INTEGER :: IQISDH = 25 ! Caudal Laguna Invernada sin deficit de riego
      INTEGER :: IQTERH = 26 ! Caudal Total entrega a riego
      INTEGER :: IQDRMH = 27 ! Caudal Deficit de Riego Minimo
      INTEGER :: IQDRAH = 28 ! Caudal Deficit de Riego Actual
      INTEGER :: IQMAUH = 29 ! Caudal Maule Horario
      INTEGER :: IQINVH = 30 ! Caudal Invernada Horario
      INTEGER :: IQARMR = 31 ! Caudal Armerillo Regulado
      INTEGER :: IQHI   = 32 ! Caudal Hoya Intermedia
      INTEGER :: IQR105 = 33 ! Caudal riego circular 105
      INTEGER :: IQA105 = 34 ! Caudal riego circular 105
      INTEGER :: IQNINV = 35 ! Caudal neto de entrada invernada
      INTEGER :: IQHINV = 36 ! Caudal holgura para embalsar invernada
      INTEGER :: IQHNEIN =37 ! Caudal holgura para no embalsar invernada en ordinario
      INTEGER :: IQHEIN = 38 ! Caudal holgura para no embalsar invernada en superior

!     restricciones
      INTEGER :: IVMGEMF_F = 1  ! Volumen Maule Gasto Derechos Electricos Mensual
      INTEGER :: IVMGEAF_F = 2  ! Volumen Maule Gasto Derechos Electricos Anual
      INTEGER :: IVMGRTF_F = 3  ! Volumen Maule Gasto Derechos Riego de temporada
      INTEGER :: IVMGOEF_F = 4  ! Volumen Maule R.Ordinario Electrica
      INTEGER :: IVMGORF_F = 5  ! Volumen Maule R.Ordinario Riego
      INTEGER :: IVMDCEF_F = 6  ! Volumen Maule Derechos de Compensacion Electrica
      INTEGER :: IVMDEIF_F = 7  ! Volumen Maule Derechos de Economias de Invernada
      INTEGER :: IVMDOEF_F = 8  ! Volumen Maule Derechos R.Ordinario Electrica
      INTEGER :: IVMDORF_F = 9  ! Volumen Maule Derechos R.Ordinario Riego
      INTEGER :: IVMUTIL_F = 10 ! Volumen Maule Util
      INTEGER :: IVMREB_F  = 11 ! Volumen Maule Rebalse
      INTEGER :: IVMDEMT_F = 12 ! Volumen Maule Derechos Electricos Mensuales totales
      INTEGER :: IVMDEAT_F = 13 ! Volumen Maule Derechos Electricos anuales totales
      INTEGER :: IVMDRTT_F = 14 ! Volumen Maule Derechos riego temporada totales
      INTEGER :: IVDGRET_F = 15 ! Gastos VMGOEF <= Derechos VMFOEF
      INTEGER :: IVDGRRT_F = 16 ! Gastos VMGRTF <= Derechos VMDRTT
      INTEGER :: IQMNEH_F  = 17 ! Caudal Maule a cuenta de Derechos de Electricos
      INTEGER :: IQMNRH_F  = 18 ! Caudal Maule a cuenta de Derechos de Riego
      INTEGER :: IQMOEH_F  = 19 ! Caudal Maule a cuenta de Reservas de Electrica
      INTEGER :: IQMORH_F  = 20 ! Caudal Maule a cuenta de Reservas de Riego
      INTEGER :: IQMCEH_F  = 21 ! Caudal Maule a cuenta de Compensacion de Electrica
      INTEGER :: IQMEIH_F  = 22 ! Caudal Maule a cuenta de Economias de Invernada
      INTEGER :: IQIDNH_F  = 23 ! Caudal Laguna Invernada deficit de nivel
      INTEGER :: IQISDH_F  = 24 ! Caudal Laguna Invernada sin deficit
      INTEGER :: IQTERH_F  = 25 ! Caudal Laguna Invernada entrago
      INTEGER :: IQDRMH_F  = 26 ! Caudal de deficit de riego minimo
      INTEGER :: IQDRAH_F  = 27 ! Caudal de deficit de riego minimo
      INTEGER :: IQARMR_F  = 28 ! Caudal Armerillo Regulado
      INTEGER :: IQMAUH_F  = 29 ! Caudal Maule Horario
      INTEGER :: IQINVH_F  = 30 ! Caudal Invernada Horario
      INTEGER :: IQNINV_F  = 31 ! Caudal neto de entrada invernada
      INTEGER :: IQRNIN_F  = 32 ! Restriccion de nivel de Invernada
      INTEGER :: IQA105_F  = 33 ! Caudal riego circular 105
      INTEGER :: IQR105_F  = 34 ! Caudal riego circular 105
                             
      LOGICAL, ALLOCATABLE :: VarEtaVol(:)
      CHARACTER*8, ALLOCATABLE :: VarEtaNames(:)

      INTEGER, ALLOCATABLE :: ColIndEta(:, :)
      INTEGER, ALLOCATABLE :: FilIndEta(:, :)

      INTEGER, ALLOCATABLE :: TipoEtaDE(:)
      INTEGER, ALLOCATABLE :: TipoEtaDR(:)

      DOUBLE PRECISION, ALLOCATABLE :: DataEta(:, :)
      DOUBLE PRECISION, ALLOCATABLE :: DataBlo(:, :)

      ! Gasto y Economias
      DOUBLE PRECISION VEmbalseUtilMin
      DOUBLE PRECISION VDerRiegoTempMax
      DOUBLE PRECISION VDerElecAnuMax
      DOUBLE PRECISION, ALLOCATABLE :: VDerElecMenMax(:)
      DOUBLE PRECISION VCompElecMax

      DOUBLE PRECISION VGastoElecMenIni
      DOUBLE PRECISION VGastoElecAnuIni
      DOUBLE PRECISION VGastoRiegoIni
      DOUBLE PRECISION VGastoRExtElecIni
      DOUBLE PRECISION VGastoRExtRiegoIni
      DOUBLE PRECISION VDerRExtElecIni
      DOUBLE PRECISION VDerRExtRiegoIni
      DOUBLE PRECISION VCompElecIni
      DOUBLE PRECISION VEconInverIni

      DOUBLE PRECISION PorcQNIDE
      DOUBLE PRECISION PorcQNIDR
      DOUBLE PRECISION GastoElecMenMax
      DOUBLE PRECISION GastoElecDiaMax
      DOUBLE PRECISION PGastoElecDiaMaxMenRes(12)
      DOUBLE PRECISION GastoRiegoMax
      DOUBLE PRECISION GastoMauleMin
      DOUBLE PRECISION VReservaOrdinaria
      DOUBLE PRECISION VReservaExtraord
      DOUBLE PRECISION PRiegoMaule(12)
      DOUBLE PRECISION, ALLOCATABLE :: PRiegoMauleAnual(:, :)
      DOUBLE PRECISION CostoRiegoNSMaule
      DOUBLE PRECISION CostoRiegoNS105
      DOUBLE PRECISION ValorRiegoMaule
      DOUBLE PRECISION ValorRiego105
      DOUBLE PRECISION ScaleVol
      DOUBLE PRECISION ScaleVolColbun
      DOUBLE PRECISION CostoCanelon
      DOUBLE PRECISION CostoEmbalsar
      DOUBLE PRECISION CostoNoEmbalsar
      
      INTEGER MesRiegoIni
      INTEGER MesRiegoFin

      INTEGER NumRetRiego
      INTEGER, ALLOCATABLE :: ICenRetRiego(:)
      DOUBLE PRECISION QRiego105(12)
      DOUBLE PRECISION, ALLOCATABLE :: QRiego105Anual(:, :)
      DOUBLE PRECISION, ALLOCATABLE :: PRetRiego(:)
      LOGICAL, ALLOCATABLE :: HolgRiego(:)

      INTEGER, ALLOCATABLE :: IQRIHC(:) ! Caudal de riego retirado horario
      INTEGER, ALLOCATABLE :: IQRHHC(:) ! Holgura de riego retirado horario
      INTEGER, ALLOCATABLE :: IQRIHF(:) ! Restriccion de riego retirado horario
      
      INTEGER IExtr425
      DOUBLE PRECISION Vol425
      DOUBLE PRECISION ExtrMax425
      INTEGER, ALLOCATABLE :: Extr425ColInd(:, :)
      INTEGER, ALLOCATABLE :: VColbunColInd(:)

      LOGICAL EconInvernUsoEnReserva
      LOGICAL EconInvernAcumAnual
      DOUBLE PRECISION EconInvernCosto

      LOGICAL RelaxInvern
      LOGICAL NoDesembInv

      LOGICAL, ALLOCATABLE :: EnRegimenNormal(:, :)
      LOGICAL, ALLOCATABLE :: CompElecMaxed(:, :)


      DOUBLE PRECISION, ALLOCATABLE :: VarEtaPrev(:, :, :)

      LOGICAL DescGastoElecArmr

      INTEGER AnoModQRiegoRes
      DOUBLE PRECISION FactCauFutRiego
      LOGICAL AutModRes105
      DOUBLE PRECISION VolMaxDerRiegoAcum(12)
      INTEGER DiasTempRiegoAcum(12)

      !  Acoplamiento
      INTEGER :: DimPDLDAcCol = 7
      INTEGER :: DimPDLDAcFila = 7
      INTEGER :: PDLDAcNFila = 0
      INTEGER :: PDLDAcNCol = 0
      LOGICAL :: UsaCorteOptim = .FALSE.

      END TYPE
