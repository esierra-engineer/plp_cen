!
!     Estructura de parametros de reserva
!
      TYPE PAR_RESERVA

      INTEGER :: NumZonas = 0
      INTEGER :: NumGrupos = 0

      CHARACTER*24 Zonas
      CHARACTER*1, ALLOCATABLE :: ZonaId(:)



!     parametros de zonas
!     costos de escacez
      DOUBLE PRECISION, ALLOCATABLE :: CE10s(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: CE5mp(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: CE5mn(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: CEpp(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: CEpn(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: CEsp(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: CEsn(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: CEtp(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: CEtn(:,:)

!     maximos de escacez
      DOUBLE PRECISION, ALLOCATABLE :: ME5mp(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: ME5mn(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: ME10s(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: MEpp(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: MEpn(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: MEsp(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: MEsn(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: MEtp(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: MEtn(:,:)

!     requerimiento de zonas
      DOUBLE PRECISION, ALLOCATABLE :: REQ10s(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: REQ5mp(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: REQ5mn(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: REQpp(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: REQpn(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: REQsp(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: REQsn(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: REQtp(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: REQtn(:,:)

!     Flag de inercia para sólo contabilizar el servicio primario de bajada
!     para el gap de generación

      LOGICAL, ALLOCATABLE :: ZonaInercia(:)

!     Atributos de las zonas

      DOUBLE PRECISION, ALLOCATABLE :: AtrZ10s(:)
      DOUBLE PRECISION, ALLOCATABLE :: AtrZ5mp(:)
      DOUBLE PRECISION, ALLOCATABLE :: AtrZ5mn(:)
      DOUBLE PRECISION, ALLOCATABLE :: AtrZpp(:)
      DOUBLE PRECISION, ALLOCATABLE :: AtrZpn(:)
      DOUBLE PRECISION, ALLOCATABLE :: AtrZsp(:)
      DOUBLE PRECISION, ALLOCATABLE :: AtrZsn(:)
      DOUBLE PRECISION, ALLOCATABLE :: AtrZtp(:)
      DOUBLE PRECISION, ALLOCATABLE :: AtrZtn(:)

!     parametros de grupo
      DOUBLE PRECISION, ALLOCATABLE :: AtrG10s(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: AtrG5mp(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: AtrG5mn(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: AtrGpp(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: AtrGpn(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: AtrGsp(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: AtrGsn(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: AtrGtp(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: AtrGtn(:,:)
!     parametros de centrales
      INTEGER :: NumCen = 0
      INTEGER, ALLOCATABLE :: Cen(:)
      CHARACTER*48, ALLOCATABLE :: CenNom(:)
      CHARACTER*24, ALLOCATABLE :: CenZonas(:)
      INTEGER, ALLOCATABLE :: CenGrupo(:)
      DOUBLE PRECISION, ALLOCATABLE :: AtrCen(:,:)

!     parametros de costo
      DOUBLE PRECISION, ALLOCATABLE :: CPpp(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: CPpn(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: CPsp(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: CPsn(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: CPtp(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: CPtn(:,:)

!     parametros de reserva
      DOUBLE PRECISION, ALLOCATABLE :: FE5mp(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: FE5mn(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: FE10s(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: FEp(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: FEn(:,:)

!     Minimos de provision de reserva
      DOUBLE PRECISION, ALLOCATABLE :: MinPpp(:,:,:)
      DOUBLE PRECISION, ALLOCATABLE :: MinPpn(:,:,:)
      DOUBLE PRECISION, ALLOCATABLE :: MinPsp(:,:,:)
      DOUBLE PRECISION, ALLOCATABLE :: MinPsn(:,:,:)
      DOUBLE PRECISION, ALLOCATABLE :: MinPtp(:,:,:)
      DOUBLE PRECISION, ALLOCATABLE :: MinPtn(:,:,:)

!     Maximos de provision de reserva
      DOUBLE PRECISION, ALLOCATABLE :: MaxPpp(:,:,:)
      DOUBLE PRECISION, ALLOCATABLE :: MaxPpn(:,:,:)
      DOUBLE PRECISION, ALLOCATABLE :: MaxPsp(:,:,:)
      DOUBLE PRECISION, ALLOCATABLE :: MaxPsn(:,:,:)
      DOUBLE PRECISION, ALLOCATABLE :: MaxPtp(:,:,:)
      DOUBLE PRECISION, ALLOCATABLE :: MaxPtn(:,:,:)

!     Factor de consumo de caudal para servicios de subida
      DOUBLE PRECISION, ALLOCATABLE :: FactorSubida(:,:)

!     Minimos de provision de reserva
      DOUBLE PRECISION, ALLOCATABLE :: CenGMin(:,:,:)

!     parametros
      INTEGER :: NumColBlo = 0
      INTEGER :: NumFilBlo = 0

      INTEGER :: EtaIni = 0
      INTEGER :: EtaFin = 0

!     fils and cols

      INTEGER, ALLOCATABLE :: zr10s_col(:)
      INTEGER, ALLOCATABLE :: zr5mp_col(:)
      INTEGER, ALLOCATABLE :: zr5mn_col(:)
      INTEGER, ALLOCATABLE :: zrpp_col(:)
      INTEGER, ALLOCATABLE :: zrpn_col(:)
      INTEGER, ALLOCATABLE :: zrsp_col(:)
      INTEGER, ALLOCATABLE :: zrsn_col(:)
      INTEGER, ALLOCATABLE :: zrtp_col(:)
      INTEGER, ALLOCATABLE :: zrtn_col(:)

      INTEGER, ALLOCATABLE :: ze10s_col(:)
      INTEGER, ALLOCATABLE :: ze5mp_col(:)
      INTEGER, ALLOCATABLE :: ze5mn_col(:)
      INTEGER, ALLOCATABLE :: zepp_col(:)
      INTEGER, ALLOCATABLE :: zepn_col(:)
      INTEGER, ALLOCATABLE :: zesp_col(:)
      INTEGER, ALLOCATABLE :: zesn_col(:)
      INTEGER, ALLOCATABLE :: zetp_col(:)
      INTEGER, ALLOCATABLE :: zetn_col(:)

      INTEGER, ALLOCATABLE :: r10s_col(:)
      INTEGER, ALLOCATABLE :: r5mp_col(:)
      INTEGER, ALLOCATABLE :: r5mn_col(:)
      !Variables de caudales usados/no usados por reserva
      INTEGER, ALLOCATABLE :: qrp_col(:) ! Caudal total utilizado en servicios de subida
      INTEGER, ALLOCATABLE :: qrn_col(:) ! Caudal total utilizado en servicios de subida
      INTEGER, ALLOCATABLE :: qrpr_col 
      INTEGER, ALLOCATABLE :: qrpe_col
      INTEGER, ALLOCATABLE :: qrpm_col
      INTEGER, ALLOCATABLE :: qrnr_col
      INTEGER, ALLOCATABLE :: qrne_col
      INTEGER, ALLOCATABLE :: qrnm_col

      INTEGER, ALLOCATABLE :: rppz_col(:,:)
      INTEGER, ALLOCATABLE :: rpnz_col(:,:)
      INTEGER, ALLOCATABLE :: rspz_col(:,:)
      INTEGER, ALLOCATABLE :: rsnz_col(:,:)
      INTEGER, ALLOCATABLE :: rtpz_col(:,:)
      INTEGER, ALLOCATABLE :: rtnz_col(:,:)

      INTEGER, ALLOCATABLE :: req10s_fil(:)
      INTEGER, ALLOCATABLE :: req5mp_fil(:)
      INTEGER, ALLOCATABLE :: req5mn_fil(:)
      INTEGER, ALLOCATABLE :: reqpp_fil(:)
      INTEGER, ALLOCATABLE :: reqpn_fil(:)
      INTEGER, ALLOCATABLE :: reqsp_fil(:)
      INTEGER, ALLOCATABLE :: reqsn_fil(:)
      INTEGER, ALLOCATABLE :: reqtp_fil(:)
      INTEGER, ALLOCATABLE :: reqtn_fil(:)

      CHARACTER*48 NArcZonas
      CHARACTER*48 NArcCens
      DOUBLE PRECISION, ALLOCATABLE :: DataBlo(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: DualBlo(:,:)

      INTEGER NumRend
      INTEGER, ALLOCATABLE :: RendCen(:)
      INTEGER, ALLOCATABLE :: RendColInd(:,:,:)
      INTEGER, ALLOCATABLE :: RendFilpInd(:,:,:)
      INTEGER, ALLOCATABLE :: RendFilnInd(:,:,:)
      INTEGER, ALLOCATABLE :: RendFilCBajInd(:,:)
      INTEGER, ALLOCATABLE :: RendColCBajInd(:,:)
      INTEGER, ALLOCATABLE :: RendColSubInd(:,:)
      INTEGER, ALLOCATABLE :: RendFilSubInd(:,:)
      INTEGER, ALLOCATABLE :: RendColBajInd(:,:)
      INTEGER, ALLOCATABLE :: RendFilBajInd(:,:)
      INTEGER, ALLOCATABLE :: FilIndEta(:,:)
      INTEGER, ALLOCATABLE :: ColIndEta(:,:)

      ! Ecuaciones/variables de etapa del convenio del Laja
      INTEGER :: NumColEta = 6
      INTEGER :: NumFilEta = 6
      CHARACTER*8, ALLOCATABLE :: VarEtaNames(:)
      ! enumeración Variables convenio del laja con CF
      INTEGER :: TQRPR = 1 ! Total caudal de RESERVA subida de derecho de riego
      INTEGER :: TQRPE = 2 ! Total Caudal de RESERVA subida de derecho eléctrico
      INTEGER :: TQRPM = 3 ! Total caudal de RESERVA subida de derecho mixto
      INTEGER :: TQRNR = 4 ! Total caudal de RESERVA subida de derecho de riego
      INTEGER :: TQRNE = 5 ! Total Caudal de RESERVA subida de derecho eléctrico
      INTEGER :: TQRNM = 6 ! Total caudal de RESERVA subida de derecho mixto
      ! Cantidad de servicios (primario subida,secundario subida,terciario subida,
      !                        primario bajada,secundario bajada,terciario bajada)
      INTEGER :: NumServicios = 6
      END TYPE
