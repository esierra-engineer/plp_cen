!     
!     Estructura de parametros de Gn
!     

      TYPE NodoGN
      CHARACTER*48 Nombre
      INTEGER :: Id

      REAL(8) :: PMin
      REAL(8) :: PMax      

      END TYPE

      TYPE DuctoGN
      CHARACTER*48 Nombre
      INTEGER :: Id

      INTEGER :: nodo_a_id
      INTEGER :: nodo_b_id
      
      REAL(8) :: C
      REAL(8) :: FMin     
      REAL(8) :: FMax      


      END TYPE

      TYPE BombaGN
      CHARACTER*48 Nombre
      INTEGER :: Id

      INTEGER :: nodo_a_id
      INTEGER :: nodo_b_id

      REAL(8) :: CFlu
      REAL(8) :: CDel

      REAL(8) :: FMax      
      REAL(8) :: DMax
      REAL(8) :: PMax
      
      END TYPE

      TYPE RegasGN
      CHARACTER*48 Nombre
      INTEGER :: Id

      INTEGER :: term_id
      INTEGER :: nodo_id
      
      REAL(8) :: CReg
      REAL(8) :: POut
      REAL(8) :: FMax      
      REAL(8) :: FReg
      
      END TYPE
      
      TYPE DemandaGN
      CHARACTER*48 Nombre
      INTEGER :: Id
      INTEGER :: nodo_id
      INTEGER :: nbloques
      INTEGER, ALLOCATABLE :: bloque(:)
      REAL(8), ALLOCATABLE :: valor(:)
      
      END TYPE

      TYPE GeneraGN
      CHARACTER*48 Nombre
      INTEGER :: Id
      
      INTEGER :: nodo_id
      REAL(8) :: rend
      REAL(8) :: pmin

      END TYPE


!
!  Estructura de gnl
!

      TYPE PAR_GN

      CHARACTER*48 :: NArcGnlO  = 'plpgn.csv'
      

      INTEGER :: NumNodos = 0
      TYPE(NodoGN), ALLOCATABLE :: Nodo(:)

      INTEGER :: NumSegmentos = 0
      INTEGER :: NumDuctos = 0
      TYPE(DuctoGN), ALLOCATABLE :: Ducto(:)

      INTEGER :: NumBombas = 0
      TYPE(BombaGN), ALLOCATABLE :: Bomba(:)

      INTEGER :: NumRegas = 0
      TYPE(RegasGN), ALLOCATABLE :: Regas(:)

      INTEGER :: NumDemandas = 0
      TYPE(DemandaGN), ALLOCATABLE :: Demanda(:)

      INTEGER :: NumGeneras = 0
      TYPE(GeneraGN), ALLOCATABLE :: Genera(:)

      END TYPE
      

