      TYPE PAR_DIMS

!
!     estructura general de simulacion
!
!     Numero de Simulaciones
      INTEGER :: Simul = 0
!     Numero de Etapas
      INTEGER :: Eta = 0
!     Numero de Bloques
      INTEGER :: Blo = 0
!     Numero de Bloques por Etapa
      INTEGER :: IBlo = 0
!     Numero de Agnos
      INTEGER :: Year = 0
!     Numero de Iteraciones
      INTEGER :: PDIter = 0

!
!     parametros estocasticos
!
!     Numero de Variables que acoplan los Programas Lineales (Embalses)
      INTEGER :: PDLDAcFila = 0
!     Numero de Variables que acoplan los Programas Lineales (Embalses)
      INTEGER :: PDLDAcCol = 0
!     Numero de Columnas con Componente Aleatoria
      INTEGER :: EstocCol = 0
!     Numero de Filas con Componente Aleatoria
      INTEGER :: EstocFila = 0
!     Numero de Centrales con Caudales Aleatorios
      INTEGER :: EstocVar = 0
!     Numero de Aperturas
      INTEGER :: Apert = 0
!     Numero Maximo de Clases
      INTEGER :: Clase = 0
!     Numero de Columnas Extras por Etapa
      INTEGER :: XCol = 0
!     Numero de Ecuaciones Extras por Etapa
      INTEGER :: XFila = 0

!
!     elementos
!
!     Numero de Centrales
      INTEGER :: Cen = 0
!     Numero de Embalses
      INTEGER :: Emb = 0
!     Numero de Lineas
      INTEGER :: Lin = 0
!     Numero Extraciones
      INTEGER :: Extr = 0
!     Numero de Barras
      INTEGER :: Bar = 0
!     Numero de Vertederos
      INTEGER :: Vert = 0
!     Numero de Series Hidraulicas
      INTEGER :: Ser = 0
!     Numero de Pasadas
      INTEGER :: Pas = 0
!     Numero de Termicas
      INTEGER :: Ter = 0
!     Numero de Baterias
      INTEGER :: Bat = 0
!     Numero de Hidraulicas
      INTEGER :: Hid = 0
!     Numero de Hidraulicas sin Pasadas Puras
      INTEGER :: HidSPP = 0
!     Numero de Centrales de Falla
      INTEGER :: Falla = 0


!
!     mantenimientos por simulacion
!
      INTEGER :: CenManS = 0
      INTEGER :: LinManS = 0
      INTEGER :: EmbManS = 0


!
!     parametros de lineas
!
!     Numero de Flujos en lineas
      INTEGER :: Flu = 0

!
!     parametros de embalses
!
!     Embalses con rebalses
      INTEGER :: EmbVReb = 0
!     Embalses con filtraciones
      INTEGER :: EmbFilt = 0
      INTEGER :: FiltParam = 0
      INTEGER :: FiltTramo = 0
!     Embalses con rendimientos
      INTEGER :: EmbRend = 0
      INTEGER :: RendParam = 0
      INTEGER :: RendTramo = 0
!     Embalses con pmax
      INTEGER :: EmbPmax = 0
      INTEGER :: PmaxParam = 0
      INTEGER :: PmaxTramo = 0
!     Numero de embalses con informacion de planos
      INTEGER :: EmbPla = 0
      
      CHARACTER*3, ALLOCATABLE :: CenLabel(:)

!     parametros internos
!     Numero de Componentes del Kit      
      INTEGER :: Kit = 4


!     tipo de corte de factibilidad
      INTEGER :: FactMode = 0
      END TYPE
