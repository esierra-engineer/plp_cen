!
!     Estructura de parametros de reserva
!
      TYPE  PAR_BATERIAS
!   Propiedades generales

      INTEGER :: NBaterias = 0  ! Total baterias
      INTEGER :: MaxIny = 0     ! Máximo de centrales que inyectan a una bateria
      INTEGER :: NInyPasada = 0 ! Número de centrales que inyectan a una bateria que son centrales de pasada

!   Propiedades de la batería
      INTEGER, ALLOCATABLE :: BatInd(:) ! Indice de las baterias
      INTEGER, ALLOCATABLE :: BatCenInd(:) ! Indice de la central bateria
      INTEGER, ALLOCATABLE :: NIny(:) ! Numero de centrales que le inyectan a la bateria
      CHARACTER*48, ALLOCATABLE :: BatNom(:) ! Nombre de las baterias      
      DOUBLE PRECISION, ALLOCATABLE :: FPD(:) ! Factor de perdida de descarga de la batería en [0,1]
      !***** NBAT X BLOQUES ********
      DOUBLE PRECISION, ALLOCATABLE :: BatEMin(:,:) ! Energía mínima de la batería por bloque
      DOUBLE PRECISION, ALLOCATABLE :: BatEMax(:,:) ! Energía máxima de la bateria por bloque
      !*****************************
      INTEGER, ALLOCATABLE :: BatBar(:) ! Barra de inyección de la bateria

!   Propiedades de centrales que inyectan
      !****** NBAT X NMaxIny **** 
      CHARACTER*48, ALLOCATABLE :: NomBatIny(:,:) ! Nombre de las centrales que le inyectan a la bateria
      INTEGER, ALLOCATABLE :: NumInyBat(:,:) ! Indice de la central que inyecta energia a la bateria
      DOUBLE PRECISION, ALLOCATABLE :: FPC (:,:) ! Factor de pérdida de carga de la central a la bateria en [0,1]
      !**************************


      !***** Variables y Restricciones ******
      INTEGER :: NumColBlo = 0 ! Se irán sumando el Numero de columnas (variables) por bloque 
      INTEGER :: NumFilBlo = 0 ! Se irán sumando el Numero de filas (restricciones) por bloque
      INTEGER :: NumColEta = 0 ! Numero de columnas por etapa
      INTEGER :: NumFilEta = 0 ! Numero de filas por etapa

      INTEGER, ALLOCATABLE :: ColIndEta(:,:) ! Número de indices de las columnas para las variables por etapa
      INTEGER, ALLOCATABLE :: FilIndEta(:,:) ! Número de indices de las filas para las restricciones por etapa

      INTEGER, ALLOCATABLE :: SoCf_1(:) ! Indice de la restricción del SoCf del bloque 1 
      INTEGER, ALLOCATABLE :: SoC_col(:) ! Indice de las columnas de las variables SoC
      INTEGER, ALLOCATABLE :: egen_col(:) ! Indice de las columnas de generación de la batería
      INTEGER, ALLOCATABLE :: ed_col(:) ! Indice de las columnas de energia descargada de la bateria
      INTEGER, ALLOCATABLE :: ec_col(:,:) ! Indice de las columnas de energia cargada de la batería por su inyector
      
      INTEGER, ALLOCATABLE :: ten_col(:,:) ! Indice de la columna de energia total por etapas, para cada bateria
      INTEGER, ALLOCATABLE :: Soc_fil(:) ! Fila de los índices de las 

      INTEGER, ALLOCATABLE :: ECg_pfil_pasada(:,:,:) ! Indice de fila de la restrición ECg_pfil por NInyPasada, por Bloque, por Etapa
      INTEGER, ALLOCATABLE :: ICen_Pasada(:) ! Indice de la central de pasada por NInyPasada
! Arrays de datos de la corrida
      !*******************************
      DOUBLE PRECISION, ALLOCATABLE :: DataBlo(:,:) ! Array de resultados por bloques, con indice de var, y bloque.
      DOUBLE PRECISION, ALLOCATABLE :: DataEta(:,:) ! Array de resultados por etapas, con indice de var, y bloque.
      DOUBLE PRECISION, ALLOCATABLE :: DualBlo(:,:) ! Array de resultados de los duales de las restricciones.
      END TYPE
