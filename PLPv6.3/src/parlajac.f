
      INCLUDE 'laja.fpp'

      INTEGER DimBloLajaC
      INTEGER DimEtaLajaC
      PARAMETER (DimBloLajaC = 25)
      PARAMETER (DimEtaLajaC = 5)


      INTEGER LajaBloQVarBeg 
      INTEGER LajaBloQVarEnd 
      INTEGER LajaBloVVarBeg 
      INTEGER LajaBloVVarEnd 
      INTEGER LajaEtaVVarBeg 
      INTEGER LajaEtaVVarEnd 

      PARAMETER (LajaBloQVarBeg = IIAflAbanico)
      PARAMETER (LajaBloQVarEnd = IIRieOpcionalLaja)
      PARAMETER (LajaBloVVarBeg = IIVertLaja)
      PARAMETER (LajaBloVVarEnd = IIVertLaja)
      PARAMETER (LajaEtaVVarBeg = IIVolLaja)
      PARAMETER (LajaEtaVVarEnd = IIVolLaja)


!
!     Estructura de parametros del Laja
!
      TYPE PAR_LAJAC

      INTEGER, ALLOCATABLE :: IBloInd(:,:,:)
      INTEGER, ALLOCATABLE :: IEtaInd(:,:)
 
      DOUBLE PRECISION, ALLOCATABLE :: UppGenElToro(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: UppGenZaCo(:,:)

      DOUBLE PRECISION CauDerAnuEND
      DOUBLE PRECISION CauDerDiaEND
      DOUBLE PRECISION CauDerMenEND
      DOUBLE PRECISION CRieCantEcol
      DOUBLE PRECISION Gasto50cm
      DOUBLE PRECISION GastoAba
      DOUBLE PRECISION GastoToro
      DOUBLE PRECISION GastoTuc
      DOUBLE PRECISION PCenLimSupCol
      DOUBLE PRECISION PCenAbZaCo
      DOUBLE PRECISION PCenTucapel
      DOUBLE PRECISION PCenOpcionalLaja
      DOUBLE PRECISION PCenZaCo
      DOUBLE PRECISION Vol50cm
      DOUBLE PRECISION VolColInf
      DOUBLE PRECISION VolColSup
      DOUBLE PRECISION VolDerAnuEND
      DOUBLE PRECISION VolDerMenEND

      INTEGER UDebLog

      END TYPE
