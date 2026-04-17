
      INCLUDE 'maule.fpp'

      INTEGER DimBloMauleC
      INTEGER DimEtaMauleC
      PARAMETER (DimBloMauleC = 25)
      PARAMETER (DimEtaMauleC = 5)


      INTEGER MauleBloQVarBeg 
      INTEGER MauleBloQVarEnd 
      INTEGER MauleBloVVarBeg 
      INTEGER MauleBloVVarEnd 
      INTEGER MauleEtaVVarBeg 
      INTEGER MauleEtaVVarEnd 

      PARAMETER (MauleBloQVarBeg = IIAflArmerillo)
      PARAMETER (MauleBloQVarEnd = IIRieS123)
      PARAMETER (MauleBloVVarBeg = IIVertCip)
      PARAMETER (MauleBloVVarEnd = IIVertMaule)
      PARAMETER (MauleEtaVVarBeg = IIVolColbun)
      PARAMETER (MauleEtaVVarEnd = IIVolMaule)


!
!     Estructura de parametros del Maule
!
      TYPE PAR_MAULEC

      INTEGER, ALLOCATABLE :: IBloInd(:,:,:)
      DOUBLE PRECISION, ALLOCATABLE :: ExtPar(:, :, :)

      DOUBLE PRECISION, ALLOCATABLE :: UppGenCip(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: LowGenCip(:,:)

      DOUBLE PRECISION, ALLOCATABLE :: UppExtMauRie(:,:)

      DOUBLE PRECISION, ALLOCATABLE :: UppGenMaule(:,:)


!     Nombres de las centrales involucradas en el convenio del Maule.
!     Constantes del convenio del Maule.

      DOUBLE PRECISION GastoMaxCMel
      DOUBLE PRECISION GastoMaxRie
      DOUBLE PRECISION GastoMedMenMax
      DOUBLE PRECISION PCenCMel
      DOUBLE PRECISION PCenCMNA1
      DOUBLE PRECISION PCenCMNA2
      DOUBLE PRECISION PCenCMNB1
      DOUBLE PRECISION PCenCMNB2
      DOUBLE PRECISION PCenOpcionalMaule
      DOUBLE PRECISION PCenOReg
      DOUBLE PRECISION PCenS123
      DOUBLE PRECISION VolColbLim
      DOUBLE PRECISION VolCompENDMax
      DOUBLE PRECISION VolCuoExtMau
      DOUBLE PRECISION VolInvMax
      DOUBLE PRECISION VolMauMax
      DOUBLE PRECISION VolMaxEND
      DOUBLE PRECISION VolMaxRie
      DOUBLE PRECISION VolPorSupMax
      DOUBLE PRECISION VolResExtMax
      DOUBLE PRECISION VolResOrdMax

      END TYPE
