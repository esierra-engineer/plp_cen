!     Arreglos para traspasar informacion a las rutinas del convenio.
!     Indices de variables INTEGER.
      INTEGER DimIMaule
      PARAMETER (DimIMaule = 47)
      INTEGER IIAflArmerillo
      INTEGER IIAflBocMaule
      INTEGER IIAflColbun
      INTEGER IIAflFictColbun
      INTEGER IIAflFictInve
      INTEGER IIAflFictMaule
      INTEGER IIAflInve
      INTEGER IIAflIsla
      INTEGER IIAflMina
      INTEGER IIAflMaule
      INTEGER IIAflPehuenche
      INTEGER IICenColbun
      INTEGER IICenInve
      INTEGER IICenMaule
      INTEGER IIExtMauEND
      INTEGER IIExtMauRie
      INTEGER IIExtInvEND
      INTEGER IIExtInvRie
      INTEGER IIControl
      INTEGER IIFiltInve
      INTEGER IICompRiegoInve
      INTEGER IIGenCip
      INTEGER IIGenMaule
      INTEGER IIMauleWrite1
      INTEGER IIMauleWrite2
      INTEGER IIMauleWrite3
      INTEGER IIMauleWrite4
      INTEGER IIndSimImpMaule
      INTEGER IIPLPExtEND
      INTEGER IIPLPExtENDCI
      INTEGER IIPLPExtRie
      INTEGER IIRieCMel
      INTEGER IIRieCMNA
      INTEGER IIRieCMNB
      INTEGER IIRieOpcionalMaule
      INTEGER IIRieOReg
      INTEGER IIRieS123
      INTEGER IIUsoConvMaule
      INTEGER IIUsoRieOpcional
      INTEGER IIVerifConvenio
      INTEGER IIVertCip
      INTEGER IIVertColbun
      INTEGER IIVertInve
      INTEGER IIVertMaule
      INTEGER IIVolColbun
      INTEGER IIVolInve
      INTEGER IIVolMaule
      PARAMETER (IIAflArmerillo       = 1 )
      PARAMETER (IIAflBocMaule        = 2 )
      PARAMETER (IIAflColbun          = 3 )
      PARAMETER (IIAflFictColbun      = 4 )
      PARAMETER (IIAflFictInve        = 5 )
      PARAMETER (IIAflFictMaule       = 6 )
      PARAMETER (IIAflInve            = 7 )
      PARAMETER (IIAflIsla            = 8 )
      PARAMETER (IIAflMina            = 47)
      PARAMETER (IIAflMaule           = 9 )
      PARAMETER (IIAflPehuenche       = 10)
      PARAMETER (IICenColbun          = 11)
      PARAMETER (IICenInve            = 12)
      PARAMETER (IICenMaule           = 13)
      PARAMETER (IICompRiegoInve      = 14)
      PARAMETER (IIExtMauEND          = 15)
      PARAMETER (IIExtMauRie          = 16)
      PARAMETER (IIExtInvEND          = 17)
      PARAMETER (IIExtInvRie          = 18)
      PARAMETER (IIControl            = 19)
      PARAMETER (IIFiltInve           = 20)
      PARAMETER (IIGenCip             = 21)
      PARAMETER (IIGenMaule           = 22)
      PARAMETER (IIRieCMel            = 23)
      PARAMETER (IIRieCMNA            = 24)
      PARAMETER (IIRieCMNB            = 25)
      PARAMETER (IIRieOpcionalMaule   = 26)
      PARAMETER (IIRieOReg            = 27)
      PARAMETER (IIRieS123            = 28)
      PARAMETER (IIVertCip            = 29)
      PARAMETER (IIVertColbun         = 30)
      PARAMETER (IIVertInve           = 31)
      PARAMETER (IIVertMaule          = 32)
      PARAMETER (IIVolColbun          = 33)
      PARAMETER (IIVolInve            = 34)
      PARAMETER (IIVolMaule           = 35)
      PARAMETER (IIMauleWrite1        = 36)
      PARAMETER (IIMauleWrite2        = 37)
      PARAMETER (IIMauleWrite3        = 38)
      PARAMETER (IIMauleWrite4        = 39)
      PARAMETER (IIndSimImpMaule      = 40)
      PARAMETER (IIPLPExtEND          = 41)
      PARAMETER (IIPLPExtENDCI        = 42)
      PARAMETER (IIPLPExtRie          = 43)
      PARAMETER (IIUsoConvMaule       = 44)
      PARAMETER (IIUsoRieOpcional     = 45)
      PARAMETER (IIVerifConvenio      = 46)



!     Indices de variables LOGICAL.
      INTEGER DimLMaule
      PARAMETER (DimLMaule = 3)
      INTEGER IFPasoPorResOrd
      INTEGER IFVieneDePorSup
      INTEGER IFVieneDeResOrd
      PARAMETER (IFPasoPorResOrd = 1)
      PARAMETER (IFVieneDePorSup = 2)
      PARAMETER (IFVieneDeResOrd = 3)
!     Indices de variables DOUBLE PRECISION.
      INTEGER DimRMaule
      PARAMETER (DimRMaule = 26)

      INTEGER DimEMaule
      PARAMETER (DimEMaule = 3)

      INTEGER ILeeExtMauEND
      INTEGER ILeeExtMauENDCI
      INTEGER ILeeExtMauRie

      INTEGER IExtMaxMauEND
      INTEGER IExtMauRie
      INTEGER IExtMinInve
      INTEGER IExtRie
      INTEGER IRieCMel
      INTEGER IRieCMNA
      INTEGER IRieCMNB
      INTEGER IRieOpcionalMaule
      INTEGER IRieOReg
      INTEGER IRieS123
      INTEGER IVolAflArme
      INTEGER IVolAflInv
      INTEGER IVolCompEND
      INTEGER IVolDisResOrdEND
      INTEGER IVolDisResOrdRie
      INTEGER IVolEcoInv
      INTEGER IVolExtMaxMauEND
      INTEGER IVolExtMaxMauRie
      INTEGER IVolExtRie
      INTEGER IVolExtMinMauEND
      INTEGER IVolFiltInv
      INTEGER IVolInv
      INTEGER IVolMau
      INTEGER IVolResCuoExt
      INTEGER IVolResMauEND
      INTEGER IVolResMauRie
      PARAMETER (ILeeExtMauEND       =  1)
      PARAMETER (ILeeExtMauENDCI     =  2)
      PARAMETER (ILeeExtMauRie       =  3)

      PARAMETER (IExtMaxMauEND       =  1)
      PARAMETER (IExtMauRie          =  2)
      PARAMETER (IExtMinInve         =  3)
      PARAMETER (IExtRie             =  4)
      PARAMETER (IRieCMel            =  5)
      PARAMETER (IRieCMNA            =  6)
      PARAMETER (IRieCMNB            =  7)
      PARAMETER (IRieOpcionalMaule   =  8)
      PARAMETER (IRieOReg            =  9)
      PARAMETER (IRieS123            = 10)
      PARAMETER (IVolAflArme         = 11)
      PARAMETER (IVolAflInv          = 12)
      PARAMETER (IVolCompEND         = 13)
      PARAMETER (IVolDisResOrdEND    = 14)
      PARAMETER (IVolDisResOrdRie    = 15)
      PARAMETER (IVolEcoInv          = 16)
      PARAMETER (IVolExtMaxMauEND    = 17)
      PARAMETER (IVolExtMaxMauRie    = 18)
      PARAMETER (IVolExtRie          = 19)
      PARAMETER (IVolExtMinMauEND    = 20)
      PARAMETER (IVolFiltInv         = 21)
      PARAMETER (IVolInv             = 22)
      PARAMETER (IVolMau             = 23)
      PARAMETER (IVolResCuoExt       = 24)
      PARAMETER (IVolResMauEND       = 25)
      PARAMETER (IVolResMauRie       = 26)





