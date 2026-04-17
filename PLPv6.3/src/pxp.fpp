!******************************************************************
!     Archivo para incluir en todas las rutinas que usen arreglos o
!     parametros con tamanhos o valores predefinidos
!******************************************************************
      CHARACTER*1 PCenTipEmb
      CHARACTER*1 PCenTipEmbAux
      CHARACTER*1 PCenTipFal
      CHARACTER*1 PCenTipPas
      CHARACTER*1 PCenTipRie
      CHARACTER*1 PCenTipSer
      CHARACTER*1 PCenTipTer
      CHARACTER*1 PCenTipMod
      CHARACTER*1 PCenTipBat


      INTEGER DimDatEmb
      INTEGER DimDatPas
      INTEGER DimDatSer
      INTEGER DimLargo
      INTEGER DimTmp

      INTEGER PEmbDatCMg
      INTEGER PEmbDatDef
      INTEGER PEmbDatVer
      INTEGER PEmbDatVol
      INTEGER PEmbDatReb
      INTEGER PEmbDatRebP
      INTEGER PEmbDatRebN
      INTEGER PEmbDatFil
      INTEGER PFiltConst
      INTEGER PFiltPend
      INTEGER PFiltVol
      INTEGER PPasDatCMg
      INTEGER PPasDatVer
      INTEGER PRendConst
      INTEGER PRendPend
      INTEGER PRendVol
      INTEGER PPMaxConst
      INTEGER PPMaxPend
      INTEGER PPMaxVol
      INTEGER PSerDatCMg
      INTEGER PSerDatVer

      LOGICAL No
      LOGICAL Si

      INCLUDE 'pxpnpla.fpp'

!     Numero de datos Embalses
!     *************************
      PARAMETER (DimDatEmb = 8)
!     Numero Datos Series Hidraulicas
!     *******************************
      PARAMETER (DimDatSer = 2)
!     Numero Datos Pasadas
!     ********************
      PARAMETER (DimDatPas = 1)
!     Mnemotecnicos
!     *************
      PARAMETER (Si = .TRUE.)
      PARAMETER (No = .FALSE.)
!     Kit
!     ***
      INTEGER ISPFA
      INTEGER IMSPFHM
      INTEGER ISDFA
      INTEGER IMSDFHM
      PARAMETER (ISPFA   = 1)
      PARAMETER (IMSPFHM = 2)
      PARAMETER (ISDFA   = 3)
      PARAMETER (IMSDFHM = 4)
!     Meses
!     *****
      INTEGER Abril
      INTEGER Mayo
      INTEGER Junio
      INTEGER Julio
      INTEGER Agosto
      INTEGER Septiembre
      INTEGER Octubre
      INTEGER Noviembre
      INTEGER Diciembre
      INTEGER Enero
      INTEGER Febrero
      INTEGER Marzo
      PARAMETER (Abril = 1)
      PARAMETER (Mayo = 2)
      PARAMETER (Junio = 3)
      PARAMETER (Julio = 4)
      PARAMETER (Agosto = 5)
      PARAMETER (Septiembre = 6)
      PARAMETER (Octubre = 7)
      PARAMETER (Noviembre = 8)
      PARAMETER (Diciembre = 9)
      PARAMETER (Enero = 10)
      PARAMETER (Febrero = 11)
      PARAMETER (Marzo = 12)

!     Dimension del arreglo de tiempo
!     *******************************
      PARAMETER (DimTmp = 7)
!     Numero de Centrales con Caudales Aleatorios
!     *******************************************
!     Longitud del string en la conversion de enteros
!     ***********************************************
      PARAMETER (DimLargo = 10)
!
!
!     Parametros del tipo de central
!     ******************************
      PARAMETER (PCenTipEmb = 'E')
      PARAMETER (PCenTipEmbAux = 'A')
      PARAMETER (PCenTipPas = 'P')
      PARAMETER (PCenTipSer = 'S')
      PARAMETER (PCenTipTer = 'T')
      PARAMETER (PCenTipMod  = 'M')
      PARAMETER (PCenTipBat = 'B')
      PARAMETER (PCenTipRie = 'R')
      PARAMETER (PCenTipFal = 'F')
!     ******************************
!     Parametros de datos
!     ******************************
      PARAMETER (PEmbDatVer = 1)
      PARAMETER (PEmbDatDef = 2)
      PARAMETER (PEmbDatVol = 3)
      PARAMETER (PEmbDatCMg = 4)
      PARAMETER (PEmbDatFil = 5)
      PARAMETER (PEmbDatReb = 6)
      PARAMETER (PEmbDatRebP = 7)
      PARAMETER (PEmbDatRebN = 8)

      PARAMETER (PSerDatVer = 1)
      PARAMETER (PSerDatCMg = 2)
      PARAMETER (PPasDatVer = 1)
      PARAMETER (PPasDatCMg = 2)
      PARAMETER (PFiltVol   = 1)
      PARAMETER (PFiltPend  = 2)
      PARAMETER (PFiltConst = 3)
      PARAMETER (PRendVol   = 1)
      PARAMETER (PRendPend  = 2)
      PARAMETER (PRendConst = 3)
      PARAMETER (PPmaxVol   = 1)
      PARAMETER (PPmaxPend  = 2)
      PARAMETER (PPmaxConst = 3)
!     Archivos entrada
!     *********************
      CHARACTER*24 NArcAfEPAR
      CHARACTER*24 NArcAflCen
      CHARACTER*24 NArcBar
      CHARACTER*24 NArcCenFil
      CHARACTER*24 NArcCenRen
      CHARACTER*24 NArcCenPMax
      CHARACTER*24 NArcCnfCen
      CHARACTER*24 NArcCnfLin
      CHARACTER*24 NArcCnfLin2
      CHARACTER*24 NArcCosCen
      CHARACTER*24 NArcDeb
      CHARACTER*24 NArcDem
      CHARACTER*24 NArcENDESA
      CHARACTER*24 NArcENDESACI
      CHARACTER*24 NArcEta
      CHARACTER*24 NArcFilEmb
      CHARACTER*24 NArcExtr
      CHARACTER*24 NArcVRebEmb
      CHARACTER*24 NArcQeEmb
      CHARACTER*24 NArcBlo
      CHARACTER*24 NArcIndApe
      CHARACTER*24 NArcIndAp2
      CHARACTER*24 NArcIndSim
      CHARACTER*24 NArcLaja
      CHARACTER*24 NArcManCen
      CHARACTER*24 NArcManCenS
      CHARACTER*24 NArcManEmb
      CHARACTER*24 NArcMinEmbH
      CHARACTER*24 NArcManEmbS
      CHARACTER*24 NArcManLin
      CHARACTER*24 NArcManLin2
      CHARACTER*24 NArcManLin3
      CHARACTER*24 NArcCenTipo
      CHARACTER*24 NArcMat
      CHARACTER*24 NArcMaule
      CHARACTER*24 NArcPAR
      CHARACTER*24 NArcPath
      CHARACTER*24 NArcPlaEmbI1
      CHARACTER*24 NArcPlaEmbI2
      CHARACTER*24 NArcRiego
!
!     Archivos auxiliares
!************************
      CHARACTER*24 NArcLog
      CHARACTER*24 NArcLogCF
      CHARACTER*24 NArcDebLog
      CHARACTER*24 NArcUStop
      CHARACTER*24 NArcBarras
      CHARACTER*24 NArcCentrales
      CHARACTER*24 NArcEmbalses
      CHARACTER*24 NArcEtapas
      CHARACTER*24 NArcLineas
      CHARACTER*24 NArcPreSuf
      CHARACTER*24 NArcSeries
      CHARACTER*24 NArcSimuls
      CHARACTER*24 NArcCostoOp
!
!     Archivos para la lectura de una posible solucion anterior
!**************************************************************
      CHARACTER*24 NArcALPPHI
      CHARACTER*24 NArcMSDFHM1
      CHARACTER*24 NArcMSDFHM2
      CHARACTER*24 NArcMSDFHMb1
      CHARACTER*24 NArcMSDFHMb2
      CHARACTER*24 NArcMSDFHMiv1
      CHARACTER*24 NArcMSDFHMiv2
      CHARACTER*24 NArcMSPFHM
      CHARACTER*24 NArcSTATUS
      INTEGER UArcALPPHI
      INTEGER UArcMSDFHM1
      INTEGER UArcMSDFHM2
      INTEGER UArcMSDFHMb1
      INTEGER UArcMSDFHMb2
      INTEGER UArcMSDFHMiv1
      INTEGER UArcMSDFHMiv2
      INTEGER UArcMSPFHM
      INTEGER UArcSTATUS
      INTEGER UnitIni
      INTEGER UDummy
!
!     Archivos entrada
!*********************
      PARAMETER (NArcAfEPAR   = 'plpafpar.dat')
      PARAMETER (NArcAflCen   = 'plpaflce.dat')
      PARAMETER (NArcBar      = 'plpbar.dat')
      PARAMETER (NArcCenFil   = 'plpcenfi.dat')
      PARAMETER (NArcFilEmb   = 'plpfilemb.dat')
      PARAMETER (NArcExtr     = 'plpextrac.dat')
      PARAMETER (NArcVRebEmb  = 'plpvrebemb.dat')
      PARAMETER (NArcQeEmb    = 'plpqebnd.dat')
      PARAMETER (NArcCenRen   = 'plpcenre.dat')
      PARAMETER (NArcCenPMax  = 'plpcenpmax.dat')
      PARAMETER (NArcCnfCen   = 'plpcnfce.dat')
      PARAMETER (NArcCnfLin   = 'plpcnfli.dat')
      PARAMETER (NArcCnfLin2  = 'linconf.csv')
      PARAMETER (NArcCosCen   = 'plpcosce.dat')
      PARAMETER (NArcDeb      = 'plpdeb.dat')
      PARAMETER (NArcDem      = 'plpdem.dat')
      PARAMETER (NArcENDESA   = 'plpendes.dat')
      PARAMETER (NArcENDESACI = 'plpendci.dat')
      PARAMETER (NArcEta      = 'plpeta.dat')
      PARAMETER (NArcBlo      = 'plpblo.dat')
      PARAMETER (NArcIndApe   = 'plpidape.dat')
      PARAMETER (NArcIndAp2   = 'plpidap2.dat')
      PARAMETER (NArcIndSim   = 'plpidsim.dat')
      PARAMETER (NArcLaja     = 'plplaja.dat')
      PARAMETER (NArcManCen   = 'plpmance.dat')
      PARAMETER (NArcManCenS  = 'plpmances.dat')
      PARAMETER (NArcManEmb   = 'plpmanem.dat')
      PARAMETER (NArcMinEmbH  = 'plpminembh.dat')
      PARAMETER (NArcManEmbS  = 'plpmanems.dat')
      PARAMETER (NArcManLin   = 'plpmanli.dat')
      PARAMETER (NArcManLin2  = 'mantlin.csv')
      PARAMETER (NArcManLin3  = 'mantlins.csv')
      PARAMETER (NArcCenTipo  = 'centipo.csv')
      PARAMETER (NArcMat      = 'plpmat.dat')
      PARAMETER (NArcMaule    = 'plpmaule.dat')
      PARAMETER (NArcPAR      = 'plppar.dat')
      PARAMETER (NArcPath     = 'plppath.dat')
      PARAMETER (NArcRiego    = 'plpriego.dat')
      PARAMETER (NArcPlaEmbI1 = 'plpplem1.dat')
      PARAMETER (NArcPlaEmbI2 = 'plpplem2.dat')
!
!     Archivos Auxiliares
!************************
      PARAMETER (NArcLog       = 'plpwarn.log')
      PARAMETER (NArcLogCF     = 'plpfact.log')
      PARAMETER (NArcDebLog    = 'plpdeb.log')
      PARAMETER (NArcUStop     = 'userstop')
      PARAMETER (NArcBarras    = 'barras.csv')
      PARAMETER (NArcCentrales = 'centrales.csv')
      PARAMETER (NArcEmbalses  = 'embalses.csv')
      PARAMETER (NArcEtapas    = 'etapas.csv')
      PARAMETER (NArcLineas    = 'lineas.csv')
      PARAMETER (NArcPreSuf    = 'presuf.csv')
      PARAMETER (NArcSeries    = 'series.csv')
      PARAMETER (NArcSimuls    = 'simuls.csv')
      PARAMETER (NArcCostoOp   = 'costosop.csv')
!
!     Archivos para la lectura de una posible solucion anterior
!**************************************************************
!
!     Los nombre de los archivos no se utilizan, pues el compilador
!     WATCOM no acepta nombres para la creacion de archivos de acceso
!     directo. Este problema se ha solucionado con la eleccion de
!     numeros reservados de unidades logicas para estos archivos
!
      PARAMETER (NArcALPPHI    = 'ALPPHI')
      PARAMETER (NArcMSDFHM1   = 'MSDFHM1')
      PARAMETER (NArcMSDFHM2   = 'MSDFHM2')
      PARAMETER (NArcMSDFHMb1  = 'MSDFHMb1')
      PARAMETER (NArcMSDFHMb2  = 'MSDFHMb2')
      PARAMETER (NArcMSDFHMiv1 = 'MSDFHMiv1')
      PARAMETER (NArcMSDFHMiv2 = 'MSDFHMiv2')
      PARAMETER (NArcMSPFHM    = 'MSPFHM')
      PARAMETER (NArcSTATUS    = 'STATUS')
!     PELIGRO, WILL ROBINSON, PELIGRO!(10)
!     siempre se debe empezar a asignar valores en forma correlativa
!     a partir de la unidad logica numero 7
      PARAMETER (UArcSTATUS    = 7)
      PARAMETER (UArcALPPHI    = 8)
      PARAMETER (UArcMSPFHM    = 9)
      PARAMETER (UArcMSDFHM1   = 10)
      PARAMETER (UArcMSDFHMb1  = 11)
      PARAMETER (UArcMSDFHMiv1 = 12)
      PARAMETER (UArcMSDFHM2   = 13)
      PARAMETER (UArcMSDFHMb2  = 14)
      PARAMETER (UArcMSDFHMiv2 = 15)
      PARAMETER (UnitIni       = 16)
      PARAMETER (UDummy       = 999)
!     Definicion Nombres Archivos Salida 04 (arc04out.inc)
!*********************************************************
      CHARACTER*24 NArcFecha
      CHARACTER*24 NArcBDCenGen
      CHARACTER*24 NArcBDCostop
      CHARACTER*24 NArcBDSerGen
      CHARACTER*24 NArcBDExtrac
      CHARACTER*24 NArcBDCMg
      CHARACTER*24 NArcBDEmb
      CHARACTER*24 NArcBDEmbE
      CHARACTER*24 NArcBDLin
      CHARACTER*24 NArcLajaOut1
      CHARACTER*24 NArcLajaOut2
      CHARACTER*24 NArcLajaOut
      CHARACTER*24 NArcMauleOut1
      CHARACTER*24 NArcMauleOut2
      CHARACTER*24 NArcMauleOut3
      CHARACTER*24 NArcMauleOut4
      CHARACTER*24 NArcMauleOut
      CHARACTER*24 NArcPlaEmbO
      CHARACTER*24 NArcPlane
      CHARACTER*24 NArcRun
!     Archivos Salida
!********************
      PARAMETER( NArcFecha     = 'fecha-corrida')
      PARAMETER( NArcBDCenGen  = 'plpcen.csv')
      PARAMETER( NArcBDCostop  = 'plpcostop.csv')
      PARAMETER( NArcBDSerGen  = 'plpser.csv')
      PARAMETER( NArcBDExtrac  = 'plpextrac.csv')
      PARAMETER( NArcBDCMg     = 'plpbar.csv')
      PARAMETER( NArcBDEmb     = 'plpemb.csv')
      PARAMETER( NArcBDEmbE    = 'plpembe.csv')
      PARAMETER( NArcBDLin     = 'plplin.csv')
      PARAMETER (NArcLajaOut   = 'plplaja.csv')
      PARAMETER (NArcLajaOut1  = 'plplaja1.csv')
      PARAMETER (NArcLajaOut2  = 'plplaja2.csv')
      PARAMETER (NArcMauleOut  = 'plpmaule.csv')
      PARAMETER (NArcMauleOut1 = 'plpmaul1.csv')
      PARAMETER (NArcMauleOut2 = 'plpmaul2.csv')
      PARAMETER (NArcMauleOut3 = 'plpmaul3.csv')
      PARAMETER (NArcMauleOut4 = 'plpmaul4.csv')
      PARAMETER (NArcPlaEmbO   = 'plpplaem.csv')
      PARAMETER (NArcPlane     = 'plpplanos.csv')
      PARAMETER (NArcRun       = 'plprun.dat')
      

!     Constantes
!************************
      DOUBLE PRECISION FactTiempoH
      PARAMETER (FactTiempoH = 3.6d0)

      INTEGER FILT_PROM
      INTEGER FILT_CONS
      INTEGER FILT_LINE

      PARAMETER (FILT_PROM = 0 )
      PARAMETER (FILT_CONS = 1 )
      PARAMETER (FILT_LINE = 2 )


      INTEGER FACT_NONE
      INTEGER FACT_PRIRAY
      INTEGER FACT_FARKAS
      INTEGER FACT_ELASTIC

      PARAMETER (FACT_NONE   = 0 )
      PARAMETER (FACT_PRIRAY = 1 )
      PARAMETER (FACT_FARKAS = 2 )
      PARAMETER (FACT_ELASTIC = 3 )
