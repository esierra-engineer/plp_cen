      SUBROUTINE LeeCenBatDim(ParBaterias,FBaterias,ULog)

      USE PLP, ONLY : PAR_BATERIAS

      TYPE(PAR_BATERIAS) ParBaterias

!
!
!     parametros y variables locales:
!

      INTEGER NBaterias,MaxIny
      INTEGER ULog
      INTEGER URead
      CHARACTER*24 NArcCenBat
!
      CHARACTER*12 AuxVar
      INTEGER Abrir
      LOGICAL FBaterias
!************************
! Se lee el archivo, en caso de que no exista,
! se marca la flag en false y no se procede con la config de baterias.
      NArcCenBat='plpcenbat.dat'
      URead = Abrir(NArcCenBat, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(ULog, '(2A)') 'leecenbat: Error, no existe archivo ',    &
     &        NArcCenBat
         FBaterias=.False.
         return
      ENDIF
!
!***************************************
!     Numero de Baterias y máximo de inyección
!***************************************
      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NBaterias, MaxIny

      ParBaterias%NBaterias = NBaterias
      ParBaterias%MaxIny = MaxIny
      CALL Cerrar(URead)

      RETURN
      END


!*************************************
!     Subrutina Lee Configuracion Centrales
!*************************************
      SUBROUTINE LeeCenBat(ParBaterias,CenNom,Dim ,ULog)

      USE PLP

      TYPE(PAR_BATERIAS) ParBaterias
      TYPE(PAR_DIMS), INTENT(IN) :: Dim

      CHARACTER*48 CenNom(Dim%Cen)
!     
!
!     parametros y variables locales:
      CHARACTER*12 AuxVar
      INTEGER Abrir
      INTEGER ULog
      INTEGER URead
      LOGICAL FWarning

      INTEGER NBaterias, MaxIny
      INTEGER BatInd
      INTEGER NIny
      CHARACTER*48 BatNom
      CHARACTER*48 NomBatIny
      DOUBLE PRECISION FPC
      INTEGER BatBar
      DOUBLE PRECISION FPD, BatEMin, BatEMax
      INTEGER IIny, IBat, ICen
      CHARACTER*24 NArcCenBat
      INTEGER NInyPasada
!
!     codigo:
      FWarning = .FALSE.
!
!************************
!     Lee datos plpcenbat.dat
!************************
      NArcCenBat='plpcenbat.dat'
      URead = Abrir(NArcCenBat, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(2A)') 'leecenbat: Error, no existe archivo ',       &
     &        NArcCenBat
         WRITE(ULog, '(2A)') 'leecenbat: Error, no existe archivo ',    &
     &        NArcCenBat
         return
      ENDIF
!********* guarda memoria para los datos *********
!
      ALLOCATE(ParBaterias%FPC(ParBaterias%NBaterias,ParBaterias%MaxIny))
      ALLOCATE(ParBaterias%NomBatIny(ParBaterias%NBaterias,ParBaterias%MaxIny))
      ALLOCATE(ParBaterias%NumInyBat(ParBaterias%NBaterias,ParBaterias%MaxIny))
      ALLOCATE(ParBaterias%BatInd(ParBaterias%NBaterias))
      ALLOCATE(ParBaterias%BatCenInd(ParBaterias%NBaterias))
      ALLOCATE(ParBaterias%NIny(ParBaterias%NBaterias))
      ALLOCATE(ParBaterias%BatNom(ParBaterias%NBaterias))
      ALLOCATE(ParBaterias%FPD(ParBaterias%NBaterias))
      ALLOCATE(ParBaterias%BatEMin(ParBaterias%NBaterias,Dim%Blo))
      ALLOCATE(ParBaterias%BatEMax(ParBaterias%NBaterias,Dim%Blo))
      ALLOCATE(ParBaterias%BatBar(ParBaterias%NBaterias))

!******** Inicializa las matrices *******
      ParBaterias%FPC(1:ParBaterias%NBaterias, 1:ParBaterias%MaxIny)= 0
      ParBaterias%NomBatIny(1:ParBaterias%NBaterias,1:ParBaterias%MaxIny)=''
      ParBaterias%NumInyBat(1:ParBaterias%NBaterias,1:ParBaterias%MaxIny)=0
      NInyPasada=0 ! Se inicializa el valor
!
!***************************************
!     Numero de Baterias y máximo de inyección
!***************************************
      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NBaterias,MaxIny
!*********************************************
!******************************
!     Lee Datos de Baterias
!******************************
      READ(URead, '(A1)') AuxVar
      
      DO IBat = 1, NBaterias
         READ(URead, *) BatInd, BatNom !Lectura del indice y nombre
         READ(URead, '(A1)') AuxVar
         READ(URead, *) NIny  !Lectura del numero de inyecciones
         ! Se asignan a la estructura ParBaterias (ver parbaterias.f)
         ParBaterias%BatInd(IBat)= BatInd
         ParBaterias%BatNom(IBat)= BatNom
         ! Consigo el índice de la central bateria
         ICen=0
         CALL NomCen2NumCen(ICen, FWarning, BatNom,  &
      &        CenNom, Dim%Cen, 'plpcenbat.dat', ULog)
         ParBaterias%BatCenInd(IBat)=ICen
         ParBaterias%NIny(IBat)= NIny
         ParBaterias%NumColBlo= ParBaterias%NumColBlo + NIny ! Se suma la variable EC_i^b
         ParBaterias%NumFilBlo= ParBaterias%NumFilBlo + NIny*2 ! Se agregan las restricciones  GMIN \leq g + EC \leq GMAX
         DO IIny=1, NIny ! Se revisan todas las inyecciones a la batería
            READ(URead, '(A1)') AuxVar
            READ(URead, *) NomBatIny , FPC      !En cada inyección se registra el nombre y el FPC
            ! Se asigna a la estructura ParBaterias
            ParBaterias%NomBatIny(IBat,IIny)= NomBatIny
            ParBaterias%FPC(IBat,IIny)= FPC
            ! Consigo el indice de la central que inyecta
            CALL NomCen2NumCen(ICen, FWarning, NomBatIny,  &
     &        CenNom, Dim%Cen, 'plpcenbat.dat', ULog)
            ! Guardo el indice de la central
            ParBaterias%NumInyBat(IBat,IIny)= ICen
            IF (Dim%CenLabel(ICen) .EQ. PCenTipPas) THEN
                  NInyPasada = NInyPasada + 1
            ENDIF
         ENDDO
         READ(URead, '(A1)') AuxVar
         !Leo los datos especificos de la bateria
         READ(URead, *)                                           &
      &        BatBar, FPD,                                       &
      &        BatEMin,BatEMax
         !Asigno los datos a la estructura
         ParBaterias%BatBar(IBat)= BatBar
         ParBaterias%FPD(IBat)= FPD
         !Estos parámetros van con Dim%Blo debido a que se pueden modificar con un mantenimiento
         ParBaterias%BatEMin(IBat,1:Dim%Blo)= BatEMin
         ParBaterias%BatEMax(IBat,1:Dim%Blo)= BatEMax
      ENDDO
      CALL Cerrar(URead)  
      ! se asigna el numero de variables y restricciones
      

      ParBaterias%NInyPasada= NInyPasada
      ParBaterias%NumColBlo= ParBaterias%NumColBlo  + NBaterias*2 ! ED, SoC
      ParBaterias%NumFilBlo= ParBaterias%NumFilBlo  + NBaterias*2 ! def ED, y balance de energia SoC
      !NumFilBlo = NBaterias*2 + 2*NBaterias*NIny(IBat)
      ParBaterias%NumColEta=NBaterias ! ET
      ParBaterias%NumFilEta=NBaterias ! Def ET

      ! Arreglos de indices de restricciones
      ALLOCATE(ParBaterias%ECg_pfil_pasada(ParBaterias%NInyPasada,Dim%IBlo,Dim%Eta))
      ALLOCATE(ParBaterias%ICen_Pasada(ParBaterias%NInyPasada))
      ALLOCATE(ParBaterias%ColIndEta(Dim%Eta, ParBaterias%NumColEta))
      ALLOCATE(ParBaterias%FilIndEta(Dim%Eta, ParBaterias%NumFilEta))

      ALLOCATE(ParBaterias%SoCf_1(ParBaterias%NBaterias))
      ALLOCATE(ParBaterias%SoC_col(ParBaterias%NBaterias))
      ALLOCATE(ParBaterias%egen_col(ParBaterias%NBaterias))
      ALLOCATE(ParBaterias%ed_col(ParBaterias%NBaterias))
      ALLOCATE(ParBaterias%ec_col(ParBaterias%NBaterias,ParBaterias%MaxIny))
      ALLOCATE(ParBaterias%ten_col(Dim%Eta,ParBaterias%NBaterias))

      ALLOCATE(ParBaterias%Soc_fil(ParBaterias%NBaterias))

      ALLOCATE(ParBaterias%DataBlo(ParBaterias%NumColBlo, Dim%Blo))
      ALLOCATE(ParBaterias%DataEta(ParBaterias%NumColEta, Dim%Eta))
      ALLOCATE(ParBaterias%DualBlo(ParBaterias%NumFilBlo, Dim%Blo))
      RETURN
      END

!****************************************
!     Lee mantenimientos de Baterias
!****************************************

      SUBROUTINE LeeManBat(ParBaterias,Dim,ULog)

      USE PLP

      TYPE(PAR_BATERIAS) ParBaterias
      TYPE(PAR_DIMS), INTENT(IN) :: Dim
      INTEGER ULog
      EXTERNAL Abrir
      INTEGER Abrir

      ! variables locales
      INTEGER URead
      CHARACTER*24 NArcManBat
      CHARACTER*8 AuxVar
      CHARACTER*48 NomCen
      INTEGER NBloMan
      INTEGER IBind
      DOUBLE PRECISION EMin, EMax
      INTEGER NBatMan
      INTEGER IMan, IBloque, IBat
      LOGICAL FWarning


      NArcManBat= 'plpmanbat.dat'
      FWarning= .FALSE.


      URead = Abrir(NArcManBat, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(ULog, '(3A)') 'leemanbat: Warning, no existe archivo ',    &
     &        NArcManBat, '.'
         RETURN
      ENDIF
      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NBatMan  !AJJAJA Batman y robin

      DO IMan=1, NBatMan
            READ(URead, '(A1)') AuxVar
            READ(URead, *) NomCen
            READ(URead, '(A1)') AuxVar
            READ(URead, *) NBloMan
            READ(URead, '(A1)') AuxVar
            DO IBloque=1, NBloMan
                  READ(URead, *) IBind,   &
            &           EMin, EMax       
                  IF ((IBind .LT. 1) .OR. (IBind .GT. Dim%Blo)) THEN
                  CYCLE
                  ENDIF

                  IBat=0
                  !Consigo el indice de la bateria.
                  CALL NomCen2NumCen(IBat, FWarning, NomCen,  &
      &        ParBaterias%BatNom, ParBaterias%NBaterias, NArcManBat, ULog)
                  IF (IBat .EQ. 0) THEN
                  CYCLE
                  ENDIF
                  !Hago esto en caso de que no se quiera modificar
                  !un valor, entonces ponerle -1 y que se mantenga por
                  !defecto el que ya existe
                  IF (EMin .GE. 0) THEN
                        ParBaterias%BatEMin(IBat,IBind)= EMin
                  ENDIF
                  IF (EMax .GE. 0) THEN
                        ParBaterias%BatEMax(IBat,IBind)=EMax
                  ENDIF

            ENDDO
      ENDDO 
      CALL Cerrar(URead)

      RETURN
      END


!********************************
!     Genera la parte de la matriz de restricciones de var. de bloques respecto a baterias
!********************************

      SUBROUTINE GenPDBatBloA(IEta,IBlo,Dim,                        &
     &      ParBaterias, COffset,FOffset,CenOffset,PDNCol,bdur,        &
     &      PDNFila,BatCOffset,A,PDNombre,Sentido)

      USE PLP
      USE A_MATRIX
      !variables externas

      TYPE(PAR_BATERIAS) ParBaterias
      TYPE(PAR_DIMS), INTENT(IN) :: Dim
      TYPE(AMatrix) A
      INTEGER IEta,IBlo
      INTEGER COffset,FOffset,CenOffset,BatCOffset
      INTEGER PDNCol
      INTEGER PDNFila
      CHARACTER*24 PDNombre (PDNCol)
      CHARACTER*1 Sentido(PDNFila)
      DOUBLE PRECISION bdur
      

      !Variables internas
      INTEGER IBat,IIny
      INTEGER ICol,IFil
      CHARACTER*(128) Nombreb,Nombreiny
      DOUBLE PRECISION factor
      INTEGER IInyPasada

      !arreglos de indices internos
      INTEGER egen_col(ParBaterias%NBaterias)
      INTEGER ed_col(ParBaterias%NBaterias)
      INTEGER soc_col(ParBaterias%NBaterias)
      INTEGER ec_col(ParBaterias%NBaterias,ParBaterias%MaxIny)
      INTEGER ed_fil(ParBaterias%NBaterias)
      INTEGER ECg_nfil(ParBaterias%NBaterias,ParBaterias%MaxIny)
      INTEGER ECg_pfil(ParBaterias%NBaterias,ParBaterias%MaxIny)
      INTEGER SoC_fil(ParBaterias%NBaterias)


      ICol=0

      ! de esta manera las columnas de bateria quedarían como 
      ! COffset + IBat
      ! NumColBlo = NBaterias
      
      DO IBat=1, ParBaterias%NBaterias
            Nombreb = trim(itoa(IBat)) // '_' // trim(itoa(IBlo))
            ICol = ICol + 1
            PDNombre(COffset + ICol) = 'soc' // Nombreb
            soc_col(IBat) = COffset + ICol
      ENDDO
      ! NumColBlo = NBaterias*2
      DO IBat=1, ParBaterias%NBaterias
            Nombreb = trim(itoa(IBat)) // '_' // trim(itoa(IBlo))
            ICol = ICol + 1
            PDNombre(COffset + ICol) = 'den' // Nombreb
            ed_col(IBat) = COffset + ICol
      ENDDO
      ! NumColBlo= NBaterias * 3 + NBaterias*NIny
      DO IBat=1, ParBaterias%NBaterias
            DO IIny=1, ParBaterias%NIny(IBat)
                  Nombreiny = trim(itoa(IBat)) // 'i'// trim(itoa(IIny)) // '_' // trim(itoa(IBlo))  
                  ICol = ICol +1
                  PDNombre(COffset + ICol) ='cen' // Nombreiny
                  ec_col(IBat,IIny) = COffset + ICol
            ENDDO
      ENDDO

      ! La inyección de energía viene dado porque la batería clasifica como central
      DO IBat=1, ParBaterias%NBaterias
            egen_col(IBat)=CenOffset + ParBaterias%BatCenInd(IBat)
      ENDDO

      IFil=0
      ! NumFilBlo = NBaterias + 2*(NBaterias*NIny)
      IInyPasada=0
      DO IBat=1, ParBaterias%NBaterias
            IFil=IFil + 1
            !Restriccion: ED_BESS^b *( FPD / bdur) = egen_BESS^b
            ed_fil(IBat)= IFil + FOffset
            factor= ParBaterias%FPD(IBat)
            CALL Am_set(A,ed_col(IBat),ed_fil(IBat), factor)

      
            CALL Am_set(A,CenOffset + ParBaterias%BatCenInd(IBat), ed_fil(IBat), - bdur)
            Sentido(ed_fil(IBat)) = 'E'
            DO IIny=1, ParBaterias%NIny(IBat)
                  IFil= IFil + 1
                  !Cota superior
                  ECg_pfil(IBat,IIny)= FOffset + IFil
                  !Se agrega la variable de generación bdur* g_{central}^b + EC_{Bess}^b <= gmax_{central}*bdur
                  CALL Am_set(A,CenOffset + ParBaterias%NumInyBat(IBat,IIny),ECg_pfil(IBat,IIny),1.0d0)
                  CALL Am_set(A,ec_col(IBat,IIny), ECg_pfil(IBat,IIny), 1.0d0 / bdur)
                  Sentido(ECg_pfil(IBat,IIny)) = 'L'

                  ! Si la central es de pasada, tengo que modificar el RHS de esta desigualdad al afluente de la central correspondiente.
                  ! por lo que guardo el número de la fila que tengo que modificar en este array para usarlo en la funcion
                  ! FijaMues del archivo plp-fijamues.f
                  IF (Dim%CenLabel(ParBaterias%NumInyBat(IBat,IIny)) .EQ. PCenTipPas) THEN
                        IInyPasada= IInyPasada + 1
                        ParBaterias%ICen_Pasada(IInyPasada)= ParBaterias%NumInyBat(IBat,IIny)
                        ParBaterias%ECg_pfil_pasada(IInyPasada,IBlo,IEta)= ECg_pfil(IBat,IIny)
                  ENDIF
                  IFil=IFil + 1
                  !Cota inferior
                  ECg_nfil(IBat,IIny)= FOffset + IFil
                  !Se agrega la variable de generación bdur* g_{central}^b  + EC_{Bess}^b >= gmin_{central}*bdur
                  CALL Am_set(A,CenOffset + ParBaterias%NumInyBat(IBat,IIny),ECg_nfil(IBat,IIny),1.0d0)
                  CALL Am_set(A,ec_col(IBat,IIny), ECg_nfil(IBat,IIny), 1.0d0 / bdur)
                  Sentido(ECg_nfil(IBat,IIny)) = 'G'
            ENDDO
      ENDDO

      ! Restriccion: ET^t_BESS = FPC \sum_{b \in BL, i \in G_BESS} EC^b_i
      DO IBat=1, ParBaterias%NBaterias
            DO IIny=1, ParBaterias%NIny(IBat)
                  factor= ParBaterias%FPC(IBat,IIny)
                  CALL Am_set(A,ec_col(IBat, IIny),  &
                  &           ParBaterias%FilIndEta(IEta,IBat) , -factor)
                  Sentido(ParBaterias%FilIndEta(IEta,IBat))= 'E'
            ENDDO
      ENDDO

      !     Primer bloque queda SoCf^1_BESS= ET - ED^1_BESS
      ! NumFilBlo = NBaterias + 2*(NBaterias*NIny) + NBaterias
      DO IBat=1, ParBaterias%NBaterias
            IFil= IFil + 1
            SoC_fil(IBat) = FOffset + IFil

            CALL Am_set(A, soc_col(IBat), SoC_fil(IBat), 1.0d0)
            CALL Am_set(A, ed_col(IBat), SoC_fil(IBat), 1.0d0)

            IF (IBlo .GT. 1) THEN
                  CALL Am_set(A,BatCOffset + IBat, SoC_fil(IBat), -1.0d0)
            ENDIF
            Sentido(SoC_fil(IBat))= 'E'
      ENDDO
      ! se guarda el arreglo de valores del bloque 1 para la restriccion SoC_1= ET - ED_1
      IF (IBlo .EQ. 1) THEN
            ParBaterias%SoCf_1(1:ParBaterias%NBaterias)= SoC_fil(1:ParBaterias%NBaterias)
      ENDIF

      ParBaterias%SoC_col(1:ParBaterias%NBaterias) = soc_col(1:ParBaterias%NBaterias) - COffset
      ParBaterias%egen_col(1:ParBaterias%NBaterias)= egen_col(1:ParBaterias%NBaterias) - CenOffset
      ParBaterias%ed_col(1:ParBaterias%NBaterias) = ed_col(1:ParBaterias%NBaterias) - COffset
      ParBaterias%ec_col(1:ParBaterias%NBaterias, 1:ParBaterias%MaxIny) = ec_col(1:ParBaterias%NBaterias, 1:ParBaterias%MaxIny) - COffset
      ParBaterias%Soc_fil(1:ParBaterias%NBaterias) = Soc_fil(1:ParBaterias%NBaterias) - FOffset
!*********************
!     OFFSET FINAL
!*********************
      COffset = COffset + ParBaterias%NumColBlo
      FOffset = FOffset + ParBaterias%NumFilBlo
      RETURN
      END



!********************************
!     Genera la parte de la matriz de restricciones de var. de etapas respecto a baterias
!********************************
      SUBROUTINE GenPDBatEtaA(IEta,                        &
     &      ParBaterias, COffset,FOffset,PDNCol,        &
     &      A,PDNombre)

      USE PLP
      USE A_MATRIX
      !variables externas
      TYPE(PAR_BATERIAS) ParBaterias
      TYPE(AMatrix) A
      INTEGER IEta
      INTEGER COffset,FOffset
      INTEGER PDNCol
      CHARACTER*24 PDNombre(PDNCol)

      !variables internas
      INTEGER IBat
      CHARACTER*12 Nombreb



      ! NumColEta = NBaterias
      DO IBat=1, ParBaterias%NumColEta
            Nombreb= trim(itoa(IBat))
            PDNombre(COffset + IBat)= 'ten' // Nombreb
            ParBaterias%ColIndEta(IEta,IBat)= COffset + IBat
            ParBaterias%FilIndEta(IEta,IBat)= FOffset + IBat
            ParBaterias%ten_col(IEta,IBat)= IBat
      ENDDO
      ! Restriccion ET = \sum FPC_i EC_i^b
      DO IBat=1,ParBaterias%NBaterias
            CALL Am_set(A, ParBaterias%ColIndEta(IEta,IBat),      &
            &     ParBaterias%FilIndEta(IEta,IBat), 1.0d0)
            ! Restriccion SoC_1= ET - ED_1
            CALL Am_set(A,ParBaterias%ColIndEta(IEta,IBat),       &
            &     ParBaterias%SoCf_1(IBat), -1.0d0)
      ENDDO
      

      

     RETURN
     END



!********************************
!     Genera FO y bounds de var. de bloques respecto a baterias
!********************************
      SUBROUTINE GenPDBatBloFO(IBlo, NBloque, IBind,                  &
      &           ParBaterias, edur,                       &
      &           COffset,PDNCol, FO, LowBnd, UppBnd)
      


      USE PLP
      USE OSI

      INTEGER IBlo
      INTEGER NBloque
      INTEGER IBInd
      INTEGER PDNCol
      INTEGER COffset
      DOUBLE PRECISION edur
      DOUBLE PRECISION FO(PDNCol)
      DOUBLE PRECISION LowBnd(PDNCol)
      DOUBLE PRECISION UppBnd(PDNCol)


      TYPE(PAR_BATERIAS) ParBaterias
      ! Varaibles locales

      INTEGER ICol, IBat
      DOUBLE PRECISION edurfp
      DOUBLE PRECISION DINFTY


      DINFTY= osi_getinfty()

      FO(COffset + 1:COffset + ParBaterias%NumColBlo) = 0
      LowBnd(COffset + 1 : COffset + ParBaterias%NumColBlo) = 0
      
      UppBnd(COffset + 1 : COffset + ParBaterias%NumColBlo)= DINFTY
      ICol= COffset
      DO IBat=1, ParBaterias%NBaterias
            ICol = COffset + ParBaterias%SoC_col(IBat)
            !Se define que las baterias solamente se pueden cargar completamente 1 vez en la etapa (1 dia)
            ! por lo que su autonomia en la etapa es de HorasEtapa=edur* HorasAutonomia/24   (Hacer regla de 3: 1 edur -> 24[h])
            !                                                                                        x edur -> HorasAutonomia
            ! y con su horas de autonomia dentro de la etapa, entonces se consigue que su capacidad máxima de almacenamiento
            ! dentro de la etapa, corresponde a PMax*HorasEtapa=PMax*edur*HorasAutonomia/24=PMax*edur*(EMax/PMax)/24
            !                                                  = EMax*edur/24
            ! Análogo para EMin.
            edurfp = edur/(24)
            UppBnd(ICol)= ParBaterias%BatEMax(IBat,IBind)* edurfp
            LowBnd(ICol)= ParBaterias%BatEMin(IBat,IBind)* edurfp

            IF (IBlo .EQ. NBloque) THEN
            ! fijo que SoC_BESS^{T} = BatEMin^{t}, donde T ultimo bloque para no dejar espacio a que guarde energía para la próxima etapa
                  UppBnd(ICol)= ParBaterias%BatEMin(IBat,IBind)*edurfp
            ENDIF
            

      ENDDO
      
      RETURN
      END


!********************************
!     Genera FO y bounds de var. de etapa respecto a baterias
!********************************

      SUBROUTINE GenPDBatEtaFO(ParBaterias,COffset,edur,IEta,       &
      &          BloUnoInd,PDNCol, FO, LowBnd, UppBnd)

      USE PLP
      USE OSI
      
      INTEGER COffset
      INTEGER PDNCol
      INTEGER IEta
      INTEGER BloUnoInd
      DOUBLE PRECISION FO(PDNCol)
      DOUBLE PRECISION LowBnd(PDNCol)
      DOUBLE PRECISION UppBnd(PDNCol)
      DOUBLE PRECISION edur
      TYPE(PAR_BATERIAS) ParBaterias

      !variables locales
      DOUBLE PRECISION DINFTY
      INTEGER IBat
      INTEGER ICol
      DOUBLE PRECISION edurfp

      DINFTY= osi_getinfty()

!     Función Objetivo
!**********************
      FO(COffset + 1 : COffset + ParBaterias%NumColEta) = 0
!     Bounds de las variables
!**********************
      UppBnd(COffset + 1 : COffset + ParBaterias%NumColEta) = DINFTY
      LowBnd(COffset + 1 : COffset + ParBaterias%NumColEta) = 0

      edurfp=edur/(24)
      DO IBat=1, ParBaterias%NBaterias
            ! Cota de ET_{BESS} \leq edur*BatEMax
            ICol=ParBaterias%ColIndEta(IEta,IBat)
            UppBnd(ICol)= ParBaterias%BatEMax(IBat,BloUnoInd)*edurfp
      ENDDO
      RETURN
      END


!********************************
!     Graba Archivo Datos de Baterias
!********************************
      SUBROUTINE GraDatBDBatN(ISimul,NBloques,BloEta,CenPGen,PmaxCen,CenPMin, &
            &     FPhi,ParBaterias,Dim,ULog)

      USE PLP
      TYPE(PAR_BATERIAS) ParBaterias
      TYPE(PAR_DIMS) Dim
      INTEGER ULog
      INTEGER NBloques
      INTEGER BloEta(Dim%Blo)
      DOUBLE PRECISION FPhi(Dim%Eta)
      DOUBLE PRECISION CenPGen(Dim%Cen, Dim%Blo)
      DOUBLE PRECISION PmaxCen(Dim%Cen, Dim%Blo)
      DOUBLE PRECISION CenPMin(Dim%Cen, Dim%Blo)
      INTEGER ISimul

      !Vars locales
      CHARACTER*24 NArcNom
      CHARACTER*24 NombreBat
      CHARACTER*8 NomSimul
      INTEGER UWrite
      INTEGER IIny
      INTEGER IBat
      INTEGER IEta
      INTEGER IBlo
      INTEGER Abrir
      DOUBLE PRECISION FT
      NArcNom = 'plpbat.csv'
      
      IF (ISimul .EQ. 1) THEN
         UWrite = Abrir(NArcNom, 'UNKNOWN', 'SEQUENTIAL', ULog)
         WRITE(UWrite, '(A)',advance="no") 'Hidro, Bloque, BatNom'
         WRITE(UWrite, '(",",A)',advance="no") 'SoCf'
         WRITE(UWrite, '(",",A)',advance="no") 'PGen'
         WRITE(UWrite, '(",",A)',advance="no") 'EnDes'
         WRITE(UWrite, '(",",A)',advance="no") 'EnTot'
         WRITE(UWrite, '(",",A)',advance="no") 'EMin'
         WRITE(UWrite, '(",",A)',advance="no") 'EMax'
         WRITE(UWrite, '(",",A)',advance="no") 'PMin'
         WRITE(UWrite, '(",",A)',advance="no") 'PMax'
         WRITE(UWrite, '(",",A)',advance="no") 'DualSoC'
         DO IIny=1, ParBaterias%MaxIny
            WRITE(UWrite, '(",",A)',advance="no") 'EnIny' // trim(itoa(IIny))
         ENDDO
         WRITE(UWrite, *)
      ELSE
         UWrite = Abrir(NArcNom, 'OLD', 'APPEND', ULog)
      ENDIF

      IF (ISimul .EQ. 0) THEN
         NomSimul = 'MEDIA'
      ELSE
         WRITE(NomSimul,'("Sim", I3)') ISimul
      ENDIF

      DO IBat = 1, ParBaterias%NBaterias
         NombreBat = ParBaterias%BatNom(IBat)
         DO IBlo = 1, NBloques
            IEta = BloEta(IBlo)
            FT=FPhi(BloEta(IBlo))            
            WRITE(UWrite, '(A,",",I4,",",A,9(",",F12.2))',advance="no") &
     &           trim(NomSimul), IBlo, NombreBat,                &
     &           ParBaterias%DataBlo(ParBaterias%Soc_col(IBat),IBlo), &
     &           CenPGen(ParBaterias%BatCenInd(IBat),IBlo), &
     &           ParBaterias%DataBlo(ParBaterias%ed_col(IBat),IBlo), &
     &           ParBaterias%DataEta(ParBaterias%ten_col(IEta,IBat),IEta), &
     &           ParBaterias%BatEMin(IBat,IBlo), &
     &           ParBaterias%BatEMax(IBat,IBlo), &
     &           CenPMin(ParBaterias%BatCenInd(IBat),IBlo),&
     &           PmaxCen(ParBaterias%BatCenInd(IBat),IBlo), &
     &           ParBaterias%DualBlo(ParBaterias%Soc_fil(IBat),IBlo)*FT
            DO IIny=1, ParBaterias%MaxIny
                  IF (IIny .GT. ParBaterias%NIny(IBat)) THEN
                        WRITE(UWrite, '(",",F12.2)',advance="no") -1.0d0
                  ELSE
                        WRITE(UWrite, '(",",F12.2)',advance="no") ParBaterias%DataBlo(ParBaterias%ec_col(IBat,IIny),IBlo)
                  ENDIF
            ENDDO
            WRITE(UWrite,*)
         ENDDO
      ENDDO
      CALL Cerrar(UWrite)

      RETURN 
      END