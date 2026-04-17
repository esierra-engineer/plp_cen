!***************************************
!     Graba Archivo Costos Provision de reserva
!***************************************
      SUBROUTINE GraDatBDCPRes(ISimul, NBloques, BloEta, BloDur, &
     &     ParRes, FPhi, Dim, ULog)
      USE PLP

      TYPE(PAR_RESERVA), INTENT(IN) :: ParRes
      TYPE(PAR_DIMS), INTENT(IN) :: Dim
      INTEGER ULog
!
      CHARACTER*24 NArcNom
      CHARACTER*8 NomSimul
      CHARACTER Zona
      DOUBLE PRECISION BloDur(Dim%Blo)
      DOUBLE PRECISION FPhi(Dim%Eta)
      INTEGER Abrir
      INTEGER IBlo
      INTEGER IEta
      INTEGER ICen
      INTEGER ISimul
      INTEGER IZona
      INTEGER NBloques
      INTEGER BloEta(Dim%Blo)
      INTEGER NCentral
      INTEGER UWrite2
      DOUBLE PRECISION suma

      NArcNom = 'plpcostpres.csv'

      IF (ISimul .EQ. 1) THEN
         UWrite2 = Abrir(NArcNom, 'UNKNOWN', 'SEQUENTIAL', ULog)
         WRITE(UWrite2, '(A)') 'Hidro, IBlo, CostoProvActual'
      ELSE
         UWrite2 = Abrir(NArcNom, 'OLD', 'APPEND', ULog)
      ENDIF

      IF (ISimul .EQ. 0) THEN
         NomSimul = 'MEDIA'
      ELSE
         WRITE(NomSimul,'("Sim", I3)') ISimul
      ENDIF

      NCentral = ParRes%NumCen
      DO IZona=1, ParRes%NumZonas
         Zona = ParRes%Zonas(IZona:IZona)
         DO IBlo = 1, NBloques
            IEta = BloEta(IBlo)
            suma = 0.0d0
            DO ICen = 1, NCentral
               suma = suma + &
      &           ( ParRes%DataBlo(ParRes%rppz_col(IZona,ICen),IBlo)*ParRes%CPpp(ICen,IEta) &
      &           + ParRes%DataBlo(ParRes%rpnz_col(IZona,ICen),IBlo)*ParRes%CPpn(ICen,IEta) &
      &           + ParRes%DataBlo(ParRes%rspz_col(IZona,ICen),IBlo)*ParRes%CPsp(ICen,IEta) &
      &           + ParRes%DataBlo(ParRes%rsnz_col(IZona,ICen),IBlo)*ParRes%CPsn(ICen,IEta) &
      &           + ParRes%DataBlo(ParRes%rtpz_col(IZona,ICen),IBlo)*ParRes%CPtp(ICen,IEta) &
      &           + ParRes%DataBlo(ParRes%rtnz_col(IZona,ICen),IBlo)*ParRes%CPtn(ICen,IEta) &
      &           ) * BloDur(IBlo) / FPhi(IEta)
            ENDDO
            WRITE(UWrite2, '(A,A,A,A,I4,A,F18.3)') NomSimul,',',Zona,',',IBlo,',',suma
         ENDDO
      ENDDO
      CALL Cerrar(UWrite2)

      RETURN
      END

!******************************************
!     Graba Archivo Datos de reserva
!******************************************
      SUBROUTINE GraDatBDResN(ISimul,                   &
     &     NBloques,                                    &
     &     CenNom, BloEta, FPhi,                        &
     &     ParRes, Dim, ULog)
!
      USE PLP

      TYPE(PAR_DIMS), INTENT(IN) :: Dim
      INTEGER ULog

      INTEGER BloEta(Dim%Blo)
      DOUBLE PRECISION FPhi(Dim%Eta)


!
      CHARACTER*48 CenNom(Dim%Cen)
      TYPE(PAR_RESERVA) ParRes
      CHARACTER*48 NArcNom
      INTEGER Abrir
      INTEGER IBlo
      INTEGER ISimul
      INTEGER NBloques
      INTEGER UWrite

      INTEGER ICen

      DOUBLE PRECISION FT
      INTEGER IZona
      CHARACTER Zona
      CHARACTER*48 CNom
      CHARACTER*8 NomSimul
!
      NArcNom = ParRes%NArcZonas

      IF (ISimul .EQ. 1) THEN
         UWrite = Abrir(NArcNom, 'UNKNOWN', 'SEQUENTIAL', ULog)
         WRITE(UWrite, '(A)',advance="no") 'Hidro, Bloque, Zona'
         WRITE(UWrite, '(",",A)',advance="no") 'zr10s'
         WRITE(UWrite, '(",",A)',advance="no") 'zr5mp'
         WRITE(UWrite, '(",",A)',advance="no") 'zr5mn'
         WRITE(UWrite, '(",",A)',advance="no") 'zrpp'
         WRITE(UWrite, '(",",A)',advance="no") 'zrpn'
         WRITE(UWrite, '(",",A)',advance="no") 'zrsp'
         WRITE(UWrite, '(",",A)',advance="no") 'zrsn'
         WRITE(UWrite, '(",",A)',advance="no") 'zrtp'
         WRITE(UWrite, '(",",A)',advance="no") 'zrtn'
         WRITE(UWrite, '(",",A)',advance="no") 'ze10s'
         WRITE(UWrite, '(",",A)',advance="no") 'ze5mp'
         WRITE(UWrite, '(",",A)',advance="no") 'ze5mn'
         WRITE(UWrite, '(",",A)',advance="no") 'zepp'
         WRITE(UWrite, '(",",A)',advance="no") 'zepn'
         WRITE(UWrite, '(",",A)',advance="no") 'zesp'
         WRITE(UWrite, '(",",A)',advance="no") 'zesn'
         WRITE(UWrite, '(",",A)',advance="no") 'zetp'
         WRITE(UWrite, '(",",A)',advance="no") 'zetn'
         WRITE(UWrite, '(",",A)',advance="no") 'zd10s'
         WRITE(UWrite, '(",",A)',advance="no") 'zd5mp'
         WRITE(UWrite, '(",",A)',advance="no") 'zd5mn'
         WRITE(UWrite, '(",",A)',advance="no") 'zdpp'
         WRITE(UWrite, '(",",A)',advance="no") 'zdpn'
         WRITE(UWrite, '(",",A)',advance="no") 'zdsp'
         WRITE(UWrite, '(",",A)',advance="no") 'zdsn'
         WRITE(UWrite, '(",",A)',advance="no") 'zdtp'
         WRITE(UWrite, '(",",A)',advance="no") 'zdtn'
         WRITE(UWrite, *)
      ELSE
         UWrite = Abrir(NArcNom, 'OLD', 'APPEND', ULog)
      END IF

      IF (ISimul .EQ. 0) THEN
         NomSimul = 'MEDIA '
      ELSE
         WRITE(NomSimul,'("Sim", I3)') ISimul
      ENDIF

      DO IZona = 1, ParRes%NumZonas
         Zona = ParRes%Zonas(IZona:IZona)
         DO IBlo = 1, NBloques
            FT = FPhi(BloEta(IBlo))
            WRITE(UWrite, '(A,",",I4,",",A,27(",",F12.2))') &
     &           trim(NomSimul), IBlo, Zona,                &
     &           ParRes%DataBlo(ParRes%zr10s_col(IZona),IBlo), &
     &           ParRes%DataBlo(ParRes%zr5mp_col(IZona),IBlo), &
     &           ParRes%DataBlo(ParRes%zr5mn_col(IZona),IBlo), &
     &           ParRes%DataBlo(ParRes%zrpp_col(IZona),IBlo), &
     &           ParRes%DataBlo(ParRes%zrpn_col(IZona),IBlo), &
     &           ParRes%DataBlo(ParRes%zrsp_col(IZona),IBlo), &
     &           ParRes%DataBlo(ParRes%zrsn_col(IZona),IBlo), &
     &           ParRes%DataBlo(ParRes%zrtp_col(IZona),IBlo), &
     &           ParRes%DataBlo(ParRes%zrtn_col(IZona),IBlo), &
     &           ParRes%DataBlo(ParRes%ze10s_col(IZona),IBlo), &
     &           ParRes%DataBlo(ParRes%ze5mp_col(IZona),IBlo), &
     &           ParRes%DataBlo(ParRes%ze5mn_col(IZona),IBlo), &
     &           ParRes%DataBlo(ParRes%zepp_col(IZona),IBlo), &
     &           ParRes%DataBlo(ParRes%zepn_col(IZona),IBlo), &
     &           ParRes%DataBlo(ParRes%zesp_col(IZona),IBlo), &
     &           ParRes%DataBlo(ParRes%zesn_col(IZona),IBlo), &
     &           ParRes%DataBlo(ParRes%zetp_col(IZona),IBlo), &
     &           ParRes%DataBlo(ParRes%zetn_col(IZona),IBlo), &
     &           ParRes%DualBlo(ParRes%req10s_fil(IZona),IBlo)*FT, &
     &           ParRes%DualBlo(ParRes%req5mp_fil(IZona),IBlo)*FT, &
     &           ParRes%DualBlo(ParRes%req5mn_fil(IZona),IBlo)*FT, &
     &           ParRes%DualBlo(ParRes%reqpp_fil(IZona),IBlo)*FT, &
     &           ParRes%DualBlo(ParRes%reqpn_fil(IZona),IBlo)*FT, &
     &           ParRes%DualBlo(ParRes%reqsp_fil(IZona),IBlo)*FT, &
     &           ParRes%DualBlo(ParRes%reqsn_fil(IZona),IBlo)*FT, &
     &           ParRes%DualBlo(ParRes%reqtp_fil(IZona),IBlo)*FT, &
     &           ParRes%DualBlo(ParRes%reqtn_fil(IZona),IBlo)*FT
         ENDDO
      ENDDO


      CALL Cerrar(UWrite)

!
!     Centrales
!
      NArcNom = ParRes%NArcCens

      IF (ISimul .EQ. 1) THEN
         UWrite = Abrir(NArcNom, 'UNKNOWN', 'SEQUENTIAL', ULog)
         WRITE(UWrite, '(A)',advance="no") 'Hidro, Bloque, Zona, CenNom'
         WRITE(UWrite, '(", ",A)',advance="no") 'rpp'
         WRITE(UWrite, '(", ",A)',advance="no") 'rpn'
         WRITE(UWrite, '(", ",A)',advance="no") 'rsp'
         WRITE(UWrite, '(", ",A)',advance="no") 'rsn'
         WRITE(UWrite, '(", ",A)',advance="no") 'rtp'
         WRITE(UWrite, '(", ",A)',advance="no") 'rtn'
         WRITE(UWrite, '(", ",A)',advance="no") 'qrp'
         WRITE(UWrite, '(", ",A)',advance="no") 'qrn'
         WRITE(UWrite, *)
      ELSE
         UWrite = Abrir(NArcNom, 'OLD', 'APPEND', ULog)
      END IF

      DO IZona = 1, ParRes%NumZonas
         Zona = ParRes%Zonas(IZona:IZona)
         DO ICen = 1, ParRes%NumCen
            CNom = CenNom(ParRes%Cen(ICen))
            DO IBlo = 1, NBloques
               WRITE(UWrite, '(A,",",I4,",",A,",",A, 8(",",F12.2))')  &
     &              trim(NomSimul), IBlo, trim(Zona), trim(CNom),     &
     &              ParRes%DataBlo(ParRes%rppz_col(IZona,ICen),IBlo), &
     &              ParRes%DataBlo(ParRes%rpnz_col(IZona,ICen),IBlo), &
     &              ParRes%DataBlo(ParRes%rspz_col(IZona,ICen),IBlo), &
     &              ParRes%DataBlo(ParRes%rsnz_col(IZona,ICen),IBlo), &
     &              ParRes%DataBlo(ParRes%rtpz_col(IZona,ICen),IBlo), &
     &              ParRes%DataBlo(ParRes%rtnz_col(IZona,ICen),IBlo), &
     &              ParRes%DataBlo(ParRes%qrp_col(ICen),IBlo),        &
     &              ParRes%DataBlo(ParRes%qrn_col(ICen),IBlo)
            ENDDO
         ENDDO
      ENDDO
!--
! se elimina este IF False que estaba acá
!-- 
      CALL Cerrar(UWrite)

      RETURN
      END



!**********************************
!     Matriz FO y Limites
!**********************************
      SUBROUTINE GenPDResFO(IEta, IBind,                                &
     &     ParRes,ParLajaM,FConvLaja,                                   &
     &     BloDur, FPhi,                                                &
     &     COffset,                                                     &
     &     PDNCol, FO, LowBnd, UppBnd)

      USE PLP
      USE OSI
!     Variables Globales
!***********************
      INTEGER IEta
      INTEGER PDNCol
      INTEGER IBind
      INTEGER COffset
      DOUBLE PRECISION BloDur
      DOUBLE PRECISION FO(PDNCol)
      DOUBLE PRECISION FPhi
      DOUBLE PRECISION LowBnd(PDNCol)
      DOUBLE PRECISION UppBnd(PDNCol)

      TYPE(PAR_RESERVA) ParRes
      TYPE(PAR_LAJAM) ParLajaM
      INTEGER FConvLaja
!     Variables Locales
!**********************
      INTEGER ICen, ICol, IZona
      DOUBLE PRECISION bdfp
      LOGICAL SkipEta
      DOUBLE PRECISION DINFTY
      DINFTY = osi_getinfty()

      SkipEta = .False.
      IF (ParRes%EtaIni .gt. 0 .and. IEta .lt. ParRes%EtaIni) THEN
         SkipEta = .True.
      ENDIF
      IF (ParRes%EtaFin .gt. 0 .and. IEta .gt. ParRes%EtaFin) THEN
         SkipEta = .True.
      ENDIF

      FO(COffset + 1:COffset + ParRes%NumColBlo) = 0
      LowBnd(COffset + 1:COffset + ParRes%NumColBlo) = 0

      IF (SkipEta) THEN
         UppBnd(COffset + 1:COffset + ParRes%NumColBlo) = 0
         RETURN
      ENDIF

      UppBnd(COffset + 1:COffset + ParRes%NumColBlo) = DINFTY

      bdfp = BloDur/FPhi

      ICol = COffset

      DO IZona = 1, ParRes%NumZonas
         ICol = COffset + ParRes%zr10s_col(IZona)
         LowBnd(ICol) = -DINFTY

         ICol = COffset + ParRes%zr5mp_col(IZona)
         LowBnd(ICol) = -DINFTY

         ICol = COffset + ParRes%zr5mn_col(IZona)
         LowBnd(ICol) = -DINFTY

         ICol = COffset + ParRes%zrpp_col(IZona)
         LowBnd(ICol) = -DINFTY

         ICol = COffset + ParRes%zrpn_col(IZona)
         LowBnd(ICol) = -DINFTY

         ICol = COffset + ParRes%zrsp_col(IZona)
         LowBnd(ICol) = -DINFTY

         ICol = COffset + ParRes%zrsn_col(IZona)
         LowBnd(ICol) = -DINFTY

         ICol = COffset + ParRes%zrtp_col(IZona)
         LowBnd(ICol) = -DINFTY

         ICol = COffset + ParRes%zrtn_col(IZona)
         LowBnd(ICol) = -DINFTY
      ENDDO

      DO IZona = 1, ParRes%NumZonas
         ICol = COffset + ParRes%ze10s_col(IZona)
         FO(ICol) = ParRes%CE10s(IZona,IEta) * bdfp
         UppBnd(ICol) = ParRes%ME10s(IZona,IEta)

         ICol = COffset + ParRes%ze5mp_col(IZona)
         FO(ICol) = ParRes%CE5mp(IZona,IEta) * bdfp
         UppBnd(ICol) = ParRes%ME5mp(IZona,IEta)

         ICol = COffset + ParRes%ze5mn_col(IZona)
         FO(ICol) = ParRes%CE5mn(IZona,IEta) * bdfp
         UppBnd(ICol) = ParRes%ME5mn(IZona,IEta)

         ICol = COffset + ParRes%zepp_col(IZona)
         FO(ICol) = ParRes%CEpp(IZona,IEta) * bdfp
         UppBnd(ICol) = ParRes%MEpp(IZona,IEta)

         ICol = COffset + ParRes%zepn_col(IZona)
         FO(ICol) = ParRes%CEpn(IZona,IEta) * bdfp
         UppBnd(ICol) = ParRes%MEpn(IZona,IEta)

         ICol = COffset + ParRes%zesp_col(IZona)
         FO(ICol) = ParRes%CEsp(IZona,IEta) * bdfp
         UppBnd(ICol) = ParRes%MEsp(IZona,IEta)

         ICol = COffset + ParRes%zesn_col(IZona)
         FO(ICol) = ParRes%CEsn(IZona,IEta) * bdfp
         UppBnd(ICol) = ParRes%MEsn(IZona,IEta)

         ICol = COffset + ParRes%zetp_col(IZona)
         FO(ICol) = ParRes%CEtp(IZona,IEta) * bdfp
         UppBnd(ICol) = ParRes%MEtp(IZona,IEta)

         ICol = COffset + ParRes%zetn_col(IZona)
         FO(ICol) = ParRes%CEtn(IZona,IEta) * bdfp
         UppBnd(ICol) = ParRes%MEtn(IZona,IEta)
      ENDDO

! Se eliminan las restricciones de Max y Min para RP+, etc
! Se agregan los costos de entrega de reserva, el cual es el mismo independiente de la zona. (hasta ahora)

      DO IZona = 1, ParRes%NumZonas
         DO ICen = 1, ParRes%NumCen

            ICol = ICol + 1
            FO(ICol) = ParRes%CPpp(ICen,IEta) * bdfp
            LowBnd(ICol) = ParRes%MinPpp(ICen,IZona,IBind)
            UppBnd(ICol) = ParRes%MaxPpp(ICen,IZona,IBind)

            ICol = ICol + 1
            FO(ICol) = ParRes%CPpp(ICen,IEta) * bdfp
            LowBnd(ICol) = ParRes%MinPpn(ICen,IZona,IBind)
            UppBnd(ICol) = ParRes%MaxPpn(ICen,IZona,IBind)

            ICol = ICol + 1
            FO(ICol) = ParRes%CPpp(ICen,IEta) * bdfp
            LowBnd(ICol) = ParRes%MinPsp(ICen,IZona,IBind)
            UppBnd(ICol) = ParRes%MaxPsp(ICen,IZona,IBind)

            ICol = ICol + 1
            FO(ICol) = ParRes%CPpp(ICen,IEta) * bdfp
            LowBnd(ICol) = ParRes%MinPsn(ICen,IZona,IBind)
            UppBnd(ICol) = ParRes%MaxPsn(ICen,IZona,IBind)

            ICol = ICol + 1
            FO(ICol) = ParRes%CPpp(ICen,IEta) * bdfp
            LowBnd(ICol) = ParRes%MinPtp(ICen,IZona,IBind)
            UppBnd(ICol) = ParRes%MaxPtp(ICen,IZona,IBind)

            ICol = ICol + 1
            FO(ICol) = ParRes%CPpp(ICen,IEta) * bdfp
            LowBnd(ICol) = ParRes%MinPtn(ICen,IZona,IBind)
            UppBnd(ICol) = ParRes%MaxPtn(ICen,IZona,IBind)
         ENDDO
      ENDDO
      IF (FConvLaja .eq. 2) THEN
         ICol= COffset + ParRes%qrpr_col
         FO(ICol) = ParLajaM%CQVarEta(1, IEta) * bdfp
         
         ICol= COffset + ParRes%qrpe_col
         FO(ICol)= ParLajaM%CQVarEta(2, IEta) * bdfp
         
         ICol= COffset + ParRes%qrpm_col
         FO(ICol)= ParLajaM%CQVarEta(3, IEta) * bdfp
      ENDIF

      RETURN
      END

!***************************
!      Matriz FO y limites para variables de etapa (Conv Laja)
!***************************
      SUBROUTINE GenPDResEtaFO(ParRes,     &
     &     COffset, PDNCol , FO, LowBnd, UppBnd)
      USE PLP, ONLY : PAR_DIMS, PAR_RESERVA
      USE OSI

!     Variables Globales
!******************
      TYPE(PAR_RESERVA) ParRes



      INTEGER PDNCol
      INTEGER COffset 
      DOUBLE PRECISION FO(PDNCol)
      DOUBLE PRECISION LowBnd(PDNCol)
      DOUBLE PRECISION UppBnd(PDNCol)



      DOUBLE PRECISION DINFTY
      DINFTY= osi_getinfty()
      
      
!     Funcion Objetivo
!**********************
      FO(COffset + 1 : COffset + ParRes%NumColEta) = 0
!     Restricciones de tipo 'x < = ' y 'x >= '
!**********************
      UppBnd(COffset + 1 : COffset + ParRes%NumColEta) = DINFTY
      LowBnd(COffset + 1 : COffset + ParRes%NumColEta) = 0

      RETURN
      END

!********************************************
      SUBROUTINE GenPDResBloA(IEta, IBlo, IBind,                  &
     &     NCentral, CenInd, CenRen,                              &
     &     ParRes,bdursc,bdur,CenGHid,Dim,        &
     &     CenOffset, COffset, FOffset,FEmbOffset,FConvLaja,ParLajaM, &
     &     A, PDNCol, PDNFila, PDNombre, Sentido)
      USE PLP
      USE A_MATRIX

      TYPE(PAR_DIMS) Dim

!     Variables Globales
!******************
      INTEGER IEta, IBlo, IBind
      INTEGER CenOffset
      INTEGER COffset
      INTEGER FOffset
      TYPE(PAR_LAJAM) ParLajaM
      TYPE(PAR_RESERVA) ParRes

      INTEGER NCentral
      DOUBLE PRECISION CenRen(NCentral)
      INTEGER CenInd(NCentral)

      TYPE(AMatrix) A
      INTEGER PDNCol
      INTEGER PDNFila
      CHARACTER*24 PDNombre(PDNCol)
      CHARACTER*1 Sentido(PDNFila)
      INTEGER FConvLaja
!     locales

      INTEGER ICen
      CHARACTER*(128) Nombre, Nombrez
      INTEGER ICol, IZona
      INTEGER NumZonas

      INTEGER zr10s_col(ParRes%NumZonas)
      INTEGER zr5mp_col(ParRes%NumZonas)
      INTEGER zrpp_col(ParRes%NumZonas)
      INTEGER zr5mn_col(ParRes%NumZonas)
      INTEGER zrpn_col(ParRes%NumZonas)
      INTEGER zrsp_col(ParRes%NumZonas)
      INTEGER zrsn_col(ParRes%NumZonas)
      INTEGER zrtp_col(ParRes%NumZonas)
      INTEGER zrtn_col(ParRes%NumZonas)
      INTEGER ze10s_col(ParRes%NumZonas)
      INTEGER ze5mp_col(ParRes%NumZonas)
      INTEGER zepp_col(ParRes%NumZonas)
      INTEGER ze5mn_col(ParRes%NumZonas)
      INTEGER zepn_col(ParRes%NumZonas)
      INTEGER zesp_col(ParRes%NumZonas)
      INTEGER zesn_col(ParRes%NumZonas)
      INTEGER zetp_col(ParRes%NumZonas)
      INTEGER zetn_col(ParRes%NumZonas)


      INTEGER qrp_col(ParRes%NumCen)
      INTEGER qrn_col(ParRes%NumCen)
      INTEGER qrpr_col,qrpe_col,qrpm_col
      INTEGER qrnr_col,qrne_col,qrnm_col
      INTEGER rppz_col(ParRes%NumZonas, ParRes%NumCen)
      INTEGER rpnz_col(ParRes%NumZonas, ParRes%NumCen)
      INTEGER rspz_col(ParRes%NumZonas, ParRes%NumCen)
      INTEGER rsnz_col(ParRes%NumZonas, ParRes%NumCen)
      INTEGER rtpz_col(ParRes%NumZonas, ParRes%NumCen)
      INTEGER rtnz_col(ParRes%NumZonas, ParRes%NumCen)

      INTEGER req10s_fil(ParRes%NumZonas)
      INTEGER req5mp_fil(ParRes%NumZonas)
      INTEGER req5mn_fil(ParRes%NumZonas)
      INTEGER reqpp_fil(ParRes%NumZonas)
      INTEGER reqpn_fil(ParRes%NumZonas)
      INTEGER reqsp_fil(ParRes%NumZonas)
      INTEGER reqsn_fil(ParRes%NumZonas)
      INTEGER reqtp_fil(ParRes%NumZonas)
      INTEGER reqtn_fil(ParRes%NumZonas)

      INTEGER rev10s_fil(ParRes%NumZonas)
      INTEGER rev5mp_fil(ParRes%NumZonas)
      INTEGER rev5mn_fil(ParRes%NumZonas)
      INTEGER revpp_fil(ParRes%NumZonas)
      INTEGER revpn_fil(ParRes%NumZonas)
      INTEGER revsp_fil(ParRes%NumZonas)
      INTEGER revsn_fil(ParRes%NumZonas)
      INTEGER revtp_fil(ParRes%NumZonas)
      INTEGER revtn_fil(ParRes%NumZonas)

      INTEGER CInd, IFil, ICentral, Idx
      DOUBLE PRECISION FE, FC
      DOUBLE PRECISION AtrC, AtrG, AtrZ
      INTEGER NumCen
      INTEGER CenGHid(Dim%Cen, 2)
      DOUBLE PRECISION bdursc,bdur
      DOUBLE PRECISION FactorSubida(1:ParRes%NumServicios)
      INTEGER FOffseti,FEmbOffset
      


      INTEGER gcp_fil(ParRes%NumZonas,ParRes%NumCen)
      INTEGER gcn_fil(ParRes%NumZonas,ParRes%NumCen)

      INTEGER cbajada_fil(ParRes%NumCen)
      INTEGER rpdef_fil(ParRes%NumCen)
      INTEGER laja_qrp_fil
      INTEGER rndef_fil(ParRes%NumCen)
      INTEGER laja_qrn_fil

      INTEGER IGrupo
      LOGICAL SkipEta
      DOUBLE PRECISION One
      DOUBLE PRECISION RC
!***************
!     Reservas
!***************
      ICol = 0

      NumZonas = ParRes%NumZonas

      SkipEta = .False.
      IF (ParRes%EtaIni .gt. 0 .and. IEta .lt. ParRes%EtaIni) THEN
         SkipEta = .True.
      ENDIF
      IF (ParRes%EtaFin .gt. 0 .and. IEta .gt. ParRes%EtaFin) THEN
         SkipEta = .True.
      ENDIF

      One = 1.0d0
      IF (SkipEta) THEN
         One = 0.0d0
      ENDIF


!     variables de requerimientos y escacez
!     NumZonas*9*2
      DO IZona = 1, NumZonas
         Nombrez = '_' // trim(itoa(IZona)) // '_' // trim(itoa(IBlo))
         ICol = ICol + 1
         PDNombre(COffset + ICol) = 'zr10s' // Nombrez
         zr10s_col(IZona) = COffset + ICol
         ICol = ICol + 1
         PDNombre(COffset + ICol) = 'zr5mp' // Nombrez
         zr5mp_col(IZona) = COffset + ICol
         ICol = ICol + 1
         PDNombre(COffset + ICol) = 'zr5mn' // Nombrez
         zr5mn_col(IZona) = COffset + ICol
         ICol = ICol + 1
         PDNombre(COffset + ICol) = 'zrpp' // Nombrez
         zrpp_col(IZona) = COffset + ICol
         ICol = ICol + 1
         PDNombre(COffset + ICol) = 'zrpn' // Nombrez
         zrpn_col(IZona) = COffset + ICol
         ICol = ICol + 1
         PDNombre(COffset + ICol) = 'zrsp' // Nombrez
         zrsp_col(IZona) = COffset + ICol
         ICol = ICol + 1
         PDNombre(COffset + ICol) = 'zrsn' // Nombrez
         zrsn_col(IZona) = COffset + ICol
         ICol = ICol + 1
         PDNombre(COffset + ICol) = 'zrtp' // Nombrez
         zrtp_col(IZona) = COffset + ICol
         ICol = ICol + 1
         PDNombre(COffset + ICol) = 'zrtn' // Nombrez
         zrtn_col(IZona) = COffset + ICol
         ICol = ICol + 1
         PDNombre(COffset + ICol) = 'ze10s' // Nombrez
         ze10s_col(IZona) = COffset + ICol
         ICol = ICol + 1
         PDNombre(COffset + ICol) = 'ze5mp' // Nombrez
         ze5mp_col(IZona) = COffset + ICol
         ICol = ICol + 1
         PDNombre(COffset + ICol) = 'ze5mn' // Nombrez
         ze5mn_col(IZona) = COffset + ICol
         ICol = ICol + 1
         PDNombre(COffset + ICol) = 'zepp' // Nombrez
         zepp_col(IZona) = COffset + ICol
         ICol = ICol + 1
         PDNombre(COffset + ICol) = 'zepn' // Nombrez
         zepn_col(IZona) = COffset + ICol
         ICol = ICol + 1
         PDNombre(COffset + ICol) = 'zesp' // Nombrez
         zesp_col(IZona) = COffset + ICol
         ICol = ICol + 1
         PDNombre(COffset + ICol) = 'zesn' // Nombrez
         zesn_col(IZona) = COffset + ICol
         ICol = ICol + 1
         PDNombre(COffset + ICol) = 'zetp' // Nombrez
         zetp_col(IZona) = COffset + ICol
         ICol = ICol + 1
         PDNombre(COffset + ICol) = 'zetn' // Nombrez
         zetn_col(IZona) = COffset + ICol
      END DO

!     variables de reserva total por central
!     0, se eliminaron
      NumCen = ParRes%NumCen

!     variables de reserva por central y por zona
!     NumZonas*NumCen*3*2
      DO IZona = 1, NumZonas
         Nombrez = '_' // trim(itoa(IZona)) // '_' // trim(itoa(IBlo))
         DO ICen = 1, NumCen
            CInd = CenInd(ParRes%Cen(ICen))
            Nombre = trim(itoa(CInd)) // trim(Nombrez)

            ICol = ICol + 1
            PDNombre(COffset + ICol) = 'rppz' // Nombre
            rppz_col(IZona,ICen) = COffset + ICol
            ICol = ICol + 1
            PDNombre(COffset + ICol) = 'rpnz' // Nombre

            rpnz_col(IZona,ICen) = COffset + ICol

            ICol = ICol + 1
            PDNombre(COffset + ICol) = 'rspz' // Nombre

            rspz_col(IZona,ICen) = COffset + ICol
            ICol = ICol + 1
            PDNombre(COffset + ICol) = 'rsnz' // Nombre

            rsnz_col(IZona,ICen) = COffset + ICol

            ICol = ICol + 1
            PDNombre(COffset + ICol) = 'rtpz' // Nombre

            rtpz_col(IZona,ICen) = COffset + ICol
            ICol = ICol + 1
            PDNombre(COffset + ICol) = 'rtnz' // Nombre

            rtnz_col(IZona,ICen) = COffset + ICol
         ENDDO
      ENDDO
      ! no es óptimo que utilice NumCen espacios sólo cuando uso los embalses y series
      ! pero es útil ya que al momento de generar el csv y de manejar las columnas
      ! no las hago condicionales según la cantidad de Emb y Ser que den reserva
      DO ICen=1, NumCen 
         CInd = CenInd(ParRes%Cen(ICen))
         ICol = ICol + 1
         Nombre =  trim(itoa(CInd)) // '_'  // trim(itoa(IBlo))
         PDNombre(COffset + ICol) = 'qrp' // Nombre
         qrp_col(ICen) = COffset + ICol

         ICol = ICol + 1
         Nombre =  trim(itoa(CInd)) // '_'  // trim(itoa(IBlo))
         PDNombre(COffset + ICol) = 'qrn' // Nombre
         qrn_col(ICen) = COffset + ICol
      ENDDO

      IF (FConvLaja .eq. 2) THEN 
         ICol=ICol + 1
         Nombre= trim(itoa(IBlo))
         ! Caudal de reserva de subida riego
         PDNombre(COffset + ICol) = 'l_qrpr' // Nombre
         qrpr_col=COffset + ICol
         ! Caudal de reserva de subida derecho eléctrico
         ICol = ICol + 1
         PDNombre(COffset + ICol) = 'l_qrpe' // Nombre
         qrpe_col=COffset + ICol
         ! Caudal de reserva de subida derecho mixto
         ICol = ICol + 1
         PDNombre(COffset + ICol) = 'l_qrpm' // Nombre
         qrpm_col=COffset + ICol

         ! Caudal de reserva de bajada derecho mixto
         ICol = ICol + 1
         PDNombre(COffset + ICol) = 'l_qrnr' // Nombre
         qrnr_col=COffset + ICol
         ! Caudal de reserva de bajada derecho mixto
         ICol = ICol + 1
         PDNombre(COffset + ICol) = 'l_qrne' // Nombre
         qrne_col=COffset + ICol
         ! Caudal de reserva de bajada derecho mixto
         ICol = ICol + 1
         PDNombre(COffset + ICol) = 'l_qrnm' // Nombre
         qrnm_col=COffset + ICol
      ENDIF
!***************
!     restricciones de requerimiento y escacez por zonas
!     EX_(z,b) ≥ REQX_(z,b)
!     NumZonas*9
      IFil = 0
      Do IZona = 1, NumZonas
         IFil = IFil + 1
         req10s_fil(IZona) = IFil + FOffset
         CALL Am_set(A, ze10s_col(IZona), req10s_fil(IZona), One)
         CALL Am_set(A, zr10s_col(IZona), req10s_fil(IZona), -One)
         IFil = IFil + 1
         req5mp_fil(IZona) = IFil + FOffset
         CALL Am_set(A, ze5mp_col(IZona), req5mp_fil(IZona), One)
         CALL Am_set(A, zr5mp_col(IZona), req5mp_fil(IZona), -One)
         IFil = IFil + 1
         req5mn_fil(IZona) = IFil + FOffset
         CALL Am_set(A, ze5mn_col(IZona), req5mn_fil(IZona), One)
         CALL Am_set(A, zr5mn_col(IZona), req5mn_fil(IZona), -One)
         IFil = IFil + 1
         reqpp_fil(IZona) = IFil + FOffset
         CALL Am_set(A, zepp_col(IZona), reqpp_fil(IZona), One)
         CALL Am_set(A, zrpp_col(IZona), reqpp_fil(IZona), -One)
         IFil = IFil + 1
         reqpn_fil(IZona) = IFil + FOffset
         CALL Am_set(A, zepn_col(IZona), reqpn_fil(IZona), One)
         CALL Am_set(A, zrpn_col(IZona), reqpn_fil(IZona), -One)
         IFil = IFil + 1
         reqsp_fil(IZona) = IFil + FOffset
         CALL Am_set(A, zesp_col(IZona), reqsp_fil(IZona), One)
         CALL Am_set(A, zrsp_col(IZona), reqsp_fil(IZona), -One)
         IFil = IFil + 1
         reqsn_fil(IZona) = IFil + FOffset
         CALL Am_set(A, zesn_col(IZona), reqsn_fil(IZona), One)
         CALL Am_set(A, zrsn_col(IZona), reqsn_fil(IZona), -One)
         IFil = IFil + 1
         reqtp_fil(IZona) = IFil + FOffset
         CALL Am_set(A, zetp_col(IZona), reqtp_fil(IZona), One)
         CALL Am_set(A, zrtp_col(IZona), reqtp_fil(IZona), -One)
         IFil = IFil + 1
         reqtn_fil(IZona) = IFil + FOffset
         CALL Am_set(A, zetn_col(IZona), reqtn_fil(IZona), One)
         CALL Am_set(A, zrtn_col(IZona), reqtn_fil(IZona), -One)


         Sentido(req10s_fil(IZona)) = 'G'
         Sentido(req5mp_fil(IZona)) = 'G'
         Sentido(req5mn_fil(IZona)) = 'G'
         Sentido(reqpp_fil(IZona)) = 'G'
         Sentido(reqpn_fil(IZona)) = 'G'
         Sentido(reqsp_fil(IZona)) = 'G'
         Sentido(reqsn_fil(IZona)) = 'G'
         Sentido(reqtp_fil(IZona)) = 'G'
         Sentido(reqtn_fil(IZona)) = 'G'
      ENDDO

!     agrega la sumatoria de las centrales a la restriccion previa para formar
!     \sum RX_(c,z,b) + EX_(z,b) ≥ REQX_(z,b)
      DO IZona = 1, NumZonas
         DO ICen = 1, ParRes%NumCen
            IF (INDEX(ParRes%CenZonas(ICen), &
     &           ParRes%ZonaId(IZona)) .eq. 0) THEN
               CYCLE
            ENDIF

            FE = ParRes%FE10s(ICen, IZona) * One
            CALL Am_set(A, rppz_col(IZona,ICen), req10s_fil(IZona), FE)
            FE = ParRes%FE5mp(ICen, IZona) * One
            CALL Am_set(A, rppz_col(IZona,ICen), req5mp_fil(IZona), FE)
            FE = ParRes%FE5mn(ICen, IZona) * One
            CALL Am_set(A, rpnz_col(IZona,ICen), req5mn_fil(IZona), FE)
            FE = ParRes%FEp(ICen, IZona) * One
            CALL Am_set(A, rppz_col(IZona,ICen), reqpp_fil(IZona), FE)
            FE = ParRes%FEn(ICen, IZona) * One
            CALL Am_set(A, rpnz_col(IZona,ICen), reqpn_fil(IZona), FE)

            FE = One
            CALL Am_set(A, rspz_col(IZona,ICen), reqsp_fil(IZona), FE)
            CALL Am_set(A, rsnz_col(IZona,ICen), reqsn_fil(IZona), FE)
            CALL Am_set(A, rtpz_col(IZona,ICen), reqtp_fil(IZona), FE)
            CALL Am_set(A, rtnz_col(IZona,ICen), reqtn_fil(IZona), FE)
         ENDDO
      ENDDO

!     restricciones de Generacion
!     pmax - RC*gc >= RPp_z + RSp_z + RTp_z
!     RC*gc - pmin >= RPn_z + RSn_z + RTn_z
!     NumCen*2*NumZonas
      DO IZona=1, NumZonas
         DO ICen = 1, NumCen
            ICentral = ParRes%Cen(ICen)
            CInd = CenOffset + ICentral

            IFil = IFil + 1
            gcp_fil(IZona,ICen) = IFil + FOffset

            RC = CenRen(ICentral)*One
            CALL Am_set(A, CInd, gcp_fil(IZona,ICen), -RC)
            CALL Am_set(A, rppz_col(IZona,ICen), gcp_fil(IZona,ICen), -One)
            CALL Am_set(A, rspz_col(IZona,ICen), gcp_fil(IZona,ICen), -One)
            CALL Am_set(A, rtpz_col(IZona,ICen), gcp_fil(IZona,ICen), -One)
            Sentido(gcp_fil(IZona,ICen)) = 'G'

            IFil = IFil + 1
            gcn_fil(IZona,ICen) = IFil + FOffset
            CALL Am_set(A, CInd, gcn_fil(IZona,ICen), RC)
            CALL Am_set(A, rpnz_col(IZona,ICen), gcn_fil(IZona,ICen), -One)
            CALL Am_set(A, rsnz_col(IZona,ICen), gcn_fil(IZona,ICen), -One)
            CALL Am_set(A, rtnz_col(IZona,ICen), gcn_fil(IZona,ICen), -One)
            Sentido(gcn_fil(IZona,ICen)) = 'G'

            DO Idx = 1, ParRes%NumRend
               IF (ICentral .ne. ParRes%RendCen(Idx)) THEN
                  CYCLE
               ENDIF
               ! acá falta modificar el rendimiento en plp-rendim.f
               ! listo
               ParRes%RendColInd(Idx, IBInd, IZona) = CInd
               ParRes%RendFilnInd(Idx, IBInd, IZona) = gcn_fil(IZona,ICen)
               ParRes%RendFilpInd(Idx, IBInd, IZona) = gcp_fil(IZona,ICen)
            ENDDO
         ENDDO
      ENDDO


!  Restricción:  RC_c P_c^b - \sum_{c,z} R_{-,c,z}^b >= 0 \quad \forall c  \in G \forall b \in BL
! Restricción para que efectivamente el margen de bajada se comparta entre las zonas, y si se configura la inercia y CF, entonces
! se asegure que la central este sobre el mínimo técnico para poder dar CF (sabiendo que da inercia). 
!     Filas agregadas= NumCen
!
      DO ICen=1, NumCen
         IFil=IFil+1
         cbajada_fil(ICen)=IFil + FOffset

         ICentral = ParRes%Cen(ICen)
         CInd = CenOffset + ICentral
         RC = CenRen(ICentral)*One
         DO IZona=1,NumZonas
            CALL Am_set(A, CInd, cbajada_fil(ICen), RC)
            CALL Am_set(A, rpnz_col(IZona,ICen), cbajada_fil(ICen), -One)
            IF (.not. ParRes%ZonaInercia(IZona)) THEN
               CALL Am_set(A, rsnz_col(IZona,ICen), cbajada_fil(ICen), -One)
               CALL Am_set(A, rtnz_col(IZona,ICen), cbajada_fil(ICen), -One)
            ENDIF
         ENDDO
         Sentido(cbajada_fil(ICen)) = 'G'


         DO Idx = 1, ParRes%NumRend
               IF (ICentral .ne. ParRes%RendCen(Idx)) THEN
                  CYCLE
               ENDIF
               ! acá falta modificar el rendimiento en plp-rendim.f
               ! listos
               ParRes%RendColCBajInd(Idx, IBInd) = CInd
               ParRes%RendFilCBajInd(Idx, IBInd) = cbajada_fil(ICen)
         ENDDO
      ENDDO

!     requerimiento Variables
!     REQX_(z,b) = R0_(z,b) + ∑_c (AtrG +  AtrZ*AtrC)∙g_(c,z,b)
!     NumZonas*9
      DO IZona = 1, NumZonas
         IFil = IFil + 1
         rev10s_fil(IZona) = IFil + FOffset
         CALL Am_set(A, zr10s_col(IZona), rev10s_fil(IZona), One)
         IFil = IFil + 1
         rev5mp_fil(IZona) = IFil + FOffset
         CALL Am_set(A, zr5mp_col(IZona), rev5mp_fil(IZona), One)
         IFil = IFil + 1
         rev5mn_fil(IZona) = IFil + FOffset
         CALL Am_set(A, zr5mn_col(IZona), rev5mn_fil(IZona), One)
         IFil = IFil + 1
         revpp_fil(IZona) = IFil + FOffset
         CALL Am_set(A, zrpp_col(IZona), revpp_fil(IZona), One)
         IFil = IFil + 1
         revpn_fil(IZona) = IFil + FOffset
         CALL Am_set(A, zrpn_col(IZona), revpn_fil(IZona), One)
         IFil = IFil + 1
         revsp_fil(IZona) = IFil + FOffset
         CALL Am_set(A, zrsp_col(IZona), revsp_fil(IZona), One)
         IFil = IFil + 1
         revsn_fil(IZona) = IFil + FOffset
         CALL Am_set(A, zrsn_col(IZona), revsn_fil(IZona), One)
         IFil = IFil + 1
         revtp_fil(IZona) = IFil + FOffset
         CALL Am_set(A, zrtp_col(IZona), revtp_fil(IZona), One)
         IFil = IFil + 1
         revtn_fil(IZona) = IFil + FOffset
         CALL Am_set(A, zrtn_col(IZona), revtn_fil(IZona), One)

         Sentido(rev10s_fil(IZona)) = 'E'
         Sentido(rev5mp_fil(IZona)) = 'E'
         Sentido(rev5mn_fil(IZona)) = 'E'
         Sentido(revpp_fil(IZona)) = 'E'
         Sentido(revpn_fil(IZona)) = 'E'
         Sentido(revsp_fil(IZona)) = 'E'
         Sentido(revsn_fil(IZona)) = 'E'
         Sentido(revtp_fil(IZona)) = 'E'
         Sentido(revtn_fil(IZona)) = 'E'
      ENDDO


! Se agrega el porcentaje de subida como consumo de agua en los Embalses
! para esto se modifica la ecuación de balance de aguas, con la siguiente restricción 
! vf - \sum_{s,z} R_{s,z}^b 
! necesito el indice de fila de la restriccion = FOffset= NBarra
! y sumarle igual que arriba
! 
! Total Subida
!     Numcen restricciones y variables

      FOffseti=FEmbOffset
      DO ICen=1,NumCen
         ICentral = ParRes%Cen(ICen)
         RC=  One*CenRen(ICentral)
      ! Balance de aguas, el agua que uso, debe ir a algun lado abajo en la serie
         IFil=IFil + 1
         rpdef_fil(ICen)=IFil+ FOffset
         CALL Am_set(A, qrp_col(ICen), rpdef_fil(ICen), RC )
         IFil=IFil + 1
         rndef_fil(ICen)=IFil+ FOffset
         CALL Am_set(A, qrn_col(ICen), rndef_fil(ICen), RC )

         IF (ICentral .LE. (Dim%Emb + Dim%Ser)) THEN
            ! se define la var auxiliar
            CALL Am_set(A, qrp_col(ICen), FOffseti+ICentral, One*bdursc)
            CALL Am_set(A, qrn_col(ICen), FOffseti+ICentral, -One*bdursc)
            DO Idx=1, (Dim%Emb + Dim%Ser)
               IF (                                                     &
         &         (CenGHid(ICentral, 1) .EQ. Idx) .OR.               &
         &         (CenGHid(ICentral, 2) .EQ. Idx)                    &
         &         ) THEN
                  CALL Am_set(A, qrp_col(ICen), FOffseti+Idx, -One*bdursc)
                  CALL Am_set(A, qrn_col(ICen), FOffseti+Idx, One*bdursc)
               ENDIF
            ENDDO
            DO Idx = 1, ParRes%NumRend
                  IF (ICentral .ne. ParRes%RendCen(Idx)) THEN
                     CYCLE
                  ENDIF
                  ! acá falta modificar el rendimiento en plp-rendim.f
                  ! listos
                  ParRes%RendColSubInd(Idx, IBInd) = qrp_col(ICen)
                  ParRes%RendFilSubInd(Idx, IBInd) = rpdef_fil(ICen)

                  ParRes%RendColBajInd(Idx, IBInd) = qrn_col(ICen)
                  ParRes%RendFilBajInd(Idx, IBInd) = rndef_fil(ICen)
            ENDDO
         ENDIF
      ENDDO
      
      ! Cambiar eventualmente a que sean 3 Factores de Subida según CF
      DO IZona=1, NumZonas
         FactorSubida(1:ParRes%NumServicios)=ParRes%FactorSubida(IZona,1:ParRes%NumServicios)
         DO ICen=1, NumCen
               CALL Am_set(A, rppz_col(IZona,ICen), rpdef_fil(ICen), -(One*FactorSubida(1)))
               CALL Am_set(A, rtpz_col(IZona,ICen), rpdef_fil(ICen), -(One*FactorSubida(2)))
               CALL Am_set(A, rspz_col(IZona,ICen), rpdef_fil(ICen), -(One*FactorSubida(3)))

               CALL Am_set(A, rpnz_col(IZona,ICen), rndef_fil(ICen), -(One*FactorSubida(4)))
               CALL Am_set(A, rtnz_col(IZona,ICen), rndef_fil(ICen), -(One*FactorSubida(5)))
               CALL Am_set(A, rsnz_col(IZona,ICen), rndef_fil(ICen), -(One*FactorSubida(6)))
         ENDDO
      ENDDO

      DO IZona = 1, NumZonas
         DO ICen = 1, NumCen
            IF (INDEX(ParRes%CenZonas(ICen), &
     &           ParRes%ZonaId(IZona)) .eq. 0) THEN
               CYCLE
            ENDIF

            CInd = CenOffset + ParRes%Cen(ICen)
            ICentral = ParRes%Cen(ICen)
            IGrupo = ParRes%CenGrupo(ICen)

            AtrC = ParRes%AtrCen(ICen, IZona)

            AtrZ = ParRes%AtrZ10s(IZona)
            AtrG = ParRes%AtrG10s(IGrupo, IZona)
            FC = (AtrG + AtrZ*AtrC) * CenRen(ICentral) * One
            CALL Am_set(A, CInd, rev10s_fil(IZona),  -FC)

            AtrZ = ParRes%AtrZ5mp(IZona)
            AtrG = ParRes%AtrG5mp(IGrupo, IZona)
            FC = (AtrG + AtrZ*AtrC) * CenRen(ICentral) * One
            CALL Am_set(A, CInd, rev5mp_fil(IZona),  -FC)

            AtrZ = ParRes%AtrZ5mn(IZona)
            AtrG = ParRes%AtrG5mn(IGrupo, IZona)
            FC = (AtrG + AtrZ*AtrC) * CenRen(ICentral) * One
            CALL Am_set(A, CInd, rev5mn_fil(IZona),  -FC)

            AtrZ = ParRes%AtrZpp(IZona)
            AtrG = ParRes%AtrGpp(IGrupo, IZona)
            FC = (AtrG + AtrZ*AtrC) * CenRen(ICentral) * One
            CALL Am_set(A, CInd, revpp_fil(IZona),   -FC)

            AtrZ = ParRes%AtrZpn(IZona)
            AtrG = ParRes%AtrGpn(IGrupo, IZona)
            FC = (AtrG + AtrZ*AtrC) * CenRen(ICentral) * One
            CALL Am_set(A, CInd, revpn_fil(IZona),   -FC)

            AtrZ = ParRes%AtrZsp(IZona)
            AtrG = ParRes%AtrGsp(IGrupo, IZona)
            FC = (AtrG + AtrZ*AtrC) * CenRen(ICentral) * One
            CALL Am_set(A, CInd, revsp_fil(IZona),   -FC)

            AtrZ = ParRes%AtrZsn(IZona)
            AtrG = ParRes%AtrGsn(IGrupo, IZona)
            FC = (AtrG + AtrZ*AtrC) * CenRen(ICentral) * One
            CALL Am_set(A, CInd, revsn_fil(IZona),   -FC)

            AtrZ = ParRes%AtrZtp(IZona)
            AtrG = ParRes%AtrGtp(IGrupo, IZona)
            FC = (AtrG + AtrZ*AtrC) * CenRen(ICentral) * One
            CALL Am_set(A, CInd, revtp_fil(IZona),   -FC)

            AtrZ = ParRes%AtrZtn(IZona)
            AtrG = ParRes%AtrGtn(IGrupo, IZona)
            FC = (AtrG + AtrZ*AtrC) * CenRen(ICentral) * One
            CALL Am_set(A, CInd, revtn_fil(IZona),   -FC)
         ENDDO
      ENDDO
      ! se agregan 2 filas mas por el convenio del laja
      IF (FConvLaja .eq. 2) THEN
         IFil= IFil + 1
         laja_qrp_fil= IFil + FOffset
         IFil= IFil + 1
         laja_qrn_fil= IFil + FOffset
         CALL Am_set(A, qrpr_col, laja_qrp_fil, One)
         CALL Am_set(A, qrpe_col, laja_qrp_fil, One)
         CALL Am_set(A, qrpm_col, laja_qrp_fil, One)

         CALL Am_set(A, qrnr_col, laja_qrn_fil, One)
         CALL Am_set(A, qrne_col, laja_qrn_fil, One)
         CALL Am_set(A, qrnm_col, laja_qrn_fil, One)
         DO ICen=1, ParRes%NumCen
            ICentral=ParRes%Cen(ICen)
            IF (ICentral .eq. ParLajaM%IEmbLaja) THEN
            ! se debe ingresar así porque qrp_col esta indexado con las ParRes%NumCen
               CALL Am_set(A, qrp_col(ICen), laja_qrp_fil, -One)
               CALL Am_set(A, qrn_col(ICen), laja_qrn_fil, -One)
            ENDIF
         ENDDO
         !Agregar una fila al ParRes 
         ! y modificar el genmatpd.f
         CALL Am_set(A, qrpr_col, ParRes%FilIndEta(IEta,ParRes%TQRPR), -One*bdur)
         CALL Am_set(A, qrpe_col, ParRes%FilIndEta(IEta,ParRes%TQRPE), -One*bdur)
         CALL Am_set(A, qrpm_col, ParRes%FilIndEta(IEta,ParRes%TQRPM), -One*bdur)

         CALL Am_set(A, qrnr_col, ParRes%FilIndEta(IEta,ParRes%TQRNR), -One*bdur)
         CALL Am_set(A, qrne_col, ParRes%FilIndEta(IEta,ParRes%TQRNE), -One*bdur)
         CALL Am_set(A, qrnm_col, ParRes%FilIndEta(IEta,ParRes%TQRNM), -One*bdur)
      ENDIF
!     restricciones de reserva total por central
!     RX_(c,b) ≥ RXZ_(c,z,b) ELIMINADOO
!     NumZonas*NumCen*3*2


!     arreglos de columnas y filas relevantes

      ParRes%zr10s_col(1:NumZonas) = zr10s_col(1:NumZonas) - COffset
      ParRes%zr5mp_col(1:NumZonas) = zr5mp_col(1:NumZonas) - COffset
      ParRes%zr5mn_col(1:NumZonas) = zr5mn_col(1:NumZonas) - COffset
      ParRes%zrpp_col(1:NumZonas) = zrpp_col(1:NumZonas) - COffset
      ParRes%zrpn_col(1:NumZonas) = zrpn_col(1:NumZonas) - COffset
      ParRes%zrsp_col(1:NumZonas) = zrsp_col(1:NumZonas) - COffset
      ParRes%zrsn_col(1:NumZonas) = zrsn_col(1:NumZonas) - COffset
      ParRes%zrtp_col(1:NumZonas) = zrtp_col(1:NumZonas) - COffset
      ParRes%zrtn_col(1:NumZonas) = zrtn_col(1:NumZonas) - COffset

      ParRes%ze10s_col(1:NumZonas) = ze10s_col(1:NumZonas) - COffset
      ParRes%ze5mp_col(1:NumZonas) = ze5mp_col(1:NumZonas) - COffset
      ParRes%ze5mn_col(1:NumZonas) = ze5mn_col(1:NumZonas) - COffset
      ParRes%zepp_col(1:NumZonas) = zepp_col(1:NumZonas) - COffset
      ParRes%zepn_col(1:NumZonas) = zepn_col(1:NumZonas) - COffset
      ParRes%zesp_col(1:NumZonas) = zesp_col(1:NumZonas) - COffset
      ParRes%zesn_col(1:NumZonas) = zesn_col(1:NumZonas) - COffset
      ParRes%zetp_col(1:NumZonas) = zetp_col(1:NumZonas) - COffset
      ParRes%zetn_col(1:NumZonas) = zetn_col(1:NumZonas) - COffset

      ParRes%qrp_col(1:NumCen) = qrp_col(1:NumCen) - COffset
      ParRes%qrn_col(1:NumCen) = qrn_col(1:NumCen) - COffset
      IF (FConvLaja .eq. 2) THEN
         ParRes%qrpr_col= qrpr_col - COffset
         ParRes%qrpe_col= qrpe_col - COffset
         ParRes%qrpm_col= qrpm_col - COffset
         ParRes%qrnr_col= qrnr_col - COffset
         ParRes%qrne_col= qrne_col - COffset
         ParRes%qrnm_col= qrnm_col - COffset
      ENDIF
      ParRes%rppz_col(1:NumZonas,1:NumCen) = rppz_col(1:NumZonas,1:NumCen) - COffset
      ParRes%rpnz_col(1:NumZonas,1:NumCen) = rpnz_col(1:NumZonas,1:NumCen) - COffset
      ParRes%rspz_col(1:NumZonas,1:NumCen) = rspz_col(1:NumZonas,1:NumCen) - COffset
      ParRes%rsnz_col(1:NumZonas,1:NumCen) = rsnz_col(1:NumZonas,1:NumCen) - COffset
      ParRes%rtpz_col(1:NumZonas,1:NumCen) = rtpz_col(1:NumZonas,1:NumCen) - COffset
      ParRes%rtnz_col(1:NumZonas,1:NumCen) = rtnz_col(1:NumZonas,1:NumCen) - COffset

      ParRes%req10s_fil(1:NumZonas) = req10s_fil(1:NumZonas) - FOffset
      ParRes%req5mp_fil(1:NumZonas) = req5mp_fil(1:NumZonas) - FOffset
      ParRes%req5mn_fil(1:NumZonas) = req5mn_fil(1:NumZonas) - FOffset
      ParRes%reqpp_fil(1:NumZonas) = reqpp_fil(1:NumZonas) - FOffset
      ParRes%reqpn_fil(1:NumZonas) = reqpn_fil(1:NumZonas) - FOffset
      ParRes%reqsp_fil(1:NumZonas) = reqsp_fil(1:NumZonas) - FOffset
      ParRes%reqsn_fil(1:NumZonas) = reqsn_fil(1:NumZonas) - FOffset
      ParRes%reqtp_fil(1:NumZonas) = reqtp_fil(1:NumZonas) - FOffset
      ParRes%reqtn_fil(1:NumZonas) = reqtn_fil(1:NumZonas) - FOffset

!***************
!     Offsets Finales
!***************
      COffset = COffset + ICol
      FOffset = FOffset + IFil

      RETURN
      END


!************************
!     Restricciones de matriz A por etapa
!************************
      SUBROUTINE GenPDResEtaA(IEta, ParLajaM, ParRes,                      &
     &        COffset,PDNCol, FOffset, FOffset_LAJA,                          &
     &        A, PDNombre)
      USE PLP
      USE A_MATRIX

      TYPE(PAR_RESERVA) ParRes

!     Variables Globales
!******************
      TYPE(PAR_LAJAM) ParLajaM
      CHARACTER*24 PDNombre (PDNCol)
      INTEGER PDNCol
      INTEGER IEta
      INTEGER COffset
      INTEGER FOffset, FOffset_LAJA

      
      TYPE(AMatrix) A

!     Variables Locales
!****************
      CHARACTER*12 Nombre
      CHARACTER*80 fconcat

      INTEGER Idx
      DOUBLE PRECISION One
      DOUBLE PRECISION etadursc
      LOGICAL SkipEta

      SkipEta = .False.
      IF (ParRes%EtaIni .gt. 0 .and. IEta .lt. ParRes%EtaIni) THEN
         SkipEta = .True.
      ENDIF
      IF (ParRes%EtaFin .gt. 0 .and. IEta .gt. ParRes%EtaFin) THEN
         SkipEta = .True.
      ENDIF

      One = 1.0d0
      IF (SkipEta) THEN
         One = 0.0d0
      ENDIF

      DO Idx=1, ParRes%NumColEta
         Nombre = ParRes%VarEtaNames(Idx)
         Nombre = fconcat('l_',Nombre)
         PDNombre(COffset + Idx) = Nombre
      ENDDO

      DO Idx=1, ParRes%NumColEta
         ParRes%ColIndEta(IEta, Idx)= COffset + Idx
      ENDDO 
      DO Idx=1, ParRes%NumFilEta
         ParRes%FilIndEta(IEta, Idx)= FOffset + Idx
      ENDDO
      
      ! Se definen las variables totales  TQRP*
      ! con la restricción
      ! tqrp* - \sum_{b \in BL} H_{tb} qrp* = 0 
      ! para * in {riego, electrico, mixto}
      CALL Am_set(A,COffset + ParRes%TQRPR, FOffset+ ParRes%TQRPR, One)
      CALL Am_set(A,COffset + ParRes%TQRPE, FOffset+ ParRes%TQRPE, One)
      CALL Am_set(A,COffset + ParRes%TQRPM, FOffset+ ParRes%TQRPM, One)

      CALL Am_set(A,COffset + ParRes%TQRNR, FOffset+ ParRes%TQRNR, One)
      CALL Am_set(A,COffset + ParRes%TQRNE, FOffset+ ParRes%TQRNE, One)
      CALL Am_set(A,COffset + ParRes%TQRNM, FOffset+ ParRes%TQRNM, One)
   

      ! Se agregan a la restricción de derechos del laja
      ! este parámetro cambia a diferencia del genpdlajam.f
      ! debido a la definición de TQRP* , donde al ser total y no horario
      ! no es necesario multiplicar por la duración de la etapa (etadur)
      etadursc= One/ParLajaM%ScaleVol
      ! Gasto anual derechos riego
      CALL Am_set(A, COffset + ParRes%TQRPR, &
     &     FOffset_LAJA + ParLajaM%IVDRF_F, etadursc)
      ! Gasto anual derechos electrico
     CALL Am_set(A, COffset + ParRes%TQRPE, &
     &     FOffset_LAJA + ParLajaM%IVDEF_F, etadursc)
      ! Gasto anual derechos mixto
     CALL Am_set(A, COffset + ParRes%TQRPM, &
     &     FOffset_LAJA + ParLajaM%IVDMF_F, etadursc)

      CALL Am_set(A, COffset + ParRes%TQRNR, &
     &     FOffset_LAJA + ParLajaM%IVDRF_F, -etadursc)
      ! Gasto anual derechos electrico
     CALL Am_set(A, COffset + ParRes%TQRNE, &
     &     FOffset_LAJA + ParLajaM%IVDEF_F, -etadursc)
      ! Gasto anual derechos mixto
     CALL Am_set(A, COffset + ParRes%TQRNM, &
     &     FOffset_LAJA + ParLajaM%IVDMF_F, -etadursc)


      RETURN
      END


      
!******************************
!     lee configuracion de reserva
!******************************
      SUBROUTINE LeeCnfRes(                             &
     &     NCentral, CenNom, FactTiempo,                &
     &     NBloques, RendNCen, RendCenInd,              &
     &     FRestReserva, ParRes, ULog, Dim, FConvLaja)
      USE PLP, ONLY : PAR_DIMS, PAR_RESERVA

      TYPE(PAR_DIMS), INTENT(IN) :: Dim
      INTEGER NCentral
      CHARACTER*48 CenNom(NCentral)

      LOGICAL FRestReserva
      TYPE(PAR_RESERVA) ParRes
      INTEGER ULog

      INTEGER, INTENT(IN) :: NBloques
      INTEGER, INTENT(IN) :: RendNCen
      INTEGER, INTENT(IN) :: RendCenInd(RendNCen)
      INTEGER :: RendCen(RendNCen)

      DOUBLE PRECISION FactTiempo

!     variables locales
      CHARACTER*12 AuxVar
      CHARACTER*48 NomCen

      INTEGER URead
      EXTERNAL Abrir
      INTEGER Abrir
      INTEGER Servicio
      INTEGER IZona, I, NumCen, J, ICen, IRend, Idx
      INTEGER FConvLaja

      DOUBLE PRECISION CE5mp, CE5mn, CE10s,   &
     &     CEpp, CEpn, CEsp, CEsn, CEtp, CEtn
      DOUBLE PRECISION ME5mp, ME5mn, ME10s,   &
     &     MEpp, MEpn, MEsp, MEsn, MEtp, MEtn
      DOUBLE PRECISION FactorSubida(ParRes%NumServicios)
      DOUBLE PRECISION REQ10s, REQ5mp, REQ5mn, &
     &     REQpp,REQpn, REQsp, REQsn, REQtp, REQtn

      INTEGER NumZonas, NumGrupos, NParGrupos, NParCen
      CHARACTER*48 NArcReservaN

      DOUBLE PRECISION FE5mp, FE5mn, FE10s, FEp, FEn

      LOGICAL FWarning

      DOUBLE PRECISION CPpp, CPpn, MaxPpp, MaxPpn
      DOUBLE PRECISION CPsp, CPsn, MaxPsp, MaxPsn
      DOUBLE PRECISION CPtp, CPtn, MaxPtp, MaxPtn

      CHARACTER Zona
      CHARACTER*12 Zonas
      LOGICAL ZonaActiva,ZonaInercia
      INTEGER Grupo, IGrupo, io
      DOUBLE PRECISION Atr
      DOUBLE PRECISION AG10s, AG5mp, AG5mn, AGpp, AGpn, &
     &     AGsp, AGsn, AGtp, AGtn
      DOUBLE PRECISION AZ10s, AZ5mp, AZ5mn, AZpp, AZpn, &
     &     AZsp, AZsn, AZtp, AZtn

      CHARACTER*1024 line
!
      ALLOCATE(ParRes%VarEtaNames(ParRes%NumColEta))
      !subida
      ParRes%VarEtaNames(ParRes%TQRPR) = 'tqrpr'
      ParRes%VarEtaNames(ParRes%TQRPE) = 'tqrpe'
      ParRes%VarEtaNames(ParRes%TQRPM) = 'tqrpm'
      !bajada
      ParRes%VarEtaNames(ParRes%TQRNR) = 'tqrnr'
      ParRes%VarEtaNames(ParRes%TQRNE) = 'tqrne'
      ParRes%VarEtaNames(ParRes%TQRNM) = 'tqrnm'
      ParRes%NumCen = 0
      ParRes%NumZonas = 0

      IF (.not. FRestReserva) THEN
         return
      ENDIF

      NArcReservaN = 'plpcnfres.dat'
      ! Apertura de archivo
      URead = Abrir(NArcReservaN, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(ULog, '(3A)') 'leereserva: Warning, no existe archivo ',  &
     &        NArcReservaN, '.'
         FRestReserva = .False.
         return
      ENDIF

      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar

      line = ''
      READ(URead,'(A)') line
      READ(line, *, IOSTAT=io) NumZonas, ParRes%EtaIni, ParRes%EtaFin
      IF (io .ne. 0) THEN
         NumZonas = 0
         ParRes%EtaIni = 0
         ParRes%EtaFin = 0
         READ(line, *) NumZonas
      ENDIF

      IF (NumZonas .EQ. 0) THEN
         FRestReserva = .False.
         return
      ENDIF

      READ(URead, '(A1)') AuxVar

      ALLOCATE(ParRes%ZonaId(NumZonas))

      ALLOCATE(ParRes%REQ10s(NumZonas, Dim%Blo))
      ALLOCATE(ParRes%REQ5mp(NumZonas, Dim%Blo))
      ALLOCATE(ParRes%REQ5mn(NumZonas, Dim%Blo))
      ALLOCATE(ParRes%REQpp(NumZonas, Dim%Blo))
      ALLOCATE(ParRes%REQpn(NumZonas, Dim%Blo))
      ALLOCATE(ParRes%REQsp(NumZonas, Dim%Blo))
      ALLOCATE(ParRes%REQsn(NumZonas, Dim%Blo))
      ALLOCATE(ParRes%REQtp(NumZonas, Dim%Blo))
      ALLOCATE(ParRes%REQtn(NumZonas, Dim%Blo))
      ALLOCATE(ParRes%ZonaInercia(NumZonas))

      Zonas = ''
      IZona = 0
      DO I = 1, NumZonas
         READ(URead, *) Zona, ZonaActiva,           &
     &        REQ10s, REQ5mp, REQ5mn, REQpp, REQpn, &
     &        REQsp, REQsn, REQtp, REQtn, ZonaInercia

         IF (.NOT. ZonaActiva) THEN
            CYCLE
         ENDIF

         IZona = IZona + 1
         Zonas(IZona:IZona) = Zona
         ParRes%ZonaId(IZona) = Zona

         ParRes%REQ10s(IZona, 1:Dim%Blo) = REQ10s
         ParRes%REQ5mp(IZona, 1:Dim%Blo) = REQ5mp
         ParRes%REQ5mn(IZona, 1:Dim%Blo) = REQ5mn
         ParRes%REQpp(IZona, 1:Dim%Blo)  = REQpp
         ParRes%REQpn(IZona, 1:Dim%Blo)  = REQpn
         ParRes%REQsp(IZona, 1:Dim%Blo)  = REQsp
         ParRes%REQsn(IZona, 1:Dim%Blo)  = REQsn
         ParRes%REQtp(IZona, 1:Dim%Blo)  = REQtp
         ParRes%REQtn(IZona, 1:Dim%Blo)  = REQtn
         ParRes%ZonaInercia(IZona) = ZonaInercia
      END DO
      ParRes%Zonas = Zonas
      ParRes%NumZonas = IZona

      ALLOCATE(ParRes%AtrZ10s(ParRes%NumZonas))
      ALLOCATE(ParRes%AtrZ5mp(ParRes%NumZonas))
      ALLOCATE(ParRes%AtrZ5mn(ParRes%NumZonas))
      ALLOCATE(ParRes%AtrZpp(ParRes%NumZonas))
      ALLOCATE(ParRes%AtrZpn(ParRes%NumZonas))
      ALLOCATE(ParRes%AtrZsp(ParRes%NumZonas))
      ALLOCATE(ParRes%AtrZsn(ParRes%NumZonas))
      ALLOCATE(ParRes%AtrZtp(ParRes%NumZonas))
      ALLOCATE(ParRes%AtrZtn(ParRes%NumZonas))

      ParRes%AtrZ10s(1:ParRes%NumZonas) =  0.0
      ParRes%AtrZ5mp(1:ParRes%NumZonas) =  0.0
      ParRes%AtrZ5mn(1:ParRes%NumZonas) =  0.0
      ParRes%AtrZpp(1:ParRes%NumZonas) =  0.0
      ParRes%AtrZpn(1:ParRes%NumZonas) =  0.0
      ParRes%AtrZsp(1:ParRes%NumZonas) =  0.0
      ParRes%AtrZsn(1:ParRes%NumZonas) =  0.0
      ParRes%AtrZtp(1:ParRes%NumZonas) =  0.0
      ParRes%AtrZtn(1:ParRes%NumZonas) =  0.0

      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar

      DO I = 1, NumZonas
         READ(URead, *) Zona,  &
     &        AZ10s, &
     &        AZ5mp, &
     &        AZ5mn, &
     &        AZpp,  &
     &        AZpn,  &
     &        AZsp,  &
     &        AZsn,  &
     &        AZtp,  &
     &        AZtn

         IZona = INDEX(Zonas, Zona)
         IF (IZona .eq. 0) THEN
            CYCLE
         ENDIF

         ParRes%AtrZ10s(IZona) = AZ10s
         ParRes%AtrZ5mp(IZona) = AZ5mp
         ParRes%AtrZ5mn(IZona) = AZ5mn
         ParRes%AtrZpp(IZona) = AZpp
         ParRes%AtrZpn(IZona) = AZpn
         ParRes%AtrZsp(IZona) = AZsp
         ParRes%AtrZsn(IZona) = AZsn
         ParRes%AtrZtp(IZona) = AZtp
         ParRes%AtrZtn(IZona) = AZtn
      ENDDO

!     costos de escacez
      ALLOCATE(ParRes%CE5mp(ParRes%NumZonas, Dim%Eta))
      ALLOCATE(ParRes%CE5mn(ParRes%NumZonas, Dim%Eta))
      ALLOCATE(ParRes%CE10s(ParRes%NumZonas, Dim%Eta))
      ALLOCATE(ParRes%CEpp(ParRes%NumZonas, Dim%Eta))
      ALLOCATE(ParRes%CEpn(ParRes%NumZonas, Dim%Eta))
      ALLOCATE(ParRes%CEsp(ParRes%NumZonas, Dim%Eta))
      ALLOCATE(ParRes%CEsn(ParRes%NumZonas, Dim%Eta))
      ALLOCATE(ParRes%CEtp(ParRes%NumZonas, Dim%Eta))
      ALLOCATE(ParRes%CEtn(ParRes%NumZonas, Dim%Eta))
      

      ParRes%CE5mp(1:ParRes%NumZonas, 1:Dim%Eta) = 0.0
      ParRes%CE5mn(1:ParRes%NumZonas, 1:Dim%Eta) = 0.0
      ParRes%CE10s(1:ParRes%NumZonas, 1:Dim%Eta) = 0.0
      ParRes%CEpp(1:ParRes%NumZonas, 1:Dim%Eta) = 0.0
      ParRes%CEpn(1:ParRes%NumZonas, 1:Dim%Eta) = 0.0
      ParRes%CEsp(1:ParRes%NumZonas, 1:Dim%Eta) = 0.0
      ParRes%CEsn(1:ParRes%NumZonas, 1:Dim%Eta) = 0.0
      ParRes%CEtp(1:ParRes%NumZonas, 1:Dim%Eta) = 0.0
      ParRes%CEtn(1:ParRes%NumZonas, 1:Dim%Eta) = 0.0

!     maximos de escacez
      ALLOCATE(ParRes%ME5mp(ParRes%NumZonas, Dim%Eta))
      ALLOCATE(ParRes%ME5mn(ParRes%NumZonas, Dim%Eta))
      ALLOCATE(ParRes%ME10s(ParRes%NumZonas, Dim%Eta))
      ALLOCATE(ParRes%MEpp(ParRes%NumZonas, Dim%Eta))
      ALLOCATE(ParRes%MEpn(ParRes%NumZonas, Dim%Eta))
      ALLOCATE(ParRes%MEsp(ParRes%NumZonas, Dim%Eta))
      ALLOCATE(ParRes%MEsn(ParRes%NumZonas, Dim%Eta))
      ALLOCATE(ParRes%MEtp(ParRes%NumZonas, Dim%Eta))
      ALLOCATE(ParRes%MEtn(ParRes%NumZonas, Dim%Eta))

      ParRes%ME5mp(1:ParRes%NumZonas, 1:Dim%Eta) = 0.0
      ParRes%ME5mn(1:ParRes%NumZonas, 1:Dim%Eta) = 0.0
      ParRes%ME10s(1:ParRes%NumZonas, 1:Dim%Eta) = 0.0
      ParRes%MEpp(1:ParRes%NumZonas, 1:Dim%Eta) = 0.0
      ParRes%MEpn(1:ParRes%NumZonas, 1:Dim%Eta) = 0.0
      ParRes%MEsp(1:ParRes%NumZonas, 1:Dim%Eta) = 0.0
      ParRes%MEsn(1:ParRes%NumZonas, 1:Dim%Eta) = 0.0
      ParRes%MEtp(1:ParRes%NumZonas, 1:Dim%Eta) = 0.0
      ParRes%MEtn(1:ParRes%NumZonas, 1:Dim%Eta) = 0.0

      ALLOCATE(ParRes%FactorSubida(ParRes%NumZonas,ParRes%NumServicios))

      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar

      DO I = 1, NumZonas
         READ(URead, *) Zona,  &
     &        CE10s, ME10s, &
     &        CE5mp, ME5mp, &
     &        CE5mn, ME5mn, &
     &        CEpp, MEpp,   &
     &        CEpn, MEpn,   &
     &        CEsp, MEsp,   &
     &        CEsn, MEsn,   &
     &        CEtp, MEtp,   &
     &        CEtn, MEtn, (FactorSubida(Servicio),Servicio=1,ParRes%NumServicios)

         IZona = INDEX(Zonas, Zona)
         IF (IZona .eq. 0) THEN
            CYCLE
         ENDIF

         ParRes%CE10s(IZona, 1:Dim%Eta) = CE10s*FactTiempo/3.6d0
         ParRes%CE5mp(IZona, 1:Dim%Eta) = CE5mp*FactTiempo/3.6d0
         ParRes%CE5mn(IZona, 1:Dim%Eta) = CE5mn*FactTiempo/3.6d0
         ParRes%CEpp(IZona, 1:Dim%Eta) = CEpp*FactTiempo/3.6d0
         ParRes%CEpn(IZona, 1:Dim%Eta) = CEpn*FactTiempo/3.6d0
         ParRes%CEsp(IZona, 1:Dim%Eta) = CEsp*FactTiempo/3.6d0
         ParRes%CEsn(IZona, 1:Dim%Eta) = CEsn*FactTiempo/3.6d0
         ParRes%CEtp(IZona, 1:Dim%Eta) = CEtp*FactTiempo/3.6d0
         ParRes%CEtn(IZona, 1:Dim%Eta) = CEtn*FactTiempo/3.6d0

         ParRes%ME10s(IZona, 1:Dim%Eta) = ME10s
         ParRes%ME5mp(IZona, 1:Dim%Eta) = ME5mp
         ParRes%ME5mn(IZona, 1:Dim%Eta) = ME5mn
         ParRes%MEpp(IZona, 1:Dim%Eta) = MEpp
         ParRes%MEpn(IZona, 1:Dim%Eta) = MEpn
         ParRes%MEsp(IZona, 1:Dim%Eta) = MEsp
         ParRes%MEsn(IZona, 1:Dim%Eta) = MEsn
         ParRes%MEtp(IZona, 1:Dim%Eta) = MEtp
         ParRes%MEtn(IZona, 1:Dim%Eta) = MEtn

         ParRes%FactorSubida(IZona, 1: ParRes%NumServicios)=FactorSubida(1:ParRes%NumServicios)
      ENDDO


      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NumGrupos

      ParRes%NumGrupos = NumGrupos
      ALLOCATE(ParRes%AtrG10s(0:NumGrupos, 1:ParRes%NumZonas))
      ALLOCATE(ParRes%AtrG5mp(0:NumGrupos, 1:ParRes%NumZonas))
      ALLOCATE(ParRes%AtrG5mn(0:NumGrupos, 1:ParRes%NumZonas))
      ALLOCATE(ParRes%AtrGpp (0:NumGrupos, 1:ParRes%NumZonas))
      ALLOCATE(ParRes%AtrGpn (0:NumGrupos, 1:ParRes%NumZonas))
      ALLOCATE(ParRes%AtrGsp (0:NumGrupos, 1:ParRes%NumZonas))
      ALLOCATE(ParRes%AtrGsn (0:NumGrupos, 1:ParRes%NumZonas))
      ALLOCATE(ParRes%AtrGtp (0:NumGrupos, 1:ParRes%NumZonas))
      ALLOCATE(ParRes%AtrGtn (0:NumGrupos, 1:ParRes%NumZonas))

      ParRes%AtrG10s(0:NumGrupos, 1:ParRes%NumZonas) =  0.0
      ParRes%AtrG5mp(0:NumGrupos, 1:ParRes%NumZonas) =  0.0
      ParRes%AtrG5mn(0:NumGrupos, 1:ParRes%NumZonas) =  0.0
      ParRes%AtrGpp (0:NumGrupos, 1:ParRes%NumZonas) =  0.0
      ParRes%AtrGpn (0:NumGrupos, 1:ParRes%NumZonas) =  0.0
      ParRes%AtrGsp (0:NumGrupos, 1:ParRes%NumZonas) =  0.0
      ParRes%AtrGsn (0:NumGrupos, 1:ParRes%NumZonas) =  0.0
      ParRes%AtrGtp (0:NumGrupos, 1:ParRes%NumZonas) =  0.0
      ParRes%AtrGtn (0:NumGrupos, 1:ParRes%NumZonas) =  0.0

      READ(URead, '(A1)') AuxVar
      READ(URead, *) NParGrupos

      READ(URead, '(A1)') AuxVar
      DO I = 1, NParGrupos
         READ(URead, *) IGrupo, Zonas, AG10s, AG5mp, AG5mn, AGpp, AGpn, &
     &        AGsp, AGsn, AGtp, AGtn

         IF (IGrupo.lt.1 .or. IGrupo.gt.NumGrupos) THEN
            CYCLE
         ENDIF

         DO J =1, LEN(TRIM(Zonas))
            IZona = INDEX(ParRes%Zonas,Zonas(J:J))
            IF (IZona .eq. 0) THEN
               CYCLE
            ENDIF

            ParRes%AtrG10s(IGrupo, IZona) = AG10s
            ParRes%AtrG5mp(IGrupo, IZona) = AG5mp
            ParRes%AtrG5mn(IGrupo, IZona) = AG5mn
            ParRes%AtrGpp(IGrupo, IZona) = AGpp
            ParRes%AtrGpn(IGrupo, IZona) = AGpn
            ParRes%AtrGsp(IGrupo, IZona) = AGsp
            ParRes%AtrGsn(IGrupo, IZona) = AGsn
            ParRes%AtrGtp(IGrupo, IZona) = AGtp
            ParRes%AtrGtn(IGrupo, IZona) = AGtn
         ENDDO
      ENDDO

!     lee parametros de las centrales
      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NumCen

      ParRes%NumCen = NumCen

      ALLOCATE(ParRes%AtrCen(NumCen, NumZonas))
      ALLOCATE(ParRes%FE5mp(NumCen, NumZonas))
      ALLOCATE(ParRes%FE5mn(NumCen, NumZonas))
      ALLOCATE(ParRes%FE10s(NumCen, NumZonas))
      ALLOCATE(ParRes%FEp(NumCen, NumZonas))
      ALLOCATE(ParRes%FEn(NumCen, NumZonas))

      ParRes%AtrCen(1:NumCen, 1:NumZonas) = 0
      ParRes%FE5mp(1:NumCen, 1:NumZonas) = 0
      ParRes%FE5mn(1:NumCen, 1:NumZonas) = 0
      ParRes%FE10s(1:NumCen, 1:NumZonas) = 0
      ParRes%FEp(1:NumCen, 1:NumZonas) = 0
      ParRes%FEn(1:NumCen, 1:NumZonas) = 0

      ALLOCATE(ParRes%Cen(NumCen))
      ALLOCATE(ParRes%CenNom(NumCen))
      ALLOCATE(ParRes%CenZonas(NumCen))
      ALLOCATE(ParRes%CenGrupo(NumCen))

      ParRes%Cen(1:NumCen) = 0
      ParRes%CenNom(1:NumCen) = ''
      ParRes%CenZonas(1:NumCen) = ''
      ParRes%CenGrupo(1:NumCen) = 0

      ALLOCATE(ParRes%CPpp(NumCen, Dim%Eta))
      ALLOCATE(ParRes%CPpn(NumCen, Dim%Eta))
      ParRes%CPpp(1:NumCen, 1:Dim%Eta) = 0
      ParRes%CPpn(1:NumCen, 1:Dim%Eta) = 0
      ALLOCATE(ParRes%CPsp (NumCen, Dim%Eta))
      ALLOCATE(ParRes%CPsn (NumCen, Dim%Eta))
      ParRes%CPsp(1:NumCen, 1:Dim%Eta) = 0
      ParRes%CPsn(1:NumCen, 1:Dim%Eta) = 0
      ALLOCATE(ParRes%CPtp(NumCen, Dim%Eta))
      ALLOCATE(ParRes%CPtn(NumCen, Dim%Eta))
      ParRes%CPtp(1:NumCen, 1:Dim%Eta) = 0
      ParRes%CPtn(1:NumCen, 1:Dim%Eta) = 0

      ALLOCATE(ParRes%MinPpp(NumCen, NumZonas, Dim%Blo))
      ALLOCATE(ParRes%MinPpn(NumCen, NumZonas, Dim%Blo))
      ALLOCATE(ParRes%MinPsp(NumCen, NumZonas, Dim%Blo))
      ALLOCATE(ParRes%MinPsn(NumCen, NumZonas, Dim%Blo))
      ALLOCATE(ParRes%MinPtp(NumCen, NumZonas, Dim%Blo))
      ALLOCATE(ParRes%MinPtn(NumCen, NumZonas, Dim%Blo))
      ParRes%MinPpp(1:NumCen, 1:NumZonas, 1:Dim%Blo) = 0
      ParRes%MinPpn(1:NumCen, 1:NumZonas, 1:Dim%Blo) = 0
      ParRes%MinPsp(1:NumCen, 1:NumZonas, 1:Dim%Blo) = 0
      ParRes%MinPsn(1:NumCen, 1:NumZonas, 1:Dim%Blo) = 0
      ParRes%MinPtp(1:NumCen, 1:NumZonas, 1:Dim%Blo) = 0
      ParRes%MinPtn(1:NumCen, 1:NumZonas, 1:Dim%Blo) = 0

      ALLOCATE(ParRes%MaxPpp(NumCen, NumZonas, Dim%Blo))
      ALLOCATE(ParRes%MaxPpn(NumCen, NumZonas, Dim%Blo))
      ALLOCATE(ParRes%MaxPsp(NumCen, NumZonas, Dim%Blo))
      ALLOCATE(ParRes%MaxPsn(NumCen, NumZonas, Dim%Blo))
      ALLOCATE(ParRes%MaxPtp(NumCen, NumZonas, Dim%Blo))
      ALLOCATE(ParRes%MaxPtn(NumCen, NumZonas, Dim%Blo))
      ParRes%MaxPpp(1:NumCen, 1:NumZonas, 1:Dim%Blo) = 0
      ParRes%MaxPpn(1:NumCen, 1:NumZonas, 1:Dim%Blo) = 0
      ParRes%MaxPsp(1:NumCen, 1:NumZonas, 1:Dim%Blo) = 0
      ParRes%MaxPsn(1:NumCen, 1:NumZonas, 1:Dim%Blo) = 0
      ParRes%MaxPtp(1:NumCen, 1:NumZonas, 1:Dim%Blo) = 0
      ParRes%MaxPtn(1:NumCen, 1:NumZonas, 1:Dim%Blo) = 0

      ALLOCATE(ParRes%CenGMin(NumCen, Dim%Blo, Dim%Simul))
      ParRes%CenGMin(1:NumCen, 1:Dim%Blo, 1:Dim%Simul) = 0

      READ(URead, '(A10)') AuxVar
      READ(URead, '(A10)') AuxVar
      ICen = 0
      IRend = 0
      DO I=1, NumCen
         line = ''
         READ(URead,'(A)') line
         READ(line, *, IOSTAT=io) NomCen, Zonas, Grupo, Atr, &
     &        FE10s, FE5mp, FE5mn, FEp, FEn,       &
     &        CPpp, MaxPpp, CPpn, MaxPpn,          &
     &        CPsp, MaxPsp, CPsn, MaxPsn,          &
     &        CPtp, MaxPtp, CPtn, MaxPtn
         IF (io .ne. 0) THEN
            Atr = 0
            FE10s = 0
            FE5mp = 0
            FE5mn = 0
            FEp = 0
            FEn = 0
            MaxPpp = 0
            MaxPpn = 0
            MaxPsp = 0
            MaxPsn = 0
            MaxPtp = 0
            MaxPtn = 0

            READ(line, *) NomCen, Zonas, Grupo, &
     &           CPpp, CPpn, CPsp, CPsn, CPtp, CPtn
         ENDIF


         NumCen = 0
         CALL NomCen2NumCen(NumCen, FWarning, NomCen,  &
     &        CenNom, NCentral, NArcReservaN, ULog)
         IF (NumCen .eq. 0) THEN
            CYCLE
         ENDIF

         ICen = ICen + 1

         ParRes%Cen(ICen) = NumCen
         ParRes%CenNom(ICen) = NomCen
         ParRes%CenZonas(ICen) = Zonas
         ParRes%CenGrupo(ICen) = Grupo

         ParRes%AtrCen(ICen, 1:NumZonas) = Atr
         ParRes%FE5mp(ICen, 1:NumZonas) = FE5mp
         ParRes%FE5mn(ICen, 1:NumZonas) = FE5mn
         ParRes%FE10s(ICen, 1:NumZonas) = FE10s
         ParRes%FEp(ICen, 1:NumZonas) = FEp
         ParRes%FEn(ICen, 1:NumZonas) = FEn

         ParRes%MaxPpp(ICen, 1:NumZonas, 1:Dim%Blo) = MaxPpp
         ParRes%MaxPpn(ICen, 1:NumZonas, 1:Dim%Blo) = MaxPpn
         ParRes%MaxPsp(ICen, 1:NumZonas, 1:Dim%Blo) = MaxPsp
         ParRes%MaxPsn(ICen, 1:NumZonas, 1:Dim%Blo) = MaxPsn
         ParRes%MaxPtp(ICen, 1:NumZonas, 1:Dim%Blo) = MaxPtp
         ParRes%MaxPtn(ICen, 1:NumZonas, 1:Dim%Blo) = MaxPtn

         ParRes%CPpp (ICen, 1:Dim%Eta) = CPpp*FactTiempo/3.6d0
         ParRes%CPpn (ICen, 1:Dim%Eta) = CPpn*FactTiempo/3.6d0
         ParRes%CPsp (ICen, 1:Dim%Eta) = CPsp*FactTiempo/3.6d0
         ParRes%CPsn (ICen, 1:Dim%Eta) = CPsn*FactTiempo/3.6d0
         ParRes%CPtp (ICen, 1:Dim%Eta) = CPtp*FactTiempo/3.6d0
         ParRes%CPtn (ICen, 1:Dim%Eta) = CPtn*FactTiempo/3.6d0

         DO Idx = 1, RendNCen
            IF (NumCen .eq. RendCenInd(Idx)) THEN
               IRend = IRend + 1
               RendCen(IRend) = NumCen
            ENDIF
         ENDDO
      ENDDO
      NumCen = ICen
      ParRes%NumCen = ICen
      ParRes%NumRend = IRend
      ALLOCATE(ParRes%RendColInd(1:IRend,1:NBloques,1:NumZonas))
      ALLOCATE(ParRes%RendFilpInd(1:IRend,1:NBloques,1:NumZonas))
      ALLOCATE(ParRes%RendFilnInd(1:IRend,1:NBloques,1:NumZonas))
      ALLOCATE(ParRes%RendFilCBajInd(1:IRend,1:NBloques))
      ALLOCATE(ParRes%RendColCBajInd(1:IRend,1:NBloques))
      ALLOCATE(ParRes%RendColSubInd(1:IRend,1:NBloques))
      ALLOCATE(ParRes%RendFilSubInd(1:IRend,1:NBloques))
      ALLOCATE(ParRes%RendColBajInd(1:IRend,1:NBloques))
      ALLOCATE(ParRes%RendFilBajInd(1:IRend,1:NBloques))
      ALLOCATE(ParRes%FilIndEta(1:Dim%Eta, 1:ParRes%NumFilEta))
      ALLOCATE(ParRes%ColIndEta(1:Dim%Eta, 1:ParRes%NumColEta))
      ALLOCATE(ParRes%RendCen(1:IRend))
      ParRes%RendCen(1:IRend) = RendCen(1:IRend)

!     opcionalmente lee parametros de las centrales que dependen de las zonas
      READ(URead, '(A1)', IOSTAT=io) AuxVar
      READ(URead, '(A1)', IOSTAT=io) AuxVar
      READ(URead, *) NParCen
      IF (NParCen < 0) THEN
         NParCen = HUGE(NParCen)
      ENDIF

      READ(URead, '(A1)', IOSTAT=io) AuxVar
      DO I=1, NParCen
         READ(URead, *, IOSTAT=io) NomCen, Zonas,           &
     &        Atr, FE10s, FE5mp, FE5mn, FEp, FEn,           &
     &        MaxPpp, MaxPpn, MaxPsp, MaxPsn, MaxPtp, MaxPtn
         IF (io .lt. 0) THEN
            EXIT
         ELSE IF (io .gt. 0) THEN
            CYCLE
         ENDIF

         ICen = 0
         CALL NomCen2NumCen(ICen, FWarning, NomCen,  &
     &        ParRes%CenNom, ParRes%NumCen, NArcReservaN, ULog)
         IF (ICen .eq. 0) THEN
            CYCLE
         ENDIF

         DO J =1, LEN(TRIM(Zonas))
            IZona = INDEX(ParRes%Zonas,Zonas(J:J))
            IF (IZona .eq. 0) THEN
               CYCLE
            ENDIF

            IF (INDEX(ParRes%CenZonas(ICen), &
     &           ParRes%ZonaId(IZona)) .eq. 0) THEN
               WRITE(6,*) 'leemanresz: Warning, zona ', ParRes%ZonaId(IZona),   &
     &              ' no valida para la central ', trim(NomCen)
               WRITE(ULog,*) 'leemanresz: Warning, zona ', ParRes%ZonaId(IZona),   &
     &              ' no valida para la central ', trim(NomCen)
               CYCLE
            ENDIF

            ParRes%AtrCen(ICen, IZona) = Atr
            ParRes%FE5mp(ICen, IZona) = FE5mp
            ParRes%FE5mn(ICen, IZona) = FE5mn
            ParRes%FE10s(ICen, IZona) = FE10s
            ParRes%FEp(ICen, IZona) = FEp
            ParRes%FEn(ICen, IZona) = FEn

            ParRes%MaxPpp(ICen, IZona, 1:Dim%Blo) = MaxPpp
            ParRes%MaxPpn(ICen, IZona, 1:Dim%Blo) = MaxPpn
            ParRes%MaxPsp(ICen, IZona, 1:Dim%Blo) = MaxPsp
            ParRes%MaxPsn(ICen, IZona, 1:Dim%Blo) = MaxPsn
            ParRes%MaxPtp(ICen, IZona, 1:Dim%Blo) = MaxPtp
            ParRes%MaxPtn(ICen, IZona, 1:Dim%Blo) = MaxPtn

         ENDDO
      ENDDO

!     arreglos de columnas y filas relevantes para almacenar
      NumCen = ParRes%NumCen
      NumZonas = ParRes%NumZonas

      ALLOCATE(ParRes%zr10s_col(1:NumZonas))
      ALLOCATE(ParRes%zr5mp_col(1:NumZonas))
      ALLOCATE(ParRes%zr5mn_col(1:NumZonas))
      ALLOCATE(ParRes%zrpp_col(1:NumZonas))
      ALLOCATE(ParRes%zrpn_col(1:NumZonas))
      ALLOCATE(ParRes%zrsp_col(1:NumZonas))
      ALLOCATE(ParRes%zrsn_col(1:NumZonas))
      ALLOCATE(ParRes%zrtp_col(1:NumZonas))
      ALLOCATE(ParRes%zrtn_col(1:NumZonas))

      ALLOCATE(ParRes%ze10s_col(1:NumZonas))
      ALLOCATE(ParRes%ze5mp_col(1:NumZonas))
      ALLOCATE(ParRes%ze5mn_col(1:NumZonas))
      ALLOCATE(ParRes%zepp_col(1:NumZonas))
      ALLOCATE(ParRes%zepn_col(1:NumZonas))
      ALLOCATE(ParRes%zesp_col(1:NumZonas))
      ALLOCATE(ParRes%zesn_col(1:NumZonas))
      ALLOCATE(ParRes%zetp_col(1:NumZonas))
      ALLOCATE(ParRes%zetn_col(1:NumZonas))

      ALLOCATE(ParRes%r10s_col(1:NumCen))
      ALLOCATE(ParRes%r5mp_col(1:NumCen))
      ALLOCATE(ParRes%r5mn_col(1:NumCen))
!     ALLOCATE(ParRes%rpp_col(1:NumCen))
!     ALLOCATE(ParRes%rpn_col(1:NumCen))
!     ALLOCATE(ParRes%rsp_col(1:NumCen))
!     ALLOCATE(ParRes%rsn_col(1:NumCen))
!     ALLOCATE(ParRes%rtp_col(1:NumCen))
!     ALLOCATE(ParRes%rtn_col(1:NumCen))
      
      ALLOCATE(ParRes%qrp_col(1:NumCen))
      ALLOCATE(ParRes%qrn_col(1:NumCen))
      IF (FConvLaja .eq. 2) THEN
         ALLOCATE(ParRes%qrpr_col)
         ALLOCATE(ParRes%qrpe_col)
         ALLOCATE(ParRes%qrpm_col)
         ALLOCATE(ParRes%qrnr_col)
         ALLOCATE(ParRes%qrne_col)
         ALLOCATE(ParRes%qrnm_col)
      ENDIF
      ALLOCATE(ParRes%rppz_col(1:NumZonas,1:NumCen))
      ALLOCATE(ParRes%rpnz_col(1:NumZonas,1:NumCen))
      ALLOCATE(ParRes%rspz_col(1:NumZonas,1:NumCen))
      ALLOCATE(ParRes%rsnz_col(1:NumZonas,1:NumCen))
      ALLOCATE(ParRes%rtpz_col(1:NumZonas,1:NumCen))
      ALLOCATE(ParRes%rtnz_col(1:NumZonas,1:NumCen))

      ALLOCATE(ParRes%req10s_fil(1:NumZonas))
      ALLOCATE(ParRes%req5mp_fil(1:NumZonas))
      ALLOCATE(ParRes%req5mn_fil(1:NumZonas))
      ALLOCATE(ParRes%reqpp_fil(1:NumZonas))
      ALLOCATE(ParRes%reqpn_fil(1:NumZonas))
      ALLOCATE(ParRes%reqsp_fil(1:NumZonas))
      ALLOCATE(ParRes%reqsn_fil(1:NumZonas))
      ALLOCATE(ParRes%reqtp_fil(1:NumZonas))
      ALLOCATE(ParRes%reqtn_fil(1:NumZonas))

!     NumCols and NumFils

      ParRes%NumColBlo =     &
     &     NumZonas*9*2          &
     &     + NumZonas*NumCen*3*2 &
     &     + NumCen*2


      ParRes%NumFilBlo =     &
     &     NumZonas*9            &
     &     + NumCen*2*NumZonas   &
     &     + NumCen              &
     &     + NumZonas*9 + NumCen*2
      ! se agregan 2 filas más por el convenio del laja.
      IF (FConvLaja .eq. 2) THEN
         ParRes%NumFilBlo = ParRes%NumFilBlo + 2 
         ParRes%NumColBlo = ParRes%NumColBlo + ParRes%NumColEta
      ENDIF

      ParRes%NArcZonas = 'plpresz.csv'
      ParRes%NArcCens = 'plpres.csv'

      ALLOCATE(ParRes%DataBlo(ParRes%NumColBlo, Dim%Blo))
      ALLOCATE(ParRes%DualBlo(ParRes%NumZonas*9, Dim%Blo)) ! ese *9 es por la cantidad de requerimientos

      RETURN

      END

!*********************************
!     Subrutina Mantenimiento Reservas centrales
!*********************************
      SUBROUTINE LeeManResc(NBloques, ParRes, ULog)

      USE PLP

      TYPE(PAR_RESERVA) :: ParRes

!     lee mantenimiento de centrales
      EXTERNAL Abrir
      INTEGER Abrir
      CHARACTER*12 AuxVar

      INTEGER NBloques
      INTEGER ULog
      INTEGER URead
      LOGICAL FStop
      LOGICAL FWarning
      CHARACTER*48 NArcManResc

!     lee mantenimiento de centrales
      CHARACTER*48 NomCen
      INTEGER ICenMan
      INTEGER IBlo
      INTEGER NCenMan
      INTEGER NBloMan
      INTEGER IBind

      CHARACTER*24 Zonas
      INTEGER ICen, IZona, J
      DOUBLE PRECISION MinPpp, MinPpn, MinPsp, MinPsn, MinPtp, MinPtn
      DOUBLE PRECISION MaxPpp, MaxPpn, MaxPsp, MaxPsn, MaxPtp, MaxPtn
!

!
      NArcManResc = 'plpmanresc.dat'

      FStop = .FALSE.
      FWarning = .FALSE.
      URead = Abrir(NArcManResc, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(ULog, '(3A)') 'leemanresc: Warning, no existe archivo ',    &
     &        NArcManResc, '.'
         RETURN
      ENDIF
      READ(URead, '(A1)') AuxVar

      FStop = .FALSE.

      READ(URead, '(A1)') AuxVar
      READ(URead, *) NCenMan
      DO ICenMan = 1, NCenMan
         READ(URead, '(A1)') AuxVar
         READ(URead, *) NomCen
         ICen = 0
         CALL NomCen2NumCen(ICen, FWarning, NomCen,  &
     &        ParRes%CenNom, ParRes%NumCen, 'plpmanresc.dat', ULog)

         READ(URead, '(A1)') AuxVar
         READ(URead, *) NBloMan
         READ(URead, '(A1)') AuxVar
         DO IBlo = 1, NBloMan
            READ(URead, *) Zonas, IBind,   &
     &           MinPpp, MinPpn,            &
     &           MinPsp, MinPsn,            &
     &           MinPtp, MinPtn,            &
     &           MaxPpp, MaxPpn,            &
     &           MaxPsp, MaxPsn,            &
     &           MaxPtp, MaxPtn
            IF ((IBind .LT. 1) .OR. (IBind .GT. NBloques)) THEN
               CYCLE
            ENDIF
            IF (ICen .EQ. 0) THEN
               CYCLE
            ENDIF

            DO J =1, LEN(TRIM(Zonas))
               IZona = INDEX(ParRes%Zonas,Zonas(J:J))
               IF (IZona .eq. 0) THEN
                  CYCLE
               ENDIF

               ParRes%MinPpp(ICen, IZona, IBind) = MinPpp
               ParRes%MinPpn(ICen, IZona, IBind) = MinPpn
               ParRes%MinPsp(ICen, IZona, IBind) = MinPsp
               ParRes%MinPsn(ICen, IZona, IBind) = MinPsn
               ParRes%MinPtp(ICen, IZona, IBind) = MinPtp
               ParRes%MinPtn(ICen, IZona, IBind) = MinPtn

               ParRes%MaxPpp(ICen, IZona, IBind) = MaxPpp
               ParRes%MaxPpn(ICen, IZona, IBind) = MaxPpn
               ParRes%MaxPsp(ICen, IZona, IBind) = MaxPsp
               ParRes%MaxPsn(ICen, IZona, IBind) = MaxPsn
               ParRes%MaxPtp(ICen, IZona, IBind) = MaxPtp
               ParRes%MaxPtn(ICen, IZona, IBind) = MaxPtn
            ENDDO
         ENDDO
      ENDDO

      CALL Cerrar(URead)
      IF (FStop) THEN
         STOP 1
      ENDIF

      RETURN
      END


!*********************************
!     Subrutina Mantenimiento gmin centrales
!*********************************
      SUBROUTINE LeeManResg(NSimul, NBloques, &
     &     ParRes, ULog)

      USE PLP

      TYPE(PAR_RESERVA) :: ParRes

!     lee mantenimiento de centrales
      EXTERNAL Abrir
      INTEGER Abrir
      CHARACTER*12 AuxVar
      INTEGER NSimul
      INTEGER NBloques
      INTEGER ULog
      INTEGER URead
      LOGICAL FStop
      LOGICAL FWarning
      CHARACTER*48 NArcManResg
!     lee mantenimiento de centrales
      CHARACTER*48 NomCen

      INTEGER ICen, IGen, NGen, IBlo, ISimul, io
      DOUBLE PRECISION GMin

!
      NArcManResg = 'plpmanresg.dat'

      FStop = .FALSE.
      FWarning = .FALSE.
      URead = Abrir(NArcManResg, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(ULog, '(3A)') 'leemanresg: Warning, no existe archivo ',    &
     &        NArcManResg, '.'
         RETURN
      ENDIF
      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar

      READ(URead, *) NGen
      READ(URead, '(A1)') AuxVar

      IF (NGen < 0) THEN
         NGen = HUGE(NGen)
      ENDIF

      DO IGen = 1, NGen

         READ(URead, *, IOSTAT=io) ISimul, IBlo, NomCen, GMin
         IF (io .lt. 0) THEN
            EXIT
         ELSE IF (io .gt. 0) THEN
            CYCLE
         ENDIF

         ICen = 0
         CALL NomCen2NumCen(ICen, FWarning, NomCen,  &
     &        ParRes%CenNom, ParRes%NumCen, 'plpmanresc.dat', ULog)

         IF (ICen .eq. 0) THEN
            CYCLE
         ENDIF

         IF (IBlo .lt. 1 .or. IBlo .gt. NBloques) THEN
            CYCLE
         ENDIF

         IF (ISimul .lt. 1 .or. ISimul .gt. NSimul) THEN
            CYCLE
         ENDIF

         ! write(*,* ) ICen, IBlo, ISimul, GMin
         ParRes%CenGMin(ICen, IBlo, ISimul) = GMin
      ENDDO

      CALL Cerrar(URead)
      IF (FStop) THEN
         STOP 1
      ENDIF

      RETURN
      END


!*********************************
!     Subrutina Mantenimiento Reservas zonas
!*********************************

      SUBROUTINE LeeManResz(NBloques, ParRes, ULog)
      USE PLP

      TYPE(PAR_RESERVA) :: ParRes

!     lee mantenimiento de zonaes
      INTEGER NBloques
      EXTERNAL Abrir
      INTEGER Abrir
      CHARACTER*12 AuxVar
      INTEGER ULog
      INTEGER URead
      LOGICAL FStop
      LOGICAL FWarning
      CHARACTER*48 NArcManResz

!
      NArcManResz = 'plpmanresz.dat'

      FStop = .FALSE.
      FWarning = .FALSE.
      URead = Abrir(NArcManResz, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(ULog, '(3A)') 'leemanresz: Warning, no existe archivo ',    &
     &        NArcManResz, '.'
         RETURN
      ENDIF
      READ(URead, '(A1)') AuxVar

      CALL LeeManReszI(NBloques, ParRes,  URead, ULog)

      CALL Cerrar(URead)
      IF (FStop) THEN
         STOP 1
      ENDIF
      RETURN
      END

!********************

      SUBROUTINE LeeManReszI(NBloques,     &
     &     ParRes,                         &
     &     URead, ULog)

      USE PLP

      TYPE(PAR_RESERVA) :: ParRes
      INTEGER ULog

!     lee mantenimiento de zonaes
      EXTERNAL Abrir
      INTEGER Abrir
      CHARACTER*12 AuxVar
      INTEGER NBloques
      INTEGER IZonaMan
      INTEGER IBlo
      INTEGER NZonaMan
      INTEGER NBloMan
      INTEGER NumBlo
      INTEGER IZona
      CHARACTER Zona
      INTEGER URead
      LOGICAL FWarning

      DOUBLE PRECISION REQ5mp, REQ5mn, REQ10s,   &
     &     REQpp, REQpn, REQsp, REQsn, REQtp, REQtn
!
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NZonaMan
      DO IZonaMan = 1, NZonaMan
         READ(URead, '(A1)') AuxVar
         READ(URead, *) Zona

         IZona = INDEX(ParRes%Zonas, Zona)
         IF (IZona .EQ. 0) THEN
            WRITE(ULog, '(3A)') 'leemanresz: Warning, zone ', Zona,   &
     &           ' no existe o no esta activa.'
            FWarning = .TRUE.
         ENDIF

         READ(URead, '(A12)') AuxVar
         READ(URead, *) NBloMan

         READ(URead, '(A1)') AuxVar
         DO IBlo = 1, NBloMan
            READ(URead, *) NumBlo, REQ10s,  &
     &           REQ5mp, REQ5mn, REQpp, REQpn, &
     &           REQsp, REQsn, REQtp, REQtn

            IF ((NumBlo .LT. 1) .OR. (NumBlo .GT. NBloques)) THEN
               CYCLE
            ENDIF
            IF (IZona .eq. 0) THEN
               CYCLE
            ENDIF

            ParRes%REQ10s(IZona, NumBlo) = REQ10s
            ParRes%REQ5mp(IZona, NumBlo) = REQ5mp
            ParRes%REQ5mn(IZona, NumBlo) = REQ5mn
            ParRes%REQpp(IZona, NumBlo) = REQpp
            ParRes%REQpn(IZona, NumBlo) = REQpn
            ParRes%REQsp(IZona, NumBlo) = REQsp
            ParRes%REQsn(IZona, NumBlo) = REQsn
            ParRes%REQtp(IZona, NumBlo) = REQtp
            ParRes%REQtn(IZona, NumBlo) = REQtn
         ENDDO
      ENDDO

      RETURN
      END

!*********************************
!     Subrutina Costos de produccion de reservas Centrales
!*********************************

      SUBROUTINE LeeCosResc(NEtapa, FactTiempo, ParRes, ULog)
      USE PLP

      TYPE(PAR_RESERVA) :: ParRes

      EXTERNAL Abrir
      INTEGER Abrir
      CHARACTER*12 AuxVar
      CHARACTER*48 NomCen
      INTEGER ICenCos
      INTEGER IEta
      INTEGER NEtapa
      INTEGER NCenCos
      INTEGER NEtaCos
      INTEGER NumEta
      INTEGER ULog
      INTEGER URead
      LOGICAL FStop
      LOGICAL FWarning
      DOUBLE PRECISION FactTiempo
      INTEGER ICen
      DOUBLE PRECISION CPpp, CPpn, CPsp, CPsn, CPtp, CPtn

      CHARACTER*48 NArcCosresc
!
      NArcCosresc  = 'plpcosresc.dat'

      FStop = .FALSE.
      FWarning = .FALSE.
      URead = Abrir(NArcCosresc, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(ULog, '(3A)') 'leecosresc: Warning, no existe archivo ',    &
     &        NArcCosresc, '.'
         RETURN
      ENDIF
      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NCenCos
      DO ICenCos = 1, NCenCos
         READ(URead, '(A1)', END = 100) AuxVar
         READ(URead, *) NomCen
         ICen = 0
         CALL NomCen2NumCen(ICen, FWarning, NomCen,  &
     &        ParRes%CenNom, ParRes%NumCen, 'plpcosresc.dat', ULog)


         READ(URead, '(A1)') AuxVar
         READ(URead, *) NEtaCos
         READ(URead, '(A1)') AuxVar
         DO IEta = 1, NEtaCos
            READ(URead, *) NumEta, CPpp, CPpn, CPsp, CPsn, CPtp, CPtn
            IF ((NumEta .LT. 1) .OR. (NumEta .GT. NEtapa)) THEN
               CYCLE
            ENDIF
            IF (ICen .EQ. 0) THEN
               CYCLE
            ENDIF
            ParRes%CPpp(ICen, NumEta) = CPpp*FactTiempo/3.6d0
            ParRes%CPpn(ICen, NumEta) = CPpn*FactTiempo/3.6d0
            ParRes%CPsp(ICen, NumEta) = CPsp*FactTiempo/3.6d0
            ParRes%CPsn(ICen, NumEta) = CPsn*FactTiempo/3.6d0
            ParRes%CPtp(ICen, NumEta) = CPtp*FactTiempo/3.6d0
            ParRes%CPtn(ICen, NumEta) = CPtn*FactTiempo/3.6d0
         ENDDO
      ENDDO
  100 CALL Cerrar(URead)
      IF (FStop) THEN
         STOP 1
      ENDIF

      RETURN
      END
!******************************************************************
!      SUBROUTINE LeeResMinEmb(NEtapa, NCenEmb, CenNom, EmbFEsc,  &
!     &     NEmbVMinH, EmbVMinHInd, EmbVMinH, EmbCMinH, ULog, Dim)
!      USE PLP
!
!      TYPE(PAR_DIMS), INTENT(IN) :: Dim
!
!!******************************************************************
!!     lee mantenimientos de embalses
!      EXTERNAL Abrir
!      INTEGER Abrir
!      CHARACTER*12 AuxVar
!      CHARACTER*48 CenNom(Dim%Cen)
!      INTEGER NEtapa
!      INTEGER NCenEmb
!      INTEGER ULog
!      INTEGER URead
!      LOGICAL FStop
!      LOGICAL FWarning
!      INTEGER NEmbVMinH
!      INTEGER EmbVMinHInd(Dim%Emb)
!      DOUBLE PRECISION EmbCMinH(Dim%Emb, Dim%Eta)
!      DOUBLE PRECISION EmbVMinH(Dim%Emb, Dim%Eta)
!      DOUBLE PRECISION EmbFEsc(Dim%Emb)
!!
!      NEmbVMinH = 0
!      
!      FStop = .FALSE.
!      FWarning = .FALSE.
!      URead = Abrir(NArcMinEmbH, 'OLD', 'SEQUENTIAL', ULog)
!      NArcResMinEmbN='plpresminemb.dat'
!      IF (URead .EQ. 0) THEN
!         WRITE(ULog, '(3A)') 'leeresminemb: Error, no existe archivo ',    &
!     &        NArcResMinEmbN, '.'
!         RETURN
!      ENDIF
!      READ(URead, '(A1)') AuxVar
!
!      CALL LeeResMinEmbi(NEtapa, NCenEmb, CenNom, EmbFEsc,  &
!     &     NEmbVMinH, EmbVMinHInd, EmbVMinH, EmbCMinH, URead, ULog, Dim)
!
!      CALL Cerrar(URead)
!
!      RETURN
!      END
!
!
!
!
!!******************************************************************
!      SUBROUTINE LeeResMinEmbi(NEtapa, NCenEmb, CenNom, EmbFEsc,  &
!     &     NEmbVMinH, EmbVMinHInd, EmbVMinH, EmbCMinH, URead, ULog, Dim)
!
!      USE PLP
!
!      TYPE(PAR_DIMS), INTENT(IN) :: Dim
!
!!******************************************************************
!!     lee mantenimientos de embalses
!      EXTERNAL Abrir
!      INTEGER Abrir
!      CHARACTER*12 AuxVar
!      CHARACTER*48 CenNom(Dim%Cen)
!      CHARACTER*48 NomEmb
!      INTEGER IEmb
!      INTEGER IEmbMan
!      INTEGER NCenEmb
!      INTEGER NumEmb
!      INTEGER ULog
!      INTEGER URead
!      INTEGER NEmbResMin
!      LOGICAL FWarning
!      INTEGER NEmbVResMin
!      INTEGER EmbVResMin(Dim%Emb)
!      DOUBLE PRECISION EmbFEsc(Dim%Emb)
!      DOUBLE PRECISION EmbVolMinRes
!      CHARACTER*42 Objeto
!
!!
!      READ(URead, '(A1)') AuxVar
!      READ(URead, *) NEmbResMin
!      IEmb = 0
!      DO IEmbMan = 1, NEmbResMin
!         READ(URead, '(A1)') AuxVar
!         READ(URead, *) NomEmb
!         
!         NumEmb = 0
!         Objeto ='embalse'
!         CALL NomCen2NumCen(NumEmb, FWarning, NomEmb,                  &
!     &        CenNom, NCenEmb, Objeto, ULog)
!         IF (NumEmb .GT. 0) THEN
!            IEmb = IEmb + 1
!            EmbVminHInd(IEmb) = NumEmb
!         ENDIF
!         
!         READ(URead, '(A1)') AuxVar
!         READ(URead, *) EmbVolMinRes
!         EmbVResMin(NumEmb) = EmbVolMinRes*EmbFEsc(NumEmb)/1D3
!      ENDDO
!
!      NEmbVResMin = IEmb
!      
!      RETURN
!      END
!