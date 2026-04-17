      SUBROUTINE AddPlane(uplane, iter, isimul, ieta, &
     &     lp,  ncount, columns, elements, rowlb, rowub)
      USE OSI

      INTEGER ISimul
      INTEGER iter
      INTEGER ieta
      INTEGER(C_SIZE_T) lp

      INTEGER uplane
      INTEGER ncount
      INTEGER columns(ncount) 
      REAL(8) elements(ncount)
      REAL(8) rowlb
      REAL(8) rowub

      integer nrow, I

      integer nz
      character*12 name
      character*12 names(ncount)
      INTEGER cols(ncount) 
      REAL(8) coeffs(ncount)

 97   FORMAT(4(I3,','), e23.15e3,',', e23.15e3)
 98   FORMAT((I7,',', e23.15e3,',', A12))

      CALL osi_lp_addrow(lp, ncount, columns, elements, rowlb, rowub)
      nrow = osi_lp_getnumrows(lp)

      IF (UPlane .eq. 0) THEN
         RETURN
      ENDIF

      nz = 0 
      DO I=1, ncount
         IF (elements(i) .ne. 0d0) THEN
            nz = nz + 1
            cols(nz) = columns(i)
            coeffs(nz) = elements(i)
            call osi_lp_getcolname(lp, columns(i), sizeof(name), loc(name))
            names(nz) = name            
         ENDIF
      ENDDO
      
!$OMP CRITICAL (addplane)

      WRITE(UPlane, 97) iter + 1, isimul, ieta, nz, rowlb, rowub
      WRITE(UPlane, 98) (cols(i), coeffs(i), names(i), i=1, nz)
      
!$OMP END CRITICAL (addplane)
      
      END SUBROUTINE AddPlane

!
!     loadplane
!
      

      SUBROUTINE LoadPlanes(PlaneFile, PlaneIterBeg, PlaneIterEnd, &
     &     maxiter, nplanes, &
     &     lp, FSeparaLP, Dim, ulog)
      USE OSI
      USE PLP

      CHARACTER*24 PlaneFile
      INTEGER PlaneIterBeg, PlaneIterEnd
      INTEGER ulog
      INTEGER Abrir
      INTEGER uplane
      LOGICAL FSeparaLP
      TYPE(PAR_DIMS), INTENT(IN)::  Dim      

      INTEGER(C_SIZE_T) lpi
      INTEGER(C_SIZE_T) lp(Dim%Simul, Dim%Eta)

      INTEGER columns(Dim%PDLDAcCol + 1) 
      REAL(8) elements(Dim%PDLDAcCol+ 1)
      REAL(8) rowlb
      REAL(8) rowub

      INTEGER :: io, nplanes

      INTEGER iter, isimul, ieta, ncols
      INTEGER ncol
      REAL(8) value
      character*12 name
      character*12 cname
      integer I, J
      INTEGER MaxIter

      INTEGER idx
      INTEGER(C_SIZE_T) cidx
      INTEGER(C_SIZE_T) cidxs(Dim%Simul, Dim%Eta)
      
 97   FORMAT(4(I3, 1x), e23.15e3, 1x, e23.15e3)
 98   FORMAT(I7, 1x, e23.15e3, 1x, A)

      cidxs(1:Dim%Simul, 1:Dim%Eta) = 0

      MaxIter = 0
      nplanes = 0

      IF (PlaneIterBeg .GT. PlaneIterEnd) THEN
         RETURN
      ENDIF
      
      uplane = Abrir(PlaneFile, 'OLD', 'SEQUENTIAL', ULog)
      if (uplane .eq. 0) then
         return
      endif
      
      DO
         READ(UPlane, 97,IOSTAT=io) iter, isimul, ieta, ncols, rowlb, rowub
         IF (io > 0) THEN
            WRITE(*,*)  'Error leyendo planos'
            WRITE(Ulog,*)  'Error leyendo planos'
            STOP 1
         ELSE IF (io < 0) THEN
            EXIT
         ENDIF

         lpi = lp(isimul, ieta)
         
         DO I =1, ncols
            READ(UPlane, 98,IOSTAT=io) ncol, value, name
            IF (io > 0) THEN
               WRITE(*,*)  'Error leyendo planos'
               WRITE(Ulog,*)  'Error leyendo planos'
               STOP 1
            ELSE IF (io < 0) THEN
               EXIT
            ENDIF
            
            call osi_lp_getcolname(lpi, ncol, sizeof(cname), loc(cname))
            if (cname .ne. name) then
               cidx = cidxs(isimul, ieta)
               IF (cidx .EQ. 0) THEN
                  cidxs(isimul, ieta) = osi_new_colidxs(lpi, sizeof(cname))
                  cidx = cidxs(isimul, ieta)
               ENDIF         
               
               idx = osi_get_colidx(cidx, loc(name), sizeof(name))
               IF (idx .eq. -1) THEN
                  write(*,*) 'addplane: nombre de col no encontrado', name
                  write(ULog,*) 'addplane: nombre de col no encontrado', name
                  STOP 1
               ELSE
                  call osi_lp_getcolname(lpi, idx, sizeof(cname), loc(cname))
               ENDIF
               if (cname .ne. name) then                  
                  write(*,*) 'addplane: nombres de columnas difieren', cname, name
                  write(ULog,*) 'addplane: nombres de columnas difieren', cname, name
                  STOP 1
               endif
               ncol = idx
            endif            
            columns(I) = ncol
            elements(I) = value
         ENDDO

         IF (iter .lt. PlaneIterBeg .or. iter .gt. PlaneIterEnd) THEN
            CYCLE
         ENDIF
         maxiter = MAX(maxiter, iter)
            
         IF ((ISimul .NE. 1) .AND. .not. FSeparaLP) THEN
            CYCLE
         ENDIF
         
         CALL osi_lp_addrow(lpi, ncols, columns, elements, rowlb, rowub)
         nplanes = nplanes + 1
         
      END DO

      DO I =1, Dim%Simul
         DO J =1, Dim%Eta
            IF (cidxs(I, J) .ne. 0) THEN
               call osi_delete_colidxs(cidxs(I, J))
            ENDIF
         ENDDO
      ENDDO
      

      CALL Cerrar(uplane)
      
      RETURN
      
      END SUBROUTINE  LoadPlanes
      
