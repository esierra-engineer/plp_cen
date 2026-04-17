!****************
!     Subrutina LeeRun
!****************
      SUBROUTINE LeeRun(PlaneFile, PlaneIterBeg, PlaneIterEnd, &
     &     PlaneOpenMode, PlaneMaxIter, ULog)
      USE PLP

      CHARACTER*24 PlaneFile
      INTEGER PlaneIterBeg
      INTEGER PlaneIterEnd
      INTEGER PlaneOpenMode
      INTEGER PlaneMaxIter
!     comun a todas las rutinas:
!     parametros y variables locales:
      EXTERNAL Abrir
      INTEGER Abrir
      CHARACTER*12 AuxVar
      INTEGER ULog

      LOGICAL FWarning
      INTEGER URead
      

      character*12 name

      INTEGER uplane
      INTEGER iter, isimul, ieta, ncols, ncol, I, io
      REAL(8) rowlb, rowub, value

 97   FORMAT(4(I3, 1x), e23.15e3, 1x, e23.15e3)
 98   FORMAT(I7, 1x, e23.15e3, 1x, A)
      
!     codigo:
!************************
!     Lee datos plprun.dat
!************************

      PlaneFile = ''
      PlaneIterBeg = -1
      PlaneIterEnd = -2
      
      FWarning = .FALSE.
      URead = Abrir(NArcRun, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leerun: Warning, no existe archivo ',          &
     &        NArcRun, '.'
         WRITE(ULog, '(3A)') 'leerun: Warning, no existe archivo ',       &
     &        NArcRun, '.'
         RETURN
      ENDIF
      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) planefile
      READ(URead, '(A1)') AuxVar
      READ(URead, *) PlaneIterBeg, PlaneIterEnd
      READ(URead, '(A1)') AuxVar
      READ(URead, *) PlaneOpenMode
      
      CALL Cerrar(URead)

      planemaxiter = 0

      uplane = Abrir(planefile, 'OLD', 'SEQUENTIAL', ULog)
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
         
         DO I =1, ncols
            READ(UPlane, 98,IOSTAT=io) ncol, value, name
            IF (io > 0) THEN
               WRITE(*,*)  'Error leyendo planos'
               WRITE(Ulog,*)  'Error leyendo planos'
               STOP 1
            ELSE IF (io < 0) THEN
               EXIT
            ENDIF
            
         ENDDO

         IF (iter .lt. PlaneIterBeg .or. iter .gt. PlaneIterEnd) THEN
            CYCLE
         ENDIF
         planemaxiter = MAX(planemaxiter, iter)
            
      END DO

      CALL Cerrar(uplane)
      
      
      RETURN
      END
