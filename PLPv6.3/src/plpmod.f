      MODULE PLP
      USE ISO_C_BINDING, ONLY: C_SIZE_T


      INCLUDE 'pxp.fpp'

      INTEGER INICIOANO
      INTEGER INICIOMES
      INTEGER INTRAETA
      INTEGER INTRARIE
      INTEGER INICIOTEMP
      INTEGER INICIOANTIC
      PARAMETER (INTRAETA  = 0)
      PARAMETER (INICIOMES = 1)
      PARAMETER (INTRARIE  = 1)
      PARAMETER (INICIOANO = 2)
      PARAMETER (INICIOTEMP = 3)
      PARAMETER (INICIOANTIC = 4)

      INTEGER ENEROHID
      PARAMETER (ENEROHID = 10)

      INTEGER HorasMesHid(12)
      PARAMETER (HorasMesHid = [ 720, 744, 720, 744, 744, 720,          &
     &     744, 720, 744, 744, 672, 744 ] )


      INCLUDE 'pardims.f'

      INCLUDE 'parlaja.f'

      INCLUDE 'parlajam.f'

      INCLUDE 'parmaule.f'
      
      INCLUDE 'parralco.f'

      INCLUDE 'parlajac.f'

      INCLUDE 'parmaulec.f'

      INCLUDE 'pargnl.f'
      INCLUDE 'pargn.f'

      INCLUDE 'parreserva.f'
      INCLUDE 'parbaterias.f'

      CONTAINS
      recursive function itoa(i) result(res)
      character*20 res
      integer,intent(in) :: i
      character(range(i)+2) :: tmp
      tmp = ' '
      write(tmp,'(i0)') i
      res = trim(tmp)
      end function

      END MODULE


