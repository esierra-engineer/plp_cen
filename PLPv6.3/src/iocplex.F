!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      DOUBLE PRECISION FUNCTION GetInfty()
      USE OSI

      GetInfty = osi_getinfty()

      RETURN
      END
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      SUBROUTINE InitSolver (FInterfaz, ULog)
      USE OSI
      INTEGER FInterfaz
      INTEGER ULog

      CHARACTER*80 fconcat
      CHARACTER*80 version
      INTEGER(C_SIZE_T) size
      INTEGER tty_stat

      CALL osi_start(tty_stat)
      IF (tty_stat .eq. 0) THEN
         FInterfaz = -1
      ENDIF
      
      version = ' '
      size = SIZEOF(version)
      CALL osi_getsolversion(size, loc(version))
      version = fconcat(version, '.')
      WRITE(6, '(A, A)') 'Solver ', version(1:size)
      WRITE(ULog, '(A, A)') 'Solver ', version(1:size)

      RETURN
      END

      SUBROUTINE EndSolver ()

      RETURN
      END


      SUBROUTINE IniSvrEnv (env, epopt, eprhs, memmode)
      USE OSI
      INTEGER(C_SIZE_T) env
      DOUBLE PRECISION epopt
      DOUBLE PRECISION eprhs
      INTEGER memmode


      
!    Initialize the environment

      env = osi_env_new()

      IF (epopt .GT. 0.0d0) THEN
         CALL osi_env_setepopt(env, epopt)
      ENDIF

      IF (eprhs .GT. 0.0d0) THEN
         CALL osi_env_seteprhs(env, eprhs)
      ENDIF

      CALL osi_env_setmemmode(env, memmode)

      RETURN
      END

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      SUBROUTINE Resuelve (IStat, lp, kappa, Dim)      
      USE OSI
      USE PLP 

      TYPE(PAR_DIMS), INTENT(IN):: Dim

      INTEGER, INTENT(OUT):: IStat
      INTEGER(C_SIZE_T), INTENT(INOUT):: lp

      DOUBLE PRECISION, INTENT(OUT):: kappa

!     Optimize the problem and obtain solution.
      LOGICAL(c_bool) presolve
      LOGICAL(c_bool) no_presolve
      
      IF (Dim%FactMode .eq. 0) THEN
         no_presolve = .FALSE.
         presolve = .TRUE.
      ELSE
         no_presolve = .FALSE.
         presolve = .TRUE.
      ENDIF

      CALL osi_lp_dualopt (lp, presolve)

      IF (.NOT. osi_lp_isprovenoptimal(lp)) THEN
         CALL osi_lp_primopt (lp, no_presolve)
      ENDIF

      IF (osi_lp_isprovenoptimal(lp)) THEN
         IStat = OSI_STAT_OPTIMAL
         kappa = osi_lp_getconditionnumber (lp)
      ELSE
         IStat = OSI_STAT_INFEASIBLE
         kappa = 1.0d0
      ENDIF


      
      RETURN
      END

