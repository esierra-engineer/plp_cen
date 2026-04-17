include(CheckFortranCompilerFlag)

if (CMAKE_Fortran_COMPILER_ID MATCHES "GNU.*")
  # gfortran
  message(STATUS "Using GNU fortran flags")

  CHECK_FORTRAN_COMPILER_FLAG(-Wno-compare-reals FORTRAN_SUPPORTS_NO_COMPARE_REALS)
  if(FORTRAN_SUPPORTS_NO_COMPARE_REALS)
    set (GCC_NoWarns_FLAGS "${GCC_NoWarns_FLAGS} -Wno-compare-reals")
  endif()

  # gurobi fails
  set (GCC_FLAGS "-pipe -fdump-core -fbacktrace -fcheck=all -ffpe-trap=zero,denormal")  
  # este funciona
  # set (GCC_FLAGS "-pipe -fdump-core -fbacktrace -fcheck=all -ffpe-trap=zero,denormal,overflow")
  set (GCC_FLAGS "-pipe -fdump-core -fbacktrace -fcheck=all")

  set (GCC_Warning_FLAGS "-W -Wall -Wextra -Wuninitialized -Wunused -Wunused-but-set-variable -Wconversion -Wsurprising -Werror")
  set (GCC_NoWarns_FLAGS "${GCC_NoWarns_FLAGS} -Wno-character-truncation ")
  set (GCC_NoWarns_FLAGS "${GCC_NoWarns_FLAGS} -Wno-unused-parameter")
  set (GCC_Fortran_FLAGS "-cpp -fno-sign-zero -ffree-form -ffree-line-length-none -fimplicit-none -frecursive")
  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${GCC_FLAGS} ${OS_FLAGS} ${GCC_Fortran_FLAGS} ${GCC_Warning_FLAGS} ${GCC_NoWarns_FLAGS} ")


  set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -O3 -g3")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g3")


  set (CMAKE_CXX_FLAGS_RELEASE "-funroll-all-loops -O3 -g3")
  set (CMAKE_CXX_FLAGS_DEBUG   "-O0 -g3")
endif()

if (CMAKE_Fortran_COMPILER_ID MATCHES "Intel.*")
  # ifort
  message(STATUS "Using Intel fortran flags")
  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp -check all -traceback -debug all -ftrapuv")
  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -warn all -gen-interfaces -free")
  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -diag-disable 8291,8290 ") #,31,7712,7984
  set (CMAKE_Fortran_FLAGS_RELEASE "-O1 -g")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")

  set (CMAKE_CXX_FLAGS_RELEASE "-funroll-all-loops -O3 -g3")
  set (CMAKE_CXX_FLAGS_DEBUG   "-O0 -g3")
endif ()


include(OpenMPFlags)
