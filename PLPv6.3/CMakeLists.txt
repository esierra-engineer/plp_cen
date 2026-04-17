cmake_minimum_required (VERSION 2.6)
project (plp Fortran)
enable_language (C)
enable_language (CXX)
enable_language (Fortran)

set(CMAKE_VERBOSE_MAKEFILE 1)

#
# CMake Path
#
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake")


#
# Release type
#

# Set a default build type if none was specified
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
endif()

#
# FFLAGS depends on the compiler
#

include(FortranFlags)

#
# Loading solver
#

option(COIN_USE_CPX "Use Cplex"  ON)
option(COIN_USE_CLP "Use Clp"    OFF)
option(COIN_USE_GRB "Use Gurobi" OFF)

set(COIN_ROOT_DIR "/opt/coinor" CACHE PATH "COIN root directory")
set(CPLEX_ROOT_DIR "/opt/cplex/cplex" CACHE PATH "CPLEX root directory")
set(GUROBI_ROOT_DIR "/opt/gurobi" CACHE PATH "CPLEX root directory")

include(Solver)

#
# PLP Definitions
#
set(PLP_SOURCE_DIR src)

set (PLP_FILES
  osicallsc.f90
  osicallsc.cpp
  amat.cpp
  amat.f90
  plpamat.f
  plpmod.f
  aadram.f
  borrespd.f
  calptra.f
  dapprl.f
  defprbpd.f
  estadist.f
  extclave.f
  extroper.f
  genmatpd.f
  genmatx.f
  genpdago.f
  genpdang.f
  genpdcen.f
  genpdcot.f
  genpdlaja.f
  genpdlajam.f
  genpdmaule.f
  genpdralco.f
  genpdlin.f
  genpdfil.f
  genpdextr.f
  genpdver.f
  genpdreb.f
  genpdminh.f
  genpdqemb.f
  getdual.f
  getprim.f
  leecnfba.f
  leecnfce.f
  leecnfli.f
  leecosce.f
  leedebug.f
  leedem.f
  leeeta.f
  leeextrac.f
  leefilemb.f
  leevrebemb.f
  leeblo.f
  leemance.f
  leemances.f
  leeminembh.f
  leemanem.f
  leemanems.f
  leemanli.f
  leemanlis.f
  leemat.f
  leeplaem.f
  leeqebnd.f
  leelajam.f
  modifbrd.f
  modifcoe.f
  nomcnumc.f
  nomcnumf.f
  num2char.f
  pdfin.f
  pdinic.f
  pdlin.f
  pdmatini.f
  pdmatinv.f
  pdmatvar.f
  pdmsjfin.f
  pdtit.f
  pricas.f
  repoprog.f
  salva.f
  sumadem.f
  trasprim.f
  unit.F
  uppcas.f
  userstop.f
  volfinem.f
  wrcharn.f
  linux-com.f
  tablrel.f
  addplane.f
  leerun.f
  plp-agrespd.f
  plp-espercnd.f
  plp-extrprsi.f
  plp-extrsimu.f
  plp-fasedual.f
  plp-faseprim.f
  plp-ffiltrac.f
  plp-fijamues.f
  plp-fijasimu.f
  plp-filtrac.f
  plp-filtracv.f
  plp-frendim.f
  plp-fpmaxvol.f
  plp-gdbdbar.f
  plp-gdbdbar2.f
  plp-gdbdcen.f
  plp-gdbdcen2.f
  plp-gdbdemb.f
  plp-gdbdemb2.f
  plp-gdbdlaj.f
  plp-gdbdlin.f
  plp-gdbdlin2.f
  plp-gdbdmau.f
  plp-gdbdple.f
  plp-gdbdple2.f
  plp-gdbdser.f
  plp-gdbdser2.f
  plp-gdbdextr.f
  plp-gradat.f
  plp-leeaflce.f
  plp-leecenfi.f
  plp-leecenre.f
  plp-leecenpmax.f
  plp-leeextma.f
  plp-leeidap2.f
  plp-leeidape.f
  plp-leeidsim.f
  plp-maule0.f
  plp-maule1.f
  plp-maule2.f
  plp-laja0.f
  plp-laja1.f
  plp-laja2.f
  plp-matincid.f
  plp-pdbssoln.f
  plp-pdconvrg.f
  plp-pdmatld.f
  plp-progdin.f
  plp-rendim.f
  plp-trasdual.f
  plp-trasdusi.f
  linux-deb.f
  iocplex.F
  plp-version.f
  getopts.f
  genpdgnl.f
  genpdgn.f
  genpdreserva.f
  genpdbaterias.f
  leecentipo.f
  )

foreach(file ${PLP_FILES})
  set(PLP_SRCS ${PLP_SRCS} "${PLP_SOURCE_DIR}/${file}")
endforeach()

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${PLP_SOURCE_DIR}")

add_library(iplp STATIC ${PLP_SRCS})
add_executable(plp ${PLP_SOURCE_DIR}/plp-main.F)
set_property(TARGET plp PROPERTY LINKER_LANGUAGE Fortran)

set(LINK_LIBS "${COIN_OSI_LIBRARIES};${SOLVER_LIBRARIES}")
target_link_libraries(plp iplp ${LINK_LIBS})

include(Doxygen)
