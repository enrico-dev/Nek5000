cmake_minimum_required(VERSION 3.4 FATAL_ERROR)
project(Nek5000 Fortran C)

# =============================================================================
# Configure options
# =============================================================================

option(USR_LOC     "Name of problem case (.usr and .rea files)")

# From bin/makenek
option(MPI         "Use MPI (default: ON)"                            ON)
option(PROFILING   "Use internal profiling (default: ON)"             ON)
option(VISIT       "Use VisIt in situ"                                OFF)
option(VISIT_STOP  "If VIST=ON, wait for VisIt connection"            ON)

# From bin/core/makenek.inc
option(MPIIO       "Enable MPI-IO (default: ON)"                      ON)
option(BGQ         "Use BGQ optimized mxm (default: OFF"              OFF)
option(XSMM        "Use libxsmm for mxm (default: OFF)"               OFF)
option(CVODE       "Enable CVODE support (default: OFF)"              OFF)
option(HYPRE       "Enable HYPRE support (default: OFF)"              OFF)
option(VENDOR_BLAS "Use VENDOR BLAS/LAPACK (default: OFF)"            OFF)
option(DPROCMAP    "Use distributed processor mapping (default: OFF)" OFF)
option(PARRSB      "Use online RSB partitioner (default: OFF)"        OFF)
option(CMTNEK      "Use compressible solver (default: OFF)"           OFF)

if (NOT USR_LOC)
  set(USR_LOC "${CMAKE_BINARY_DIR}")
  message(STATUS "Nek5000 was not provided with -DUSR_LOC.  Attempting to find .usr files in ${USR_LOC}")
endif()

get_filename_component(USR_LOC "${USR_LOC}" ABSOLUTE BASE_DIR "${CMAKE_BINARY_DIR}")

if (USR_LOC MATCHES ".*\.usr")
  if (EXISTS "${USR_LOC}")
    get_filename_component(CASENAME "${USR_LOC}" NAME_WE)
    get_filename_component(CASEDIR "${USR_LOC}" DIRECTORY)
  else  ()
    message(FATAL_ERROR "Could not locate a Nek5000 .usr file at '${USR_LOC}'. Please check that -DUSR_LOC is correctly set.")
  endif()
else()
  file(GLOB usr_files "${USR_LOC}/*.usr")
  if (usr_files)
    list(GET usr_files 0 first_usr_file)
    get_filename_component(CASENAME "${first_usr_file}" NAME_WE)
	  set(CASEDIR "${USR_LOC}")
    unset(first_usr_file)
  else ()
    message(FATAL_ERROR "Could not locate a Nek5000 .usr file at '${USR_LOC}'. Please check that -DUSR_LOC is correctly set.")
  endif()
endif()

message(STATUS "Nek5000 configured with '${CASENAME}.usr' found in ${CASEDIR}")

# =============================================================================
# Compiler Options
# =============================================================================

# Detect name-mangling scheme
include(FortranCInterface)
FortranCInterface_VERIFY()

# Set fixed-format source
# set(CMAKE_Fortran_FORMAT "FIXED")

# Use preprocessor and 8-byte reals
if(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
      set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fdefault-real-8 -fdefault-double-8 -cpp -std=legacy")
  else()
      set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fdefault-real-8 -fdefault-double-8 -cpp -mcmodel=large -std=legacy")
  endif()
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL PGI)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -r8 -Mpreprocess -mcmodel=medium")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -r8 -fpconstant -fpp -mcmodel=large")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL XL)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -qrealsize=8 -qdpc=e -qsuffix=cpp=f -qsuppress=cmpmsg")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL Flang)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp -r8 -std=legacy")
else()
  message(FATAL_ERROR "Specified compiler (${CMAKE_Fortran_COMPILER_ID}) is not supported by Nek5000")
endif()

# =============================================================================
# Nek5000 Sources and Targets
# =============================================================================

# Create <casename>.f from <casename>.usr
set(USR_SOURCE ${CMAKE_CURRENT_BINARY_DIR}/${CASENAME}.f)
ADD_CUSTOM_COMMAND(
    OUTPUT ${USR_SOURCE}
    COMMAND ${CMAKE_COMMAND}
    -DCASENAME=${CASENAME} -DINFILE_DIR=${CASEDIR} -DOUTFILE_DIR=${CMAKE_CURRENT_BINARY_DIR}
    -P ${CMAKE_CURRENT_LIST_DIR}/core/mkuserfile.cmake
    DEPENDS ${CASEDIR}/${CASENAME}.usr
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
)

set(CORE_SOURCES
    core/3rd_party/dictionary.c
    core/3rd_party/finiparser.c
    core/3rd_party/iniparser.c
    core/3rd_party/nek_in_situ.f
    core/bdry.f
    core/byte.c
    core/byte_mpi.f
    core/calcz.f
    core/chelpers.c
    core/coef.f
    core/conduct.f
    core/connect1.f
    core/connect2.f
    core/convect.f
    core/convect2.f
    core/crs_amg.c
    core/crs_xxt.c
    core/cvode_driver.f
    core/drive1.f
    core/drive2.f
    core/dssum.f
    core/edgec.f
    core/eigsolv.f
    core/experimental/crs_hypre.c
    core/experimental/fem_amg_preco.c
    core/fast3d.f
    core/fasts.f
    core/fcrs.c
    core/gauss.f
    core/genxyz.f
    core/gfldr.f
    core/gmres.f
    core/hmholtz.f
    core/hpf.f
    core/hsmg.f
    core/ic.f
    core/induct.f
    core/interp.f
    core/makeq.f
    core/makeq_aux.f
    core/map2.f
    core/math.f
    core/multimesh.f
    core/mvmesh.f
    core/mxm_wrapper.f
    core/navier0.f
    core/navier1.f
    core/navier2.f
    core/navier3.f
    core/navier4.f
    core/navier5.f
    core/navier6.f
    core/navier7.f
    core/navier8.f
    core/papi.f
    core/partitioner.c
    core/perturb.f
    core/plan4.f
    core/plan5.f
    core/planx.f
    core/postpro.f
    core/prepost.f
    core/reader_par.f
    core/reader_re2.f
    core/reader_rea.f
    core/speclib.f
    core/ssolv.f
    core/subs1.f
    core/subs2.f
    core/vprops.f
    )

# TODO: Make mpif.h a proper dependency
if(MPI)
  file(REMOVE ${CMAKE_CURRENT_BINARY_DIR}/mpif.h)
  set(COMM_MPI_SOURCES core/comm_mpi.f)
else()
  configure_file(core/mpi_dummy.h ${CMAKE_CURRENT_BINARY_DIR}/mpif.h)
  set(COMM_MPI_SOURCES
      core/comm_mpi.f
      core/mpi_dummy.f)
endif()

if(CMTNEK)
  set(CMT_SOURCES
    core/cmt/artvisc.f
    core/cmt/ausm.f
    core/cmt/bc.f
    core/cmt/diagnostics.f
    core/cmt/diffusive_cmt.f
    core/cmt/drive1_cmt.f
    core/cmt/drive2_cmt.f
    core/cmt/driver3_cmt.f
    core/cmt/eqnsolver_cmt.f
    core/cmt/face.f
    core/cmt/filters_cmt.f
    core/cmt/inflow_bc.f
    core/cmt/intpdiff.f
    core/cmt/outflow_bc.f
    core/cmt/state1.f
    core/cmt/step.f
    core/cmt/surface_fluxes.f
    core/cmt/wall_bc.f)
  include_directories(core/cmt/)
else()
  unset(CMT_SOURCES)
endif()

if(DPROCMAP)
  set(CORE_SOURCES ${CORE_SOURCES} core/dprocmap.f)
  configure_file(core/PARALLEL.dprocmap ${CMAKE_CURRENT_BINARY_DIR}/PARALLEL COPYONLY)
else()
  configure_file(core/PARALLEL.default ${CMAKE_CURRENT_BINARY_DIR}/PARALLEL COPYONLY)
endif()

if(VISIT)
  set(VISIT_SOURCES core/3rd_party/visit.f)
else()
  unset(VISIT_SOURCES)
endif()

if(BGQ)
  set(MXM_SOURCES core/mxm_bgq.f)
else()
  set(MXM_SOURCES core/mxm_std.f)
endif()

# Library
add_library(libnek5000
    ${USR_SOURCE} ${CORE_SOURCES} ${BLAS_SOURCES} ${MXM_SOURCES}
    ${NEKNEK_SOURCES} ${COMM_MPI_SOURCES} ${CMT_SOURCES} ${VISIT_SOURCES})
set_target_properties(libnek5000 PROPERTIES OUTPUT_NAME nek5000)
target_include_directories(libnek5000 PUBLIC
    ${CASEDIR} ${CMAKE_BINARY_DIR} ${CMAKE_CURRENT_BINARY_DIR} core core/3rd_party core/cmt
    core/experimental)

# Driver
add_executable(nek5000 core/drive.f)
target_link_libraries(nek5000 PUBLIC libnek5000)

# Tools
# add_subdirectory(tools EXCLUDE_FROM_ALL)

# =============================================================================
# Preprocessor Symbols
# =============================================================================

if(MPI)
  target_compile_definitions(libnek5000 PRIVATE -DMPI)
endif()

if(NOT MPI OR NOT MPIIO)
  target_compile_definitions(libnek5000 PRIVATE -DNOMPIIO)
endif()

if(FortranCInterface_GLOBAL_SUFFIX STREQUAL "_")
  target_compile_definitions(libnek5000 PRIVATE -DUNDERSCORE)
endif()

# Hardcoded in makenek.inc but unused by Nek5000 itself. It is passed to the gslib build.
# Rather than defining it here, it is defined in the gslib CMakeLists
# set(LONGLONGINT ON)
# if(LONGLONGINT)
#   target_compile_definitions(libnek5000 PRIVATE -DGLOBAL_LONG_LONG)
# endif()

if(PROFILING)
  target_compile_definitions(libnek5000 PRIVATE -DTIMER)
endif()

foreach(opt CMTNEK DPROCMAP PARRSB PARMETIS CVODE HYPRE BGQ VENDOR_BLAS)
  if(${opt})
    target_compile_definitions(libnek5000 PRIVATE -D${opt})
  endif()
endforeach()

if(VISIT)
  target_compile_definitions(libnek5000 PRIVATE -DVISIT)
  if(VISIT_STOP)
    target_compile_definitions(libnek5000 PRIVATE -DVISIT_STOP)
  endif()
endif()


# =============================================================================
# Link to 3rd-party libraries
# =============================================================================

# -----------------------------------------------------------------------------
# gslib
# -----------------------------------------------------------------------------

add_subdirectory(3rd_party/gslib/gslib/)
target_include_directories(nek5000 PRIVATE 3rd_party/gslib/include)
target_link_libraries(libnek5000 PUBLIC libgs)

# -----------------------------------------------------------------------------
# BLAS/LAPACK
# -----------------------------------------------------------------------------

if(VENDOR_BLAS)
  find_package(BLAS REQUIRED)
  if(NOT BLAS_FOUND)
    message(FATAL_ERROR "Could not find BLAS!")
  endif()
else()
  add_subdirectory(3rd_party/blasLapack)
  target_link_libraries(libnek5000 PRIVATE blasLapack)
endif()

# # CVODE
# # This will find and link to a prebuild CVODE library.
# # TODO: Build CVODE from scratch
# if(CVODE)
#   set(CVODE_LIBDIR core/3rd_party/cvode/lib/)
#   find_library(SUNDIALS_FCVODE sundials_fcvode PATHS ${CVODE_LIBDIR})
#   find_library(SUNDIALS_CVODE sundials_cvode PATHS ${CVODE_LIBDIR})
#   if(MPI)
#     find_library(SUNDIALS_FNVEC sundials_fnvecparallel PATHS ${CVODE_LIBDIR})
#     find_library(SUNDIALS_NVEC sundials_fcvodeparallel PATHS ${CVODE_LIBDIR})
#   else()
#     find_library(SUNDIALS_FNVEC sundials_fnvecserial PATHS ${CVODE_LIBDIR})
#     find_library(SUNDIALS_NVEC sundials_fcvodeserial PATHS ${CVODE_LIBDIR})
#   endif()
#   target_link_libraries(nek5000 PUBLIC
#       ${SUNDIALS_FCVODE} ${SUNDIALS_CVODE}
#       ${SUNDIALS_FVEC} ${SUNDIALS_FVEC})
# endif()
#
# # BLAS
# # TODO: Check for BLAS libraries
# # if(VENDOR_BLAS)
# # endif()
#
# # VISIT
# # TODO: Did visit libraries, but need to do visit headers
# if(VISIT)
#   target_include_directories(libnek5000 PUBLIC
#       ${VISIT_INSTALL}/libsim/V2/include/
#       core/3rd_party/)
#   target_include_directories(nek5000 PUBLIC
#       ${VISIT_INSTALL}/libsim/V2/include/
#       core/3rd_party/)
# endif()
