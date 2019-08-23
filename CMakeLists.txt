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
# Internal options
# =============================================================================

# One libsundials_fcvode_static is only available as a static library, so we must
# force this...
if (CVODE)
  set(BUILD_SHARED_LIBS OFF CACHE BOOL "Build shared libraries" FORCE)
endif()

# =============================================================================
# Nek5000 Sources and Targets
# =============================================================================

# Create <casename>.f from <casename>.usr
set(USR_SOURCE ${CMAKE_CURRENT_BINARY_DIR}/${CASENAME}.f)
ADD_CUSTOM_COMMAND(
    OUTPUT ${USR_SOURCE}
    COMMAND ${CMAKE_COMMAND}
    -DCASENAME=${CASENAME}
    -DINFILE_DIR=${CASEDIR}
    -DOUTFILE_DIR=${CMAKE_CURRENT_BINARY_DIR}
    -DCVODE=${CVODE}
    -P ${CMAKE_CURRENT_LIST_DIR}/core/mkuserfile.cmake
    DEPENDS ${CASEDIR}/${CASENAME}.usr
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
)

set(SIZE_SOURCE ${CMAKE_CURRENT_BINARY_DIR}/SIZE)
ADD_CUSTOM_COMMAND(
    OUTPUT ${SIZE_SOURCE}
    COMMAND ${CMAKE_COMMAND}
    -DINFILE_DIR=${CASEDIR}
    -DOUTFILE_DIR=${CMAKE_CURRENT_BINARY_DIR}
    -P ${CMAKE_CURRENT_LIST_DIR}/core/mksizefile.cmake
    DEPENDS ${CASEDIR}/SIZE
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
    core/nek_interface.f
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
    ${SIZE_SOURCE}
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
    ${CMAKE_CURRENT_BINARY_DIR} core core/3rd_party core/cmt
    core/experimental)

# Driver
add_executable(nek5000 core/drive.f)
target_link_libraries(nek5000 PUBLIC libnek5000)

# Tools

add_executable(genbox EXCLUDE_FROM_ALL
    tools/genbox/byte.c
    tools/genbox/genbox.f)

add_executable(gencon EXCLUDE_FROM_ALL
    core/byte.c
    tools/gencon/gencon.f)

add_executable(genmap EXCLUDE_FROM_ALL
    core/byte.c
    tools/genmap/genmap.f )

add_executable(gmsh2nek EXCLUDE_FROM_ALL
    core/byte.c
    core/speclib.f
    tools/gmsh2nek/gmsh2nek.f90
    tools/gmsh2nek/mod_SIZE.f90
    tools/gmsh2nek/mxm.f )

set(POSTNEK_CORE_SOURCES
  core/3rd_party/dictionary.c
  core/3rd_party/finiparser.c
  core/3rd_party/iniparser.c
  tools/postnek/animate.f
  tools/postnek/blas.f
  tools/postnek/byte.c
  tools/postnek/coef.f
  tools/postnek/g3d.f
  tools/postnek/genxyz.f
  tools/postnek/getfld.f
  tools/postnek/legend.f
  tools/postnek/locglob.f
  tools/postnek/mxm.f
  tools/postnek/plot.f
  tools/postnek/postnek.f
  tools/postnek/postnek2.f
  tools/postnek/postnek3.f
  tools/postnek/postnek5.f
  tools/postnek/postnek5a.f
  tools/postnek/postnek6.f
  tools/postnek/postnek7.f
  tools/postnek/postnek8.f
  tools/postnek/postnek9.f
  tools/postnek/revert.c
  tools/postnek/scrdmp.f
  tools/postnek/screen.f
  tools/postnek/speclib.f
  tools/postnek/ssyev.f
  tools/postnek/subs.f
  tools/postnek/trap.f
  tools/postnek/tsort.f
  tools/postnek/userf.f
  tools/postnek/xdriver.c
  tools/postnek/xinterface.f )
add_executable(postx EXCLUDE_FROM_ALL ${POSTNEK_CORE_SOURCES} tools/postnek/iolib.f)
add_executable(postex EXCLUDE_FROM_ALL ${POSTNEK_CORE_SOURCES} tools/postnek/iolib_no_graph.f)
foreach(target postx postex)
  target_include_directories(${target} PRIVATE tools/postnek)
endforeach()

add_executable(reatore2
    core/byte.c
    tools/reatore2/reatore2.f
    tools/reatore2/strings.f)

add_executable(re2torea
    core/byte.c
    tools/reatore2/re2torea.f
    tools/reatore2/strings.f)

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
  set(real8 -fdefault-real-8 -fdefault-double-8)
  if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    set(fopts -cpp -std=legacy)
  else()
    set(fopts -cpp -mcmodel=large -std=legacy)
  endif()
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL PGI)
  set(real8 -r8)
  set(fopts -Mpreprocess -mcmodel=medium)
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
  set(real8 -r8)
  set(fopts -fpconstant -fpp -mcmodel=large)
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL XL)
  set(real8 -qrealsize=8 )
  set(fopts -qdpc=e -qsuffix=cpp=f -qsuppress=cmpmsg)
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL Flang)
  set(real8 -r8)
  set(fopts -cpp -std=legacy)
else()
  message(FATAL_ERROR "Specified compiler (${CMAKE_Fortran_COMPILER_ID}) is not supported by Nek5000")
endif()

foreach(target libnek5000 nek5000 genbox genmap gmsh2nek reatore2 re2torea)
  target_compile_options(${target} PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:>${fopts};${real8})
endforeach()

foreach(target postx postex)
  target_compile_options(${target} PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:>${fopts})
endforeach()

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
  # For both libnek5000 and tools
  add_definitions(-DUNDERSCORE)
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

# For tools only
foreach(target genbox gencon genmap gmsh2nek postx postex reatore2 re2torea)
  if(NOT MAXNEL)
    set(MAXNEL 150000)
  endif()
  target_compile_definitions(${target} PRIVATE -DMAXNEL=${MAXNEL})
endforeach()


# =============================================================================
# Link to 3rd-party libraries
# =============================================================================

# -----------------------------------------------------------------------------
# MPI
# -----------------------------------------------------------------------------

if(MPI)
  # TODO: Check MPI library using find_package(MPI)
  set(MPI_C_COMPILER ${CMAKE_C_COMPILER})
  set(MPI_Fortran_COMPILER ${CMAKE_Fortran_COMPILER})
endif()

# -----------------------------------------------------------------------------
# gslib
# -----------------------------------------------------------------------------

add_subdirectory(3rd_party/gslib/gslib/)
target_include_directories(libnek5000 PUBLIC 3rd_party/gslib/gslib/include)
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

# -----------------------------------------------------------------------------
# X11 (for postnek)
# -----------------------------------------------------------------------------

find_package(X11)
if(X11_FOUND)
  foreach(target postx postex)
    target_include_directories(${target} PRIVATE ${X11_INCLUDE_DIR})
    target_link_libraries(${target} PRIVATE ${X11_LIBRARIES})
  endforeach()
else()
  message(WARNING "X11 was not found.  postnek will be unavailable.")
endif()

# -----------------------------------------------------------------------------
# HYPRE
# -----------------------------------------------------------------------------

if(HYPRE)
  set(HYPRE_BIGINT ON CACHE BOOL "Use long long int for HYPRE_Int" FORCE)
  set(HYPRE_USING_FEI OFF CACHE BOOL "Use FEI for HYPRE" FORCE)
  add_subdirectory(3rd_party/hypre/hypre/src)
  target_include_directories(libnek5000 PUBLIC
      ${CMAKE_CURRENT_BINARY_DIR}/3rd_party/hypre/hypre/src
      3rd_party/hypre/hypre/src
      3rd_party/hypre/hypre/src/IJ_mv
      3rd_party/hypre/hypre/src/krylov
      3rd_party/hypre/hypre/src/multivector
      3rd_party/hypre/hypre/src/parcsr_ls
      3rd_party/hypre/hypre/src/parcsr_mv
      3rd_party/hypre/hypre/src/seq_mv
      3rd_party/hypre/hypre/src/utilities)
  target_link_libraries(libnek5000 PUBLIC HYPRE)
endif()

if(CVODE)
  set(BUILD_ARKODE OFF CACHE BOOL "Build the ARKODE library for CVODE" FORCE)
  set(BUILD_CVODES OFF CACHE BOOL "Build the CVODES library for CVODE" FORCE)
  set(BUILD_IDA OFF CACHE BOOL "Build the IDA library for CVODE" FORCE)
  set(BUILD_IDAS OFF CACHE BOOL "Build the IDAS library for CVODE" FORCE)
  set(BUILD_KINSOL OFF CACHE BOOL "Build the KINSOL library for CVODE" FORCE)
  set(EXAMPLES_INSTALL OFF CACHE BOOL "Install example files for CVODE" FORCE)
  set(EXAMPLES_ENABLE OFF CACHE BOOL "Build the SUNDIALS examples for CVODE" FORCE)
  set(MPI_ENABLE ON CACHE BOOL "Enable MPI for CVODE" FORCE)
  set(FCMIX_ENABLE ON CACHE BOOL "Enable Fortran-C support for CVODE" FORCE)
  set(MPI_MPICC "${CMAKE_C_COMPILER}" CACHE STRING "MPI C Compiler for CVODE" FORCE)
  set(MPI_MPIF77 "${CMAKE_Fortran_COMPILER}" CACHE STRING "MPI Fortran Compiler for CVODE" FORCE)

  add_subdirectory(3rd_party/cvode/sundials-2.7.0)

  target_include_directories(libnek5000 PUBLIC
      ${CMAKE_CURRENT_BINARY_DIR}/3rd_party/cvode/sundials-2.7.0/include/sundials
      3rd_party/cvode/sundials-2.7.0/include/cvode
      3rd_party/cvode/sundials-2.7.0/include/nvector
      3rd_party/cvode/sundials-2.7.0/src/cvode/fcmix)

  if(MPI)
    target_link_libraries(libnek5000 PUBLIC
        sundials_fcvode_static
        sundials_cvode_static
        sundials_fnvecparallel_static
        sundials_nvecparallel_static)
  else()
    target_link_libraries(libnek5000 PUBLIC
        sundials_fcvode_static
        sundials_cvode_static
        sundials_fnvecserial_static
        sundials_nvecserial_static)
  endif()
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
