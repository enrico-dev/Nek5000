# =============================================================================
#
# mkuserfile.cmake: Generates a .f file from a .usr file.
# 
# A direct port of Nek5000/core/mkuserfile. This is intended to be the COMMAND 
# for a call to add_custom_command() that generates the .usr file. It can also 
# be run directly from the command-line:
#     $ cmake -DCASENAME=casename [-DCVODE] [-DCMT] -P mkuserfile.cmake
#
# =============================================================================

# =============================================================================
# Read .usr file
# =============================================================================

file(READ ${INFILE_DIR}/${CASENAME}.usr usr_str)
string(TOLOWER "${usr_str}" usr_str_lower)

# =============================================================================
# Fail if userq is defined.  It must be specifically defined for coupling
# =============================================================================

if(usr_str_lower MATCHES "subroutine.*userq")
  message(FATAL_ERROR "userq() was defined in ${CASENAME}.usr. For coupling, userq() is \
  predifined and you cannot redefine your own.  Remove the definition of userq() in \
  ${CASENAME}.usr and run make again.")
endif()

# =============================================================================
# Add standard subroutines
# =============================================================================

if(NOT usr_str_lower MATCHES "subroutine.*uservp")
  string(CONCAT usr_str "${usr_str}"
"
c automatically added by makenek
      subroutine uservp(ix,iy,iz,eg)

      return
      end
")
endif()

if(NOT usr_str_lower MATCHES "subroutine.*userf")
  string(CONCAT usr_str "${usr_str}"
"
c automatically added by makenek
      subroutine userf(ix,iy,iz,eg)

      return
      end
")
endif()

if(NOT usr_str_lower MATCHES "subroutine.*useric")
  string(CONCAT usr_str "${usr_str}"
"
c automatically added by makenek
      subroutine useric(ix,iy,iz,eg)

      return
      end
")
endif()

if(NOT usr_str_lower MATCHES "subroutine.*userbc")
  string(CONCAT usr_str "${usr_str}"
"
c automatically added by makenek
      subroutine userbc(ix,iy,iz,iside,eg)

      return
      end
")
endif()

if(NOT usr_str_lower MATCHES "subroutine.*userchk")
  string(CONCAT usr_str "${usr_str}"
"
c automatically added by makenek
      subroutine userchk()

      return
      end
")
endif()

if(NOT usr_str_lower MATCHES "subroutine.*usrdat0")
  string(CONCAT usr_str "${usr_str}"
"
c automatically added by makenek
      subroutine usrdat0()

      return
      end
")
endif()

if(NOT usr_str_lower MATCHES "subroutine.*usrdat")
  string(CONCAT usr_str "${usr_str}"
"
c automatically added by makenek
      subroutine usrdat()

      return
      end
")
endif()

if(NOT usr_str_lower MATCHES "subroutine.*usrdat2")
  string(CONCAT usr_str "${usr_str}"
"
c automatically added by makenek
      subroutine usrdat2()

      return
      end
")
endif()

if(NOT usr_str_lower MATCHES "subroutine.*usrdat3")
  string(CONCAT usr_str "${usr_str}"
"
c automatically added by makenek
      subroutine usrdat3

      return
      end
")
endif()

if(NOT usr_str_lower MATCHES "subroutine.*usrsetvert")
  string(CONCAT usr_str "${usr_str}"
"
c automatically added by makenek
      subroutine usrsetvert(glo_num,nel,nx,ny,nz) ! to modify glo_num
      integer*8 glo_num(1)

      return
      end
")
endif()

if(NOT usr_str_lower MATCHES "subroutine.*userqtl")
  string(CONCAT usr_str "${usr_str}"
"
c automatically added by makenek
      subroutine userqtl

      call userqtl_scig

      return
      end
")
endif()

# =============================================================================
# Add CVODE subroutines
# =============================================================================

if(CVODE)

  if(NOT usr_str_lower MATCHES "include.*cvode_aux.*h")
    string(CONCAT usr_str "${usr_str}" 
"
c automatically added by makenek
\#include \"cvode_aux.h\"
"
    )
  endif()

  if(NOT usr_str_lower MATCHES "include.*cvode_jtimes.*h")
    string(CONCAT usr_str "${usr_str}" 
"
c automatically added by makenek
\#include \"cvode_jtimes.h\"
"
    )
  endif()

  if(NOT usr_str_lower MATCHES "include.*cvode_preco.*h")
    string(CONCAT usr_str "${usr_str}" 
"
c automatically added by makenek
\#include \"cvode_preco_dummy.h\"
"
    )
  endif()

endif()


# =============================================================================
# Write .f file
# =============================================================================

file(WRITE ${OUTFILE_DIR}/${CASENAME}.f ${usr_str})
