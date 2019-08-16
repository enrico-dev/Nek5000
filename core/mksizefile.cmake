# =============================================================================
# Read SIZE file
# =============================================================================

file(READ SIZE size_str)
file(WRITE SIZE.bk "${size_str}")
string(TOLOWER "${size_str}" size_str_lower)

# =============================================================================
# Tweak all SIZE files
# =============================================================================

if(NOT size_str_lower MATCHES "lelr")
  string(CONCAT size_str "${size_str}"
"
c automatically added by cmake
      integer lelr
      parameter (lelr=lelt) ! max number of local elements per restart file
")
endif()

if(NOT size_str_lower MATCHES "ldimt_proj")
  string(CONCAT size_str "${size_str}"
"
c automatically added by cmake
      integer ldimt_proj
      parameter(ldimt_proj=1) ! max auxiliary fields residual projection
")
endif()

# =============================================================================
# Tweak legacy SIZE files
# =============================================================================

if(NOT size_str_lower MATCHES "SIZE.inc")
  message(WARNING "Deprecated SIZE file will be unsupported in future release.")

  if(NOT size_str_lower MATCHES "optlevel")
    string(CONCAT size_str "${size_str}"
"
c automatically added by cmake
      integer optlevel,loglevel
      common /lolevels/ optlevel,loglevel
")
  endif()

  if(NOT size_str_lower MATCHES "lxo")
    string(CONCAT size_str "${size_str}"
"
c automatically added by cmake
      parameter(lxo   = lx1) ! max output grid size (lxo>=lx1)
")
  endif()

  if(NOT size_str_lower MATCHES "ax1")
    string(CONCAT size_str "${size_str}"
"
c automatically added by cmake
      integer ax1,ay1,az1,ax2,ay2,az2
      parameter (ax1=lx1,ay1=ly1,az1=lz1,ax2=lx2,ay2=ly2,az2=lz2) ! running averages
")
  endif()

  if(NOT size_str_lower MATCHES "lys=lxs")
    string(REGEX REPLACE "[\r\n]+[^\r\n]+lxs[^\r\n]+[\r\n]+" "" size_str "${size_str}")
    string(CONCAT size_str "${size_str}"
"
c automatically added by cmake
      parameter (lxs=1,lys=lxs,lzs=(lxs-1)*(ldim-2)+1) !New Pressure Preconditioner
")
  endif()

  if(NOT size_str_lower MATCHES "lcvx1")
    string(CONCAT size_str "${size_str}"
"
c automatically added by cmake
      integer lcvx1,lcvy1,lcvz1,lcvelt
      parameter (lcvx1=1,lcvy1=1,lcvz1=1,lcvelt=1) ! cvode arrays
")
  endif()

  if(NOT size_str_lower MATCHES "lfdm")
    string(CONCAT size_str "${size_str}"
"
c automatically added by cmake
      parameter (lfdm=0)  ! == 1 for fast diagonalization method
")
  endif()

  if(NOT size_str_lower MATCHES "nsessmax")
    string(CONCAT size_str "${size_str}"
"
c automatically added by cmake
      parameter (nsessmax=1)  ! max sessions to NEKNEK
")
  endif()

  if(NOT size_str_lower MATCHES "nmaxl_nn")
    string(CONCAT size_str "${size_str}"
"
c automatically added by cmake
      parameter (nmaxl_nn=
     $          min(1+(nsessmax-1)*2*ldim*lxz*lelt,2*ldim*lxz*lelt))
")
  endif()

  if(NOT size_str_lower MATCHES "nfldmax_nn")
    string(CONCAT size_str "${size_str}"
"
c automatically added by cmake
      parameter (nfldmax_nn=
     $          min(1+(nsessmax-1)*(ldim+1+ldimt),ldim+1+ldimt))
")
  endif()

  if(NOT size_str_lower MATCHES "nio")
    string(CONCAT size_str "${size_str}"
"
c automatically added by cmake
      common/IOFLAG/nio  ! for logfile verbosity control
")
  endif()

  if(NOT size_str_lower MATCHES "toteq")
    string(CONCAT size_str "${size_str}"
"
c automatically added by cmake
      integer toteq
      parameter(toteq = 5  ) ! Number of conserved variables
")
  endif()

endif()

file(WRITE SIZE "${size_str}")
