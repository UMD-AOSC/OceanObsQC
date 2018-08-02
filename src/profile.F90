MODULE profile_mod
  
  IMPLICIT NONE
  PRIVATE


  TYPE, PUBLIC :: profile
     REAL(8) :: lat
     REAL(8) :: lon
     REAL :: time
     REAL, ALLOCATABLE :: depth(:)
     REAL, ALLOCATABLE :: salt(:)
     REAL, ALLOCATABLE :: temp(:)
  END TYPE profile
  
END MODULE profile_mod


! Define a dynamic array "ftlDynArrayProfile" that holds objects of type "Profile"
#define FTL_TEMPLATE_TYPE profile
#define FTL_TEMPLATE_TYPE_IS_DERIVED
#define FTL_TEMPLATE_TYPE_NAME Profile
#define FTL_TEMPLATE_TYPE_MODULE profile_mod
#define FTL_INSTANTIATE_TEMPLATE
#include "ftlDynArray.F90_template"
