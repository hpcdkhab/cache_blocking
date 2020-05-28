!Fortran Modul mit der Typedefiniton für Real und Integer
module mod_types

  use, intrinsic :: ISO_C_BINDING !C-Binding
  implicit none

  integer, parameter :: rk = 8  ! kind for real
  integer, parameter :: ik = 8  ! kind for integer
  integer, parameter :: lk = 4  ! kind for logical

  !Gitterdefinition mit den Distributionsparametern
  type, bind(c) :: type_dom
    integer(kind=ik) :: num_ii !Anzahl der inneren Gebietszellen in X-Richtung
    integer(kind=ik) :: num_jj !Anzahl der inneren Gebietszellen in Y-Richtung 
    integer(kind=ik) :: num_kk !Anzahl der inneren Gebietszellen in Z-Richtung
    integer(kind=ik) :: num_blk_ii !Anzahl der Bloecke in X-Richtung
    integer(kind=ik) :: num_blk_jj !Anzahl der Bloecke in Y-Richtung
    integer(kind=ik) :: num_blk_kk !Anzahl der Bloecke in Z-Richtung
    integer(kind=ik) :: blk_num_ii !Anzahl der inneren Blockzellen in X-Richtung
    integer(kind=ik) :: blk_num_jj !Anzahl der inneren Blockzellen in Y-Richtung
    integer(kind=ik) :: blk_num_kk !Anzahl der inneren Blockzellen in Z-Richtung
  end type


end module mod_types
