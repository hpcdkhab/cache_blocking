module mod_domain3d

  use mod_types !Fortran Modul mit der Typedefiniton für Real und Integer
  use, intrinsic :: ISO_C_BINDING !C-Binding

  implicit none
  !Gitterdefinition mit den Distributionsparametern
  type, bind(c) :: type_dom
    integer(kind=ik) :: num_ii !Anzahl der inneren Gebietszellen in Y-Richtung
    integer(kind=ik) :: num_jj !Anzahl der inneren Gebietszellen in X-Richtung 
    integer(kind=ik) :: num_kk !Anzahl der inneren Gebietszellen in Z-Richtung
    integer(kind=ik) :: num_blk_ii !Anzahl der Bloecke in Y-Richtung
    integer(kind=ik) :: num_blk_jj !Anzahl der Bloecke in X-Richtung
    integer(kind=ik) :: num_blk_kk !Anzahl der Bloecke in Z-Richtung
    integer(kind=ik) :: blk_num_ii !Anzahl der inneren Blockzellen in Y-Richtung
    integer(kind=ik) :: blk_num_jj !Anzahl der inneren Blockzellen in X-Richtung
    integer(kind=ik) :: blk_num_kk !Anzahl der inneren Blockzellen in Z-Richtung
  end type type_dom

end module mod_domain3d 
