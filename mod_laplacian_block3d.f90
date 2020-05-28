!Laplacian für 3D-Giter, die Sub-Domains sind geblocked
!Stencil from High-Order Compact Finite Diference Schemes for Computational Mechanics
!William Frederick Spotz, B.S., M.S

module mod_laplacian_block3d

  use mod_types !Fortran Modul mit der Typedefiniton für Real und Integer
  use OMP_LIB !We use OpenMP threads
  use, intrinsic :: ISO_C_BINDING !C-Binding
  
  implicit none

  !Maybe Todo: use var of type_dom  
  integer(kind=ik), private :: num_ii !Anzahl der inneren Gebietszellen in Y-Richtung
  integer(kind=ik), private :: num_jj !Anzahl der inneren Gebietszellen in X-Richtung 
  integer(kind=ik), private :: num_kk !Anzahl der inneren Gebietszellen in Z-Richtung
  integer(kind=ik), private :: num_blk_ii !Anzahl der Bloecke in Y-Richtung
  integer(kind=ik), private :: num_blk_jj !Anzahl der Bloecke in X-Richtung
  integer(kind=ik), private :: num_blk_kk !Anzahl der Bloecke in Z-Richtung
  integer(kind=ik), private :: blk_num_ii !Anzahl der inneren Blockzellen in Y-Richtung
  integer(kind=ik), private :: blk_num_jj !Anzahl der inneren Blockzellen in X-Richtung
  integer(kind=ik), private :: blk_num_kk !Anzahl der inneren Blockzellen in Z-Richtung
    
  real(kind=rk), allocatable, dimension(:,:,:,:,:,:), public :: uu_block3d ! Vektor uu_block3d
  real(kind=rk), allocatable, dimension(:,:,:,:,:,:), public :: dd_block3d ! Vector dd_block3d
contains

!Prüfe die Blocking-Parameter
!Wenn die Parameter nicht passen, versuche die passende Blockgroesse zu finden
function check_blocking_block3d(domain_decomp) result(ok)
  type(type_dom), intent(inout) :: domain_decomp
  integer(kind=ik) :: num_xx, num_yy, num_zz,nn,start_blk,end_blk
  logical(kind=lk) :: ok,ok_xx,ok_yy,ok_zz
  num_xx=domain_decomp%num_blk_jj*domain_decomp%blk_num_jj
  num_yy=domain_decomp%num_blk_ii*domain_decomp%blk_num_ii
  num_zz=domain_decomp%num_blk_kk*domain_decomp%blk_num_kk
  ok_xx = (num_xx .eq. domain_decomp%num_jj)
  ok_yy = (num_yy .eq. domain_decomp%num_ii)
  ok_zz = (num_zz .eq. domain_decomp%num_kk)
  if( .not. ok_xx ) then
    start_blk=int(domain_decomp%blk_num_jj*1.2,kind=ik)
    end_blk= int(domain_decomp%blk_num_jj*0.8,kind=ik)
    do nn=start_blk,end_blk, -1
      domain_decomp%num_blk_jj=domain_decomp%num_jj/nn
      num_xx=domain_decomp%num_blk_jj*nn
      if(num_xx .eq. domain_decomp%num_jj) then
        domain_decomp%blk_num_jj=nn
        ok_xx=.true.
        write(*,'(A,I0)') "Neue Größe für X-Block:",domain_decomp%blk_num_jj
        exit
      endif
    end do
  end if
  if( ok_xx .and. (.not. ok_yy) ) then
    start_blk=int(domain_decomp%blk_num_ii*1.2,kind=ik)
    end_blk=int(domain_decomp%blk_num_ii*0.8,kind=ik)
    do nn=start_blk,end_blk, -1
      domain_decomp%num_blk_ii=domain_decomp%num_ii/nn
      num_yy=domain_decomp%num_blk_ii*nn
      if(num_yy .eq. domain_decomp%num_ii) then
        domain_decomp%blk_num_ii=nn
        ok_yy=.true.
        write(*,'(A,I0)') "Neue Größe für Y-Block:",domain_decomp%blk_num_ii
        exit
      endif
    end do
  end if
  if( ok_xx .and. ok_yy .and. (.not. ok_zz) ) then
    start_blk=int(domain_decomp%blk_num_kk*1.2,kind=ik)
    end_blk=int(domain_decomp%blk_num_kk*0.8,kind=ik)
    do nn=start_blk,end_blk, -1
      domain_decomp%num_blk_kk=domain_decomp%num_kk/nn
      num_zz=domain_decomp%num_blk_kk*nn
      if(num_zz .eq. domain_decomp%num_kk) then
        domain_decomp%blk_num_kk=nn
        ok_zz=.true.
        write(*,'(A,I0)') "Neue Größe für Z-Block:",domain_decomp%blk_num_kk
        exit
      endif
    end do
  end if
  ok = ok_xx .and. ok_yy .and. ok_zz

end function check_blocking_block3d

!Allokierung für die Felder uu_block3d und dd_block3d;
!UU-Array hat wegen der Halo-Daten mehr Elementen als DD-Array
subroutine alloc_mem_block3d(domain_decomp)
  type(type_dom), intent(IN) :: domain_decomp
  integer :: sys_stat
  
  call laplacian_set_param_block3d(domain_decomp)
  if(allocated(uu_block3d)) deallocate(uu_block3d)
  if(allocated(dd_block3d)) deallocate(dd_block3d)

  allocate(uu_block3d(0:blk_num_ii+1,&
            & 0:blk_num_jj+1,&
            & 0:blk_num_kk+1,&
            & 1:num_blk_ii,&
            & 1:num_blk_jj,&
            & 1:num_blk_kk),stat=sys_stat)
  if(sys_stat .ne. 0) then
    write(*,'(A,I0)') "Error: allocate(uu_block3d) returns ",sys_stat
    stop
  endif
  allocate(dd_block3d(1:blk_num_ii,1:blk_num_jj,1:blk_num_kk,1:num_blk_ii,1:num_blk_jj,1:num_blk_kk),stat=sys_stat)
  if(sys_stat .ne. 0) then
    write(*,'(A,I0)') "Error: allocate(dd_block3d) returns ",sys_stat
    stop
  endif
  write(*,'(A,I0)') "Info:Block 3D  allocated(blocked uu_block3d) GiB:", sizeof(uu_block3d)/2**30
  write(*,'(A,I0)') "Info:Block 3D  allocated(blocked dd_block3d) GiB:", sizeof(dd_block3d)/2**30
end subroutine alloc_mem_block3d

!Speicherfreigabe für die Felder uu_block3d und dd_block3d;
subroutine dealloc_mem_block3d()

  if(allocated(uu_block3d)) deallocate(uu_block3d)
  if(allocated(dd_block3d)) deallocate(dd_block3d)
  call laplacian_reset_param_block3d()

end subroutine dealloc_mem_block3d

!Initialisierung des Felders uu_block3d mit f=x*y^2+z^3) 
subroutine init_uu_block3d()
  integer(kind=ik) :: ii, jj, kk
  integer(kind=ik) :: cell_blk_ii, cell_blk_jj, cell_blk_kk ! Index in X-,Y- und Z- Richtung des ersten inneren Zelle eines Blockes
  integer(kind=ik) :: blk_ii, blk_jj, blk_kk
  integer(kind=ik) :: ii_end, jj_end, kk_end
  integer(kind=ik) :: num_blks,blk
  real(kind=rk) :: hh,xx,yy,zz,val 
    
  ii_end = blk_num_ii+1_ik
  jj_end = blk_num_jj+1_ik
  kk_end = blk_num_kk+1_ik
  hh = 1.0_rk/real(num_ii,kind=rk)
  num_blks=num_blk_jj*num_blk_ii*num_blk_kk
  !Iteriere uber die Bloecke
  !$omp parallel do default(none) &
    !$omp& private(ii,jj,kk, &
    !$omp& xx,yy,zz,val, &
    !$omp& blk_kk,blk_jj,blk_ii,blk, &
    !$omp& cell_blk_kk,cell_blk_jj,cell_blk_ii) &
    !$omp& shared(uu_block3d,blk_num_kk,blk_num_jj,blk_num_ii,&
    !$omp& num_blk_kk,num_blk_jj,num_blk_ii,num_blks, &
    !$omp& ii_end,jj_end,kk_end,hh)
  do blk=1,num_blks
    blk_ii=mod(blk-1,num_blk_ii)
    blk_jj=mod((blk-1-blk_ii)/num_blk_ii,num_blk_jj)
    blk_kk=(blk-1-blk_ii-blk_jj*num_blk_ii)/(num_blk_ii*num_blk_jj)
    blk_ii=blk_ii+1
    blk_jj=blk_jj+1
    blk_kk=blk_kk+1
    cell_blk_kk=blk_num_kk*(blk_kk-1_ik)!Index der ersten Halo-Zelle der Blöcken mit blk_kk=1
    cell_blk_jj=blk_num_jj*(blk_jj-1_ik)
    cell_blk_ii=blk_num_ii*(blk_ii-1_ik)
    !Iteriere über die Zellen im Block
    do kk=0, kk_end
      do jj=0, jj_end
        do ii=0, ii_end
          xx = hh*(cell_blk_jj+jj)
          yy = hh*(cell_blk_ii+ii)
          zz = hh*(cell_blk_kk+kk)
          val = xx*yy*yy+zz*zz*zz
          uu_block3d(ii,jj,kk,blk_ii,blk_jj,blk_kk) = val
        end do
      end do
    end do
  end do
end subroutine init_uu_block3d

!Example for cache blocking 
subroutine laplacian_block3d()

  integer(kind=ik) :: ii, jj, kk
  integer(kind=ik) :: blk_ii, blk_jj, blk_kk
  integer(kind=ik) :: num_blks
  integer(kind=ik) :: blk
  real(kind=rk) :: tmp1, tmp2, tmp3
  real(kind=rk) :: tmp4, tmp5, tmp6
  real(kind=rk) :: tmp7, tmp8, tmp9
  real(kind=rk) :: step_inv
  real(kind=rk) :: hh
  hh=1.0/real(num_ii,kind=rk)
  step_inv=1.0/(30.0*hh*hh)
  num_blks=num_blk_jj*num_blk_ii*num_blk_kk
  !Iteriere uber die Bloecke und innere Zellen
  !$omp parallel do default(none) &
    !$omp& private(ii,jj,kk,tmp1,tmp2,tmp3, &
    !$omp& tmp4,tmp5,tmp6,tmp7,tmp8,tmp9, &
    !$omp& blk_kk,blk_jj,blk_ii,blk) &
    !$omp& shared(uu_block3d,dd_block3d,blk_num_kk,blk_num_jj,blk_num_ii,&
    !$omp& num_blk_kk,num_blk_jj,num_blk_ii,step_inv,num_blks)
  do blk=1,num_blks
    blk_ii=mod(blk-1,num_blk_ii)
    blk_jj=mod((blk-1-blk_ii)/num_blk_ii,num_blk_jj)
    blk_kk=(blk-1-blk_ii-blk_jj*num_blk_ii)/(num_blk_ii*num_blk_jj)
    blk_ii=blk_ii+1
    blk_jj=blk_jj+1
    blk_kk=blk_kk+1
!  do blk_kk=1, num_blk_kk
!    do blk_jj=1, num_blk_jj
!      do blk_ii=1, num_blk_ii
        !Iteriere über die Zellen im Block
        do kk=1, blk_num_kk
          do jj=1, blk_num_jj
            !DIR$ VECTOR ALIGNED 
            !DIR$ UNROLL=4
            do ii=1, blk_num_ii
              tmp1 = uu_block3d(ii-1,jj-1,kk-1,blk_ii,blk_jj,blk_kk)+3.0*uu_block3d(ii,jj-1,kk-1,blk_ii,blk_jj,blk_kk)+uu_block3d(ii+1,jj-1,kk-1,blk_ii,blk_jj,blk_kk)
              tmp2 = 3.0*uu_block3d(ii-1,jj,kk-1,blk_ii,blk_jj,blk_kk)+14.0*uu_block3d(ii,jj,kk-1,blk_ii,blk_jj,blk_kk)+3.0*uu_block3d(ii+1,jj,kk-1,blk_ii,blk_jj,blk_kk)
              tmp3 = uu_block3d(ii-1,jj+1,kk-1,blk_ii,blk_jj,blk_kk)+3.0*uu_block3d(ii,jj+1,kk-1,blk_ii,blk_jj,blk_kk)+uu_block3d(ii+1,jj+1,kk-1,blk_ii,blk_jj,blk_kk)

              tmp4 = 3.0*uu_block3d(ii-1,jj-1,kk,blk_ii,blk_jj,blk_kk)+14.0*uu_block3d(ii,jj-1,kk,blk_ii,blk_jj,blk_kk)+3.0*uu_block3d(ii+1,jj-1,kk,blk_ii,blk_jj,blk_kk)
              tmp5 = 14.0*uu_block3d(ii-1,jj,kk,blk_ii,blk_jj,blk_kk)-128.0*uu_block3d(ii,jj,kk,blk_ii,blk_jj,blk_kk)+14.0*uu_block3d(ii+1,jj,kk,blk_ii,blk_jj,blk_kk)
              tmp6 = 3.0*uu_block3d(ii-1,jj+1,kk,blk_ii,blk_jj,blk_kk)+14.0*uu_block3d(ii,jj+1,kk,blk_ii,blk_jj,blk_kk)+3.0*uu_block3d(ii+1,jj+1,kk,blk_ii,blk_jj,blk_kk)

              tmp7 = uu_block3d(ii-1,jj-1,kk+1,blk_ii,blk_jj,blk_kk)+3.0*uu_block3d(ii,jj-1,kk+1,blk_ii,blk_jj,blk_kk)+uu_block3d(ii+1,jj-1,kk+1,blk_ii,blk_jj,blk_kk)
              tmp8 = 3.0*uu_block3d(ii-1,jj,kk+1,blk_ii,blk_jj,blk_kk)+14.0*uu_block3d(ii,jj,kk+1,blk_ii,blk_jj,blk_kk)+3.0*uu_block3d(ii+1,jj,kk+1,blk_ii,blk_jj,blk_kk)
              tmp9 = uu_block3d(ii-1,jj+1,kk+1,blk_ii,blk_jj,blk_kk)+3.0*uu_block3d(ii,jj+1,kk+1,blk_ii,blk_jj,blk_kk)+uu_block3d(ii+1,jj+1,kk+1,blk_ii,blk_jj,blk_kk)

              dd_block3d(ii,jj,kk,blk_ii,blk_jj,blk_kk) = (tmp1+tmp2+tmp3+tmp4+tmp5+tmp6+tmp7+tmp8+tmp9)*step_inv
            end do
          end do
        end do
 !     end do
 !   end do
 ! end do
  end do

end subroutine laplacian_block3d

!Berechne Laplacian
!Optimierung: Zerlege die innere Schleife in drei Schleifen  
subroutine laplacian_block3d_unroll()

  integer(kind=ik) :: ii, jj, kk
  integer(kind=ik) :: blk_ii, blk_jj, blk_kk
  integer(kind=ik) :: num_blks
  integer(kind=ik) :: blk
  real(kind=rk) :: tmp1, tmp2, tmp3
  real(kind=rk) :: tmp4, tmp5, tmp6
  real(kind=rk) :: tmp7, tmp8, tmp9
  real(kind=rk) :: step_inv
  real(kind=rk) :: hh
  hh=1.0/real(num_ii,kind=rk)
  step_inv=1.0/(30.0*hh*hh)
  num_blks=num_blk_jj*num_blk_ii*num_blk_kk
  !Iteriere uber die Bloecke und innere Zellen
  !$omp parallel do default(none) &
    !$omp& private(ii,jj,kk,tmp1,tmp2,tmp3, &
    !$omp& tmp4,tmp5,tmp6,tmp7,tmp8,tmp9, &
    !$omp& blk_kk,blk_jj,blk_ii,blk) &
    !$omp& shared(uu_block3d,dd_block3d,blk_num_kk,blk_num_jj,blk_num_ii,&
    !$omp& num_blk_kk,num_blk_jj,num_blk_ii,step_inv,num_blks)
  do blk=1,num_blks
    ! Iteriere über die Blocks->zur besseren Parallelesierung 
    ! iteriere über eine Schleife statt drei Schleifen 
    !(sind unten auskommentiert)
    blk_ii=mod(blk-1,num_blk_ii)
    blk_jj=mod((blk-1-blk_ii)/num_blk_ii,num_blk_jj)
    blk_kk=(blk-1-blk_ii-blk_jj*num_blk_ii)/(num_blk_ii*num_blk_jj)
    blk_ii=blk_ii+1
    blk_jj=blk_jj+1
    blk_kk=blk_kk+1
!  do blk_kk=1, num_blk_kk
!    do blk_jj=1, num_blk_jj
!      do blk_ii=1, num_blk_ii
        !Iteriere über die Zellen im Block
        do kk=1, blk_num_kk
          do jj=1, blk_num_jj
            !DIR$ VECTOR ALIGNED 
            !DIR$ UNROLL=4
            do ii=1, blk_num_ii
              tmp1 = uu_block3d(ii-1,jj-1,kk-1,blk_ii,blk_jj,blk_kk)+3.0*uu_block3d(ii,jj-1,kk-1,blk_ii,blk_jj,blk_kk)+uu_block3d(ii+1,jj-1,kk-1,blk_ii,blk_jj,blk_kk)
              tmp2 = 3.0*uu_block3d(ii-1,jj,kk-1,blk_ii,blk_jj,blk_kk)+14.0*uu_block3d(ii,jj,kk-1,blk_ii,blk_jj,blk_kk)+3.0*uu_block3d(ii+1,jj,kk-1,blk_ii,blk_jj,blk_kk)
              tmp3 = uu_block3d(ii-1,jj+1,kk-1,blk_ii,blk_jj,blk_kk)+3.0*uu_block3d(ii,jj+1,kk-1,blk_ii,blk_jj,blk_kk)+uu_block3d(ii+1,jj+1,kk-1,blk_ii,blk_jj,blk_kk)
              dd_block3d(ii,jj,kk,blk_ii,blk_jj,blk_kk) = (tmp1+tmp2+tmp3)*step_inv
            end do
          end do
          do jj=1, blk_num_jj
            !DIR$ VECTOR ALIGNED 
            !DIR$ UNROLL=4
            do ii=1, blk_num_ii
              tmp4 = 3.0*uu_block3d(ii-1,jj-1,kk,blk_ii,blk_jj,blk_kk)+14.0*uu_block3d(ii,jj-1,kk,blk_ii,blk_jj,blk_kk)+3.0*uu_block3d(ii+1,jj-1,kk,blk_ii,blk_jj,blk_kk)
              tmp5 = 14.0*uu_block3d(ii-1,jj,kk,blk_ii,blk_jj,blk_kk)-128.0*uu_block3d(ii,jj,kk,blk_ii,blk_jj,blk_kk)+14.0*uu_block3d(ii+1,jj,kk,blk_ii,blk_jj,blk_kk)
              tmp6 = 3.0*uu_block3d(ii-1,jj+1,kk,blk_ii,blk_jj,blk_kk)+14.0*uu_block3d(ii,jj+1,kk,blk_ii,blk_jj,blk_kk)+3.0*uu_block3d(ii+1,jj+1,kk,blk_ii,blk_jj,blk_kk)
              dd_block3d(ii,jj,kk,blk_ii,blk_jj,blk_kk) = dd_block3d(ii,jj,kk,blk_ii,blk_jj,blk_kk)+(tmp4+tmp5+tmp6)*step_inv
            end do
          end do
          do jj=1, blk_num_jj
            !DIR$ VECTOR ALIGNED 
            !DIR$ UNROLL=4
            do ii=1, blk_num_ii
              tmp7 = uu_block3d(ii-1,jj-1,kk+1,blk_ii,blk_jj,blk_kk)+3.0*uu_block3d(ii,jj-1,kk+1,blk_ii,blk_jj,blk_kk)+uu_block3d(ii+1,jj-1,kk+1,blk_ii,blk_jj,blk_kk)
              tmp8 = 3.0*uu_block3d(ii-1,jj,kk+1,blk_ii,blk_jj,blk_kk)+14.0*uu_block3d(ii,jj,kk+1,blk_ii,blk_jj,blk_kk)+3.0*uu_block3d(ii+1,jj,kk+1,blk_ii,blk_jj,blk_kk)
              tmp9 = uu_block3d(ii-1,jj+1,kk+1,blk_ii,blk_jj,blk_kk)+3.0*uu_block3d(ii,jj+1,kk+1,blk_ii,blk_jj,blk_kk)+uu_block3d(ii+1,jj+1,kk+1,blk_ii,blk_jj,blk_kk)
              dd_block3d(ii,jj,kk,blk_ii,blk_jj,blk_kk) = dd_block3d(ii,jj,kk,blk_ii,blk_jj,blk_kk)+(tmp7+tmp8+tmp9)*step_inv
            end do
          end do
        end do
 !     end do
 !   end do
 ! end do
  end do
end subroutine laplacian_block3d_unroll

!Berechne Absolute und relative Fehlernorm der Lösung
!Theoretische Lösung ist 2.0_rk*xx+6.0*zz
! ||vv||_2
! ||dd_block3d||_2
! ||dd_block3d-vv||_2
! ||dd_block3d-vv||_2/||vv||_2
function laplacian_check_block3d() result(norm)
  integer(kind=ik) :: ii, jj, kk
  integer(kind=ik) :: blk_ii, blk_jj, blk_kk
  integer(kind=ik) :: cell_blk_ii, cell_blk_jj, cell_blk_kk ! Index in X-,Y- und Z- Richtung des ersten inneren Zelle eines Blockes
  real(kind=rk), dimension(4) :: norm
  real(kind=rk) normerr, diff, normxx, normdd_block3d
  real(kind=rk) :: step_inv
  real(kind=rk) :: hh,xx,zz,vv
  integer(kind=ik) :: num_blks,blk
  hh=1.0/real(num_ii,kind=rk)
  step_inv=1.0/(30.0*hh*hh)
  
  normerr=0.0_rk
  normxx=0.0_rk
  normdd_block3d=0.0_rk
  norm=0.0_rk
  num_blks=num_blk_jj*num_blk_ii*num_blk_kk
  !Iteriere uber die Bloecke
  !$omp parallel do reduction(+:normxx) &
    !$omp& reduction(+:normdd_block3d) & 
    !$omp& reduction(+:normerr) & 
    !$omp& default(none) &
    !$omp& private(ii,jj,kk, &
    !$omp& blk_kk,blk_jj,blk_ii,blk,diff, &
    !$omp& zz,xx,vv, &
    !$omp& cell_blk_kk,cell_blk_jj,cell_blk_ii) &
    !$omp& shared(uu_block3d,blk_num_kk,blk_num_jj,blk_num_ii,&
    !$omp& num_blk_kk,num_blk_jj,num_blk_ii,num_blks,dd_block3d,&
    !$omp& hh)
  do blk=1,num_blks
    blk_ii=mod(blk-1,num_blk_ii)
    blk_jj=mod((blk-1-blk_ii)/num_blk_ii,num_blk_jj)
    blk_kk=(blk-1-blk_ii-blk_jj*num_blk_ii)/(num_blk_ii*num_blk_jj)
    blk_ii=blk_ii+1
    blk_jj=blk_jj+1
    blk_kk=blk_kk+1
    cell_blk_kk=blk_num_kk*(blk_kk-1_ik)!Index der ersten Halo-Zelle der Blöcken mit blk_kk=1
    cell_blk_jj=blk_num_jj*(blk_jj-1_ik)
    cell_blk_ii=blk_num_ii*(blk_ii-1_ik)
    !Iteriere über die inneren Zellen im Block
    do kk=1, blk_num_kk
      do jj=1, blk_num_jj
        do ii=1, blk_num_ii
          xx = hh*(cell_blk_jj+jj)
          zz = hh*(cell_blk_kk+kk)
          vv = 2.0_rk*xx+6*zz
          normxx=normxx + vv*vv
          normdd_block3d=normdd_block3d + dd_block3d(ii,jj,kk,blk_ii,blk_jj,blk_kk)*dd_block3d(ii,jj,kk,blk_ii,blk_jj,blk_kk)
          diff=dd_block3d(ii,jj,kk,blk_ii,blk_jj,blk_kk)-vv
          normerr=normerr+diff*diff
        end do
      end do
    end do
  end do
  normerr=SQRT(normerr)
  normxx=SQRT(normxx)
  normdd_block3d=SQRT(normdd_block3d)
  norm(1)=normxx
  norm(2)=normdd_block3d
  norm(3)=normerr
  if ( normxx .gt. 1.0e-9 ) then
    norm(4)=normerr/normxx
  else
    norm(4)=-999.0
  endif
end function laplacian_check_block3d

!Kopiere Variablen vom Typ type_dom 
subroutine laplacian_set_param_block3d(parinput)
  type(type_dom), intent(IN) :: parinput
    num_ii = parinput%num_ii
    num_jj = parinput%num_jj
    num_kk = parinput%num_kk
    num_blk_ii = parinput%num_blk_ii
    num_blk_jj = parinput%num_blk_jj
    num_blk_kk = parinput%num_blk_kk
    blk_num_ii = parinput%blk_num_ii
    blk_num_jj = parinput%blk_num_jj
    blk_num_kk = parinput%blk_num_kk

end subroutine laplacian_set_param_block3d

!Test Variable vom Typ type_dom 
subroutine laplacian_check_param_block3d() 
  if(.not. &
      (num_ii .eq. num_blk_ii*blk_num_ii .and. &
     & num_jj .eq. num_blk_jj*blk_num_jj .and. &
     & num_kk .eq. num_blk_kk*blk_num_kk)) then
    write(*,'(A,I0)') &
    "Error: Check input parameters"
  stop
  endif

end subroutine laplacian_check_param_block3d

!Reset Variable vom Typ type_dom 
subroutine laplacian_reset_param_block3d()
  
  num_ii = -1_ik
  num_jj = -1_ik
  num_kk = -1_ik
  num_blk_ii = -1_ik
  num_blk_jj = -1_ik
  num_blk_kk = -1_ik
  blk_num_ii = -1_ik
  blk_num_jj = -1_ik
  blk_num_kk = -1_ik

end subroutine laplacian_reset_param_block3d 

!Print Variable vom Typ type_dom 
subroutine laplacian_print_param_block3d
 
  write(*,'(A,I0)') "num_ii: ", num_ii
  write(*,'(A,I0)') "num_jj: ", num_jj
  write(*,'(A,I0)') "num_kk: ", num_kk
  write(*,'(A,I0)') "num_blk_ii: ", num_blk_ii
  write(*,'(A,I0)') "num_blk_jj: ", num_blk_jj
  write(*,'(A,I0)') "num_blk_kk: ", num_blk_kk
  write(*,'(A,I0)') "blk_num_ii: ", blk_num_ii
  write(*,'(A,I0)') "blk_num_jj: ", blk_num_jj
  write(*,'(A,I0)') "blk_num_kk: ", blk_num_kk

end subroutine laplacian_print_param_block3d

end module mod_laplacian_block3d 
