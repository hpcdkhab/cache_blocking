!Laplacian für 3D-Giter
!Stencil from High-Order Compact Finite Diference Schemes for Computational Mechanics
!William Frederick Spotz, B.S., M.S

module mod_laplacian3d

  use mod_types !Fortran Modul mit der Typedefiniton für Real und Integer
  use OMP_LIB !We use OpenMP threads
  implicit none

  integer(kind=ik), private :: num_ii !Anzahl der inneren Zellen in Y-Richtung
  integer(kind=ik), private :: num_jj !Anzahl der inneren Zellen in X-Richtung 
  integer(kind=ik), private :: num_kk !Anzahl der inneren Zellen in Z-Richtung

  real(kind=rk), allocatable, dimension(:,:,:), public :: uu3d !Vektor uu3d ist aequivalent zur Funktion F(x,y,z)
  real(kind=rk), allocatable, dimension(:,:,:), public :: dd3d !Vektor L2(F)=d2F/dx2+d2F/dy2+d2F/dz2

contains

!Speicherreservierung für die Felder uu3d und dd3d;
subroutine alloc_mem3d(num_xx, num_yy, num_zz)
  integer(kind=ik), intent(IN) :: num_xx
  integer(kind=ik), intent(IN) :: num_yy
  integer(kind=ik), intent(IN) :: num_zz
  integer :: sys_stat !

  if(allocated(uu3d)) deallocate(uu3d)
  if(allocated(dd3d)) deallocate(dd3d)

  allocate(uu3d(0:num_yy+1,0:num_xx+1,0:num_zz+1),stat=sys_stat)
  if(sys_stat .ne. 0) then
    write(*,'(A,I0)') "Error: allocate(uu3d) returns ",sys_stat
    stop
  endif
  allocate(dd3d(1:num_yy,1:num_xx,1:num_zz),stat=sys_stat)
  if(sys_stat .ne. 0) then
    write(*,'(A,I0)') "Error: allocate(dd3d) returns ",sys_stat
    stop
  endif
  write(*,'(A,I0)') "Info: allocated(uu3d) GiB:", sizeof(uu3d)/2**30
  write(*,'(A,I0)') "Info: allocated(dd3d) GiB:", sizeof(dd3d)/2**30

  num_ii=num_yy
  num_jj=num_xx
  num_kk=num_zz
end subroutine alloc_mem3d

!Speicherfreigabe für die Felder uu3d und dd3d;
subroutine dealloc_mem3d()
  if(allocated(uu3d)) deallocate(uu3d)
  if(allocated(dd3d)) deallocate(dd3d)
  num_ii=-1_ik
  num_jj=-1_ik
  num_kk=-1_ik
end subroutine dealloc_mem3d

!Initialisierung des Felders uu3d mit f=x*y^2+z^3) 
subroutine init_uu3d()
  integer(kind=ik) :: ii, jj, kk
  integer(kind=ik) :: ii_end, jj_end, kk_end
  real(kind=rk) :: hh,xx,yy,zz,val

  ii_end = num_ii+1_ik
  jj_end = num_jj+1_ik
  kk_end = num_kk+1_ik
  hh = 1.0_rk/real(num_ii,kind=rk)
  !$omp parallel do default(none) &
    !$omp& private(ii,jj,kk, &
    !$omp& xx,yy,zz,val) &
    !$omp& shared(uu3d,hh, &
    !$omp& kk_end,jj_end,ii_end)
  do kk=0, kk_end
    do jj=0, jj_end
      do ii=0, ii_end
        xx = hh*jj
        yy = hh*ii
        zz = hh*kk
        val = xx*yy*yy+zz*zz*zz
        uu3d(ii,jj,kk) = val
      end do
    end do
  end do
end subroutine

!Copy 2D Array into uu3d
subroutine copy2d_to_uu3d(data2d,zz)
  real(kind=rk), dimension(:,:), intent(in) :: data2d
  integer(kind=ik), intent(in) :: zz
  integer(kind=ik) :: ii, jj
  integer(kind=ik) :: ii_end, jj_end, kk_end

  ii_end = num_ii+1_ik
  jj_end = num_jj+1_ik
  kk_end = num_kk+1_ik
  do jj=1,num_jj
    do ii=1,num_ii
      uu3d(ii,jj,zz) = data2d(ii,jj)
    end do
  end do
  uu3d(1:num_ii,0,zz) = data2d(:,1)
  uu3d(1:num_ii,kk_end,zz) = data2d(:,num_jj)
  uu3d(0,0,zz)=data2d(1,1)
  uu3d(ii_end,0,zz)=data2d(num_ii,1)
  uu3d(0,jj_end,zz)=data2d(1,num_jj)
  uu3d(ii_end,jj_end,zz)=data2d(num_ii,num_jj)

end subroutine copy2d_to_uu3d

!Di Methode wendet den Laplace-Operator in allen Gittezellen 
!Berechnungs Gebiets (kein Cache Blocking)
!Die Variablen sind:
!num_ii-Anzahl der inneren Gebietszellen in Y-Richtung
!num_jj-Anzahl der inneren Gebietszellen in X-Richtung
!num_kk-Anzahl der inneren Gebietszellen in Z-Richtung
!uu3d-Dreidimensionales Array mit den Funktionswerten
!dd3d-Dreidimensionalen Array mit der zweiten Ableitung
!temp1,..,temp9- Hilfsvariablen
subroutine laplacian_simple3d()

  integer(kind=ik) :: ii, jj, kk
  real(kind=rk) :: tmp1, tmp2, tmp3
  real(kind=rk) :: tmp4, tmp5, tmp6
  real(kind=rk) :: tmp7, tmp8, tmp9
  real(kind=rk) :: hh,xx,yy,zz,val
  real(kind=rk) :: step_inv
  hh=1.0/real(num_ii,kind=rk)
  step_inv=1.0/(30.0*hh*hh)

  !$omp parallel do default(none) &
      !$omp& private(ii,jj,kk,tmp1,tmp2,tmp3, &
      !$omp& tmp4,tmp5,tmp6,tmp7,tmp8,tmp9, &
      !$omp& xx,yy,zz,val) &
      !$omp& shared(uu3d,dd3d,num_kk,num_jj,num_ii,step_inv)
  do kk=1,num_kk
    do jj=1,num_jj
      !!DIR$ VECTOR NONTEMPORAL 
      do ii=1,num_ii ! uu3d(dy,dx,dz)
        tmp1 = uu3d(ii-1,jj-1,kk-1)+3.0*uu3d(ii,jj-1,kk-1)+uu3d(ii+1,jj-1,kk-1)
        tmp2 = 3.0*uu3d(ii-1,jj,kk-1)+14.0*uu3d(ii,jj,kk-1)+3.0*uu3d(ii+1,jj,kk-1)
        tmp3 = uu3d(ii-1,jj+1,kk-1)+3.0*uu3d(ii,jj+1,kk-1)+uu3d(ii+1,jj+1,kk-1)

        tmp4 = 3.0*uu3d(ii-1,jj-1,kk)+14.0*uu3d(ii,jj-1,kk)+3.0*uu3d(ii+1,jj-1,kk)
        tmp5 = 14.0*uu3d(ii-1,jj,kk)-128.0*uu3d(ii,jj,kk)+14.0*uu3d(ii+1,jj,kk)
        tmp6 = 3.0*uu3d(ii-1,jj+1,kk)+14.0*uu3d(ii,jj+1,kk)+3.0*uu3d(ii+1,jj+1,kk)

        tmp7 = uu3d(ii-1,jj-1,kk+1)+3.0*uu3d(ii,jj-1,kk+1)+uu3d(ii+1,jj-1,kk+1)
        tmp8 = 3.0*uu3d(ii-1,jj,kk+1)+14.0*uu3d(ii,jj,kk+1)+3.0*uu3d(ii+1,jj,kk+1)
        tmp9 = uu3d(ii-1,jj+1,kk+1)+3.0*uu3d(ii,jj+1,kk+1)+uu3d(ii+1,jj+1,kk+1)

        dd3d(ii,jj,kk) = (tmp1+tmp2+tmp3+tmp4+tmp5+tmp6+tmp7+tmp8+tmp9)*step_inv
      end do
    end do
  end do
end subroutine laplacian_simple3d

!Simple-VBlocked3d Implementierung der 3D Laplace-Operators mit "virtueller"
!Aufteilung des Rechengebiets
!blk_size_xx - Anzahl der Zellen in einem Sub-Gebiet in die Richtung X
!blk_size_yy - Anzahl der Zellen in einem Sub-Gebiet in die Richtung Y
!blk_size_zz - Anzahl der Zellen in einem Sub-Gebiet in die Richtung Z
subroutine laplacian_simple_vblocked3d(blk_size_xx,blk_size_yy,blk_size_zz)
  integer(kind=ik), intent(in) :: blk_size_xx
  integer(kind=ik), intent(in) :: blk_size_yy
  integer(kind=ik), intent(in) :: blk_size_zz
  integer(kind=ik) :: iiblock, ii
  integer(kind=ik) :: jjblock, jj
  integer(kind=ik) :: kkblock, kk
  integer(kind=ik) :: blk_size_yy_one
  integer(kind=ik) :: blk_size_xx_one
  integer(kind=ik) :: blk_size_zz_one
  real(kind=rk) :: tmp1, tmp2, tmp3
  real(kind=rk) :: tmp4, tmp5, tmp6
  real(kind=rk) :: tmp7, tmp8, tmp9
  real(kind=rk) :: step_inv,hh
  hh=1.0/real(num_ii,kind=rk)
  step_inv=1.0/(30.0*hh*hh)

  blk_size_xx_one = blk_size_xx-1
  blk_size_yy_one = blk_size_yy-1
  blk_size_zz_one = blk_size_zz-1
  !$omp parallel do default(none) &
      !$omp& private(tmp1,tmp2,tmp3, &
      !$omp& tmp4,tmp5,tmp6,tmp7,tmp8,tmp9, &
      !$omp& ii,jj,kk, &
      !$omp& kkblock,jjblock,iiblock) &
      !$omp& shared(blk_size_xx_one, &
      !$omp& blk_size_yy_one,blk_size_zz_one, &
      !$omp& num_kk,num_jj,num_ii, &
      !$omp& blk_size_xx,blk_size_yy, &
      !$omp& blk_size_zz,uu3d,dd3d,step_inv)
  do kkblock=1,num_kk,blk_size_zz
    do jjblock=1,num_jj,blk_size_xx
      do iiblock=1,num_ii,blk_size_yy
        do kk=kkblock,kkblock+blk_size_zz_one
          do jj=jjblock,jjblock+blk_size_xx_one
            !!DIR$ VECTOR NONTEMPORAL 
            !!DIR$ UNROLL=8
            do ii=iiblock,iiblock+blk_size_yy_one
              tmp1 = uu3d(ii-1,jj-1,kk-1)+3.0*uu3d(ii,jj-1,kk-1)+uu3d(ii+1,jj-1,kk-1)
              tmp2 = 3.0*uu3d(ii-1,jj,kk-1)+14.0*uu3d(ii,jj,kk-1)+3.0*uu3d(ii+1,jj,kk-1)
              tmp3 = uu3d(ii-1,jj+1,kk-1)+3.0*uu3d(ii,jj+1,kk-1)+uu3d(ii+1,jj+1,kk-1)

              tmp4 = 3.0*uu3d(ii-1,jj-1,kk)+14.0*uu3d(ii,jj-1,kk)+3.0*uu3d(ii+1,jj-1,kk)
              tmp5 = 14.0*uu3d(ii-1,jj,kk)-128.0*uu3d(ii,jj,kk)+14.0*uu3d(ii+1,jj,kk)
              tmp6 = 3.0*uu3d(ii-1,jj+1,kk)+14.0*uu3d(ii,jj+1,kk)+3.0*uu3d(ii+1,jj+1,kk)

              tmp7 = uu3d(ii-1,jj-1,kk+1)+3.0*uu3d(ii,jj-1,kk+1)+uu3d(ii+1,jj-1,kk+1)
              tmp8 = 3.0*uu3d(ii-1,jj,kk+1)+14.0*uu3d(ii,jj,kk+1)+3.0*uu3d(ii+1,jj,kk+1)
              tmp9 = uu3d(ii-1,jj+1,kk+1)+3.0*uu3d(ii,jj+1,kk+1)+uu3d(ii+1,jj+1,kk+1)

              dd3d(ii,jj,kk) = (tmp1+tmp2+tmp3+tmp4+tmp5+tmp6+tmp7+tmp8+tmp9)*step_inv
            end do
          end do
        end do
      end do
    end do
  end do

end subroutine laplacian_simple_vblocked3d

!Maybe Todo compute opt. block size
subroutine laplacian_auto_blk3d()
  write(*,'(A,I0)') &
    "Error: laplacian_auto_blocks is not implemented "
  stop
end subroutine laplacian_auto_blk3d

!Berechne Absolute und relative Fehlernorm der Lösung
!Theoretische Lösung ist 2.0_rk*xx+6.0*zz
! ||vv||_2
! ||uu3d||_2
! ||uu3d-vv||_2
! ||uu3d-vv||_2/||xx||_2
function laplacian_check3d() result(norm)
  integer(kind=ik) :: ii, jj, kk
  real(kind=rk), dimension(4) :: norm
  real(kind=rk) normerr, diff, normxx, normdd3d
  real(kind=rk) :: hh,xx,yy,zz,vv
  
  hh = 1.0_rk/real(num_ii,kind=rk)
  normerr=0.0_rk
  normxx=0.0_rk
  normdd3d=0.0_rk
  norm=0.0_rk

!  !Iteriere uber die Bloecke
!  !$omp parallel do reduction(+:normxx) &
!    !$omp& reduction(+:normdd3d) & 
!    !$omp& reduction(+:normerr) & 
!    !$omp& default(none) &
!    !$omp& private(zz,yy,xx,ii,jj,kk,diff,vv) &
!    !$omp& shared(dd3d,hh,num_kk,num_jj,num_ii)
  do kk=1,num_kk
    do jj=1,num_jj
      do ii=1,num_ii
        xx = hh*jj
        yy = hh*ii
        zz = hh*kk
        vv = 2.0_rk*xx+6.0*zz
        normxx=normxx + vv*vv
        normdd3d=normdd3d + dd3d(ii,jj,kk)*dd3d(ii,jj,kk)
        diff=dd3d(ii,jj,kk)-vv
        normerr=normerr+diff*diff
      end do
    end do
  end do
  normerr=SQRT(normerr)
  normxx=SQRT(normxx)
  normdd3d=SQRT(normdd3d)
  norm(1)=normxx
  norm(2)=normdd3d
  norm(3)=normerr
  norm(4)=normerr/normxx
end function laplacian_check3d

end module mod_laplacian3d 
