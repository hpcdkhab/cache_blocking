!Hilfsroutine, wie zum Beispiel Berechnung der Thread-Affinität
module mod_miscelanos

  use mod_types !Fortran Modul mit der Typedefiniton für Real und Integer
  use OMP_LIB !We use OpenMP threads
  use, intrinsic :: ISO_C_BINDING !C-Binding

  implicit none
  integer(kind=c_int), dimension(256):: pin_cores !Array mit core-ids für Thread-Pinning
contains

 !Initialisiere Array pin_cores
 subroutine set_cpu_affinity(has_numas, num_threads, num_numas, numa_cores)
  integer(kind=ik), intent(IN) :: has_numas
  integer(kind=ik), intent(IN) :: num_numas
  integer(kind=c_int), intent(IN) :: num_threads
  integer(kind=ik), intent(IN) :: numa_cores
  integer(kind=ik), dimension(256) :: first_numa_cores
  integer(kind=ik) :: ii,jj,curr_thread,num_cores_node
  integer(kind=ik) :: first_numa_core

  num_cores_node=num_numas*numa_cores
  if(has_numas .eq. 0) then
    do ii=1, num_threads
      pin_cores(ii)=int(ii-1,kind=c_int)
    end do
  else
    if(num_numas .eq. 8) then
      first_numa_cores(1)=0
      first_numa_cores(3)=num_cores_node/4
      first_numa_cores(5)=num_cores_node/8
      first_numa_cores(7)=num_cores_node/4+num_cores_node/8
      first_numa_cores(2)=num_cores_node/2
      first_numa_cores(4)=num_cores_node/2+num_cores_node/4
      first_numa_cores(6)=num_cores_node/2+num_cores_node/8
      first_numa_cores(8)=num_cores_node-num_cores_node/8
      curr_thread=1
      do ii=1, numa_cores
        do jj=1,num_numas
          first_numa_core=first_numa_cores(jj)
          pin_cores(curr_thread)=first_numa_core+ii-1
          curr_thread=curr_thread+1
        end do
      end do
    elseif(num_numas .eq. 4) then
      !Für ROME mit 128 cores
      first_numa_cores(1)=0
      first_numa_cores(2)=num_cores_node/4
      first_numa_cores(3)=num_cores_node/2
      first_numa_cores(4)=num_cores_node/2+num_cores_node/4
      curr_thread=1
      do ii=1, numa_cores
        do jj=1,num_numas
          first_numa_core=first_numa_cores(jj)
          pin_cores(curr_thread)=first_numa_core+ii-1
          curr_thread=curr_thread+1
        end do
      end do
    else
      num_cores_node=num_numas*numa_cores
      curr_thread=1
      do ii=1, numa_cores
        do jj=1,num_numas
          first_numa_core=(jj-1)*numa_cores
          pin_cores(curr_thread)=first_numa_core+ii-1
          curr_thread=curr_thread+1
        end do
      end do
    endif
  endif
end subroutine set_cpu_affinity

end module mod_miscelanos
