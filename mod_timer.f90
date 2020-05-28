!Fortran Modul mit dem Interface zur Zeit- und Tickmessung + OS Tools (z.B. Pinning)
module mod_timer

  implicit none

  integer, parameter :: tik = 8
  integer, parameter :: trk = 8
 
  interface

  function getrdtsc() result(cycles) bind(c, name = 'getrdtsc')
   use, intrinsic :: iso_c_binding
   integer(kind = c_long) :: cycles
  end function getrdtsc

  function get_time() result(seconds) bind(c, name = 'get_time')
   use, intrinsic :: iso_c_binding
   real(kind = c_double) :: seconds
  end function get_time

  subroutine get_time_stamp(seconds,nanoseconds) BIND(c,name="get_time_stamp")
    use, INTRINSIC :: ISO_C_BINDING
    integer(kind = c_long_long) :: seconds
    integer(kind = c_long_long) :: nanoseconds
  end subroutine get_time_stamp

  function sec_tsc_cycle() &
  result(tsc_cycles) bind(c, name = 'sec_tsc_cycle')
   use, intrinsic :: iso_c_binding
   real(kind = c_double) :: tsc_cycles
  end function sec_tsc_cycle

  subroutine micro_sleep(micro_seconds) BIND(c,name="micro_sleep")
    use, INTRINSIC :: ISO_C_BINDING
    integer(kind = c_long) :: micro_seconds
  end subroutine micro_sleep

  function pin_thread(thread_id) result(err_msg) BIND(c,name="pin_thread")
    use, INTRINSIC :: ISO_C_BINDING
    integer(kind = c_int) :: thread_id
    integer(kind = c_int) :: err_msg
  end function pin_thread

  end interface

end module mod_timer


