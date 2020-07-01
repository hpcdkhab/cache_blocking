module mod_inparam

use mod_types
use mod_cmdline

implicit none

type type_inparam
  integer(kind=ik):: benchmark_id !Auswahl der Laplacian-Implementierung (0,2)
  integer(kind=ik):: num_threads !Anzahl der Threads
  integer(kind=ik):: num_numas !Anzahl der Numa-Nodes
  integer(kind=ik):: numa_cores !Anzahl der Cores pro Numa-Nodes
  integer(kind=ik):: has_numas !Ob der compute node Numa-NOdes hat
  integer(kind=ik):: size_xx !Gebietsgroeßen zum Testen
  integer(kind=ik):: size_yy !Gebietsgroeßen zum Testen
  integer(kind=ik):: size_zz !Gebietsgroeßen zum Testen
  integer(kind=ik):: min_size_xx !Gebietsgroeßen zum Testen
  integer(kind=ik):: max_size_xx !Gebietsgroeßen zum Testen
  integer(kind=ik):: min_size_yy !Gebietsgroeßen zum Testen
  integer(kind=ik):: max_size_yy !Gebietsgroeßen zum Testen
  integer(kind=ik):: min_size_zz !Gebietsgroeßen zum Testen
  integer(kind=ik):: max_size_zz !Gebietsgroeßen zum Testen
  integer(kind=ik):: step_size_xx !Gebietsgroeßen zum Testen
  integer(kind=ik):: step_size_yy !Gebietsgroeßen zum Testen
  integer(kind=ik):: step_size_zz !Gebietsgroeßen zum Testen
  real(kind=rk):: min_time !Angabe für die minimale Dauer eines Testes (Groeßenordnung)
  integer(kind=ik):: blk_size_xx !Bloeckgroesse in X-Richtung
  integer(kind=ik):: blk_size_yy !Bloeckgroesse in Y-Richtung
  integer(kind=ik):: blk_size_zz !Bloeckgroesse in Z-Richtung
  integer(kind=ik):: min_blk_size_xx !Bloeckgroesse in X-Richtung
  integer(kind=ik):: max_blk_size_xx !Bloeckgroesse in X-Richtung
  integer(kind=ik):: step_blk_size_xx !Bloeckgroesse in X-Richtung
  integer(kind=ik):: min_blk_size_yy !Bloeckgroesse in Y-Richtung
  integer(kind=ik):: max_blk_size_yy !Bloeckgroesse in Y-Richtung
  integer(kind=ik):: step_blk_size_yy !Bloeckgroesse in Y-Richtung
  integer(kind=ik):: min_blk_size_zz !Bloeckgroesse in Z-Richtung
  integer(kind=ik):: max_blk_size_zz !Bloeckgroesse in Z-Richtung
  integer(kind=ik):: step_blk_size_zz !Bloeckgroesse in Z-Richtung
  integer(kind=ik):: num_repetitions !Anzahl der Wiederholungen
  integer(kind=ik):: check_solution !0 - no check, >0 chek each solution
  integer(kind=ik):: clear_cache !0 - Verdränge Keine Daten aus dem cache! 1-Verdränge Daten aus dem Cache
  integer(kind=ik):: deep_level !0 - Alle JPEGS aus dem Verzeichnis auslesen; > 0 Anzahl der JPEGS zu verarbeiten
  integer(kind=ik):: check_laplacian !0 - Arbeite mit JPEGs, sonst uu = xx*yy*yy+zz*zz*zz->dd=2.0_rk*xx+6.0*zz
  real(kind=rk)   :: scale_coeff!default 1 and is used to calculate the step h
  integer(kind=ik):: verbosity !Verbosity
  integer(kind=ik):: jobid
  character(len=1024) :: filepath !Dateiname für die Performance-Statistik
  character(len=1024) :: jpeg_topdir !Verzeichnis mit JPEG-Dateien
  character(len=1024) :: png_outdir !Verzeichnis für neue JPEG-Dateien
  character(len=1024) :: profile_filepath !Dateiname mit den Anwendungspahen als Timestamps gespeichert
  integer(kind=ik) :: has_profile !Ob die Anwendungsphasen, die Timestamps, gespeichert werden
end type type_inparam


type(type_inparam)   :: input_param
contains

subroutine input_param_print()

  write(*,'(A,I0)') "Eingabeparameter:"
  write(*,'(A,I0)') "verbosity:", input_param%verbosity
  write(*,'(A,I0)') "jobid:", input_param%jobid
  write(*,'(A,I0)') "benchmark_id:", input_param%benchmark_id
  write(*,'(A,I0)') "num_threads:", input_param%num_threads
  write(*,'(A,I0)') "size_xx:", input_param%size_xx
  write(*,'(A,I0)') "size_yy:", input_param%size_yy
  write(*,'(A,I0)') "size_zz:", input_param%size_zz
  write(*,'(A,I0)') "min_size_xx:", input_param%min_size_xx
  write(*,'(A,I0)') "max_size_xx:", input_param%max_size_xx
  write(*,'(A,I0)') "min_size_yy:", input_param%min_size_yy
  write(*,'(A,I0)') "max_size_yy:", input_param%max_size_yy
  write(*,'(A,I0)') "min_size_kk:", input_param%min_size_zz
  write(*,'(A,I0)') "max_size_kk:", input_param%max_size_zz
  write(*,'(A,I0)') "step_size_xx:", input_param%step_size_xx
  write(*,'(A,I0)') "step_size_yy:", input_param%step_size_yy
  write(*,'(A,I0)') "step_size_zz:", input_param%step_size_zz
  write(*,'(A,E13.6)') "min_time:", input_param%min_time
  write(*,'(A,I0)') "blk_size_xx:", input_param%blk_size_xx
  write(*,'(A,I0)') "blk_size_yy:", input_param%blk_size_yy
  write(*,'(A,I0)') "blk_size_zz:", input_param%blk_size_zz
  write(*,'(A,I0)') "min_blk_size_xx:", input_param%min_blk_size_xx
  write(*,'(A,I0)') "max_blk_size_xx:", input_param%max_blk_size_xx
  write(*,'(A,I0)') "step_blk_size_xx:", input_param%step_blk_size_xx
  write(*,'(A,I0)') "min_blk_size_yy:", input_param%min_blk_size_yy
  write(*,'(A,I0)') "max_blk_size_yy:", input_param%max_blk_size_yy
  write(*,'(A,I0)') "step_blk_size_yy:", input_param%step_blk_size_yy
  write(*,'(A,I0)') "min_blk_size_zz:", input_param%min_blk_size_zz
  write(*,'(A,I0)') "max_blk_size_zz:", input_param%max_blk_size_zz
  write(*,'(A,I0)') "step_blk_size_zz:", input_param%step_blk_size_zz
  write(*,'(A,I0)') "check_solution:", input_param%check_solution
  write(*,'(A,I0)') "num_repetitions:", input_param%num_repetitions
  write(*,'(A,I0)') "clear_cache:", input_param%clear_cache
  write(*,'(A,A)') "filepath:", trim(input_param%filepath)
  write(*,'(A,A)') "profile_filepath:", trim(input_param%profile_filepath) 
  write(*,'(A,A)') "jpeg_topdir:", trim(input_param%jpeg_topdir)
  write(*,'(A,A)') "png_outdir:", trim(input_param%png_outdir)
  write(*,'(A,I0)') "deep_level:", input_param%deep_level
  write(*,'(A,I0)') "check_laplacian:", input_param%check_laplacian
  write(*,'(A,I0)') "num_numas:", input_param%num_numas
  write(*,'(A,I0)') "numa_cores:", input_param%numa_cores
  write(*,'(A,I0)') "has_numas:", input_param%has_numas
  write(*,'(A,E13.6)') "scale_coeff:", input_param%scale_coeff
  write(*,'(A,I0)') "has_profile:", input_param%has_profile

end subroutine input_param_print

subroutine input_param_read()
  character(len=2048) :: message
  character(len=2048) :: cmd
  integer(kind=ik) :: help
  
  call input_param_def()
  call get_command(cmd)
  message='Eingabeparameter:' &
    &//' -verbosity <int>' &
    &//' -jobid <int>' &
    &//' -benchmark_id <int> ' &
    &//' -num_threads <int> ' &
    &//' -min_size_x <int>' &
    &//' -max_size_x <int>' &
    &//' -min_size_y <int>' &
    &//' -max_size_y <int>' &
    &//' -min_size_z <int>' &
    &//' -max_size_z <int>' &
    &//' -size_x <int>' &
    &//' -size_y <int>' &
    &//' -size_z <int>' &
    &//' -step_size_x <int>' &
    &//' -step_size_y <int>' &
    &//' -step_size_z <int>' &
    &//' -min_time <real>' &
    &//' -blk_size_x <int>' &
    &//' -blk_size_y <int>' &
    &//' -blk_size_z <int>' &
    &//' -min_blk_size_x <int>' &
    &//' -max_blk_size_x <int>' &
    &//' -step_blk_size_x <int>' &
    &//' -min_blk_size_y <int>' &
    &//' -max_blk_size_y <int>' &
    &//' -step_blk_size_y <int>' &
    &//' -min_blk_size_z <int>' &
    &//' -max_blk_size_z <int>' &
    &//' -step_blk_size_z <int>' &
    &//' -num_repetitions <int>' &
    &//' -filepath <filename>' &
    &//' -check_solution <int>' &
    &//' -clear_cache <int>' &
    &//' -jpeg_topdir <filename>' &
    &//' -png_outdir <filename>' &
    &//' -profile_filepath <filename>' &
    &//' -deep_level <int>' &
    &//' -check_laplacian <int>' &
    &//' -scale_coeff <real>' &
    &//' -num_numas <int>' &
    &//' -numa_cores <int>' &
    &//' -has_numas <int>' &
    &//' -has_profile <int>' &
    &//' -help <int>'
    if(extract_command_parameter(cmd,'-help',stop_on_error=.false.,&
                value=help, syntax=message) ==0) then
      if(help .GE. 0) then
        write (*,'(A)') trim(message)
        if(help .GT. 0) then
          stop
        endif
      end if
  endif
  if(extract_command_parameter(cmd,'-verbosity',stop_on_error=.false.,&
                value=input_param%verbosity, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-jobid',stop_on_error=.false.,&
                value=input_param%jobid, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-benchmark_id',stop_on_error=.false.,&
                value=input_param%benchmark_id, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-num_threads',stop_on_error=.false.,&
                value=input_param%num_threads, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-min_time',stop_on_error=.false.,&
                value=input_param%min_time, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-size_x',stop_on_error=.false.,&
                value=input_param%size_xx, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-size_y',stop_on_error=.false.,&
                value=input_param%size_yy, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-size_z',stop_on_error=.false.,&
                value=input_param%size_zz, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-min_size_x',stop_on_error=.false.,&
                value=input_param%min_size_xx, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-max_size_x',stop_on_error=.false.,&
                value=input_param%max_size_xx, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-min_size_y',stop_on_error=.false.,&
                value=input_param%min_size_yy, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-max_size_y',stop_on_error=.false.,&
                value=input_param%max_size_yy, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-min_size_z',stop_on_error=.false.,&
                value=input_param%min_size_zz, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-max_size_z',stop_on_error=.false.,&
                value=input_param%max_size_zz, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-step_size_x',stop_on_error=.false.,&
                value=input_param%step_size_xx, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-step_size_y',stop_on_error=.false.,&
                value=input_param%step_size_yy, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-step_size_z',stop_on_error=.false.,&
                value=input_param%step_size_zz, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-blk_size_x',stop_on_error=.false.,&
                value=input_param%blk_size_xx, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-blk_size_y',stop_on_error=.false.,&
                value=input_param%blk_size_yy, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-blk_size_z',stop_on_error=.false.,&
                value=input_param%blk_size_zz, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-min_blk_size_x',stop_on_error=.false.,&
                value=input_param%min_blk_size_xx, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-max_blk_size_x',stop_on_error=.false.,&
                value=input_param%max_blk_size_xx, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-step_blk_size_x',stop_on_error=.false.,&
                value=input_param%step_blk_size_xx, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-min_blk_size_y',stop_on_error=.false.,&
                value=input_param%min_blk_size_yy, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-max_blk_size_y',stop_on_error=.false.,&
                value=input_param%max_blk_size_yy, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-step_blk_size_y',stop_on_error=.false.,&
                value=input_param%step_blk_size_yy, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-min_blk_size_z',stop_on_error=.false.,&
                value=input_param%min_blk_size_zz, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-max_blk_size_z',stop_on_error=.false.,&
                value=input_param%max_blk_size_zz, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-step_blk_size_z',stop_on_error=.false.,&
                value=input_param%step_blk_size_zz, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-check_solution',stop_on_error=.false.,&
                value=input_param%check_solution, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-clear_cache',stop_on_error=.false.,&
                value=input_param%clear_cache, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-filepath',stop_on_error=.false.,&
                value=input_param%filepath, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-jpeg_topdir',stop_on_error=.false.,&
                value=input_param%jpeg_topdir, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-png_outdir',stop_on_error=.false.,&
                value=input_param%png_outdir, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-deep_level',stop_on_error=.false.,&
                value=input_param%deep_level, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-check_laplacian',stop_on_error=.false.,&
                value=input_param%check_laplacian, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-scale_coeff',stop_on_error=.false.,&
                value=input_param%scale_coeff, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-has_numas',stop_on_error=.false.,&
                value=input_param%has_numas, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-num_numas',stop_on_error=.false.,&
                value=input_param%num_numas, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-numa_cores',stop_on_error=.false.,&
                value=input_param%numa_cores, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-num_repetitions',stop_on_error=.false.,&
                value=input_param%num_repetitions, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-profile_filepath',stop_on_error=.false.,&
                value=input_param%profile_filepath, syntax=message) /=0) then
  endif
  if(extract_command_parameter(cmd,'-has_profile',stop_on_error=.false.,&
                value=input_param%has_profile, syntax=message) /=0) then
  endif
  if(input_param%size_xx .gt. 0) then
    input_param%min_size_xx=input_param%size_xx
    input_param%max_size_xx=input_param%size_xx
    input_param%step_size_xx=input_param%size_xx
  endif
  if(input_param%size_yy .gt. 0) then
    input_param%min_size_yy=input_param%size_yy
    input_param%max_size_yy=input_param%size_yy
    input_param%step_size_yy=input_param%size_yy
  endif
  if(input_param%size_zz .gt. 0) then
    input_param%min_size_zz=input_param%size_zz
    input_param%max_size_zz=input_param%size_zz
    input_param%step_size_zz=input_param%size_zz
  endif

  if(input_param%blk_size_xx .gt. 0) then
    input_param%min_blk_size_xx=input_param%blk_size_xx
    input_param%max_blk_size_xx=input_param%blk_size_xx
    input_param%step_blk_size_xx=input_param%blk_size_xx
  endif
  if(input_param%blk_size_yy .gt. 0) then
    input_param%min_blk_size_yy=input_param%blk_size_yy
    input_param%max_blk_size_yy=input_param%blk_size_yy
    input_param%step_blk_size_yy=input_param%blk_size_yy
  endif
  if(input_param%blk_size_zz .gt. 0) then
    input_param%min_blk_size_zz=input_param%blk_size_zz
    input_param%max_blk_size_zz=input_param%blk_size_zz
    input_param%step_blk_size_zz=input_param%blk_size_zz
  endif
  if(input_param%check_laplacian .gt. 0) then
    input_param%min_blk_size_zz=input_param%blk_size_zz
    input_param%max_blk_size_zz=input_param%blk_size_zz
    input_param%step_blk_size_zz=input_param%blk_size_zz
  endif
  call input_param_check()
        
end subroutine input_param_read

subroutine input_param_def()

  input_param%benchmark_id=0
  input_param%num_threads=1
  input_param%num_numas=2
  input_param%numa_cores=12
  input_param%has_numas=0
  input_param%size_xx=0
  input_param%size_yy=0
  input_param%size_zz=0
  input_param%min_size_xx=1920
  input_param%min_size_yy=1920
  input_param%min_size_zz=180
  input_param%max_size_xx=1920
  input_param%max_size_yy=1920
  input_param%max_size_zz=180
  input_param%step_size_xx=64
  input_param%step_size_yy=64
  input_param%step_size_zz=64
  input_param%min_time=1.0
  input_param%blk_size_xx=0
  input_param%blk_size_yy=0
  input_param%blk_size_zz=0
  input_param%min_blk_size_xx=128
  input_param%max_blk_size_xx=128
  input_param%step_blk_size_xx=128 
  input_param%min_blk_size_yy=128
  input_param%max_blk_size_yy=128
  input_param%step_blk_size_yy=128
  input_param%min_blk_size_zz=180
  input_param%max_blk_size_zz=180
  input_param%step_blk_size_zz=180
  input_param%num_repetitions=0
  input_param%check_solution=1
  input_param%clear_cache=1
  input_param%deep_level=0
  input_param%check_laplacian=0
  input_param%scale_coeff=1.0
  input_param%has_profile=0
  write(input_param%filepath,'(A,I0,4(A,I0,A,I0),A,I0,A)') &
      & "./bench_",input_param%benchmark_id, &
      & "_size_",input_param%min_size_xx,"-",input_param%max_size_xx, &
      & "_size_",input_param%min_size_yy,"-",input_param%max_size_yy, &
      & "_size_",input_param%min_size_zz,"-",input_param%max_size_zz, &
      & "_block_",input_param%blk_size_xx,"x",input_param%blk_size_yy, &
      & "x",input_param%blk_size_zz, &
      & ".csv"
  write(input_param%jpeg_topdir,'(A)') "/nas_home/hpcdkhab/example_cache_blocking/fotos/"
  write(input_param%png_outdir,'(A)') "/nas_home/hpcdkhab/example_cache_blocking/new_fotos/"
  write(input_param%png_outdir,'(A)') "/nas_home/hpcdkhab/example_cache_blocking/data/profile.csv"
end subroutine input_param_def

subroutine input_param_check()

  if(input_param%benchmark_id .gt. 4) then
    write(*,'(A,I0)') "Error: check benchmark_id (0..4):",&
      & input_param%benchmark_id
    stop
  endif
  if(input_param%min_size_xx .gt. input_param%max_size_xx) then
    write(*,'(A,I0)') "Error: check min_size_x and max_size_x (max_size must be greater than min_size)"
    write(*,'(2(A,I0))') "min_size_x: ", input_param%min_size_xx, "; max_size_x: ", input_param%max_size_xx
    stop
  endif
  if(input_param%min_size_yy .gt. input_param%max_size_yy) then
    write(*,'(A,I0)') "Error: check min_size_y and max_size_y (max_size must be greater than min_size)"
    write(*,'(2(A,I0))') "min_size_y: ", input_param%min_size_yy, "; max_size_y: ", input_param%max_size_yy
    stop
  endif
  if(input_param%min_size_zz .gt. input_param%max_size_zz) then
    write(*,'(A,I0)') "Error: check min_size_z and max_size_z (max_size must be greater than min_size)"
    write(*,'(2(A,I0))') "min_size_z: ", input_param%min_size_zz, "; max_size_z: ", input_param%max_size_zz
    stop
  endif
  if(input_param%max_blk_size_xx .gt. input_param%min_size_xx) then
    write(*,'(A,I0)') "Error: block_size_x (block_size must be equal or less  than min_size)"
    stop
  endif
  if(input_param%max_blk_size_yy .gt. input_param%min_size_yy) then
    write(*,'(A,I0)') "Error: block_size_y (block_size must be equal or less  than min_size)"
    stop
  endif
  if(input_param%max_blk_size_zz .gt. input_param%min_size_zz) then
    write(*,'(A,I0)') "Error: block_size_z (block_size must be equal or less  than min_size)"
    stop
  endif
  if(input_param%has_numas .gt. 0) then
    if(input_param%num_threads .gt. input_param%num_numas*input_param%numa_cores) then
      write(*,'(A,I0)') "Error: num_threads greater than number of cores=num_numas*numa_cores"
      stop
    endif
  endif
  if(input_param%min_time .lt. 1.e-6) then
    write(*,'(A,I0)') "Error: check min_time (too small)"
    stop
  endif

end subroutine input_param_check


end module mod_inparam
