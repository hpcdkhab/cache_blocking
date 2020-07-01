!Benchmark "3D 27-point Stecil Laplacian" für das Modul "Performance-Optmimierung/Single-node Optimization"

program laplacian3d
  use iso_c_binding !Iso binding für Interface mit C
  use mod_types !Fortran Modul mit der Typedefiniton z.B. für für Real und Integer
  use mod_inparam !Fortran Modul für die Eingabeparameter (commandline arguments)
  use mod_timer !Fortran Modul mit dem Interface zur Zeit- und Tickmessung + OS Tools (z.B. Pinning)
  use mod_miscelanos!Fortran moModul für die Berechnung der Thread-Affinity
  use mod_laplacian3d !1. Implementierung der Testroutinen für 3D 27-point Laplacian
  use mod_laplacian_block3d !2. Implementierung der Testroutinen für 3D 27-point Laplacian
  !use mod_c_laplacian_block3d !Interface zur C-Methode für 3D 27-point Laplacian
  use mod_profile !Speichern die Stamtimes in eine Datei aus
  use OMP_LIB !We use OpenMP threads

implicit none
  type(type_dom)   :: domain_decomp !Structure mit Gebietsparametern
  integer(kind=ik) :: benchmark_id !Auswahl der Laplacian-Implementierung (0,1,2)
  integer(kind=ik) :: min_size_xx, max_size_xx, step_size_xx !Gebietsgroeßen zum Testen
  integer(kind=ik) :: min_size_yy, max_size_yy, step_size_yy !Gebietsgroeßen zum Testen
  integer(kind=ik) :: min_size_zz, max_size_zz, step_size_zz !Gebietsgroeßen zum Testen
  integer(kind=ik) :: min_blk_size_xx, max_blk_size_xx, step_blk_size_xx !Bloeckgroesse in X-Richtung
  integer(kind=ik) :: min_blk_size_yy, max_blk_size_yy, step_blk_size_yy !Blockgroesse  in Y-Richtung
  integer(kind=ik) :: min_blk_size_zz, max_blk_size_zz, step_blk_size_zz !Blockgroesse  in Z-Richtung
  integer(kind=ik) :: num_tests !Anzahl von Tests
  real(kind=trk) :: num_flops !Anzahl der FLOPs
  real(kind=trk) :: ticks_in_sec !Wieviel Ticks in einer Sekunde
  real(kind=trk) :: tsc_resol_sec !Genauigkeit der Ticksmessung in seconds
  real(kind=trk) :: tsc_resol_ticks !Genauigkeit der Ticksmessung in ticks
  real(kind=trk), allocatable, dimension(:) :: flop !Anzahl der Gleitkommazahlenoperationen
  real(kind=trk), allocatable, dimension(:) :: perf_first !Performance des erstes Durchlaufs
  real(kind=trk), allocatable, dimension(:) :: perf_avg !Durchschnittliche Performance
  real(kind=trk), allocatable, dimension(:) :: allticks !Anzahl der ticks (Basis Frequenz der CPU)
  real(kind=trk), allocatable, dimension(:) :: times !Dauer der Tests
  real(kind=trk), allocatable, dimension(:) :: first_times !Dauer der ersten Test
  real(kind=trk), allocatable, dimension(:) :: rbytes !Anzahl der Bytes zum Lesen
  real(kind=trk), allocatable, dimension(:) :: wbytes !Anzahl der Bytes zum Schreiben
  logical(kind=lk), allocatable, dimension(:) :: valid !Test is valid
  integer(kind=tik), allocatable, dimension(:) :: test_size_xx, test_size_yy, test_size_zz !Gebietsgroeßen in den Tests
  integer(kind=tik), allocatable, dimension(:) :: test_blk_size_xx, test_blk_size_yy, test_blk_size_zz !Blockgroeßen in den Tests
  integer(kind=tik), allocatable, dimension(:) :: num_reps !Anzahl der Wiederholungen
  real(kind=rk), allocatable, dimension(:,:) ::error_norm2 !Absolute und relative Norm des Fehlervektors ||uu-xx||_2 zur Korrektheitsprüfung
  real(kind=trk) :: numr_byte !Bytes zum Lesen
  real(kind=trk) :: numw_byte !Bytes zum Schreiben
  integer(kind=ik) :: tt,tt1,tt2,tt3,tt4,tt5,tt6,ttt,num_rep,num_rep_first !Testindexen
  integer(kind=ik) :: curr_size, curr_size_xx, curr_size_yy, curr_size_zz !Gebietsgroeße im laufenden Test
  integer(kind=ik) :: curr_blk_size_xx, curr_blk_size_yy, curr_blk_size_zz !Blockgroeße im laufenden Test
  integer(kind=ik) :: num_timer_tests !Anzahl von Messungen für die Bestimmung der Timergenauigkeit
  integer(kind=tik) :: start_tick,end_tick !Variablen für ticks-Messung
  real(kind=trk) :: ticks !Variablen für ticks-Messung
  real(kind=trk) :: start_time,end_time,time  !Variablen für Zeitmessung
  real(kind=trk) :: min_time, min_time_ticks !Angabe für die minimale Dauer eines Testes (Groeßenordnung)
  integer :: iostat, perf_unit !IO variables
  integer(kind=c_int) :: thread_id, num_threads, err_msg
  integer(kind=ik) :: lap_min_time_idx,lap_min_ftime_idx
  real(kind=ik) :: lap_min_time, tmp_real
  real(kind=ik) :: lap_min_ftime
  type(profile_type),dimension(:),allocatable :: profiles
  
  !Parse die Arguments für die Benchmark Konfiguration
  call input_param_read()
  if(input_param%verbosity .gt. 0) call input_param_print()
  benchmark_id=input_param%benchmark_id
  min_size_xx=input_param%min_size_xx
  max_size_xx=input_param%max_size_xx
  min_size_yy=input_param%min_size_yy
  max_size_yy=input_param%max_size_yy
  min_size_zz=input_param%min_size_zz
  max_size_zz=input_param%max_size_zz
  step_size_xx=input_param%step_size_xx
  step_size_yy=input_param%step_size_yy
  step_size_zz=input_param%step_size_zz
  min_time=input_param%min_time
  num_threads=int(input_param%num_threads,kind=c_int)
  min_blk_size_xx=input_param%min_blk_size_xx
  max_blk_size_xx=input_param%max_blk_size_xx
  step_blk_size_xx=input_param%step_blk_size_xx
  min_blk_size_yy=input_param%min_blk_size_yy
  max_blk_size_yy=input_param%max_blk_size_yy
  step_blk_size_yy=input_param%step_blk_size_yy
  min_blk_size_zz=input_param%min_blk_size_zz
  max_blk_size_zz=input_param%max_blk_size_zz
  step_blk_size_zz=input_param%step_blk_size_zz
  
  !Berechnen die Testanzahl für die unterschiedlichen Gebietsgroessen
  num_tests=(1+((max_size_xx-min_size_xx)/step_size_xx))*&
    (1+((max_size_yy-min_size_yy)/step_size_yy))*&
    (1+((max_size_zz-min_size_zz)/step_size_zz))*&
    (1+((max_blk_size_xx-min_blk_size_xx)/step_blk_size_xx))*&
    (1+((max_blk_size_yy-min_blk_size_yy)/step_blk_size_yy))*&
    (1+((max_blk_size_zz-min_blk_size_zz)/step_blk_size_zz))
  
  !Fixiere die Anzahl der Threads, das Pinning und Typ von OMP Scheduling
  call omp_set_dynamic(.false.)
  call omp_set_nested(.false.)
  call omp_set_num_threads(num_threads)
  call omp_set_schedule(omp_sched_static,0)
  !Berechne die Core-Ids für Thread-Pinning
  call set_cpu_affinity(input_param%has_numas, &
    num_threads, input_param%num_numas, input_param%numa_cores)
  !Führe in OMP-Region  das Pinning der Threads aus
  !$omp parallel default(none) &
  !$omp& private(thread_id,err_msg) &
  !$omp& shared(pin_cores,input_param)
  thread_id = omp_get_thread_num()
  err_msg=pin_thread(pin_cores(thread_id+1))
  if(input_param%verbosity .gt. 0) then
    write(*,'(2(A,I0))') "Pin the thread ",thread_id, &
        " to the core ",pin_cores(thread_id+1)
  endif
  if(err_msg .ne. 0) then
    write(*,'(2(A,I0))') "Error: pin_thread ", thread_id, " returns ",err_msg
    stop
  endif
  !$omp end parallel
  !Allocate Arrays für die Performancemessung
  allocate(flop(num_tests))
  allocate(perf_first(num_tests))
  allocate(perf_avg(num_tests))
  allocate(test_size_xx(num_tests))
  allocate(test_size_yy(num_tests))
  allocate(test_size_zz(num_tests))
  allocate(test_blk_size_xx(num_tests))
  allocate(test_blk_size_yy(num_tests))
  allocate(test_blk_size_zz(num_tests))
  allocate(rbytes(num_tests))
  allocate(wbytes(num_tests))
  allocate(num_reps(num_tests))
  allocate(allticks(num_tests))
  allocate(times(num_tests))
  allocate(first_times(num_tests))
  allocate(valid(num_tests))
  allocate(error_norm2(4,num_tests))
  allocate(profiles(6*num_tests))
  !Ueberprüfe die Timer-Aufloesung
  num_timer_tests=int(1.0e6)
  ticks_in_sec=1.0_trk/sec_tsc_cycle()
  write(*,'(A,E16.6)') "Ticks in a second:", ticks_in_sec
  ticks=0.0
  time=0.0
  do tt=1, num_timer_tests
    start_tick=getrdtsc() 
    end_tick=getrdtsc() 
    ticks=ticks+real(end_tick-start_tick,kind=trk)
  enddo
  do tt=1, num_timer_tests
    start_time=get_time() 
    end_time=get_time() 
    time=time+(end_time-start_time)
  enddo
  tsc_resol_ticks = real(ticks/num_timer_tests,kind=trk)
  tsc_resol_sec = tsc_resol_ticks/ticks_in_sec
  
  if(input_param%verbosity .gt. 0) then
    write(*,'(A,E13.6,A,2(E13.6,A))') "TSC: ", &
      & ticks/real(num_timer_tests,kind=trk), " ticks (", &
      & ticks/real(num_timer_tests,kind=trk)/ticks_in_sec, " s), Timer:", &
      & time/real(num_timer_tests,kind=trk), " s;"
  endif
  min_time_ticks = min_time*ticks_in_sec

  !Oeffne eine Datei zur Statistiksspeicherung in einer csv Datei
  if(input_param%verbosity .gt. 0) then
    write(*,'(A,A)') "Datei mit statistik: ",trim(input_param%filepath)
  endif
  !Öffne die Datei
  perf_unit=777
  open(unit=perf_unit,iostat=iostat,file=trim(input_param%filepath), &
      action='READWRITE',status='REPLACE')
  if(iostat .gt. 0) then
    write(*,'(A,A)') &
      & "Error: could not open file for the output: ", input_param%filepath
    stop
  end if
  !Schreibe die Kopfzeile
  write(perf_unit,'(A)',advance="no") "numTest;"
  write(perf_unit,'(A)',advance="no") "benchId;"
  write(perf_unit,'(A)',advance="no") "clear_cache;"
  write(perf_unit,'(A)',advance="no") "threads;"
  write(perf_unit,'(A)',advance="no") "sizex;"
  write(perf_unit,'(A)',advance="no") "sizey;"
  write(perf_unit,'(A)',advance="no") "sizez;"
  write(perf_unit,'(A)',advance="no") "blockx;"
  write(perf_unit,'(A)',advance="no") "blocky;"
  write(perf_unit,'(A)',advance="no") "blockz;"
  write(perf_unit,'(A)',advance="no") "num_reps;"
  write(perf_unit,'(A)',advance="no") "first_flops;"
  write(perf_unit,'(A)',advance="no") "avg_flops;"
  write(perf_unit,'(A)',advance="no") "avg_bw_eff;"
  write(perf_unit,'(A)',advance="no") "time;"
  write(perf_unit,'(A)',advance="no") "first_time;"
  write(perf_unit,'(A)',advance="no") "time_per_one;"
  write(perf_unit,'(A)',advance="no") "flop;"
  write(perf_unit,'(A)',advance="no") "ticks;"
  write(perf_unit,'(A)',advance="no") "|xx|;"
  write(perf_unit,'(A)',advance="no") "|dd|;"
  write(perf_unit,'(A)',advance="no") "|xx-dd|;"
  write(perf_unit,'(A)',advance="no") "|xx-dd|/|xx|;"
  write(perf_unit,'(A)',advance="no") "tscErrTicks;"
  write(perf_unit,'(A)',advance="no") "tscErrSec;"
  write(perf_unit,'(A)',advance="no") "ticksInSec;"
  write(perf_unit,'(A)',advance="no") "rbytes;"
  write(perf_unit,'(A)',advance="no") "wbytes;"
  write(perf_unit,'(A)',advance="no") "has_pinning;"
  write(perf_unit,'(A)',advance="no") "num_numas;"
  write(perf_unit,'(A)',advance="no") "numa_cores;"
  write(perf_unit,'(A)',advance="yes") "jobid;"
  close(perf_unit)
  !Fuehre die Tests für die angegebenen Gebiets- und Blockgroeßen aus
  tt=0
  tt1=0
  tt2=0
  tt3=0
  tt4=0
  tt5=0
  tt6=0
  do curr_size_xx=min_size_xx, max_size_xx, step_size_xx
    do curr_size_yy=min_size_yy, max_size_yy, step_size_yy
      do curr_size_zz=min_size_zz, max_size_zz, step_size_zz
  do curr_blk_size_xx=min_blk_size_xx, max_blk_size_xx, step_blk_size_xx
    do curr_blk_size_yy=min_blk_size_yy, max_blk_size_yy, step_blk_size_yy
      do curr_blk_size_zz=min_blk_size_zz, max_blk_size_zz, step_blk_size_zz
        tt=tt+1
        tt1=(tt-1)*6+1
        tt2=(tt-1)*6+2
        tt3=(tt-1)*6+3
        tt4=(tt-1)*6+4
        tt5=(tt-1)*6+5
        tt6=(tt-1)*6+6
        profiles(tt1)%id=tt1
        profiles(tt2)%id=tt2
        profiles(tt3)%id=tt3
        profiles(tt4)%id=tt4
        profiles(tt5)%id=tt5
        profiles(tt6)%id=tt6
        write(*, "(79('-'))")
        curr_size = curr_size_xx*curr_size_yy*curr_size_zz
        num_flops=real(curr_size,kind=trk)*46.0_trk
        numr_byte=real(curr_size,kind=trk)*27.0_trk*real(rk,kind=trk)
        numw_byte=real(curr_size,kind=trk)*real(rk,kind=trk)
        !Set parameters
        domain_decomp%num_ii=curr_size_yy !Anzahl der inneren Gebietszellen in Y-Richtung
        domain_decomp%num_jj=curr_size_xx !Anzahl der inneren Gebietszellen in X-Richtung 
        domain_decomp%num_kk=curr_size_zz !Anzahl der inneren Gebietszellen in Z-Richtung
        domain_decomp%num_blk_ii=curr_size_yy/curr_blk_size_yy !Anzahl der Bloecke in Y-Richtung
        domain_decomp%num_blk_jj=curr_size_xx/curr_blk_size_xx !Anzahl der Bloecke in X-Richtung
        domain_decomp%num_blk_kk=curr_size_zz/curr_blk_size_zz !Anzahl der Bloecke in Z-Richtung
        domain_decomp%blk_num_ii=curr_blk_size_yy !Anzahl der inneren Blockzellen in Y-Richtung
        domain_decomp%blk_num_jj=curr_blk_size_xx !Anzahl der inneren Blockzellen in X-Richtung
        domain_decomp%blk_num_kk=curr_blk_size_zz !Anzahl der inneren Blockzellen in Z-Richtung
        !Prüfe die Blockgrössen und korregiere diese, wenn notwendig und möglich
        if(benchmark_id .gt. 0) then
          valid(tt)=check_blocking_block3d(domain_decomp)
        else
          valid(tt)=.true.
        endif
        if(input_param%verbosity .gt. 0) then
          write(*, "(79('-'))")
          write(*, "(79('-'))")
          write(*,'(9(A,I0),A)') "TestId: ", tt, &
            & "; von: ", num_tests, & 
            & "; Gebietsgroeße: ", curr_size_xx,&
            & "x", curr_size_yy,&
            & "x", curr_size_zz,&
          & "; Blockgroeße: ", domain_decomp%blk_num_jj,&
            & "x",domain_decomp%blk_num_ii ,&
            & "x",domain_decomp%blk_num_kk,&
            & "; BenchId: ", benchmark_id,";"
          write(*, "(79('-'))")
        endif
        if( .not. valid(tt) ) then
          if(input_param%verbosity .gt. 0) then
            write(*,'(A)') "Test is skipped due to wrong blocking!" 
          endif
          cycle
        endif

        !Allokiere den Speicher für die aktuelle Gebietsgroeße
        !und Initialisiere das Feld uu (3d- und block3d- Implementierung)
        select case (benchmark_id)
          case(0) !3d:
            call get_time_stamp(profiles(tt1)%start_time_sec, profiles(tt1)%start_time_nanosec)
            call alloc_mem3d(curr_size_xx, curr_size_yy, curr_size_zz)
            call init_uu3d()
            call get_time_stamp(profiles(tt1)%end_time_sec, profiles(tt1)%end_time_nanosec)
          case(1) !3d blocked loop:
            call get_time_stamp(profiles(tt1)%start_time_sec, profiles(tt1)%start_time_nanosec)
            call alloc_mem3d(curr_size_xx, curr_size_yy, curr_size_zz)
            call init_uu3d()
            call get_time_stamp(profiles(tt1)%end_time_sec, profiles(tt1)%end_time_nanosec)
          case(2) !block3d:
            call get_time_stamp(profiles(tt1)%start_time_sec, profiles(tt1)%start_time_nanosec)
            call alloc_mem_block3d(domain_decomp)
            call init_uu_block3d()
            call get_time_stamp(profiles(tt1)%end_time_sec, profiles(tt1)%end_time_nanosec)
          case(3) !block3d optimmiert:
            call get_time_stamp(profiles(tt1)%start_time_sec, profiles(tt1)%start_time_nanosec)
            call alloc_mem_block3d(domain_decomp)
            call init_uu_block3d()
            call get_time_stamp(profiles(tt1)%end_time_sec, profiles(tt1)%end_time_nanosec)
          case(4) !block3d C-Implementierung
            call get_time_stamp(profiles(tt1)%start_time_sec, profiles(tt1)%start_time_nanosec)
            call alloc_mem_block3d(domain_decomp)
            call init_uu_block3d()
            call get_time_stamp(profiles(tt1)%end_time_sec, profiles(tt1)%end_time_nanosec)
          case default
            write(*,'(A,I0)') "Error: Falsche Benchamark ID ", benchmark_id
            stop
        end select
        write(profiles(tt1)%label,'(A)') "init_uu3d"
        !Der erste Durchlauf eines Testes
        select case (benchmark_id)
          case (0) !3d:
            call get_time_stamp(profiles(tt2)%start_time_sec, profiles(tt2)%start_time_nanosec)
            if(input_param%clear_cache .gt.0) call init_uu3d()
            start_tick=getrdtsc() 
            call laplacian_simple3d()
            end_tick=getrdtsc()
            call get_time_stamp(profiles(tt2)%end_time_sec, profiles(tt2)%end_time_nanosec)
          case (1) !3d and blocked loop:
            call get_time_stamp(profiles(tt2)%start_time_sec, profiles(tt2)%start_time_nanosec)
            if(input_param%clear_cache .gt.0) call init_uu3d()
            start_tick=getrdtsc() 
            call laplacian_simple_vblocked3d(domain_decomp%blk_num_jj, &
                             & domain_decomp%blk_num_ii,domain_decomp%blk_num_kk)
            end_tick=getrdtsc()
            call get_time_stamp(profiles(tt2)%end_time_sec, profiles(tt2)%end_time_nanosec)
          case (2) !block3d:
            call get_time_stamp(profiles(tt2)%start_time_sec, profiles(tt2)%start_time_nanosec)
            if(input_param%clear_cache .gt.0) call init_uu_block3d()
            start_tick=getrdtsc() 
            call laplacian_block3d()
            end_tick=getrdtsc() 
            call get_time_stamp(profiles(tt2)%end_time_sec, profiles(tt2)%end_time_nanosec)
          case (3) !block3d optimiert:
            call get_time_stamp(profiles(tt2)%start_time_sec, profiles(tt2)%start_time_nanosec)
            if(input_param%clear_cache .gt.0) call init_uu_block3d()
            start_tick=getrdtsc() 
            call laplacian_block3d_unroll()
            end_tick=getrdtsc() 
            call get_time_stamp(profiles(tt2)%end_time_sec, profiles(tt2)%end_time_nanosec)
          case (4) !block3d C-Implementierung
            call get_time_stamp(profiles(tt2)%start_time_sec, profiles(tt2)%start_time_nanosec)
            if(input_param%clear_cache .gt.0) call init_uu_block3d()
            start_tick=getrdtsc()
            call laplacian_block3d_cc(uu_block3d, dd_block3d,&
                &  domain_decomp, num_threads)
            end_tick=getrdtsc() 
            call get_time_stamp(profiles(tt2)%end_time_sec, profiles(tt2)%end_time_nanosec)
          case default
            write(*,'(A,I0)') "Error: Falsche Benchamark ID ", benchmark_id
            stop
        end select
        if(input_param%clear_cache .gt.0) then
          write(profiles(tt2)%label,'(A)') "init_uu3d+1.simple3d"
        else
          write(profiles(tt2)%label,'(A)') "1.simple3d"
        endif
        ticks=ticks+real(end_tick-start_tick,kind=trk)
        time=ticks/ticks_in_sec
        !Sammele die Statistik für der ersten Durchlauf
        perf_first(tt)=(num_flops/time)
        first_times(tt)=time
        !Berechne die Wiederholungsanzahl für die laufende Gebietsgroesse
        if(min_time .lt. time) then
          min_time=min_time*time
        endif
        if (min_time < 1e-6) then
          num_rep_first=input_param%num_repetitions
        else
          num_rep_first = int(1.0*min_time/time,kind=ik)+input_param%num_repetitions
        endif
        if(input_param%verbosity .gt. 1) then
          write(*,'(A,E13.6,A)') "Der erste Durchlauf dauerte: ", time, " s;"
          write(*,'(A,E13.6,A)') "Performance: ", perf_first(tt), " FLOPS;"
          write(*,'(A,I0,A)') "Anzahl der Durchläufe: ", num_rep_first,";"
        endif
        !Wiederhole den Test num_rep Mal, jedoch minimum min_time_ticks
        ticks=0.0_trk
        num_rep=0
        select case (benchmark_id)
          case (0) !3d:
            call get_time_stamp(profiles(tt3)%start_time_sec, profiles(tt3)%start_time_nanosec)
            do while (ticks .lt. min_time_ticks)
              num_rep=num_rep+num_rep_first
              start_tick=getrdtsc() 
              do ttt=1, num_rep
                if(input_param%clear_cache .gt.0) call init_uu3d()
                start_tick=getrdtsc() 
                call laplacian_simple3d()
                end_tick=getrdtsc()
                ticks=ticks+real(end_tick-start_tick,kind=trk)
              end do
            enddo
            call get_time_stamp(profiles(tt3)%end_time_sec, profiles(tt3)%end_time_nanosec)
          case (1) !3d and loop blocked:
            call get_time_stamp(profiles(tt3)%start_time_sec, profiles(tt3)%start_time_nanosec)
            do while (ticks .lt. min_time_ticks)
              num_rep=num_rep+num_rep_first
              do ttt=1, num_rep
                if(input_param%clear_cache .gt.0) call init_uu3d()
                start_tick=getrdtsc() 
                call laplacian_simple_vblocked3d(domain_decomp%blk_num_jj, &
                             & domain_decomp%blk_num_ii,domain_decomp%blk_num_kk)
                end_tick=getrdtsc()
                ticks=ticks+real(end_tick-start_tick,kind=trk)
              end do
            enddo
            call get_time_stamp(profiles(tt3)%end_time_sec, profiles(tt3)%end_time_nanosec)
          case (2) !block3d:
            call get_time_stamp(profiles(tt3)%start_time_sec, profiles(tt3)%start_time_nanosec)
            do while (ticks .lt. min_time_ticks)
              num_rep=num_rep+num_rep_first
              do ttt=1, num_rep
                if(input_param%clear_cache .gt.0) call init_uu_block3d()
                start_tick=getrdtsc()
                call laplacian_block3d()
                end_tick=getrdtsc()
                ticks=ticks+real(end_tick-start_tick,kind=trk)
              end do
            enddo
            call get_time_stamp(profiles(tt3)%end_time_sec, profiles(tt3)%end_time_nanosec)
          case (3) !block3d optimiert:
            call get_time_stamp(profiles(tt3)%start_time_sec, profiles(tt3)%start_time_nanosec)
            do while (ticks .lt. min_time_ticks)
              num_rep=num_rep+num_rep_first
              do ttt=1, num_rep
                if(input_param%clear_cache .gt.0) call init_uu_block3d()
                start_tick=getrdtsc() 
                call laplacian_block3d_unroll()
                end_tick=getrdtsc()
                ticks=ticks+real(end_tick-start_tick,kind=trk)
              end do
            enddo
            call get_time_stamp(profiles(tt3)%end_time_sec, profiles(tt3)%end_time_nanosec)
          case (4) !block3d C-Implementierung
              call get_time_stamp(profiles(tt3)%start_time_sec, profiles(tt3)%start_time_nanosec)
              do while (ticks .lt. min_time_ticks)
              num_rep=num_rep+num_rep_first
              do ttt=1, num_rep
                if(input_param%clear_cache .gt.0) call init_uu_block3d()
                start_tick=getrdtsc()
                call laplacian_block3d_cc(uu_block3d, dd_block3d,&
                  & domain_decomp, num_threads)
                end_tick=getrdtsc()
                ticks=ticks+real(end_tick-start_tick,kind=trk)
              end do
            enddo
            call get_time_stamp(profiles(tt3)%end_time_sec, profiles(tt3)%end_time_nanosec)
          case default
            write(*,'(A,I0)') "Error: Falsche Benchamark ID ", benchmark_id
            stop
        end select                
         if(input_param%clear_cache .gt.0) then
           write(profiles(tt3)%label,'(A)') "init_uu3d+simple3d"
         else
           write(profiles(tt3)%label,'(A)') "simple3d"
         endif    
        time=ticks/ticks_in_sec
        !Sammele die Statistik
        allticks(tt)=ticks
        times(tt)=time
        flop(tt)=real(num_flops,kind=trk)*real(num_rep,kind=trk)
        perf_avg(tt)=flop(tt)/time
        test_size_xx(tt)=int(curr_size_xx,kind=tik)
        test_size_yy(tt)=int(curr_size_yy,kind=tik)
        test_size_zz(tt)=int(curr_size_zz,kind=tik)
        test_blk_size_xx(tt)=int(domain_decomp%blk_num_jj,kind=tik)
        test_blk_size_yy(tt)=int(domain_decomp%blk_num_ii,kind=tik)
        test_blk_size_zz(tt)=int(domain_decomp%blk_num_kk,kind=tik)
        rbytes(tt)=real(numr_byte,kind=trk)*real(num_rep,kind=trk)
        wbytes(tt)=real(numw_byte,kind=trk)*real(num_rep,kind=trk)
        num_reps(tt)=num_rep
        call get_time_stamp(profiles(tt4)%start_time_sec, profiles(tt4)%start_time_nanosec)
        if(input_param%check_solution .gt. 0) then
          !Korrektheitsprüfung
          select case (benchmark_id)
            case(0) !3d:
              error_norm2(1:4,tt)=laplacian_check3d()
            case(1) !3d blocked loop:
              error_norm2(1:4,tt)=laplacian_check3d()
            case(2) !block3d:
              error_norm2(1:4,tt)=laplacian_check_block3d()
            case(3) !block3d optimiert:
              error_norm2(1:4,tt)=laplacian_check_block3d()
            case(4) !block3d C-Implementierung:
              error_norm2(1:4,tt)=laplacian_check_block3d()
          end select
        else
          error_norm2(1:4,tt)=0.0
        end if
        call get_time_stamp(profiles(tt4)%end_time_sec, profiles(tt4)%end_time_nanosec)
        write(profiles(tt4)%label,'(A)') "check3d"
        call get_time_stamp(profiles(tt5)%start_time_sec, profiles(tt5)%start_time_nanosec)
        if(input_param%verbosity .gt. 1) then
          !Performance Ausgabe
          write(*,'(A,I0,A)') "Durchlaeufe: ", num_rep,";"
          write(*,'(A,2(E16.9),A)') "|xx|,|dd|: ", error_norm2(1,tt),error_norm2(2,tt),";"
          write(*,'(A,2(E16.9),A)') "|xx-dd|,|xx-dd|/|xx|: ", error_norm2(3,tt),error_norm2(4,tt),";"
          write(*,'(A,E13.6,A)') "Zeit: ", time," s;"
          write(*,'(A,E13.6,A)') "Zeit per einen Lauf: ", time/real(num_rep,kind=trk)," s;"
          write(*,'(A,E13.6,A)') "Ticks: ", allticks(tt)," ticks;"
          write(*,'(A,E13.6,A)') "Operationen: ",&
            & num_flops*real(num_rep,kind=trk)," FLOP;"
          write(*,'(A,2(E13.6,A))') "Speicherplatz: ",&
            & real(curr_size_xx*curr_size_yy*curr_size_zz*rk,kind=trk)/2.0**10," KB (",&
            & real(curr_size_xx*curr_size_yy*curr_size_zz*rk,kind=trk)/2.0**20," MB);"
          write(*,'(A,E13.6,A)') "Performance: ",&
            & perf_avg(tt)," FLOPS;"; 
          write(*,'(A,E13.6,A)') "Performance: ",&
            & flop(tt)/allticks(tt)," FLOP/Tick;"; 
          write(*,'(A,E13.6,A)') "Bandbreite(R+W): ", &
            & (rbytes(tt)+wbytes(tt))&
            & /(time*(2.0_trk**30.0_trk))," GB/s;"; 
          write(*,'(A,E13.6,A)') "Bandbreite(R+2xW): ",&
            & (rbytes(tt)+2.0_trk*wbytes(tt))&
            & /(time*(2.0_trk**30.0_trk))," GB/s;"; 
          write(*,'(A,E13.6,A)') "Bandbreite(R+W): ", &
            & (rbytes(tt)+wbytes(tt))&
            & /allticks(tt)," B/tick;"; 
          write(*,'(A,E13.6,A)') "Bandbreite(R+2xW): ", &
            & (rbytes(tt)+2.0_trk*wbytes(tt)) &
            & /allticks(tt)," B/tick;";
        endif
        call get_time_stamp(profiles(tt5)%end_time_sec, profiles(tt5)%end_time_nanosec)
        write(profiles(tt5)%label,'(A)') "std-output"
        !Deallokiere Speicher
        select case (benchmark_id)
          case (0) !3d:
            call dealloc_mem3d()
          case (1) !3d blocked loop:
            call dealloc_mem3d()
          case (2) !block3d:
            call dealloc_mem_block3d()
          case (3) !block3d optimiert:
            call dealloc_mem_block3d()
          case (4) !block3d C-Implementierung
            call dealloc_mem_block3d()
          case default
            write(*,'(A,I0)') "Error: Falsche Benchmark ID ", benchmark_id
            stop
        end select
        call get_time_stamp(profiles(tt6)%start_time_sec, profiles(tt6)%start_time_nanosec)
        if(input_param%verbosity .gt. 1) then
          write(*,'(A,A)',advance="yes") "Open file:",trim(input_param%filepath)
        endif
        !Schreibe die Performancewerte
        open(unit=perf_unit,iostat=iostat,file=trim(input_param%filepath), &
        action='READWRITE',status='OLD',position='APPEND')
        if(iostat .gt. 0) then
          write(*,'(A,A)') &
          & "Error: could not open file for the output: ", input_param%filepath
          stop
        end if
        write(perf_unit,'(I0,A)',advance="no") tt,";"
        write(perf_unit,'(I0,A)',advance="no") benchmark_id,";"
        write(perf_unit,'(I0,A)',advance="no") input_param%clear_cache,";"
        write(perf_unit,'(I0,A)',advance="no") num_threads,";"
        write(perf_unit,'(I0,A)',advance="no") test_size_xx(tt),";"
        write(perf_unit,'(I0,A)',advance="no") test_size_yy(tt),";"
        write(perf_unit,'(I0,A)',advance="no") test_size_zz(tt),";"
        write(perf_unit,'(I0,A)',advance="no") test_blk_size_xx(tt),";"
        write(perf_unit,'(I0,A)',advance="no") test_blk_size_yy(tt),";"
        write(perf_unit,'(I0,A)',advance="no") test_blk_size_zz(tt),";"
        write(perf_unit,'(I0,A)',advance="no") num_reps(tt),";"
        write(perf_unit,'(E16.9,A)',advance="no") perf_first(tt),";"
        write(perf_unit,'(E16.9,A)',advance="no") perf_avg(tt),";"
        write(perf_unit,'(E16.9,A)',advance="no") (rbytes(tt) &
              +wbytes(tt))/times(tt), ";"
        write(perf_unit,'(E16.9,A)',advance="no") times(tt),";"
        write(perf_unit,'(E16.9,A)',advance="no") first_times(tt),";"
        write(perf_unit,'(E16.9,A)',advance="no") times(tt)/num_reps(tt),";"
        write(perf_unit,'(E19.6,A)',advance="no") flop(tt),";"
        write(perf_unit,'(E16.9,A)',advance="no") allticks(tt),";"
        write(perf_unit,'(E16.9,A)',advance="no") error_norm2(1,tt),";"
        write(perf_unit,'(E16.9,A)',advance="no") error_norm2(2,tt),";"
        write(perf_unit,'(E16.9,A)',advance="no") error_norm2(3,tt),";"
        write(perf_unit,'(E16.9,A)',advance="no") error_norm2(4,tt),";"
        write(perf_unit,'(E16.9,A)',advance="no") tsc_resol_ticks,";"
        write(perf_unit,'(E16.9,A)',advance="no") tsc_resol_sec,";"
        write(perf_unit,'(E16.9,A)',advance="no") ticks_in_sec,";"
        write(perf_unit,'(E16.9,A)',advance="no") rbytes(tt),";"
        write(perf_unit,'(E16.9,A)',advance="no") wbytes(tt),";"
        write(perf_unit,'(I0,A)',advance="no") input_param%has_numas,";"
        write(perf_unit,'(I0,A)',advance="no") input_param%num_numas,";"
        write(perf_unit,'(I0,A)',advance="no") input_param%numa_cores,";"
        if(tt .eq. num_tests) then
          write(perf_unit,'(I0)',advance="no") input_param%jobid
        else
          write(perf_unit,'(I0)') input_param%jobid
        endif
        if(input_param%verbosity .gt. 1) then
          write(*,'(A,A)',advance="yes") "Close file:",trim(input_param%filepath)
        endif
        close(perf_unit)
        call get_time_stamp(profiles(tt6)%end_time_sec, profiles(tt6)%end_time_nanosec)
        write(profiles(tt6)%label,'(A)') "file-output"
      end do
    end do
  end do
      end do
    end do
  end do

  !Finde die beste Blockgröße
  tt=0
  do curr_size_xx=min_size_xx, max_size_xx, step_size_xx
    do curr_size_yy=min_size_yy, max_size_yy, step_size_yy
      do curr_size_zz=min_size_zz, max_size_zz, step_size_zz
        lap_min_time_idx=1
        lap_min_ftime_idx=1
        lap_min_time=huge(1.0_rk)
        lap_min_ftime=huge(1.0_rk)
        do curr_blk_size_xx=min_blk_size_xx, max_blk_size_xx, step_blk_size_xx
          do curr_blk_size_yy=min_blk_size_yy, max_blk_size_yy, step_blk_size_yy
            do curr_blk_size_zz=min_blk_size_zz, max_blk_size_zz, step_blk_size_zz
              tt=tt+1
              if (valid(tt)) then
                tmp_real=times(tt)/num_reps(tt)
                if(lap_min_time .gt. tmp_real) then
                  lap_min_time=tmp_real
                  lap_min_time_idx=tt
                endif
                tmp_real=first_times(tt)
                if(lap_min_ftime .gt. tmp_real) then
                  lap_min_ftime=tmp_real
                  lap_min_ftime_idx=tt
                endif
              endif
            end do
          end do
        end do
!        write(*, "(79('-'))")
!        write(*,'(A,I0,A)') "Best First Time TestId: ",lap_min_ftime_idx,":"
!        write(*,'(7(A,I0))') "; BenchId: ", benchmark_id,&
!            & "; Gebietsgroeße: ", curr_size_xx,&
!            & "x", curr_size_yy,&
!            & "x", curr_size_zz,&
!            & "; Blockgroeße: ", test_blk_size_xx(lap_min_ftime_idx),&
!            & "x", test_blk_size_yy(lap_min_ftime_idx),&
!            & "x", test_blk_size_zz(lap_min_ftime_idx)
!        write(*,'(3(A,E16.9,A))') "; Time: ", lap_min_ftime," s;",&
!            & "; Avg. perf: ", perf_avg(lap_min_ftime_idx)," Flops;",&
!            & "; First perf: ", perf_first(lap_min_ftime_idx)," Flops;"
        write(*, "(79('-'))")
        write(*,'(A,I0,A)') "Best Avrg. Time TestId: ",lap_min_time_idx,":"
        write(*,'(7(A,I0))') "; BenchId: ", benchmark_id,&
            & "; Gebietsgroeße: ", curr_size_xx,&
            & "x", curr_size_yy,&
            & "x", curr_size_zz,&
            & "; Blockgroeße: ", test_blk_size_xx(lap_min_time_idx),&
            & "x", test_blk_size_yy(lap_min_time_idx),&
            & "x", test_blk_size_zz(lap_min_time_idx)
        write(*,'(3(A,E16.9,A))') "; Time: ", lap_min_time," s;",&
            & "; Avg. perf: ", perf_avg(lap_min_time_idx)," Flops;",&
            & "; First perf: ", perf_first(lap_min_time_idx)," Flops;"
        write(*, "(79('-'))")
      end do
    end do
  end do
  if(input_param%has_profile) then
    call profile_write(profiles,input_param%profile_filepath)
  endif
 
  if(allocated(flop)) deallocate(valid)
  if(allocated(flop)) deallocate(flop)
  if(allocated(perf_first)) deallocate(perf_first)
  if(allocated(perf_avg)) deallocate(perf_avg)
  if(allocated(test_size_xx)) deallocate(test_size_xx)
  if(allocated(test_size_yy)) deallocate(test_size_yy)
  if(allocated(test_size_zz)) deallocate(test_size_zz)
  if(allocated(test_blk_size_xx)) deallocate(test_blk_size_xx)
  if(allocated(test_blk_size_yy)) deallocate(test_blk_size_yy)
  if(allocated(test_blk_size_zz)) deallocate(test_blk_size_zz)
  if(allocated(rbytes)) deallocate(rbytes)
  if(allocated(wbytes)) deallocate(wbytes)
  if(allocated(num_reps)) deallocate(num_reps)
  if(allocated(allticks)) deallocate(allticks)
  if(allocated(times)) deallocate(times)
  if(allocated(first_times)) deallocate(first_times)
  if(allocated(error_norm2)) deallocate(error_norm2)
  if(allocated(profiles)) deallocate(profiles)

end program laplacian3d

