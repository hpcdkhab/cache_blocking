 Benchmark Cache-Blocking
=
German (english follows):

Der Benchmark „Cache-Blocking“ berechnet auf dem dreidimensionales Gebiet D ≡
[0, 1] × [0, 1] × [0, 1] ⊂ R^3 die zweite Ableitung einer zweimal differenzierbaren 
Funktion f (x, y, z). Dieses wird mit Hilfe eines linearen Differentialoperators, 
den sogenannten „3D Laplace-Operator“ ∆, berechnet [52].

Das Diskretisierungsschema des Algorithmus verwendet ein reguläres Gitter mit einem
Schritt h und den 27-Point Stencil für 3D Laplace-Operator. Der diskretisierungsfehler
des Stencil’s in der sechsten Ordnung O(h^6). Der interessierte Leserkann mehr
Information über den verwendeten Stencil in [49] finden.

Benchmark-Compilieren
=
Die Sourcen des Benchmarks „Cache-Blocking“ werden mit dem Bash-Skript comp3d.sh
kompiliert. Das einzige Argument ist der Compilername. Das Programm, die Assemblercodes
und die Optimierungsberichte werden miterstellt.

Benchmark-Ausführung
=
Der Benchmark kann mit dem PBS-Skript job3d_hawk.sh in das PBS-System zur
Ausführung eingestellt werden. Die vordefinierten Umgebungsvariablen am Anfang
der PBS-Skript setzen die wesentlichen Parameter des Benchmarks, wie zum Beispiel
die minimale und maximale Anzahl der OMP-Threads. Falls die Ausführung mit der
unterschiedlichen Anzahl der Threads eingestellt ist, wird der Benchmark mehrmals
in einem Bash-Loop gestartet (siehe job3d.sh) und erstellt je eine Statistik-Datei
per Thread. Die Dateien werden im Verzeichnis data3d/data_JOBID gespeichert. 

Note: Passen Sie am Anfang des Job-Scriptes die Umgebungsvariable PROJECT_DIR an!

Main
=
Die Sourcedatei laplacian3d.f90 definiert die Main-Funktion des Benchmarks.
Der Benchmark kann mit den unterschiedlichen Gittergrößen, der Anzahl der OMP-
Threads und verschiedenen Optimierungsparametern konfiguriert werden. Es sind
fünf Berechnungsmethoden für den 27-Point Stencil des 3D Laplace-Operators 
implementiert.

3D-Laplace implementierung Simple3d
=

Die erste Methode laplacian_simple3d() gehört zum Fortran-Modul mod_laplacian_simple3d.
Die Methode benutzt keine Cache-Blocking-Optimierung.

Die Funktionswerte einer zweimal differenzierbaren Funktion f (x, y, z) und derren
zweite Ableitung ∆f (x, y, z) werden in dreidimensionalen Arrays dd3d und uu3d
gespeichert. Diese Arrays sind am Anfang des Moduls als „public“ deklariert. Die
Methoden alloc_mem3d und dealloc_mem3d verwalten den Speicher der Arrays.

Die Funktionswerte in den Gebietszellen des äußeren Randes, außerhalb des Gebietes
D, werden ebenfalls berechnet und unter den Indizien (0,0:numm_jj+1,0:numm_kk+1),
(0:numm_ii+1,0,0:numm_kk+1), (0:numm_ii+1,0:numm_jj+1,0) im Array uu3d gespeichert.

Die nächste Implementierung der 3D-Laplacian-Operator laplacian_simple_vblocked3d(
...) benutzt das gleiche Schema für die Abbildung des dreidimensionalen Gebiets D
auf den Fortran-Arrays uu3d und dd3d wie die Simple3d Methode.

Der Unterschied besteht darin, dass das Gebiet D blockweise duchgegangen wird.
Dadurch versucht man die Cache-Wiederverwendung zu erhöhen.

3D-Laplace implementierung Block3d
=

Im zweiten Modul mod_laplacian_block3d wird das Blocking des Gebietes in den Untergebieten 
nicht „virtuell“, sondern direkt in der Datenstruktur der Arrays implementiert.
Die ersten drei Indizien im 6-Tupel-Index referenzieren die Zellen in einem Block und
die Letzten drei referenzieren der Block selbst. Die Blöcke des Arrays uu_block3d
enthalten die Halo-Daten, die die Funktionswerte aus den Zellen der inneren Ränder
der benachbarten Blöcken speichern. Somit kann die zweite Ableitung für alle Zellen
eines Blockes berechnet werden, ohne auf die Datenbereiche der benachbarten Blöcke
zu zugreifen.


3D-Laplace implementierung Block3d Unroll
=

Die  Methode laplacian_block3d_unroll() aus dem Modul mod_c_laplacian_block3d
unterscheidet sich von der Methode Block3d darin, dass die Berechnung der
zweiten Ableitung in drei Schritten durchgeführt wird: Zwei inneren Schleifen über
die Indexen ii und jj werden drei Mal für den gleichen Index kk (z-Richtung)
wiederholt. Im ersten Durchgang wird die Summe der Funktionswerten auf der xy-
Ebene mit dem Index kk − 1, im zweiten mit dem Index kk und im dritten mit Index
kk + 1 berechnet. Somit wird versucht, die Cache-Wiederverwendung zusätzlich zu
erhöhen.

Eine C-Implementierung des 3D Laplace-Operators ist in der Datei cc_laplacian_block3d.c
definiert. In den Parametern bekommt die Methode zwei Zeiger auf die Arrays uu_block3d
und dd_block3d , die Gebietsaufteilungsparameter und die Anzahl der
Threads. Die OMP-Parallelelisierung ist nicht durchgeführt.

Die Sourcen des Benchmarks „Cache-Blocking“ werden mit dem Bash-Skript comp3d.sh
kompiliert. Das einzige Argument ist der Compilername. Das Programm, 
die Assemblercodes und die Optimierungsberichte werden miterstellt.

Benchmark-Parameter
=

Der Benchmark hat viele Parameter, weil das Hauptprogramm in einem Lauf mehrere
Gebietsgröße und Gebietsaufteilungen testen kann. Die zu testende Methode, die
Anzahl der Threads und die Pinningstrategie werden dagegen beim Start festgesetzt.

Die wichtigsten Parameter sind

-verbosity: Detailsniveau der Ausgabe auf stdout, 0 - keine Ausgabe (3 ist emp-
fehlenswert);

-benchmark_id: Id-Nummer der zu testenden Methode;

0: Simple3d - keine „Cache-Blocking“-Opimierung;
1: VBlocked3d - Virtuelle „Cache-Blocking“;
2: Block3d - „Cache-Blocking“ mit Blocksaufteilung;
3: Block3d-Unroll - „Cache-Blocking“ mit Blocksaufteilung und drei „aufrollenden“
doppelt geschachtelten Schleifen über die xy-Ebenen des 27-Point Stencils;

-num_threads: Anzahl der OMP-Threads zum Starten;

-size_x, -size_y, -size_z: Anzahl der Gebietszellen in drei Richtungen; Falls man
mehrere Gebietsgroße testen will, können die Intervalle mit den zusätzlichen
Parametern angegeben werden. Für die X-Richtung sind es -min_size_x ,
-max_size_x , -step_size_x . Entsprechend heißen die Intervall-Parameter
für die Y-, und z-Richtungen.

-min_blk_size_x , -max_blk_size_x , -step_blk_size_x : Definition der Blockgrößen
in die X-Richtung. Entsprechend heißen die Intervall-Parameter für die
Y-, und z-Richtungen.

-min_time: Minimale Zeit zum Testen der ausgewählten Methode (Mehr als 1 Sekunde
ist empfehlenswert).

-filepath: Dateiname zur Speicherung der Performance-Statistik;

-clear_cache: Wenn „1“ angegeben ist, dann wird die Methode zur Berechnung der
Funktionswerte vor jedem Aufruf der zu testenden Methode aufgerufen (1 ist
empfehlenswert).

-check_solution: Wenn „1“ angegeben ist, dann wird das Resultat validiert (1 ist
empfehlenswert).

-has_numas: Wenn „1“ ist angegeben, dann werden die Threads regelmäßig über die
NUMA-Nodes verteilt. Wenn „0“ ist angegeben, dann werden die Threads zu
den Cores in der lexikografischen Ordnung zugewiesen.


-num_numas: Anzahl der NUMA-Nodes;

-numa_cores: Anzahl der Cores in einem NUMA-Node;


Benchmark-Ausgabe
=

Nach der Ausführung werden die Statistikdaten im CVS-Format gespeichert. Die
Statistikdaten werden mit der WRITE-Anweisung für jede Gebiets- und Blockgröße
zeilenweise in die CVS-Datei gespeichert. Die wichtigsten Spalten sind

benchId: ID der getesteten Methode (von 0 bis 4);

threads: Anzahl der Threads;

sizex;sizey;sizez: Anzahl der Gebietszellen in X-, Y- und z-Richtung;

blockx;blocky;blockz: Blockgröße;

num_reps: Wie viel Mal war die Methode ausgeführt, um die Anforderung nach der
minimalen Zeit zu erfüllen;

avg_flops: Durchschnittliche Performance der getesteten Methode;

first_flops: Performance der ersten Ausführung der getesteten Methode;

|xx-dd|: L2-Norm des Fehlers zwischen der analytisch berechneten xx und der 
nummerisch berechneten zweiten Ableitung dd;

|xx-dd|/|xx|: Relativer Fehler;


Literaturverzeichnis
=

[52] WikipediA. Laplace-Operator. Accessed: 2020-03-23. Feb. 2020. url: https:
//de.wikipedia.org/wiki/Laplace-Operator.

[49] William Frederick Spotz. „High-Order Compact Finite Difference Schemes for
Computational Mechanics“. In: 1995. Kap. 27-Point O(h6) HOC stencil for
the 3D Laplace operator, S. 110. url: http://citeseerx.ist.psu.edu/
viewdoc/download?doi=10.1.1.56.1164&rep=rep1&type=pdf.
