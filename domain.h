#ifndef _domain_h_
#define _domain_h_

  struct type_dom{
    long long num_ii; //Anzahl der inneren Gebietszellen in Y-Richtung
    long long num_jj; //Anzahl der inneren Gebietszellen in X-Richtung 
    long long num_kk; //Anzahl der inneren Gebietszellen in Z-Richtung
    long long num_blk_ii; //Anzahl der Bloecke in Y-Richtung
    long long num_blk_jj; //Anzahl der Bloecke in X-Richtung
    long long num_blk_kk; //Anzahl der Bloecke in Z-Richtung
    long long blk_num_ii; //Anzahl der inneren Blockzellen in Y-Richtung
    long long blk_num_jj; //Anzahl der inneren Blockzellen in X-Richtung
    long long blk_num_kk; //Anzahl der inneren Blockzellen in Z-Richtung
  };


#endif //_domain_h_
