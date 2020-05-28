#include "domain.h"
#include <stdio.h>
#include <unistd.h>

/*Vektor uu3d - Function F(x,y,z)
Vector L2(F)=d2F/dx2+d2F/dy2+d2F/z2
*/
void laplacian_block3d_cc_(double* uu_first_in, double* dd_first_in, struct type_dom* domain_decomp, int* num_thread)
{
  double* uu = (double*) uu_first_in;
  double* dd = (double*) dd_first_in;
  double* uu_block = (double*)0;
  double* dd_block = (double*)0;
  long long ii,jj,kk;
  long long blk_ii,blk_jj,blk_kk;
  long long num_blk_ii,num_blk_jj,num_blk_kk;
  long long blk_num_ii,blk_num_jj,blk_num_kk;
  long long num_ii;
  long long num_el_block, blk_offset, xy_offset, x_offset;
  long long dd_num_el_block, dd_blk_offset, dd_xy_offset, dd_x_offset;
  double hh, step_inv;
  double tmp1,tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9;

  num_ii=domain_decomp->num_ii;
  num_blk_ii=domain_decomp->num_blk_ii;
  num_blk_jj=domain_decomp->num_blk_jj;
  num_blk_kk=domain_decomp->num_blk_kk;
  blk_num_ii=domain_decomp->blk_num_ii;
  blk_num_jj=domain_decomp->blk_num_jj;
  blk_num_kk=domain_decomp->blk_num_kk;
  hh=1.0/(double)num_ii;
  step_inv=1.0/(30.0*hh*hh);
  num_el_block= (blk_num_ii+2)*(blk_num_jj+2)*(blk_num_kk+2);
  dd_num_el_block= blk_num_ii*blk_num_jj*blk_num_kk;
  xy_offset=(blk_num_ii+2)*(blk_num_jj+2);
  dd_xy_offset=blk_num_ii*blk_num_jj;
  x_offset=blk_num_ii+2;
  dd_x_offset=blk_num_ii;
  for(blk_kk=0; blk_kk<num_blk_kk; blk_kk++)
  {
    for(blk_jj=0; blk_jj<num_blk_jj; blk_jj++)
    {
      for(blk_ii=0; blk_ii<num_blk_ii; blk_ii++)
      {
        blk_offset=(blk_kk*num_blk_jj*num_blk_ii+blk_jj*num_blk_ii+blk_ii)*num_el_block;
        dd_blk_offset=(blk_kk*num_blk_jj*num_blk_ii+blk_jj*num_blk_ii+blk_ii)*dd_num_el_block;
        uu_block=&uu[blk_offset];
        dd_block=&dd[dd_blk_offset];   
        for(kk=1; kk<=blk_num_kk; kk++)
        {
          for(jj=1; jj<=blk_num_jj; jj++)
          {
            for(ii=1; ii<=blk_num_ii; ii++)
            {
              tmp1 = uu_block[ii-1+(jj-1)*x_offset+(kk-1)*xy_offset]+3.0*uu_block[ii+(jj-1)*x_offset+(kk-1)*xy_offset]+uu_block[ii+1+(jj-1)*x_offset+(kk-1)*xy_offset];
              tmp2 = 3.0*uu_block[ii-1+jj*x_offset+(kk-1)*xy_offset]+14.0*uu_block[ii+jj*x_offset+(kk-1)*xy_offset]+3.0*uu_block[ii+1+jj*x_offset+(kk-1)*xy_offset];
              tmp3 = uu_block[ii-1+(jj+1)*x_offset+(kk-1)*xy_offset]+3.0*uu_block[ii+(jj+1)*x_offset+(kk-1)*xy_offset]+uu_block[ii+1+(jj+1)*x_offset+(kk-1)*xy_offset];

              tmp4 = 3.0*uu_block[ii-1+(jj-1)*x_offset+(kk)*xy_offset]+14.0*uu_block[ii+(jj-1)*x_offset+(kk)*xy_offset]+3.0*uu_block[ii+1+(jj-1)*x_offset+(kk)*xy_offset];
              tmp5 = 14.0*uu_block[ii-1+(jj)*x_offset+(kk)*xy_offset]-128.0*uu_block[ii+(jj)*x_offset+(kk)*xy_offset]+14.0*uu_block[ii+1+(jj)*x_offset+(kk)*xy_offset];
              tmp6 = 3.0*uu_block[ii-1+(jj+1)*x_offset+(kk)*xy_offset]+14.0*uu_block[ii+(jj+1)*x_offset+(kk)*xy_offset]+3.0*uu_block[ii+1+(jj+1)*x_offset+(kk)*xy_offset];

              tmp7 = uu_block[ii-1+(jj-1)*x_offset+(kk+1)*xy_offset]+3.0*uu_block[ii+(jj-1)*x_offset+(kk+1)*xy_offset]+uu_block[ii+1+(jj-1)*x_offset+(kk+1)*xy_offset];
              tmp8 = 3.0*uu_block[ii-1+(jj)*x_offset+(kk+1)*xy_offset]+14.0*uu_block[ii+(jj)*x_offset+(kk+1)*xy_offset]+3.0*uu_block[ii+1+(jj)*x_offset+(kk+1)*xy_offset];
              tmp9 = uu_block[ii-1+(jj+1)*x_offset+(kk+1)*xy_offset]+3.0*uu_block[ii+(jj+1)*x_offset+(kk+1)*xy_offset]+uu_block[ii+1+(jj+1)*x_offset+(kk+1)*xy_offset];

              dd_block[ii-1+(jj-1)*dd_x_offset+(kk-1)*dd_xy_offset] = (tmp1+tmp2+tmp3+tmp4+tmp5+tmp6+tmp7+tmp8+tmp9)*step_inv;
             }
          }
        }
      }
    }
  }
}

