/**
 * This is an example of how to invoke two instructions in SM4Plugin
 */

#include "riscv.h"

#define SM4_ENC 0
#define SM4_DEC 1

#define sm4ed(rs1, rs2) opcode_R(SM4CUSTOM, 0x00, 0x18, rs1, rs2)
#define sm4ks(rs1, rs2) opcode_R(SM4CUSTOM, 0x00, 0x1a, rs1, rs2)

static const unsigned int FK[4] = {
	0xc6bab1a3, 0x5033aa56, 0x97917d67, 0xdc2270b2
};

static const unsigned int CK[32] = {
    0x150e0700U, 0x312a231cU, 0x4d463f38U, 0x69625b54U,
    0x857e7770U, 0xa19a938cU, 0xbdb6afa8U, 0xd9d2cbc4U,
    0xf5eee7e0U, 0x110a03fcU, 0x2d261f18U, 0x49423b34U,
    0x655e5750U, 0x817a736cU, 0x9d968f88U, 0xb9b2aba4U,
    0xd5cec7c0U, 0xf1eae3dcU, 0x0d06fff8U, 0x29221b14U,
    0x453e3730U, 0x615a534cU, 0x7d766f68U, 0x99928b84U,
    0xb5aea7a0U, 0xd1cac3bcU, 0xede6dfd8U, 0x0902fbf4U,
    0x251e1710U, 0x413a332cU, 0x5d564f48U, 0x79726b64U
};

static inline __attribute__((always_inline)) void vexriscv_sm4_set_key(unsigned char *userkey,
                                                                      const int mode,
                                                                      unsigned char *rk
																	  ){
  unsigned int mk[4];
  unsigned int rk_tmp[36];

  for (int i = 0; i < 4; i++)
  {
    mk[i] = (( unsigned int*)userkey)[i];
    rk_tmp[i] = mk[i] ^ FK[i];
  }

  for (int i = 0; i < 32; i++)
  {
    rk_tmp[i+4] = sm4ks(rk_tmp[i], (rk_tmp[i+1] ^ rk_tmp[i+2] ^ rk_tmp[i+3] ^ CK[i]));
  }

  if(mode == SM4_ENC)
    {
        for (int i = 0; i < 32; i++)
        {
        	((volatile unsigned int*)rk)[i] = (rk_tmp[i+4]);
        }
    }
    else if(mode == SM4_DEC)
    {
        for (int i = 0; i < 32; i++)
        {
        	((volatile unsigned int*)rk)[i] = (rk_tmp[35-i]);
        }
    }
    else
    {
      printf("SET KEY ERROR!");
    }
  }


static inline __attribute__((always_inline)) void vexriscv_sm4_crypto(unsigned char *in,
                                                                      unsigned char *out,
                                                                      unsigned int *rk
  )
  {
	 unsigned int x0, x1, x2, x3 ;

    x0 = ((volatile unsigned int*)in)[0];
    x1 = ((volatile unsigned int*)in)[1];
    x2 = ((volatile unsigned int*)in)[2];
    x3 = ((volatile unsigned int*)in)[3];

    for(int i = 0; i < 8; i++)
    {
    	x0 = sm4ed(x0, (x1 ^ x2 ^ x3 ^ rk[i*4]));
    	x1 = sm4ed(x1, (x2 ^ x3 ^ x0 ^ rk[i*4+1]));
    	x2 = sm4ed(x2, (x3 ^ x0 ^ x1 ^ rk[i*4+2]));
    	x3 = sm4ed(x3, (x0 ^ x1 ^ x2 ^ rk[i*4+3]));

    	if(i == 7)
    	{
    		((volatile unsigned int*)out)[0] = (x3);
    		((volatile unsigned int*)out)[1] = (x2);
    		((volatile unsigned int*)out)[2] = (x1);
    		((volatile unsigned int*)out)[3] = (x0);
    	}
    }
  }


