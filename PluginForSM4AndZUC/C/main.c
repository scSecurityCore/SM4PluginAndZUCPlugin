#include <stdint.h>
#include <string.h>

#include "riscv.h"
#include "type.h"
#include "sm4_custom.h"
#include "zuc_vexriscv.h"
#include "zuc.h"

#define SM4_ENC 0
#define SM4_DEC 1
#define SM4_NR 32


////////////////////////// TEST FOR SM4 /////////////////////////////////
#if 1

static void print_buffer(const char *prefix, void *buffer, unsigned int length)
{
	printf("%8s: ", prefix);
	for (unsigned int i = 0; i < length; i++)
	{
	  printf("%02x", ((uint8_t *)buffer)[i]);
	}
	printf("\r\n");
}

void SM4_encrypt(const unsigned char *in, unsigned char *out, const unsigned char *key);
void SM4_set_key(const unsigned char *userKey, int mode, const unsigned char *key);

const uint8_t key[16] __attribute__((aligned(4))) = {0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xfe, 0xdc, 0xba, 0x98, 0x76, 0x54, 0x32, 0x10};
const uint8_t plaintext[16] __attribute__((aligned(4))) = {0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xfe, 0xdc, 0xba, 0x98, 0x76, 0x54, 0x32, 0x10};
const uint8_t reference[16] __attribute__((aligned(4))) = {0x68, 0x1e, 0xdf, 0x34, 0xd2, 0x06, 0x96, 0x5e, 0x86, 0xb3, 0xe9, 0x4f, 0x53, 0x6e, 0x42, 0x46};
uint8_t sm4key_st[4*SM4_NR] = {0};
uint8_t output[16] = {0};

void main()
{
	printf("Welcome SM4 Test!\r\n");

	//key schedule
	vexriscv_sm4_set_key(key, SM4_ENC, sm4key_st);

	//do crypto
    vexriscv_sm4_crypto(plaintext, output, sm4key_st);

    //show the result
    print_buffer("Plain", plaintext, 16);
    print_buffer("Key", key, 16);
    print_buffer("Result", output, 16);

/*test whether match or not*/
    int match = 1;
    for (int i = 0; i < 16; i++)
    {
        if(output[i] != reference[i])
        {
            match = 0;
        }
    }
    if(match == 1){
        printf("Matched!\r\n");
    } else
    {
        printf("Mismatch!\r\n");
    }

    printf("DONE\r\n");

    while(1);

}
#endif


////////////////////////// TEST FOR ZUC /////////////////////////////////
#if 0
//reference 27bede74  018082da
const uint8_t key[16] __attribute__((aligned(4))) = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
const uint8_t iv[16] __attribute((aligned(4))) = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};

//reference 0657cfa0  7096398b
//const uint8_t key[16] __attribute__((aligned(4))) = {0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff};
//const uint8_t iv[16] __attribute((aligned(4))) = {0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff};


//reference 14f1c272    3279c419
//const uint8_t key[16] __attribute__((aligned(4))) = {0x3d, 0x4c, 0x4b, 0xe9, 0x6a, 0x82, 0xfd, 0xae, 0xb5, 0x8f, 0x64, 0x1d, 0xb1, 0x7b, 0x45, 0x5b};
//const uint8_t iv[16] __attribute((aligned(4))) = {0x84, 0x31, 0x9a, 0xa8, 0xde, 0x69, 0x15, 0xca, 0x1f, 0x6b, 0xda, 0x6b, 0xfb, 0xd8, 0xc7, 0x66};

void main()
{
	unsigned int zuc_result;
	printf("TEST FOR ZUC\r\n");
	int NR = 2;

	vexriscv_zuc_set(key, iv);
	vexriscv_zuc_init();

	for(int i = 0; i < NR; i++) {
		zuc_result = zucwork();
		printf("%08x\r\n", EndianSwap_SW(zuc_result));

	}

	printf("DONE\r\n");
	while(1);

}


//To call the zuc instructions continuously, please refer to the code in zuc_test.h
/*
#define DST_ADDR 0x80000000
void main()
{

	register unsigned int zuc_result1;
	register unsigned int zuc_result2;

	printf("TEST FOR ZUC\r\n");
	int NR = 100000;
	volatile unsigned int * p1;
	volatile unsigned int * p2;
	p1 = DST_ADDR;

	for(int i = 0; i < NR; i++)
	{
		vexriscv_zuc_set(key, iv);
		vexriscv_zuc_init();

		for(int i = 0; i < 16; i++)
		{
			WORK32(p1, zuc_result1, zuc_result2);
			WORK32(p1, zuc_result1, zuc_result2);
			WORK32(p1, zuc_result1, zuc_result2);
			WORK32(p1, zuc_result1, zuc_result2);
		}
	}

//		printf("%08x\r\n", EndianSwap_SW(*(p)));

	printf("DONE\r\n");
	while(1);
}
*/

#endif
