#ifndef ZUC_VEXRISCV_H__
#define ZUC_VEXRISCV_H__

#include <stdint.h>
#include <string.h>

#include "type.h"
#include "riscv.h"
#include "zuc_test.h"

#define zucset(rs1, rs2, pt) opcode_R(ZUCCUSTOM, 0x00, (pt << 3) | 7 |(1 << 5), rs1, rs2)
#define zucinit() opcode_R(ZUCCUSTOM, 0x00, (1 << 3) | 7, 0, 0)
#define zucwork() opcode_R(ZUCCUSTOM, 0x00, 7, 0, 0)

static inline __attribute__((always_inline)) void vexriscv_zuc_set(unsigned char *key, unsigned char *iv){

	unsigned int rst[4];

	{
		rst[0] = zucset(((unsigned int*)key)[0], ((unsigned int*)iv)[0], 0);

		rst[1] = zucset(((unsigned int*)key)[1], ((unsigned int*)iv)[1], 1);

		rst[2] = zucset(((unsigned int*)key)[2], ((unsigned int*)iv)[2], 2);

		rst[3] = zucset(((unsigned int*)key)[3], ((unsigned int*)iv)[3], 3);

	}

}

static inline __attribute__((always_inline)) void vexriscv_zuc_init(){

	unsigned int initrst[4];

	INIT8(initrst[0]);
	INIT8(initrst[1]);
	INIT8(initrst[2]);
	INIT8(initrst[3]);

	initrst[0] = zucwork();
	asm("nop");
}

#endif

