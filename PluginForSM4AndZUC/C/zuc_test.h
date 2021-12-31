#ifndef ZUC_TEST_H__
#define ZUC_TEST_H__

#include <stdint.h>
#include <string.h>

#include "type.h"
#include "riscv.h"

#include "zuc_vexriscv.h"

#define WORK8(pt, rd1, rd2) {\
	 rd1 = zucwork();\
	 asm("nop");\
	 rd2 = zucwork();\
	 *(pt) = rd1;\
	 rd1 = zucwork();\
	 *(pt+1) = rd2;\
	 rd2 = zucwork();\
	 *(pt+2) = rd1;\
	 rd1 = zucwork();\
	 *(pt+3) = rd2;\
	 rd2 = zucwork();\
	 *(pt+4) = rd1;\
	 rd1 = zucwork();\
	 *(pt+5) = rd2;\
	 rd2 = zucwork();\
	 *(pt+6) = rd1;\
	 *(pt+7) = rd2;}

#define WORK32(pt, rd1, rd2) {\
	WORK8(pt, rd1, rd2);\
	WORK8(pt+8, rd1, rd2);\
	WORK8(pt+16, rd1, rd2);\
	WORK8(pt+24, rd1, rd2);\
}

#define INIT(rd) {rd = zucinit(); asm("nop");}
#define INIT2(rd) {INIT(rd); INIT(rd);}
#define INIT4(rd) {INIT2(rd); INIT2(rd);}
#define INIT8(rd) {INIT4(rd); INIT4(rd);}
#define INIT16(rd) {INIT8(rd); INIT8(rd);}
#define INIT32(rd) {INIT16(rd); INIT16(rd);}


#endif
