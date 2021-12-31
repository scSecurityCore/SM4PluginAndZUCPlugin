
/**
 * The SM4Plugin allow to reduce the instruction count of each SM4 round by providing the following instruction :
 * 1) sm4ed(rs1, rs2).        rd = rs1 ^ L(t(rs2))
 * 2) sm4ks(rs1, rs2).        rd = rs1 ^ L'(t(rs2))
 *
 * Here is what those inner functions mean:
 * - t apply the sbox transformation on every byte of the 32 bits word
 * - L means the linear transformation    L(B) = B ^ (B <<< 2) ^ (B <<< 10) ^ (B <<< 18) ^ (B <<< 24)
 * - L' means the linear transformation   L'(B) = B ^ (B <<< 13) ^ (B <<< 23)
 *
 * You can find a complet example of those instruction usage in sm4_custom.h in vexriscv_sm4_crypto and
 * vexriscv_sm4_set_key. And those function are made to work on little endian.
 *
 * So, on each instruction, the following is done (in order)
 * 1) Get 4 bytes from RS2
 * 2) Read data from ROM according to the 4 bytes obtained in 1)
 * 3) Do linear transformation depending the instruction
 * 4) Xor the result with RS1 and return that as instruction result
 *
 * The instructions are encoded by default as following :
 * --110M0XXXXXYYYYY000ZZZZZ0110011
 *
 * Where :
 * - M=1 mean key schedule, M=0 mean crypto round
 * - XXXXX is the register file source 2 (RS2)
 * - YYYYY is the register file source 1 (RS1)
 * - ZZZZZ is the register file destination
 *
 * We have carried out a practical test on Xilinx Artix-7 FPGA(XC7A50T-1FTG256C), the results are as following :
 *  type      16 bytes    256 bytes     1024 bytes    2048 bytes    8192 bytes    16384 bytes
 *  SM4_SW    3.14        5.43          5.61          5.67          5.69          5.71
 *  SM4_HW    28.13       70.91         79.15         80.04         80.51         81.82
 */


package vexriscv.plugin

import spinal.core._
import vexriscv.{DecoderService, Stageable, VexRiscv}

case class SM4Plugin(encoding : MaskedLiteral = M"--110-0----------000-----0110011") extends Plugin[VexRiscv]{

  object IS_SM4 extends Stageable(Bool)
  object CALC_SM4 extends Stageable(Bits(32 bits))
  val mapping = new{
    def MODE = 26
  }

  override def setup(pipeline: VexRiscv): Unit = {
    import pipeline.config._

    val decoderService = pipeline.service(classOf[DecoderService])

    decoderService.addDefault(IS_SM4, False)
    decoderService.add(
      key = encoding,
      List(
        IS_SM4              -> True,
        REGFILE_WRITE_VALID      -> True,
        BYPASSABLE_EXECUTE_STAGE -> False,
        BYPASSABLE_MEMORY_STAGE  -> False,
        RS1_USE                  -> True,
        RS2_USE                  -> True
      )
    )
  }

  /////////SBOX//////////
  override def build(pipeline: VexRiscv): Unit = {
    import pipeline._
    import pipeline.config._

    def endianSwap(x: Bits): Bits = {
      val ret = x(0, 8 bits) ## x(8, 8 bits) ## x(16, 8 bits) ## x(24, 8 bits)
      return ret
    }

    def BANK = SBOX.map(sbox => sbox.toLong)

    val onExecute = execute plug new Area{
      import execute._
      val romAddress1 = U(input(RS2)(31 downto 24))
      val romAddress2 = U(input(RS2)(23 downto 16))
      val romAddress3 = U(input(RS2)(15 downto 8))
      val romAddress4 = U(input(RS2)(7 downto 0))
    }
    memory plug new Area{
      import memory._

      // SBOX
      val rom = new Area {
        val storage = Mem(Bits(8 bits), 256) initBigInt(BANK.map(BigInt(_)))
        val t1 = storage.readSync(onExecute.romAddress1, !arbitration.isStuck).resize(8)
        val t2 = storage.readSync(onExecute.romAddress2, !arbitration.isStuck).resize(8)
        val t3 = storage.readSync(onExecute.romAddress3, !arbitration.isStuck).resize(8)
        val t4 = storage.readSync(onExecute.romAddress4, !arbitration.isStuck).resize(8)
        val output = t4 ## t3 ## t2 ## t1
      }

      // L
      val wordDesuffle = new Area{
        var output = rom.output ^ rom.output.rotateLeft(2) ^ rom.output.rotateLeft(10) ^ rom.output.rotateLeft(18) ^ rom.output.rotateLeft(24)
        when(input(INSTRUCTION)(mapping.MODE)){
          output := rom.output ^ rom.output.rotateLeft(13) ^ rom.output.rotateLeft(23)
        }
      }

      // XOR
      val xored = endianSwap(wordDesuffle.output) ^ input(RS1)
      insert(CALC_SM4) := xored
    }

    writeBack plug new Area {
      import writeBack._
      when(input(IS_SM4)) {
        output(REGFILE_WRITE_DATA) := input(CALC_SM4)
      }
    }
  }


  def SBOX = List(
    0xd6,    0x90,    0xe9,    0xfe,
    0xcc,    0xe1,    0x3d,    0xb7,
    0x16,    0xb6,    0x14,    0xc2,
    0x28,    0xfb,    0x2c,    0x05,
    0x2b,    0x67,    0x9a,    0x76,
    0x2a,    0xbe,    0x04,    0xc3,
    0xaa,    0x44,    0x13,    0x26,
    0x49,    0x86,    0x06,    0x99,
    0x9c,    0x42,    0x50,    0xf4,
    0x91,    0xef,    0x98,    0x7a,
    0x33,    0x54,    0x0b,    0x43,
    0xed,    0xcf,    0xac,    0x62,
    0xe4,    0xb3,    0x1c,    0xa9,
    0xc9,    0x08,    0xe8,    0x95,
    0x80,    0xdf,    0x94,    0xfa,
    0x75,    0x8f,    0x3f,    0xa6,
    0x47,    0x07,    0xa7,    0xfc,
    0xf3,    0x73,    0x17,    0xba,
    0x83,    0x59,    0x3c,    0x19,
    0xe6,    0x85,    0x4f,    0xa8,
    0x68,    0x6b,    0x81,    0xb2,
    0x71,    0x64,    0xda,    0x8b,
    0xf8,    0xeb,    0x0f,    0x4b,
    0x70,    0x56,    0x9d,    0x35,
    0x1e,    0x24,    0x0e,    0x5e,
    0x63,    0x58,    0xd1,    0xa2,
    0x25,    0x22,    0x7c,    0x3b,
    0x01,    0x21,    0x78,    0x87,
    0xd4,    0x00,    0x46,    0x57,
    0x9f,    0xd3,    0x27,    0x52,
    0x4c,    0x36,    0x02,    0xe7,
    0xa0,    0xc4,    0xc8,    0x9e,
    0xea,    0xbf,    0x8a,    0xd2,
    0x40,    0xc7,    0x38,    0xb5,
    0xa3,    0xf7,    0xf2,    0xce,
    0xf9,    0x61,    0x15,    0xa1,
    0xe0,    0xae,    0x5d,    0xa4,
    0x9b,    0x34,    0x1a,    0x55,
    0xad,    0x93,    0x32,    0x30,
    0xf5,    0x8c,    0xb1,    0xe3,
    0x1d,    0xf6,    0xe2,    0x2e,
    0x82,    0x66,    0xca,    0x60,
    0xc0,    0x29,    0x23,    0xab,
    0x0d,    0x53,    0x4e,    0x6f,
    0xd5,    0xdb,    0x37,    0x45,
    0xde,    0xfd,    0x8e,    0x2f,
    0x03,    0xff,    0x6a,    0x72,
    0x6d,    0x6c,    0x5b,    0x51,
    0x8d,    0x1b,    0xaf,    0x92,
    0xbb,    0xdd,    0xbc,    0x7f,
    0x11,    0xd9,    0x5c,    0x41,
    0x1f,    0x10,    0x5a,    0xd8,
    0x0a,    0xc1,    0x31,    0x88,
    0xa5,    0xcd,    0x7b,    0xbd,
    0x2d,    0x74,    0xd0,    0x12,
    0xb8,    0xe5,    0xb4,    0xb0,
    0x89,    0x69,    0x97,    0x4a,
    0x0c,    0x96,    0x77,    0x7e,
    0x65,    0xb9,    0xf1,    0x09,
    0xc5,    0x6e,    0xc6,    0x84,
    0x18,    0xf0,    0x7d,    0xec,
    0x3a,    0xdc,    0x4d,    0x20,
    0x79,    0xee,    0x5f,    0x3e,
    0xd7,    0xcb,    0x39,    0x48
  )
}
