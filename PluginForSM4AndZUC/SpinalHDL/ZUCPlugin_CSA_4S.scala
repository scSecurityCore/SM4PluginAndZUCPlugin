package vexriscv.plugin

import spinal.core._
import vexriscv.{DecoderService, Stageable, VexRiscv}

/**
 * ZUC
 * 1) initSet set s0~s15 according to key and iv
 * 2) init mode
 * 3) work mode
 * -SBI111XXXXXYYYYY000ZZZZZ0110011
 *
 * S = 1, set key and iv, part 1 to 4 decided by BI
 * S = 0, I = 1   init mode
 * S = 0, I = 0   work mode
 */

case class ZUCPlugin_CSA_4S(encoding : MaskedLiteral = M"----111----------000-----0110011") extends Plugin[VexRiscv]{

  object IS_ZUC extends Stageable(Bool)
  object ZUC_C extends Stageable(Bits(31 bits))
  object ZUC_W extends Stageable(Bits(32 bits))
  object ZUC_S extends Stageable(Bits(31 bits))

  val mapping = new{
    def INIT_SEL = 28
    def SET_SEL = 30
  }

  override def setup(pipeline: VexRiscv): Unit = {
    import pipeline.config._

    val decoderService = pipeline.service(classOf[DecoderService])

    decoderService.addDefault(IS_ZUC, False)
    decoderService.add(
      key = encoding,

      List(
        IS_ZUC                    -> True,
        REGFILE_WRITE_VALID        -> True,
        BYPASSABLE_EXECUTE_STAGE  -> False,
        BYPASSABLE_MEMORY_STAGE   -> True,
        RS1_USE                   -> True,
        RS2_USE                   -> True
      )
    )
  }

  override def build(pipeline: VexRiscv): Unit = {
    import pipeline._
    import pipeline.config._

    val sReg = Vec(Reg(Bits(31 bits)) init 0, 16)
    val rReg = Vec(Reg(Bits(32 bits)) init 0, 2)

    def BANKD = EK_d.map(ek_d => ek_d.toLong)
    def BANK0 = S0.map(s0 => s0.toLong)
    def BANK1 = S1.map(s1 => s1.toLong)

    def addMod32(x: Bits, y: Bits): Bits = {
      val ret = Bits(32 bits)
      val c = x.asUInt + y.asUInt
      ret := c(31 downto 0).asBits
      return ret
    }

    def addMod31(x: Bits, y: Bits): Bits ={
      val C = (x.resize(32).asUInt+ y.resize(32).asUInt)
      val ret = ((C >> 31) + C(30 downto 0)).asBits(30 downto 0)
      return ret
    }

    def l1(x: Bits): UInt = {
      val ret = (x ^ x.rotateLeft(2) ^ x.rotateLeft(10) ^ x.rotateLeft(18) ^ x.rotateLeft(24)).asUInt
      return ret
    }

    def l2(x: Bits): UInt = {
      val ret = (x ^ x.rotateLeft(8) ^ x.rotateLeft(14) ^ x.rotateLeft(22) ^ x.rotateLeft(30)).asUInt
      return ret
    }

    def endianSwap(x: Bits): Bits = {
      val ret = x(0, 8 bits) ## x(8, 8 bits) ## x(16, 8 bits) ## x(24, 8 bits)
      return ret
    }

    val onExecute = execute plug new Area{
      import execute._
      //BR
      val BR = new Area {
        val X0 = sReg(15)(30 downto 15) ## sReg(14)(15 downto 0)
        val X1 = sReg(11)(15 downto 0) ## sReg(9)(30 downto 15)
        val X2 = sReg(7)(15 downto 0) ## sReg(5)(30 downto 15)
//        val X3 = sReg(2)(15 downto 0) ## sReg(0)(30 downto 15)
      }

      //F
      val F = new Area {
        val W =  (addMod32((BR.X0 ^ rReg(0)), rReg(1)))
        val WSH = W >> 1
        val w1 = addMod32(rReg(0), BR.X1)
        val w2 = rReg(1) ^ BR.X2
        val sboxAddr0 = l1(w1(15 downto 0) ## w2(31 downto 16))
        val sboxAddr1 = l2(w2(15 downto 0) ## w1(31 downto 16))
      }

      //CSA * 6
      val add6 = new Area {
        val tmp1 = sReg(0) ^ sReg(0).rotateLeft(8) ^ sReg(4).rotateLeft(20)
        val tmp2 = (sReg(0)&(sReg(0).rotateLeft(8)) ^ sReg(0)&(sReg(4).rotateLeft(20)) ^ sReg(0).rotateLeft(8)&sReg(4).rotateLeft(20)).rotateLeft(1)

        val tmp3 = sReg(15).rotateLeft(15) ^ sReg(13).rotateLeft(17) ^ sReg(10).rotateLeft(21)
        val tmp4 = (sReg(15).rotateLeft(15)&sReg(13).rotateLeft(17) ^ sReg(15).rotateLeft(15)&sReg(10).rotateLeft(21) ^ sReg(13).rotateLeft(17)&sReg(10).rotateLeft(21)).rotateLeft(1)

        val tmp5 = tmp1 ^ tmp2 ^ tmp3
        val tmp6 = (tmp1&tmp2 ^ tmp1&tmp3 ^ tmp3&tmp2).rotateLeft(1)

        val tmp7 = tmp5 ^ tmp6 ^ tmp4
        val tmp8 = (tmp5&tmp6 ^ tmp5&tmp4 ^ tmp6&tmp4).rotateLeft(1)

        val tmp_l = addMod31(tmp7, tmp8)
      }

      //CSA * 7
      val add7 = new Area {
        val tmp1 = sReg(0) ^ sReg(0).rotateLeft(8) ^ sReg(4).rotateLeft(20)
        val tmp2 = (sReg(0)&(sReg(0).rotateLeft(8)) ^ sReg(0)&(sReg(4).rotateLeft(20)) ^ sReg(0).rotateLeft(8)&sReg(4).rotateLeft(20)).rotateLeft(1)

        val tmp3 = sReg(15).rotateLeft(15) ^ sReg(13).rotateLeft(17) ^ sReg(10).rotateLeft(21)
        val tmp4 = (sReg(15).rotateLeft(15)&sReg(13).rotateLeft(17) ^ sReg(15).rotateLeft(15)&sReg(10).rotateLeft(21) ^ sReg(13).rotateLeft(17)&sReg(10).rotateLeft(21)).rotateLeft(1)

        val tmp5 = tmp1 ^ tmp2 ^ tmp3
        val tmp6 = (tmp1&tmp2 ^ tmp1&tmp3 ^ tmp3&tmp2).rotateLeft(1)

        val tmp7 = tmp5 ^ tmp6 ^ tmp4
        val tmp8 = (tmp5&tmp6 ^ tmp5&tmp4 ^ tmp6&tmp4).rotateLeft(1)

        val tmp9 = (tmp7 ^ tmp8 ^ F.WSH)
        val tmp10 = (tmp7&F.WSH ^ tmp8&F.WSH ^ tmp7&tmp8).rotateLeft(1)
        val tmp_l = addMod31(tmp9, tmp10)
      }

      insert(ZUC_C) := add6.tmp_l
      insert(ZUC_S) := add7.tmp_l
      insert(ZUC_W) := F.W     //rd result

    }



    memory plug new Area {
      import memory._

      val storaged = Mem(Bits(15 bits), 16) initBigInt((BANKD.map(BigInt(_))))
      val storage0 = Mem(Bits(8 bits), 256) initBigInt(BANK0.map(BigInt(_)))
      val storage1 = Mem(Bits(8 bits), 256) initBigInt(BANK1.map(BigInt(_)))

      val rReg_tmp0 = storage0.readSync(onExecute.F.sboxAddr0(24, 8 bits), !arbitration.isStuck).resize(8) ## storage1.readSync(onExecute.F.sboxAddr0(16, 8 bits), !arbitration.isStuck).resize(8) ## storage0.readSync(onExecute.F.sboxAddr0(8, 8 bits), !arbitration.isStuck).resize(8) ## storage1.readSync(onExecute.F.sboxAddr0(0, 8 bits), !arbitration.isStuck).resize(8)
      val rReg_tmp1 = storage0.readSync(onExecute.F.sboxAddr1(24, 8 bits), !arbitration.isStuck).resize(8) ## storage1.readSync(onExecute.F.sboxAddr1(16, 8 bits), !arbitration.isStuck).resize(8) ## storage0.readSync(onExecute.F.sboxAddr1(8, 8 bits), !arbitration.isStuck).resize(8) ## storage1.readSync(onExecute.F.sboxAddr1(0, 8 bits), !arbitration.isStuck).resize(8)

      when(input(IS_ZUC) && arbitration.isFiring){
        when(input(INSTRUCTION)(mapping.SET_SEL)){
          val initSetAddress = ((input(INSTRUCTION)(mapping.INIT_SEL, 2 bits)) << 2).asUInt
          sReg(initSetAddress) := input(RS1)(7 downto 0) ## storaged.readSync(initSetAddress, !execute.arbitration.isStuck).resize(15) ## input(RS2)(7 downto 0)
          sReg(initSetAddress + 1) := input(RS1)(15 downto 8) ## storaged.readSync(initSetAddress+1, !execute.arbitration.isStuck).resize(15) ## input(RS2)(15 downto 8)
          sReg(initSetAddress + 2) := input(RS1)(23 downto 16) ## storaged.readSync(initSetAddress+2, !execute.arbitration.isStuck).resize(15) ## input(RS2)(23 downto 16)
          sReg(initSetAddress + 3) := input(RS1)(31 downto 24) ## storaged.readSync(initSetAddress+3, !execute.arbitration.isStuck).resize(15) ## input(RS2)(31 downto 24)
          (0 until 2).foreach(i => {
            rReg(i) := B"32'x0"
          })
          output(REGFILE_WRITE_DATA) := sReg(initSetAddress).resize(32)
        } otherwise {
          (0 until 15).foreach(i => {
            sReg(i) := sReg(i + 1)
          })
          rReg(0) := rReg_tmp0
          rReg(1) := rReg_tmp1
          when(input(INSTRUCTION)(mapping.INIT_SEL)){
            val s16_init = input(ZUC_S)
            sReg(15) := (s16_init === B"31'x0") ? B"31'x7fffffff" | s16_init
            output(REGFILE_WRITE_DATA) := endianSwap(input(ZUC_W))
          } otherwise{
            val s16_tmp = input(ZUC_C)
            sReg(15) := (s16_tmp === B"31'x0") ? B"31'x7fffffff" | s16_tmp
            output(REGFILE_WRITE_DATA) := endianSwap(input(ZUC_W) ^ (sReg(2)(15 downto 0) ## sReg(0)(30 downto 15)))
          }
        }
      }
    }
  }

  def EK_d = List(
    0x44D7, 0x26BC, 0x626B, 0x135E, 0x5789, 0x35E2, 0x7135, 0x09AF,
    0x4D78, 0x2F13, 0x6BC4, 0x1AF1, 0x5E26, 0x3C4D, 0x789A, 0x47AC
  )

  def S0 = List(
    0x3e,0x72,0x5b,0x47,0xca,0xe0,0x00,0x33,0x04,0xd1,0x54,0x98,0x09,0xb9,0x6d,0xcb,
    0x7b,0x1b,0xf9,0x32,0xaf,0x9d,0x6a,0xa5,0xb8,0x2d,0xfc,0x1d,0x08,0x53,0x03,0x90,
    0x4d,0x4e,0x84,0x99,0xe4,0xce,0xd9,0x91,0xdd,0xb6,0x85,0x48,0x8b,0x29,0x6e,0xac,
    0xcd,0xc1,0xf8,0x1e,0x73,0x43,0x69,0xc6,0xb5,0xbd,0xfd,0x39,0x63,0x20,0xd4,0x38,
    0x76,0x7d,0xb2,0xa7,0xcf,0xed,0x57,0xc5,0xf3,0x2c,0xbb,0x14,0x21,0x06,0x55,0x9b,
    0xe3,0xef,0x5e,0x31,0x4f,0x7f,0x5a,0xa4,0x0d,0x82,0x51,0x49,0x5f,0xba,0x58,0x1c,
    0x4a,0x16,0xd5,0x17,0xa8,0x92,0x24,0x1f,0x8c,0xff,0xd8,0xae,0x2e,0x01,0xd3,0xad,
    0x3b,0x4b,0xda,0x46,0xeb,0xc9,0xde,0x9a,0x8f,0x87,0xd7,0x3a,0x80,0x6f,0x2f,0xc8,
    0xb1,0xb4,0x37,0xf7,0x0a,0x22,0x13,0x28,0x7c,0xcc,0x3c,0x89,0xc7,0xc3,0x96,0x56,
    0x07,0xbf,0x7e,0xf0,0x0b,0x2b,0x97,0x52,0x35,0x41,0x79,0x61,0xa6,0x4c,0x10,0xfe,
    0xbc,0x26,0x95,0x88,0x8a,0xb0,0xa3,0xfb,0xc0,0x18,0x94,0xf2,0xe1,0xe5,0xe9,0x5d,
    0xd0,0xdc,0x11,0x66,0x64,0x5c,0xec,0x59,0x42,0x75,0x12,0xf5,0x74,0x9c,0xaa,0x23,
    0x0e,0x86,0xab,0xbe,0x2a,0x02,0xe7,0x67,0xe6,0x44,0xa2,0x6c,0xc2,0x93,0x9f,0xf1,
    0xf6,0xfa,0x36,0xd2,0x50,0x68,0x9e,0x62,0x71,0x15,0x3d,0xd6,0x40,0xc4,0xe2,0x0f,
    0x8e,0x83,0x77,0x6b,0x25,0x05,0x3f,0x0c,0x30,0xea,0x70,0xb7,0xa1,0xe8,0xa9,0x65,
    0x8d,0x27,0x1a,0xdb,0x81,0xb3,0xa0,0xf4,0x45,0x7a,0x19,0xdf,0xee,0x78,0x34,0x60
  )

  def S1 = List(
    0x55,0xc2,0x63,0x71,0x3b,0xc8,0x47,0x86,0x9f,0x3c,0xda,0x5b,0x29,0xaa,0xfd,0x77,
    0x8c,0xc5,0x94,0x0c,0xa6,0x1a,0x13,0x00,0xe3,0xa8,0x16,0x72,0x40,0xf9,0xf8,0x42,
    0x44,0x26,0x68,0x96,0x81,0xd9,0x45,0x3e,0x10,0x76,0xc6,0xa7,0x8b,0x39,0x43,0xe1,
    0x3a,0xb5,0x56,0x2a,0xc0,0x6d,0xb3,0x05,0x22,0x66,0xbf,0xdc,0x0b,0xfa,0x62,0x48,
    0xdd,0x20,0x11,0x06,0x36,0xc9,0xc1,0xcf,0xf6,0x27,0x52,0xbb,0x69,0xf5,0xd4,0x87,
    0x7f,0x84,0x4c,0xd2,0x9c,0x57,0xa4,0xbc,0x4f,0x9a,0xdf,0xfe,0xd6,0x8d,0x7a,0xeb,
    0x2b,0x53,0xd8,0x5c,0xa1,0x14,0x17,0xfb,0x23,0xd5,0x7d,0x30,0x67,0x73,0x08,0x09,
    0xee,0xb7,0x70,0x3f,0x61,0xb2,0x19,0x8e,0x4e,0xe5,0x4b,0x93,0x8f,0x5d,0xdb,0xa9,
    0xad,0xf1,0xae,0x2e,0xcb,0x0d,0xfc,0xf4,0x2d,0x46,0x6e,0x1d,0x97,0xe8,0xd1,0xe9,
    0x4d,0x37,0xa5,0x75,0x5e,0x83,0x9e,0xab,0x82,0x9d,0xb9,0x1c,0xe0,0xcd,0x49,0x89,
    0x01,0xb6,0xbd,0x58,0x24,0xa2,0x5f,0x38,0x78,0x99,0x15,0x90,0x50,0xb8,0x95,0xe4,
    0xd0,0x91,0xc7,0xce,0xed,0x0f,0xb4,0x6f,0xa0,0xcc,0xf0,0x02,0x4a,0x79,0xc3,0xde,
    0xa3,0xef,0xea,0x51,0xe6,0x6b,0x18,0xec,0x1b,0x2c,0x80,0xf7,0x74,0xe7,0xff,0x21,
    0x5a,0x6a,0x54,0x1e,0x41,0x31,0x92,0x35,0xc4,0x33,0x07,0x0a,0xba,0x7e,0x0e,0x34,
    0x88,0xb1,0x98,0x7c,0xf3,0x3d,0x60,0x6c,0x7b,0xca,0xd3,0x1f,0x32,0x65,0x04,0x28,
    0x64,0xbe,0x85,0x9b,0x2f,0x59,0x8a,0xd7,0xb0,0x25,0xac,0xaf,0x12,0x03,0xe2,0xf2
  )
}


