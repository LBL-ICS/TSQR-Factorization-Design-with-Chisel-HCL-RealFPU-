/*************************************
 * 10-19-2023
 * Author: Blair Reasoner
 * Release version: 1
 * **********************************/

package tsqr_fsm_package
import Binary_Modules.BinaryDesigns._
import FP_Modules.FloatingPointDesigns._
import chisel3._
import chisel3.util._
import Chisel.{log2Ceil, log2Floor}
import chiseltest.RawTester.test
import chisel3.tester._
import chisel3.{RawModule, withClockAndReset}
import java.io.PrintWriter
import scala.collection.mutable

object Main{
  def main(args: Array[String]) : Unit = {
    val sw2 = new PrintWriter("fsm.v")

    sw2.println(getVerilogString(new chisel_fsm.fsm(32, 16, 16)))
    sw2.close()
  }
}
object chisel_fsm{
class fsm(bw:Int, streaming_width:Int, CNT_WIDTH: Int) extends RawModule{

    val clk = IO(Input(Clock()))
    val rst = IO(Input(Bool()))
    val tsqr_en = IO(Input(Bool()))
    val tile_no = IO(Input(UInt((CNT_WIDTH).W)))
    val hh_cnt = IO(Output((UInt((CNT_WIDTH).W))))
    val mx_cnt = IO(Output((UInt((CNT_WIDTH).W))))
    val d1_rdy = IO(Output(Bool()))
    val d1_vld = IO(Output(Bool()))
    val d2_rdy = IO(Output(Bool()))
    val d2_vld = IO(Output(Bool()))
    val vk1_rdy = IO(Output(Bool()))
    val vk1_vld = IO(Output(Bool()))
    val d3_rdy = IO(Output(Bool()))
    val d3_vld = IO(Output(Bool()))
    val tk_rdy = IO(Output(Bool()))
    val tk_vld = IO(Output(Bool()))
    val d4_rdy = IO(Output(Bool()))
    val d4_vld = IO(Output(Bool()))
    val d5_rdy = IO(Output(Bool()))
    val d5_vld = IO(Output(Bool()))
    val yjp_rdy = IO(Output(Bool()))
    val yjp_vld = IO(Output(Bool()))
    val yj_sft = IO(Output(Bool()))
    val d4_sft = IO(Output(Bool()))
    val hh_st = IO(Output(Bool()))
    val mem0_fi = IO(Output(Bool()))
    val mem1_fi = IO(Output(Bool()))
    val tsqr_fi = IO(Output(Bool()))
    val dmx0_mem_ena = IO(Output(Bool()))
    val dmx0_mem_wea = IO(Output(UInt((streaming_width*2).W)))
    val dmx0_mem_addra = IO(Output(UInt((log2Ceil(streaming_width)-1).W)))
    val dmx0_mem_enb = IO(Output(Bool()))
    val dmx0_mem_addrb = IO(Output(UInt((log2Ceil(streaming_width)-1).W)))
    val dmx1_mem_ena = IO(Output(Bool()))
    val dmx1_mem_wea = IO(Output(UInt((streaming_width*2).W)))
    val dmx1_mem_addra = IO(Output(UInt((log2Ceil(streaming_width)-1).W)))
    val dmx1_mem_enb = IO(Output(Bool()))
    val dmx1_mem_addrb = IO(Output(UInt((log2Ceil(streaming_width)-1).W)))
    val rtri_mem_ena = IO(Output(Bool()))
    val rtri_mem_wea = IO(Output(UInt((streaming_width*2).W)))
    val rtri_mem_addra = IO(Output(UInt((log2Ceil(streaming_width)-1).W)))
    val rtri_mem_enb = IO(Output(Bool()))
    val rtri_mem_addrb = IO(Output(UInt((log2Ceil(streaming_width)-1).W)))

    withClockAndReset(clk,rst){
        val DDOT_CY = 12+13*log2Ceil(streaming_width)
        val HQR3_CY = 139  
        val HQR5_CY = 13 
        val HQR7_CY = 129
        val HQR10_CY = 10
        val HQR11_CY = 23
        val YJ_SFT_NO = DDOT_CY + HQR7_CY + HQR10_CY -2
        val D4_SFT_NO = HQR7_CY - 1
        val MEM_RD_CY = 1
        val VK_CY = DDOT_CY + HQR3_CY + HQR5_CY
        val TK_CY = VK_CY + DDOT_CY + HQR7_CY
        val TR_CY_MACRO = DDOT_CY + HQR7_CY + HQR10_CY + HQR11_CY - MEM_RD_CY
        val HH_CY = TK_CY + HQR10_CY + HQR11_CY +3 +2//edit 1-19-24
 
        val hh_en = Reg(Bool())
        val nxt_hh_en = Reg(Bool())//added new
/////
        when(rst){
            nxt_hh_en := 0.U
        }.elsewhen(mem0_fi | mem1_fi){
            nxt_hh_en := 0.U
        }.elsewhen(tsqr_en){
            nxt_hh_en := 1.U
        }

        when(rst){
            hh_en := 0.U
        }.otherwise{
            hh_en := nxt_hh_en
        }
/////
        val cnt = Reg(UInt(CNT_WIDTH.W))
        val nxt_cnt = Wire(UInt(CNT_WIDTH.W))
        val nxt_hh_cnt = Reg(UInt(CNT_WIDTH.W))
        val nxt_mx_cnt = Reg(UInt(CNT_WIDTH.W))

        when(cnt === ((HH_CY).U)){
            nxt_cnt := 0.U
        }.elsewhen(hh_en){
            nxt_cnt := cnt + 1.U
        }.otherwise{
            nxt_cnt := cnt
        }

        when((cnt === (HH_CY).U)&(hh_cnt === (streaming_width.U/2.U-1.U))){
            nxt_hh_cnt := 0.U
        }.elsewhen(hh_en & (cnt === (HH_CY).U)){
            nxt_hh_cnt := hh_cnt + 1.U
        }.otherwise{
            nxt_hh_cnt := hh_cnt
        }

        when((hh_cnt === (streaming_width/2-1).U)&(mx_cnt === (tile_no ))&(cnt ===  (HH_CY).U)){//tile_no -1
            nxt_mx_cnt := 0.U
        }.elsewhen( (hh_cnt === (streaming_width/2-1).U)&(cnt === (HH_CY).U -1.U)){//hh_en &
            nxt_mx_cnt := mx_cnt + 1.U
        }.otherwise{
            nxt_mx_cnt := mx_cnt
        }

        when(rst){
            cnt := 0.U
            hh_cnt := 0.U
            mx_cnt := 0.U
        }.otherwise{
            cnt := nxt_cnt
            hh_cnt := nxt_hh_cnt
            mx_cnt := nxt_mx_cnt
        }

        val tr_cy_reg = Reg(UInt(CNT_WIDTH.W))
        val tr_cnt_en = Reg(Bool())
        val tr_cnt = Reg(UInt(CNT_WIDTH.W))
        val rd_mem_fst = Wire(Bool())
        val wr_mem_st = Wire(Bool())
        val rd_mem_st = Wire(Bool())

        when(rst){
            rd_mem_fst := 0.U
            wr_mem_st := 0.U
            hh_st := 0.U
            rd_mem_st := 0.U
        }.otherwise{ 
            rd_mem_fst := (tsqr_en & (~hh_en | ~nxt_hh_en))
            wr_mem_st := hh_en & (tr_cnt === (TR_CY_MACRO-1).U)
            hh_st := hh_en & (tr_cnt === (TR_CY_MACRO+3).U)
            rd_mem_st := hh_en & (cnt === (VK_CY-2).U)
        }


        val tr_cy = Wire(UInt(CNT_WIDTH.W))

        when(rd_mem_st){
            tr_cy := ((streaming_width/2).U - hh_cnt)
        }.otherwise{
            tr_cy :=  tr_cy_reg
        }

        val hh_fi = Wire(Bool())
        hh_fi := (tr_cnt === (TR_CY_MACRO.U + tr_cy + 1.U))//

        val nxt_tr_cnt = Wire(UInt(CNT_WIDTH.W))
        when(hh_fi){
            nxt_tr_cnt := 0.U
        }.elsewhen(tr_cnt_en){
            nxt_tr_cnt := tr_cnt + 1.U
        }.otherwise{
            nxt_tr_cnt := tr_cnt
        }

        when(rst){
            tr_cnt := 0.U
            tr_cy_reg := 0.U
        }.otherwise{
            tr_cnt := nxt_tr_cnt
            tr_cy_reg := tr_cy
        }

        when(rst){
            tr_cnt_en := 0.B
        }.elsewhen(hh_fi){
            tr_cnt_en := 0.B
        }.elsewhen( hh_en & (cnt === (VK_CY+3).U)){
            tr_cnt_en := 1.B
        }
        val tr_cnt_en_2 = Reg(Bool())

        when(rst){
            tr_cnt_en := 0.B
        }.elsewhen(hh_fi){
            tr_cnt_en := 0.B
        }.elsewhen( hh_en & (cnt === (VK_CY+3).U)){
            tr_cnt_en := 1.B
        }
        when(rst){
            tr_cnt_en_2 := 0.B
        }.elsewhen(hh_fi){
            tr_cnt_en_2 := 0.B
        }.elsewhen( hh_en & (cnt === (VK_CY+2).U)){
            tr_cnt_en_2 := 1.B
        }

        d1_rdy := RegNext(hh_en & cnt===1.U)//+2
        d1_vld := RegNext(hh_en & (cnt === (DDOT_CY+1).U))//+2
        d2_rdy := RegNext(hh_en & (cnt === (DDOT_CY+1).U))
        d2_vld := RegNext(hh_en & (cnt === ((DDOT_CY+1) + HQR3_CY).U))
        vk1_rdy := RegNext(hh_en & (cnt === ( (DDOT_CY+1)+ HQR3_CY).U))
        vk1_vld := RegNext(hh_en & (cnt === (VK_CY+1).U))
        d3_rdy := RegNext(hh_en & (cnt === (VK_CY+1).U))
        d3_vld := RegNext(hh_en & (cnt ===(VK_CY + (DDOT_CY+1)).U))
        tk_rdy := RegNext(hh_en & (cnt ===( VK_CY+(DDOT_CY+1) ).U))
        tk_vld := RegNext(hh_en & (cnt === (TK_CY+1).U))

        d4_rdy := tr_cnt_en & (tr_cnt < tr_cy )
        d4_vld := tr_cnt_en & (tr_cnt >= (DDOT_CY.U)) & (tr_cnt < (DDOT_CY.U + tr_cy))
        d5_rdy := tr_cnt_en & (tr_cnt >= ( DDOT_CY.U+ D4_SFT_NO.U)) & (tr_cnt < ( DDOT_CY.U+ tr_cy +D4_SFT_NO.U ))
        d5_vld := tr_cnt_en & (tr_cnt >= ( DDOT_CY.U+ D4_SFT_NO.U + HQR10_CY.U)) & (tr_cnt < ( DDOT_CY.U+ tr_cy + D4_SFT_NO.U +HQR10_CY.U ))
        yjp_rdy := tr_cnt_en & (tr_cnt >= ( DDOT_CY.U+D4_SFT_NO.U  +HQR10_CY.U )) & (tr_cnt < ( DDOT_CY.U+ tr_cy + D4_SFT_NO.U +HQR10_CY.U ))
        yjp_vld := tr_cnt_en &  tr_cnt < ( DDOT_CY.U+ tr_cy + D4_SFT_NO.U +HQR10_CY.U  + HQR11_CY.U) & (tr_cnt >= ( DDOT_CY.U+ D4_SFT_NO.U +HQR10_CY.U + HQR11_CY.U))

        d4_sft := tr_cnt_en & (tr_cnt >= DDOT_CY.U) & (tr_cnt < ( DDOT_CY.U+ D4_SFT_NO.U + tr_cy))
        //yj_sft :=RegNext(tr_cnt_en & (tr_cnt < ( YJ_SFT_NO.U + tr_cy )))
        yj_sft :=RegNext(tr_cnt_en_2 & (tr_cnt < ( YJ_SFT_NO.U + tr_cy - 1.U )))
      
        val dmx0_mem_enb_wire = Wire(Bool())
        val dmx1_mem_enb_wire = Wire(Bool())
        val rtri_mem_enb_wire = Wire(Bool())   
        val dmx0_mem_ena_wire = Wire(Bool())
        val dmx1_mem_ena_wire = Wire(Bool())
        val rtri_mem_ena_wire = Wire(Bool())   
        val dmx0_mem_enb_reg = Reg(Bool())
        val dmx1_mem_enb_reg = Reg(Bool())
        val rtri_mem_enb_reg = Reg(Bool())   
        val dmx0_mem_ena_reg = Reg(Bool())
        val dmx1_mem_ena_reg = Reg(Bool())
        val rtri_mem_ena_reg = Reg(Bool()) 
        val rd_dmx0_en = Wire(Bool())
        val rd_dmx1_en = Wire(Bool())
        val rd_rtri_en = Wire(Bool())

        when(rst){
            rd_dmx0_en := 0.U
            rd_dmx1_en := 0.U
            rd_rtri_en := 0.U
            dmx0_mem_enb_wire := 0.U
            dmx1_mem_enb_wire := 0.U
            rtri_mem_enb_wire := 0.U
        }.otherwise{
            rd_dmx0_en := ~mx_cnt(0) & (cnt >= (VK_CY).U & cnt < (VK_CY).U + tr_cy + 1.U)
            rd_dmx1_en := mx_cnt(0) & (cnt >=(VK_CY).U  & cnt <  (VK_CY).U + tr_cy + 1.U)
            rd_rtri_en := (cnt >=(VK_CY).U  & cnt < (VK_CY).U + tr_cy + 1.U)
            dmx0_mem_enb_wire := (~mx_cnt(0) & (rd_mem_fst )) | rd_dmx0_en 
            dmx1_mem_enb_wire := (mx_cnt(0) & (rd_mem_fst)) | rd_dmx1_en 
            rtri_mem_enb_wire := rd_mem_fst | rd_rtri_en
        }

        val dmx0_mem_addrb_reg = Reg(UInt((log2Ceil(streaming_width)-1).W))
        val dmx1_mem_addrb_reg = Reg(UInt((log2Ceil(streaming_width)-1).W))
        val rtri_mem_addrb_reg = Reg(UInt((log2Ceil(streaming_width)-1).W))
        val dmx0_mem_addrb_wire = Wire(UInt((log2Ceil(streaming_width)-1).W))
        val dmx1_mem_addrb_wire = Wire(UInt((log2Ceil(streaming_width)-1).W))
        val rtri_mem_addrb_wire = Wire(UInt((log2Ceil(streaming_width)-1).W))
        val dmx0_mem_addra_wire = Wire(UInt((log2Ceil(streaming_width)-1).W))
        val dmx1_mem_addra_wire = Wire(UInt((log2Ceil(streaming_width)-1).W))
        val rtri_mem_addra_wire = Wire(UInt((log2Ceil(streaming_width)-1).W))

        when(rd_mem_fst){
            dmx0_mem_addrb_wire := 0.U
        }.elsewhen(~mx_cnt(0) & (hh_en & (cnt === (VK_CY).U))){
            dmx0_mem_addrb_wire:= hh_cnt
        }.elsewhen(rd_dmx0_en){
            dmx0_mem_addrb_wire := dmx0_mem_addrb_reg + 1.U
        }.otherwise{
            dmx0_mem_addrb_wire := dmx0_mem_addrb_reg
        }

        when(rd_mem_fst){
            dmx1_mem_addrb_wire := 0.U
        }.elsewhen(mx_cnt(0) & (hh_en & (cnt === (VK_CY).U))){
            dmx1_mem_addrb_wire := hh_cnt
        }.elsewhen(rd_dmx1_en){
            dmx1_mem_addrb_wire := dmx1_mem_addrb_reg + 1.U
        }.otherwise{
            dmx1_mem_addrb_wire := dmx1_mem_addrb_reg
        }

        when(rd_mem_fst){
            rtri_mem_addrb_wire := 0.U
        }.elsewhen((hh_en & (cnt === (VK_CY).U))){
            rtri_mem_addrb_wire := hh_cnt
        }.elsewhen(rd_rtri_en){
            rtri_mem_addrb_wire := rtri_mem_addrb_reg + 1.U
        }.otherwise{
            rtri_mem_addrb_wire := rtri_mem_addrb_reg
        }

        val dmx0_mem_wea_reg = Reg(UInt((streaming_width*2).W))
        val dmx1_mem_wea_reg = Reg(UInt((streaming_width*2).W))
        val rtri_mem_wea_reg = Reg(UInt((streaming_width*2).W))
        val dmx0_mem_wea_update = Reg(UInt((streaming_width*2).W))
        val dmx1_mem_wea_update = Reg(UInt((streaming_width*2).W))
        val rtri_mem_wea_update = Reg(UInt((streaming_width*2).W))

        when(rst){
            dmx0_mem_ena_wire := 0.U
            dmx1_mem_ena_wire := 0.U
            rtri_mem_ena_wire := 0.U
        }.otherwise{
            dmx0_mem_ena_wire := ~mx_cnt(0) & (tr_cnt >=(TR_CY_MACRO).U ) & (tr_cnt < ((TR_CY_MACRO).U+ tr_cy))
            dmx1_mem_ena_wire := mx_cnt(0) & (tr_cnt >=(TR_CY_MACRO).U ) & (tr_cnt < ((TR_CY_MACRO).U + tr_cy))
            rtri_mem_ena_wire := (tr_cnt >=(TR_CY_MACRO).U ) & (tr_cnt < ( (TR_CY_MACRO).U+ tr_cy))
        }

        when(~mx_cnt(0) & wr_mem_st){
            dmx0_mem_wea := dmx0_mem_wea_update
        }.otherwise{
            dmx0_mem_wea := dmx0_mem_wea_reg
        }

        when(mx_cnt(0) & wr_mem_st){
            dmx1_mem_wea := dmx1_mem_wea_update
        }.otherwise{
            dmx1_mem_wea := dmx1_mem_wea_reg
        }

        when(wr_mem_st){
            rtri_mem_wea := rtri_mem_wea_update
        }.otherwise{
            rtri_mem_wea := rtri_mem_wea_reg
        }

        when(rst){
            dmx0_mem_wea_reg := 0.U
            dmx1_mem_wea_reg := 0.U
            rtri_mem_wea_reg := 0.U
        }.otherwise{
            dmx0_mem_wea_reg := dmx0_mem_wea 
            dmx1_mem_wea_reg := dmx1_mem_wea
            rtri_mem_wea_reg := rtri_mem_wea
        }

        val dmx0_mem_addra_reg = Reg(UInt((log2Ceil(streaming_width)-1).W))
        val dmx1_mem_addra_reg = Reg(UInt((log2Ceil(streaming_width)-1).W))
        val rtri_mem_addra_reg = Reg(UInt((log2Ceil(streaming_width)-1).W))

        when(~mx_cnt(0) & (hh_en & (tr_cnt === (TR_CY_MACRO).U))){
            dmx0_mem_addra_wire := hh_cnt
        }.elsewhen(dmx0_mem_ena_wire){
            dmx0_mem_addra_wire := dmx0_mem_addra_reg + 1.U
        }.otherwise{
            dmx0_mem_addra_wire := dmx0_mem_addra_reg
        }

        when(mx_cnt(0) & (hh_en & (tr_cnt === (TR_CY_MACRO).U))){
            dmx1_mem_addra_wire := hh_cnt
        }.elsewhen(dmx1_mem_ena_wire){
            dmx1_mem_addra_wire := dmx1_mem_addra_reg + 1.U
        }.otherwise{
            dmx1_mem_addra_wire := dmx1_mem_addra_reg
        }

        when((hh_en & (tr_cnt === (TR_CY_MACRO).U))){
            rtri_mem_addra_wire := hh_cnt
        }.elsewhen(rtri_mem_ena_wire){
            rtri_mem_addra_wire := rtri_mem_addra_reg + 1.U
        }.otherwise{
            rtri_mem_addra_wire := rtri_mem_addra_reg
        }

    when(rst){
        dmx0_mem_addra_reg := 0.U
        dmx1_mem_addra_reg := 0.U
        rtri_mem_addra_reg := 0.U
        dmx0_mem_addrb_reg := 0.U
        dmx1_mem_addrb_reg := 0.U
        rtri_mem_addrb_reg := 0.U
        dmx0_mem_enb_reg := 0.U
        dmx1_mem_enb_reg := 0.U
        rtri_mem_enb_reg := 0.U 
        dmx0_mem_ena_reg := 0.U
        dmx1_mem_ena_reg := 0.U
        rtri_mem_ena_reg := 0.U
    }.otherwise{
        dmx0_mem_addra_reg := dmx0_mem_addra_wire
        dmx1_mem_addra_reg := dmx1_mem_addra_wire
        rtri_mem_addra_reg := rtri_mem_addra_wire
        dmx0_mem_addrb_reg := dmx0_mem_addrb_wire
        dmx1_mem_addrb_reg := dmx1_mem_addrb_wire
        rtri_mem_addrb_reg := rtri_mem_addrb_wire
        dmx0_mem_enb_reg := dmx0_mem_enb_wire
        dmx1_mem_enb_reg := dmx1_mem_enb_wire
        rtri_mem_enb_reg := rtri_mem_enb_wire
        dmx0_mem_ena_reg := dmx0_mem_ena_wire
        dmx1_mem_ena_reg := dmx1_mem_ena_wire
        rtri_mem_ena_reg := rtri_mem_ena_wire

    }
    dmx0_mem_addrb := dmx0_mem_addrb_reg
    dmx1_mem_addrb := dmx1_mem_addrb_reg
    rtri_mem_addrb := rtri_mem_addrb_reg
    dmx0_mem_addra := dmx0_mem_addra_reg
    dmx1_mem_addra := dmx1_mem_addra_reg
    rtri_mem_addra := rtri_mem_addra_reg

   mem0_fi := ~mx_cnt(0) & (tr_cnt === (TR_CY_MACRO.U + tr_cy + 0.U)) & (tr_cy === 1.U)
   mem1_fi := mx_cnt(0) &(tr_cnt === (TR_CY_MACRO.U + tr_cy + 0.U)) & (tr_cy === 1.U)

    tsqr_fi := (mem0_fi | mem1_fi) & (mx_cnt === (tile_no -1.U))//-1

    when(rst){
        dmx0_mem_wea_update := ~(dmx0_mem_wea_update & 0.U)
        dmx1_mem_wea_update := ~(dmx1_mem_wea_update & 0.U)
        rtri_mem_wea_update := ~(rtri_mem_wea_update & 0.U)
    }.elsewhen(hh_en & (tr_cnt === (TR_CY_MACRO-2).U)){
        rtri_mem_wea_update := rtri_mem_wea_update >> (hh_cnt*4.U)
    }.otherwise{
        dmx0_mem_wea_update := ~(dmx0_mem_wea_update & 0.U)
        dmx1_mem_wea_update := ~(dmx1_mem_wea_update & 0.U)
        rtri_mem_wea_update := ~(rtri_mem_wea_update & 0.U)
    }
        dmx0_mem_enb := dmx0_mem_enb_reg
        dmx1_mem_enb := dmx1_mem_enb_reg
        rtri_mem_enb := rtri_mem_enb_reg
        dmx0_mem_ena := dmx0_mem_ena_reg
        dmx1_mem_ena := dmx1_mem_ena_reg
        rtri_mem_ena := rtri_mem_ena_reg

    }
}
}