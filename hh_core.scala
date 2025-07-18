/*************************************
 * 10-19-2023
 * Author: Blair Reasoner
 * Release version: 1
 * **********************************/
package tsqr_hh_core
import tsqr_hh_datapath.hh_datapath_chisel._
import Binary_Modules.BinaryDesigns._
import FP_Modules.FloatingPointDesigns._
import chisel3._
import chisel3.util._
import Chisel.{log2Ceil, log2Floor}
import chiseltest.RawTester.test
import chisel3.tester._
import java.io.PrintWriter
import scala.collection.mutable

object Main{
  def main(args: Array[String]) : Unit = {
    val sw2 = new PrintWriter("hh_core_16_chisel_test_v2.v")
    sw2.println(getVerilogString(new hh_core_chisel.hh_core(32, 16, 16)))
    sw2.close()
  }
}

object hh_core_chisel{
    class hh_core(bw:Int, streaming_width:Int, CNT_WIDTH: Int)extends Module{
        val io = IO {
            new Bundle() {
                val clk = Input(Clock())
                val rst = Input(Bool())
                val hh_cnt = Input(UInt((CNT_WIDTH).W))
                val d1_rdy = Input(Bool())
                val d1_vld = Input(Bool())
                val d2_rdy = Input(Bool())
                val d2_vld = Input(Bool())
                val vk1_rdy = Input(Bool())
                val vk1_vld = Input(Bool())
                val d3_rdy = Input(Bool())
                val d3_vld = Input(Bool())
                val tk_rdy = Input(Bool())
                val tk_vld = Input(Bool())
                val d4_rdy = Input(Bool())
                val d4_vld = Input(Bool())
                val d5_rdy = Input(Bool())
                val d5_vld = Input(Bool())
                val yjp_rdy = Input(Bool())
                val yjp_vld = Input(Bool())
                val yj_sft = Input(Bool())
                val d4_sft = Input(Bool())
                val hh_st = Input(Bool())
                val mem0_fi = Input(Bool())
                val mem1_fi = Input(Bool())
                val dmx0_mem_ena = Input(Bool())
                val dmx0_mem_wea = Input(UInt((streaming_width*2).W))
                val dmx0_mem_addra = Input(UInt((log2Ceil(streaming_width)-1).W))
                val dmx0_mem_dina = Input(UInt((streaming_width*16).W))
                val dmx0_mem_enb = Input(Bool())
                val dmx0_mem_addrb = Input(UInt((log2Ceil(streaming_width)-1).W))
                val dmx0_mem_doutb = Output(UInt((streaming_width*16).W))
                val dmx1_mem_ena = Input(Bool())
                val dmx1_mem_wea = Input(UInt((streaming_width*2).W))
                val dmx1_mem_addra = Input(UInt((log2Ceil(streaming_width)-1).W))
                val dmx1_mem_dina = Input(UInt((streaming_width*16).W))
                val dmx1_mem_enb = Input(Bool())
                val dmx1_mem_addrb = Input(UInt((log2Ceil(streaming_width)-1).W))
                val dmx1_mem_doutb = Output(UInt((streaming_width*16).W))
                val rtri_mem_ena = Input(Bool())
                val rtri_mem_wea = Input(UInt((streaming_width*2).W))
                val rtri_mem_addra = Input(UInt((log2Ceil(streaming_width)-1).W))
                val rtri_mem_dina = Input(UInt((streaming_width*16).W))
                val rtri_mem_enb = Input(Bool())
                val rtri_mem_addrb = Input(UInt((log2Ceil(streaming_width)-1).W))
                val rtri_mem_doutb = Output(UInt((streaming_width*16).W))
                val hh_dout = Output(UInt((streaming_width*bw).W))
            }
        }

        val hh_dout = Wire(UInt((streaming_width*bw).W))
        withClockAndReset (io.clk, io.rst){
            val hh0_din_rdy = Reg(Bool())
            val hh1_din_rdy = Reg(Bool())
            val hh_din_wire = Wire(UInt((streaming_width*bw).W))
            val hh_din_reg = Reg(UInt((streaming_width*bw).W))
            val hh_din_update = Wire(UInt((streaming_width*bw).W))
            val hh_dout_update = Wire(UInt((streaming_width*bw).W))
            val hh_din = Reg(UInt((streaming_width*bw).W))

            when(io.rst){
                hh0_din_rdy := 0.U
                hh1_din_rdy := 0.U
                hh_din_reg := 0.U
                hh_din := 0.U
            }.otherwise{
                hh0_din_rdy := (io.dmx0_mem_enb & io.rtri_mem_enb)
                hh1_din_rdy := (io.dmx1_mem_enb & io.rtri_mem_enb)
                hh_din_reg := hh_din_wire
                hh_din := hh_din_wire
            }
            

            when(hh0_din_rdy | hh1_din_rdy){
                hh_din_wire := hh_din_update
            }.elsewhen(io.hh_st){
                hh_din_wire := hh_dout_update
        
            }.otherwise{
                hh_din_wire := hh_din_reg
            }

            val dmx_mem_doutb = Wire(UInt((streaming_width*bw/2).W))

            when(hh0_din_rdy){
                dmx_mem_doutb := io.dmx0_mem_doutb
            }.elsewhen(hh1_din_rdy){
                dmx_mem_doutb := io.dmx1_mem_doutb
            }.otherwise{
                dmx_mem_doutb := 0.U
            }

            val myTriMemVec = Wire(Vec(streaming_width/2, UInt(width = bw.W)))

            for(i <- 0 until streaming_width/2){
                myTriMemVec(i) := io.rtri_mem_doutb(streaming_width*bw/2-(i*32)-1,(streaming_width*bw/2-(32*(i+1))))
            }

            val myTriMemVec2 = Wire(Vec(streaming_width/2, UInt(width = bw.W)))
            for(i <- 0 until streaming_width/2){
                myTriMemVec2(streaming_width/2 -1-i) :=  myTriMemVec(i.U + io.hh_cnt)
            }

            val myTriMemWire = Wire(UInt((streaming_width*bw/2).W))
            myTriMemWire := myTriMemVec2.asUInt

            when(hh0_din_rdy | hh1_din_rdy){
                hh_din_update := (Cat(myTriMemWire >> (io.hh_cnt*32.U), dmx_mem_doutb))
            }.elsewhen(io.rst){
                hh_din_update := 0.U
            }.otherwise{
                hh_din_update := hh_din_reg
            }

            val myHhdoutVec = Wire(Vec(streaming_width, UInt(width = bw.W)))

            for(i <- 0 until streaming_width){
                myHhdoutVec(i) := io.hh_dout(streaming_width*bw-(i*32)-1,(streaming_width*bw-(32*(i+1))))
            }

            val myHhdoutVec2 = Wire(Vec(streaming_width, UInt(width = bw.W)))
            for(i <- 0 until streaming_width){
                myHhdoutVec2(streaming_width -1-i) :=  myHhdoutVec(i.U + io.hh_cnt + 1.U)
            }

            val myHhdoutWire = Wire(UInt((streaming_width*bw).W))
            myHhdoutWire := myHhdoutVec2.asUInt

            when(io.rst){
                hh_dout_update := 0.U
            }.elsewhen(io.hh_st){
                hh_dout_update := RegNext(myHhdoutWire) >> (io.hh_cnt*32.U)
            }.otherwise{
                hh_dout_update := hh_din_reg
            }

            val u_dmx0= Module(new simple_dual(bw,streaming_width)).io
            
            u_dmx0.clka := io.clk
            u_dmx0.ena := io.dmx0_mem_ena
            u_dmx0.wea := io.dmx0_mem_wea
            u_dmx0.addra := io.dmx0_mem_addra
            u_dmx0.dina := io.dmx0_mem_dina
            u_dmx0.clkb := io.clk
            u_dmx0.enb := io.dmx0_mem_enb
            u_dmx0.addrb := io.dmx0_mem_addrb
            io.dmx0_mem_doutb := u_dmx0.doutb 

            val u_dmx1= Module(new simple_dual(bw,streaming_width)).io
            u_dmx1.clka := io.clk
            u_dmx1.ena := io.dmx1_mem_ena
            u_dmx1.wea := io.dmx1_mem_wea
            u_dmx1.addra := io.dmx1_mem_addra
            u_dmx1.dina := io.dmx1_mem_dina
            u_dmx1.clkb := io.clk
            u_dmx1.enb := io.dmx1_mem_enb
            u_dmx1.addrb := io.dmx1_mem_addrb
            io.dmx1_mem_doutb := u_dmx1.doutb

            val u_rtri= Module(new simple_dual(bw,streaming_width)).io
            u_rtri.clka := io.clk
            u_rtri.ena := io.rtri_mem_ena
            u_rtri.wea := io.rtri_mem_wea
            u_rtri.addra := io.rtri_mem_addra
            u_rtri.dina := io.rtri_mem_dina
            u_rtri.clkb := io.clk
            u_rtri.enb := io.rtri_mem_enb
            u_rtri.addrb := io.rtri_mem_addrb
            io.rtri_mem_doutb := u_rtri.doutb

            val u_hh_datapath= Module(new hh_datapath_1(bw,streaming_width, CNT_WIDTH))

            u_hh_datapath.io.clk := io.clk
            u_hh_datapath.io.rst := io.rst
            u_hh_datapath.io.hh_cnt := io.hh_cnt
            u_hh_datapath.io.d1_rdy := io.d1_rdy
            u_hh_datapath.io.d1_vld := io.d1_vld
            u_hh_datapath.io.d2_rdy := io.d2_rdy
            u_hh_datapath.io.d2_vld := io.d2_vld
            u_hh_datapath.io.vk1_rdy := io.vk1_rdy
            u_hh_datapath.io.vk1_vld := io.vk1_vld
            u_hh_datapath.io.d3_rdy := io.d3_rdy
            u_hh_datapath.io.d3_vld := io.d3_vld
            u_hh_datapath.io.tk_rdy := io.tk_rdy
            u_hh_datapath.io.tk_vld := io.tk_vld
            u_hh_datapath.io.d4_rdy := io.d4_rdy
            u_hh_datapath.io.d4_vld := io.d4_vld
            u_hh_datapath.io.d5_rdy := io.d5_rdy
            u_hh_datapath.io.d5_vld := io.d5_vld
            u_hh_datapath.io.yjp_rdy := io.yjp_rdy
            u_hh_datapath.io.yjp_vld := io.yjp_vld
            u_hh_datapath.io.yj_sft := io.yj_sft
            u_hh_datapath.io.d4_sft := io.d4_sft
            u_hh_datapath.io.hh_din := hh_din
            hh_dout := u_hh_datapath.io.hh_dout
            when(io.rst){
                io.hh_dout:= 0.U
            }.otherwise{
            io.hh_dout := hh_dout
            }
        }
    }
  class simple_dual(bw:Int, streaming_width:Int)extends BlackBox{
        val io = IO {
            new Bundle() {
                val clka = Input(Clock())
                val clkb = Input(Clock())
                val ena = Input(Bool())
                val enb = Input(Bool())
                val wea = Input(UInt((streaming_width*2).W))
                val addra = Input(UInt((log2Ceil(streaming_width)-1).W))
                val addrb = Input(UInt((log2Ceil(streaming_width)-1).W))
                val dina = Input(UInt((streaming_width*bw/2).W))
                val doutb = Output(UInt((streaming_width*bw/2).W))
            }
        }
    }
 /*   
    class simple_dual(bw:Int, streaming_width:Int)extends Module{
        val io = IO {
            new Bundle() {
                val clka = Input(Clock())
                val clkb = Input(Clock())
                val ena = Input(Bool())
                val enb = Input(Bool())
                val wea = Input(UInt((streaming_width*2).W))
                val addra = Input(UInt((log2Ceil(streaming_width)-1).W))
                val addrb = Input(UInt((log2Ceil(streaming_width)-1).W))
                val dina = Input(UInt((streaming_width*bw/2).W))
                val doutb = Output(UInt((streaming_width*bw/2).W))
            }
        }
        withClock (io.clka){
            val doutb = Reg(UInt((streaming_width*bw/2).W))
            io.doutb := doutb
            val ram = Mem(streaming_width/2, UInt((streaming_width*bw/2).W)) 
            val ramtemp = Wire(Vec(streaming_width/2, UInt(width = (bw).W)))
            val dintemp = Wire(Vec(streaming_width/2, UInt(width = (bw).W)))
            when(io.ena){
                for(i <- 0 until streaming_width/2){
                dintemp(streaming_width/2-1-i) := (io.dina(streaming_width*bw/2-1-(i*bw),streaming_width*bw/2-((i+1)*bw)))&(Cat((io.wea(streaming_width*2-1-(i*4),streaming_width*2-((i+1)*4))),(io.wea(streaming_width*2-1-(i*4),streaming_width*2-((i+1)*4))),(io.wea(streaming_width*2-1-(i*4),streaming_width*2-((i+1)*4))),(io.wea(streaming_width*2-1-(i*4),streaming_width*2-((i+1)*4))),(io.wea(streaming_width*2-1-(i*4),streaming_width*2-((i+1)*4))),(io.wea(streaming_width*2-1-(i*4),streaming_width*2-((i+1)*4))),(io.wea(streaming_width*2-1-(i*4),streaming_width*2-((i+1)*4))),(io.wea(streaming_width*2-1-(i*4),streaming_width*2-((i+1)*4)))))
                ramtemp(streaming_width/2-1-i) := ((ram(io.addra))(streaming_width*bw/2-1-(i*bw),streaming_width*bw/2-((i+1)*bw)))& ~(Cat((io.wea(streaming_width*2-1-(i*4),streaming_width*2-((i+1)*4))),(io.wea(streaming_width*2-1-(i*4),streaming_width*2-((i+1)*4))),(io.wea(streaming_width*2-1-(i*4),streaming_width*2-((i+1)*4))),(io.wea(streaming_width*2-1-(i*4),streaming_width*2-((i+1)*4))),(io.wea(streaming_width*2-1-(i*4),streaming_width*2-((i+1)*4))),(io.wea(streaming_width*2-1-(i*4),streaming_width*2-((i+1)*4))),(io.wea(streaming_width*2-1-(i*4),streaming_width*2-((i+1)*4))),(io.wea(streaming_width*2-1-(i*4),streaming_width*2-((i+1)*4)))))
                }
                ram.write(io.addra, ramtemp.asUInt + dintemp.asUInt)
            }.otherwise{
                for(i <- 0 until streaming_width/2){
                dintemp(i) := 0.U
                ramtemp(i) := 0.U
                }
            }
            withClock (io.clkb){
                when(io.enb){
                    doutb := ram.read(io.addrb)
                }
            }
        }
    }
*/
}