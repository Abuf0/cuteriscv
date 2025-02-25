module ahb_master
#(
    parameter ADDR_WIDTH    = 32   ,   // 10~64
    parameter DATA_WIDTH    = 32  ,   // 8,16,32,64,128,256,512,1024
    parameter HBURST_WIDTH  = 3  ,   // 0,3
    parameter HPROT_WIDTH   = 4      // 0,4,7
    //parameter HMASTER_WIDTH = 8     // 0~8
)
(
    // ------ From cpucore ------ //
    input                                  HCLK        ,
    input                                  HRESETn     ,
    output logic                           rvalid_ifu  ,
    output logic                           rvalid_lsu  ,
    output logic                           wvalid      ,
    output logic                           rready_ifu  ,
    output logic                           rready_lsu  ,
    output logic [DATA_WIDTH-1:0]          rdata       ,  
    input                                  re          ,
    input       [ADDR_WIDTH-1:0]           raddr       ,
    input       [DATA_WIDTH/8-1:0]         rsel        ,  
    input       [ADDR_WIDTH-1:0]           waddr       ,
    input                                  we          ,  
    input       [DATA_WIDTH-1:0]           wdata       ,  
    input       [DATA_WIDTH/8-1:0]         wsel        ,   
    input       [1:0]                      master_read_id  ,
    // ------ To interconnect ------ //
    output logic [ADDR_WIDTH-1:0]          HADDR       ,
    output logic [HBURST_WIDTH-1:0]        HBURST      ,
    output logic                           HMASTLOCK   ,
    output logic [HPROT_WIDTH-1:0]         HPROT       ,
    output logic [2:0]                     HSIZE       ,
    output logic [1:0]                     HTRANS      ,
    output logic [DATA_WIDTH-1:0]          HWDATA      ,
    output logic                           HWRITE      ,
    // ------ From interconnect ------ //
    //input                                  trans_pend  ,
    input                                  HREADY      ,
    input [DATA_WIDTH-1:0]                 HRDATA      ,
    input                                  HRESP         
);

logic transfer_on;
logic seq_done;
logic [1:0] scnt;
logic [1:0] seq_num;
logic [2:0] wsize;
logic [ADDR_WIDTH-1:0] waddr_real;

// todo
assign seq_num = 2'd1;
//assign rvalid = re && HREADY;
//assign wvalid = we && HREADY;

always@(*) begin
    case(wsel) 
        4'b0001: begin
            waddr_real <= waddr;
            wsize <= 3'b000;
        end
        4'b0010: begin
            waddr_real <= {waddr[ADDR_WIDTH-1:2],2'b01};
            wsize <= 3'b000;
       end
        4'b0100: begin
            waddr_real <= {waddr[ADDR_WIDTH-1:2],2'b10};
            wsize <= 3'b000;
       end
        4'b1000: begin
            waddr_real <= {waddr[ADDR_WIDTH-1:2],2'b11};
            wsize <= 3'b000;
       end
        4'b0011: begin
            waddr_real <= waddr;
            wsize <= 3'b001;
       end
        4'b1100: begin
            waddr_real <= {waddr[ADDR_WIDTH-1:2],2'b10};
            wsize <= 3'b001;
       end
        4'b1111: begin
            waddr_real <= waddr;
            wsize <= 3'b010;
       end
       default: begin
            waddr_real <= waddr;
            wsize <= 3'b010;
       end
    endcase
end

typedef enum logic [2:0] {IDLE,BUSY,NONSEQ,SEQ,WAIT} state_t;
state_t state_c,state_n;
always_ff@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn)
        state_c <= IDLE;
    else 
        state_c <= state_n;
end
always @(*) begin
    if(~HRESETn)
        state_n = IDLE;
    else begin
        state_n = IDLE;
        case(state_c)
            IDLE: begin
                state_n = (transfer_on && HREADY)?  NONSEQ:IDLE;
            end
            BUSY: begin
                if(HREADY) begin
                    //state_n = seq_done? IDLE:SEQ;   // TODO
                    if(transfer_on) state_n = (seq_num==2'd1)? NONSEQ : SEQ;
                    else state_n = IDLE;
                end
                else begin
                    state_n = BUSY;
                end
            end
            NONSEQ: begin
                if(HREADY) begin
                    if(transfer_on)
                        state_n = HRESP?   IDLE:
                                  (seq_num==2'd1)?  NONSEQ : SEQ;
                    else    state_n = IDLE;
                end
                else begin
                    state_n = WAIT;
                    //state_n = NONSEQ;
                end
            end
            WAIT: begin // not real busy
                if(HREADY) begin
                    //state_n = seq_done? IDLE:SEQ;   // TODO
                    if(transfer_on) state_n = (seq_num==2'd1)? NONSEQ : SEQ;
                    else state_n = IDLE;
                end
                else begin
                    state_n = WAIT;
                end
            end
            SEQ:  begin
                if(HREADY) begin
                    if(HRESP)   state_n = IDLE;
                    else        state_n = seq_done? IDLE:SEQ;
                end
                else begin
                    state_n = WAIT;    
                end            
            end
            default:state_n = IDLE;
        endcase
    end
end           
/*
我感觉具体的传输模式是因地制宜的，是事前确定的，
这里选择：Four-beat wrapping burst, WRAP4
*/  

logic [1:0] ahb_read_id;

always_ff@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn)
        ahb_read_id <= 'd0;
    else if(state_c != IDLE)
        ahb_read_id <= we?  'd0 : master_read_id;   // start addr
end

logic first_trans;  // 每个slave的第一笔传输的第一个周期是pending的
logic rvalid_ifu_tmp;
logic rvalid_lsu_tmp;
logic wvalid_tmp;
logic we_d1;
logic [1:0] master_read_id_lat;
logic [1:0] master_read_id_mask;
/******************   todo   ***********************/
//              ________________
//  id      ___/                \___________
//                  ________________
//  id_lat  _______/                \_______
//          _______________     ____________
//  HREADY                 \___/
//                  ___________
//  mask    _______/           \____________ (id_hit pos-> 1, HREADY neg -> 0)
//                  _______     ____
//  rvld_p  _______/       \___/    \_______ (first rvld belong to last id)
//                              ____
//  rvld    ___________________/    \_______
//
/******************   todo   ***********************/

//assign rvalid_ifu = rvalid_ifu_tmp; // TODO
//assign rvalid_lsu = rvalid_lsu_tmp;
//assign rvalid_ifu = HREADY && master_read_id_lat[0] && ~HWRITE && HTRANS[1];
//assign rvalid_lsu = HREADY && master_read_id_lat[1] && ~HWRITE && HTRANS[1];
//assign rvalid_ifu = HREADY && master_read_id[0] && ~HWRITE && HTRANS[1] && ~first_trans;
//assign rvalid_lsu = HREADY && master_read_id[1] && ~HWRITE && HTRANS[1] && ~first_trans;
logic re_d1;
assign rready_ifu = HREADY && master_read_id[0] && ~HWRITE && HTRANS[1];
//assign rready_lsu = HREADY && master_read_id[1] && ~HWRITE && HTRANS[1];
assign rvalid_ifu = HREADY && master_read_id_lat[0] && (re || re_d1) && (state_c == NONSEQ || state_c == WAIT);
assign rvalid_lsu = HREADY && master_read_id_lat[1] && (re || re_d1) && (state_c == NONSEQ || state_c == WAIT);
assign rready_lsu = rvalid_lsu;

assign wvalid = HREADY && HWRITE && HTRANS[1] && (state_c == NONSEQ || state_c == WAIT) && ~(we && ~we_d1); // TODO 
//assign first_trans = (re && state_c == IDLE && state_n != IDLE);
//assign first_trans = (re && state_c == IDLE && state_n != IDLE) && (master_read_id != master_read_id_lat);
always_ff@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn)
        master_read_id_lat <= 'd0;
    else if(HREADY)
        master_read_id_lat <= master_read_id;
end
always_ff@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn)
        re_d1 <= 1'b0;
    else
        re_d1 <= re;
end
/*
always_ff@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn)
        first_trans <= 1'b0;
    else if(state_c == IDLE && state_n != IDLE)
        first_trans <= 1'b1;
    else
        first_trans <= 1'b0;
end
*/
always_ff@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn)
        rvalid_ifu_tmp <= 1'b0;
    else if(re && HREADY && master_read_id[0] && (state_c == NONSEQ))
        rvalid_ifu_tmp <= 1'b1;
    else 
        rvalid_ifu_tmp <= 1'b0;
end
always_ff@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn)
        rvalid_lsu_tmp <= 1'b0;
    else if(re && HREADY && master_read_id[1] && (state_c == NONSEQ))
        rvalid_lsu_tmp <= 1'b1;
    else 
        rvalid_lsu_tmp <= 1'b0;
end
always_ff@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn)
        we_d1 <= 1'b0;
    else
        we_d1 <= we;
end
assign wvalid_tmp = (HWRITE && HREADY && HTRANS[1]);
/*    
always_ff@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn)
        transfer_on <= 1'b0;
    else if(cmd_buff[47:46]==2'b00 && (cmd_buff[55:49]==8'b0 || cmd_buff[55:49]==8'b1))
        transfer_on <= 1'b1;
    else
        transfer_on <= 1'b0;
end
*/
assign transfer_on = re || we;
always_ff@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn)
        seq_done <= 1'b0;
    else if(state_c == SEQ && scnt == seq_num-1)
        seq_done <= 1'b1;
    else 
        seq_done <= 1'b0;
end
always_ff@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn)
        scnt <= 2'd0;
    else if(state_c == SEQ)
        scnt <= scnt+1'b1;
    else if(state_c ==  IDLE)
        scnt <= 2'd0;
end

//assign HTRANS = state_n;     
assign HTRANS = (state_n == IDLE)?  2'b00 :
                (state_n == NONSEQ || (state_c == NONSEQ && state_n == WAIT))?  2'b10 :
                (state_n == SEQ || (state_c == SEQ && state_n == WAIT))?  2'b11 : 2'b01;

assign HPROT = 4'd1;    // todo with instr data
/*
always_ff@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn)
        HBURST <= 'd0;
    else if(state_c == IDLE && state_n != IDLE)
        HBURST <= 3'b000;   // SINGLE
end
*/
assign HBURST = 3'b000;
/*
always_ff@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn)
        HSIZE <= 'd0;
    else if(state_c != IDLE)
        HSIZE <= we?    wsize : 3'b010;   // WORD
    else
        HSIZE <= 3'b010;
end
*/
assign HSIZE = we? wsize : 3'b010;
/*
always_ff@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn)
        HWRITE <= 1'b0;
    //else if(state_c == IDLE && state_n != IDLE)
    else if(state_c !=  IDLE)
        HWRITE <= we;  //优先回写内存
    else
        HWRITE <= 1'b0;
end
*/
assign HWRITE = (state_n == IDLE)?  1'b0 : we;

always_ff@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn)
        HWDATA <= 'd0;
    //else if(state_c == IDLE && state_n != IDLE && we)
    else if(state_n != IDLE && we)
        HWDATA <= wdata; 
end

//assign HWDATA = (state_c == IDLE)?  'b0 : wdata;
/*
always_ff@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn)
        HMASTLOCK <= 1'b0;
    else if(state_c == IDLE && state_n != IDLE)
        HMASTLOCK <= 1'b1; 
    else if(state_n == IDLE)
        HMASTLOCK <= 1'b0;
end
*/
assign HMASTLOCK = (state_n != IDLE);
logic [ADDR_WIDTH-1:0] HADDR_next;
assign HADDR_next = HADDR + 'd4;
//always_ff@(posedge HCLK or negedge HRESETn) begin
//    if(~HRESETn)
//        HADDR <= 'd0;
//    else if(state_c != IDLE)
always@(*) begin
    if(state_n != IDLE)
        HADDR <= we?    waddr_real : raddr;   // start addr
    else if(state_n == SEQ) begin
        if(HADDR[3:0] > HADDR_next[3:0])    // 16-byte boundary
            HADDR <= HADDR_next & {{(ADDR_WIDTH-5){1'b1}},5'b01111};
    end
    else if(HRESP==1'b1) begin
        HADDR <= we?    waddr_real : raddr; 
    end
    else begin
        HADDR <= we?    waddr_real : raddr;
    end
end
/*
always_ff@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn) begin
        rdata <= 'd0;
    end
    else if(re && HREADY) begin
        rdata <= HRDATA;
    end
end
*/
assign rdata = HRDATA;
endmodule
//-----------------------------------------------------------------------------
// The confidential and proprietary information contained in this file may
// only be used by a person authorised under and to the extent permitted
// by a subsisting licensing agreement from ARM Limited.
//
//            (C) COPYRIGHT 2001-2013-2025 ARM Limited.
//                ALL RIGHTS RESERVED
//
// This entire notice must be reproduced on all copies of this file
// and copies of this file may only be made by a person if such person is
// permitted to do so under the terms of a subsisting license agreement
// from ARM Limited.
//
//      SVN Information
//
//      Checked In          : $Date: 2012-10-15 18:01:36 +0100 (Mon, 15 Oct 2012) $
//
//      Revision            : $Revision: 225465 $
//
//      Release Information : Cortex-M System Design Kit-r1p0-01rel0
//
//-----------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
//  Abstract            : BusMatrixLite is a wrapper module that wraps around
//                        the BusMatrix module to give AHB Lite compliant
//                        slave and master interfaces.
//
//-----------------------------------------------------------------------------

`timescale 1ns/1ps

module ahb_connect (

    // Common AHB signals
    HCLK,
    HRESETn,

    // System Address Remap control
    REMAP,

    // Input port SI0 (inputs from master 0)
    HADDRS1,
    HTRANSS1,
    HWRITES1,
    HSIZES1,
    HBURSTS1,
    HPROTS1,
    HWDATAS1,
    HMASTLOCKS1,

    // Output port MI0 (inputs from slave 0)
    HRDATAM1,
    HREADYOUTM1,
    HRESPM1,

    // Output port MI1 (inputs from slave 1)
    HRDATAM2,
    HREADYOUTM2,
    HRESPM2,

    // Output port MI2 (inputs from slave 2)
    HRDATAM3,
    HREADYOUTM3,
    HRESPM3,

    // Output port MI3 (inputs from slave 3)
    HRDATAM4,
    HREADYOUTM4,
    HRESPM4,

    // Output port MI4 (inputs from slave 4)
    HRDATAM5,
    HREADYOUTM5,
    HRESPM5,

    // Output port MI5 (inputs from slave 5)
    HRDATAM6,
    HREADYOUTM6,
    HRESPM6,

    // Output port MI6 (inputs from slave 6)
    HRDATAM7,
    HREADYOUTM7,
    HRESPM7,

    // Scan test dummy signals; not connected until scan insertion
    SCANENABLE,   // Scan Test Mode Enable
    SCANINHCLK,   // Scan Chain Input


    // Output port MI0 (outputs to slave 0)
    HSELM1,
    HADDRM1,
    HTRANSM1,
    HWRITEM1,
    HSIZEM1,
    HBURSTM1,
    HPROTM1,
    HWDATAM1,
    HMASTLOCKM1,
    HREADYMUXM1,

    // Output port MI1 (outputs to slave 1)
    HSELM2,
    HADDRM2,
    HTRANSM2,
    HWRITEM2,
    HSIZEM2,
    HBURSTM2,
    HPROTM2,
    HWDATAM2,
    HMASTLOCKM2,
    HREADYMUXM2,

    // Output port MI2 (outputs to slave 2)
    HSELM3,
    HADDRM3,
    HTRANSM3,
    HWRITEM3,
    HSIZEM3,
    HBURSTM3,
    HPROTM3,
    HWDATAM3,
    HMASTLOCKM3,
    HREADYMUXM3,

    // Output port MI3 (outputs to slave 3)
    HSELM4,
    HADDRM4,
    HTRANSM4,
    HWRITEM4,
    HSIZEM4,
    HBURSTM4,
    HPROTM4,
    HWDATAM4,
    HMASTLOCKM4,
    HREADYMUXM4,

    // Output port MI4 (outputs to slave 4)
    HSELM5,
    HADDRM5,
    HTRANSM5,
    HWRITEM5,
    HSIZEM5,
    HBURSTM5,
    HPROTM5,
    HWDATAM5,
    HMASTLOCKM5,
    HREADYMUXM5,

    // Output port MI5 (outputs to slave 5)
    HSELM6,
    HADDRM6,
    HTRANSM6,
    HWRITEM6,
    HSIZEM6,
    HBURSTM6,
    HPROTM6,
    HWDATAM6,
    HMASTLOCKM6,
    HREADYMUXM6,

    // Output port MI6 (outputs to slave 6)
    HSELM7,
    HADDRM7,
    HTRANSM7,
    HWRITEM7,
    HSIZEM7,
    HBURSTM7,
    HPROTM7,
    HWDATAM7,
    HMASTLOCKM7,
    HREADYMUXM7,

    // Input port SI0 (outputs to master 0)
    HRDATAS1,
    HREADYS1,
    HRESPS1,

    // Scan test dummy signals; not connected until scan insertion
    SCANOUTHCLK   // Scan Chain Output

    );

// -----------------------------------------------------------------------------
// Input and Output declarations
// -----------------------------------------------------------------------------

    // Common AHB signals
    input         HCLK;            // AHB System Clock
    input         HRESETn;         // AHB System Reset

    // System Address Remap control
    input   [3:0] REMAP;           // System Address REMAP control

    // Input port SI0 (inputs from master 0)
    input  [31:0] HADDRS1;         // Address bus
    input   [1:0] HTRANSS1;        // Transfer type
    input         HWRITES1;        // Transfer direction
    input   [2:0] HSIZES1;         // Transfer size
    input   [2:0] HBURSTS1;        // Burst type
    input   [3:0] HPROTS1;         // Protection control
    input  [31:0] HWDATAS1;        // Write data
    input         HMASTLOCKS1;     // Locked Sequence

    // Output port MI0 (inputs from slave 0)
    input  [31:0] HRDATAM1;        // Read data bus
    input         HREADYOUTM1;     // HREADY feedback
    input         HRESPM1;         // Transfer response

    // Output port MI1 (inputs from slave 1)
    input  [31:0] HRDATAM2;        // Read data bus
    input         HREADYOUTM2;     // HREADY feedback
    input         HRESPM2;         // Transfer response

    // Output port MI2 (inputs from slave 2)
    input  [31:0] HRDATAM3;        // Read data bus
    input         HREADYOUTM3;     // HREADY feedback
    input         HRESPM3;         // Transfer response

    // Output port MI3 (inputs from slave 3)
    input  [31:0] HRDATAM4;        // Read data bus
    input         HREADYOUTM4;     // HREADY feedback
    input         HRESPM4;         // Transfer response

    // Output port MI4 (inputs from slave 4)
    input  [31:0] HRDATAM5;        // Read data bus
    input         HREADYOUTM5;     // HREADY feedback
    input         HRESPM5;         // Transfer response

    // Output port MI5 (inputs from slave 5)
    input  [31:0] HRDATAM6;        // Read data bus
    input         HREADYOUTM6;     // HREADY feedback
    input         HRESPM6;         // Transfer response

    // Output port MI6 (inputs from slave 6)
    input  [31:0] HRDATAM7;        // Read data bus
    input         HREADYOUTM7;     // HREADY feedback
    input         HRESPM7;         // Transfer response

    // Scan test dummy signals; not connected until scan insertion
    input         SCANENABLE;      // Scan enable signal
    input         SCANINHCLK;      // HCLK scan input


    // Output port MI0 (outputs to slave 0)
    output        HSELM1;          // Slave Select
    output [31:0] HADDRM1;         // Address bus
    output  [1:0] HTRANSM1;        // Transfer type
    output        HWRITEM1;        // Transfer direction
    output  [2:0] HSIZEM1;         // Transfer size
    output  [2:0] HBURSTM1;        // Burst type
    output  [3:0] HPROTM1;         // Protection control
    output [31:0] HWDATAM1;        // Write data
    output        HMASTLOCKM1;     // Locked Sequence
    output        HREADYMUXM1;     // Transfer done

    // Output port MI1 (outputs to slave 1)
    output        HSELM2;          // Slave Select
    output [31:0] HADDRM2;         // Address bus
    output  [1:0] HTRANSM2;        // Transfer type
    output        HWRITEM2;        // Transfer direction
    output  [2:0] HSIZEM2;         // Transfer size
    output  [2:0] HBURSTM2;        // Burst type
    output  [3:0] HPROTM2;         // Protection control
    output [31:0] HWDATAM2;        // Write data
    output        HMASTLOCKM2;     // Locked Sequence
    output        HREADYMUXM2;     // Transfer done

    // Output port MI2 (outputs to slave 2)
    output        HSELM3;          // Slave Select
    output [31:0] HADDRM3;         // Address bus
    output  [1:0] HTRANSM3;        // Transfer type
    output        HWRITEM3;        // Transfer direction
    output  [2:0] HSIZEM3;         // Transfer size
    output  [2:0] HBURSTM3;        // Burst type
    output  [3:0] HPROTM3;         // Protection control
    output [31:0] HWDATAM3;        // Write data
    output        HMASTLOCKM3;     // Locked Sequence
    output        HREADYMUXM3;     // Transfer done

    // Output port MI3 (outputs to slave 3)
    output        HSELM4;          // Slave Select
    output [31:0] HADDRM4;         // Address bus
    output  [1:0] HTRANSM4;        // Transfer type
    output        HWRITEM4;        // Transfer direction
    output  [2:0] HSIZEM4;         // Transfer size
    output  [2:0] HBURSTM4;        // Burst type
    output  [3:0] HPROTM4;         // Protection control
    output [31:0] HWDATAM4;        // Write data
    output        HMASTLOCKM4;     // Locked Sequence
    output        HREADYMUXM4;     // Transfer done

    // Output port MI4 (outputs to slave 4)
    output        HSELM5;          // Slave Select
    output [31:0] HADDRM5;         // Address bus
    output  [1:0] HTRANSM5;        // Transfer type
    output        HWRITEM5;        // Transfer direction
    output  [2:0] HSIZEM5;         // Transfer size
    output  [2:0] HBURSTM5;        // Burst type
    output  [3:0] HPROTM5;         // Protection control
    output [31:0] HWDATAM5;        // Write data
    output        HMASTLOCKM5;     // Locked Sequence
    output        HREADYMUXM5;     // Transfer done

    // Output port MI5 (outputs to slave 5)
    output        HSELM6;          // Slave Select
    output [31:0] HADDRM6;         // Address bus
    output  [1:0] HTRANSM6;        // Transfer type
    output        HWRITEM6;        // Transfer direction
    output  [2:0] HSIZEM6;         // Transfer size
    output  [2:0] HBURSTM6;        // Burst type
    output  [3:0] HPROTM6;         // Protection control
    output [31:0] HWDATAM6;        // Write data
    output        HMASTLOCKM6;     // Locked Sequence
    output        HREADYMUXM6;     // Transfer done

    // Output port MI6 (outputs to slave 6)
    output        HSELM7;          // Slave Select
    output [31:0] HADDRM7;         // Address bus
    output  [1:0] HTRANSM7;        // Transfer type
    output        HWRITEM7;        // Transfer direction
    output  [2:0] HSIZEM7;         // Transfer size
    output  [2:0] HBURSTM7;        // Burst type
    output  [3:0] HPROTM7;         // Protection control
    output [31:0] HWDATAM7;        // Write data
    output        HMASTLOCKM7;     // Locked Sequence
    output        HREADYMUXM7;     // Transfer done

    // Input port SI0 (outputs to master 0)
    output [31:0] HRDATAS1;        // Read data bus
    output        HREADYS1;     // HREADY feedback
    output        HRESPS1;         // Transfer response

    // Scan test dummy signals; not connected until scan insertion
    output        SCANOUTHCLK;     // Scan Chain Output

// -----------------------------------------------------------------------------
// Wire declarations
// -----------------------------------------------------------------------------

    // Common AHB signals
    wire         HCLK;            // AHB System Clock
    wire         HRESETn;         // AHB System Reset

    // System Address Remap control
    wire   [3:0] REMAP;           // System REMAP signal

    // Input Port SI0
    wire  [31:0] HADDRS1;         // Address bus
    wire   [1:0] HTRANSS1;        // Transfer type
    wire         HWRITES1;        // Transfer direction
    wire   [2:0] HSIZES1;         // Transfer size
    wire   [2:0] HBURSTS1;        // Burst type
    wire   [3:0] HPROTS1;         // Protection control
    wire  [31:0] HWDATAS1;        // Write data
    wire         HMASTLOCKS1;     // Locked Sequence

    wire  [31:0] HRDATAS1;        // Read data bus
    wire         HREADYS1;     // HREADY feedback
    wire         HRESPS1;         // Transfer response

    // Output Port MI0
    wire         HSELM1;          // Slave Select
    wire  [31:0] HADDRM1;         // Address bus
    wire   [1:0] HTRANSM1;        // Transfer type
    wire         HWRITEM1;        // Transfer direction
    wire   [2:0] HSIZEM1;         // Transfer size
    wire   [2:0] HBURSTM1;        // Burst type
    wire   [3:0] HPROTM1;         // Protection control
    wire  [31:0] HWDATAM1;        // Write data
    wire         HMASTLOCKM1;     // Locked Sequence
    wire         HREADYMUXM1;     // Transfer done

    wire  [31:0] HRDATAM1;        // Read data bus
    wire         HREADYOUTM1;     // HREADY feedback
    wire         HRESPM1;         // Transfer response

    // Output Port MI1
    wire         HSELM2;          // Slave Select
    wire  [31:0] HADDRM2;         // Address bus
    wire   [1:0] HTRANSM2;        // Transfer type
    wire         HWRITEM2;        // Transfer direction
    wire   [2:0] HSIZEM2;         // Transfer size
    wire   [2:0] HBURSTM2;        // Burst type
    wire   [3:0] HPROTM2;         // Protection control
    wire  [31:0] HWDATAM2;        // Write data
    wire         HMASTLOCKM2;     // Locked Sequence
    wire         HREADYMUXM2;     // Transfer done

    wire  [31:0] HRDATAM2;        // Read data bus
    wire         HREADYOUTM2;     // HREADY feedback
    wire         HRESPM2;         // Transfer response

    // Output Port MI2
    wire         HSELM3;          // Slave Select
    wire  [31:0] HADDRM3;         // Address bus
    wire   [1:0] HTRANSM3;        // Transfer type
    wire         HWRITEM3;        // Transfer direction
    wire   [2:0] HSIZEM3;         // Transfer size
    wire   [2:0] HBURSTM3;        // Burst type
    wire   [3:0] HPROTM3;         // Protection control
    wire  [31:0] HWDATAM3;        // Write data
    wire         HMASTLOCKM3;     // Locked Sequence
    wire         HREADYMUXM3;     // Transfer done

    wire  [31:0] HRDATAM3;        // Read data bus
    wire         HREADYOUTM3;     // HREADY feedback
    wire         HRESPM3;         // Transfer response

    // Output Port MI3
    wire         HSELM4;          // Slave Select
    wire  [31:0] HADDRM4;         // Address bus
    wire   [1:0] HTRANSM4;        // Transfer type
    wire         HWRITEM4;        // Transfer direction
    wire   [2:0] HSIZEM4;         // Transfer size
    wire   [2:0] HBURSTM4;        // Burst type
    wire   [3:0] HPROTM4;         // Protection control
    wire  [31:0] HWDATAM4;        // Write data
    wire         HMASTLOCKM4;     // Locked Sequence
    wire         HREADYMUXM4;     // Transfer done

    wire  [31:0] HRDATAM4;        // Read data bus
    wire         HREADYOUTM4;     // HREADY feedback
    wire         HRESPM4;         // Transfer response

    // Output Port MI4
    wire         HSELM5;          // Slave Select
    wire  [31:0] HADDRM5;         // Address bus
    wire   [1:0] HTRANSM5;        // Transfer type
    wire         HWRITEM5;        // Transfer direction
    wire   [2:0] HSIZEM5;         // Transfer size
    wire   [2:0] HBURSTM5;        // Burst type
    wire   [3:0] HPROTM5;         // Protection control
    wire  [31:0] HWDATAM5;        // Write data
    wire         HMASTLOCKM5;     // Locked Sequence
    wire         HREADYMUXM5;     // Transfer done

    wire  [31:0] HRDATAM5;        // Read data bus
    wire         HREADYOUTM5;     // HREADY feedback
    wire         HRESPM5;         // Transfer response

    // Output Port MI5
    wire         HSELM6;          // Slave Select
    wire  [31:0] HADDRM6;         // Address bus
    wire   [1:0] HTRANSM6;        // Transfer type
    wire         HWRITEM6;        // Transfer direction
    wire   [2:0] HSIZEM6;         // Transfer size
    wire   [2:0] HBURSTM6;        // Burst type
    wire   [3:0] HPROTM6;         // Protection control
    wire  [31:0] HWDATAM6;        // Write data
    wire         HMASTLOCKM6;     // Locked Sequence
    wire         HREADYMUXM6;     // Transfer done

    wire  [31:0] HRDATAM6;        // Read data bus
    wire         HREADYOUTM6;     // HREADY feedback
    wire         HRESPM6;         // Transfer response

    // Output Port MI6
    wire         HSELM7;          // Slave Select
    wire  [31:0] HADDRM7;         // Address bus
    wire   [1:0] HTRANSM7;        // Transfer type
    wire         HWRITEM7;        // Transfer direction
    wire   [2:0] HSIZEM7;         // Transfer size
    wire   [2:0] HBURSTM7;        // Burst type
    wire   [3:0] HPROTM7;         // Protection control
    wire  [31:0] HWDATAM7;        // Write data
    wire         HMASTLOCKM7;     // Locked Sequence
    wire         HREADYMUXM7;     // Transfer done

    wire  [31:0] HRDATAM7;        // Read data bus
    wire         HREADYOUTM7;     // HREADY feedback
    wire         HRESPM7;         // Transfer response


// -----------------------------------------------------------------------------
// Signal declarations
// -----------------------------------------------------------------------------
    wire   [3:0] tie_hi_4;
    wire         tie_hi;
    wire         tie_low;
    wire   [1:0] i_hrespS1;

    wire   [3:0]        i_hmasterM1;
    wire   [1:0] i_hrespM1;
    wire   [3:0]        i_hmasterM2;
    wire   [1:0] i_hrespM2;
    wire   [3:0]        i_hmasterM3;
    wire   [1:0] i_hrespM3;
    wire   [3:0]        i_hmasterM4;
    wire   [1:0] i_hrespM4;
    wire   [3:0]        i_hmasterM5;
    wire   [1:0] i_hrespM5;
    wire   [3:0]        i_hmasterM6;
    wire   [1:0] i_hrespM6;
    wire   [3:0]        i_hmasterM7;
    wire   [1:0] i_hrespM7;

// -----------------------------------------------------------------------------
// Beginning of main code
// -----------------------------------------------------------------------------

    assign tie_hi   = 1'b1;
    assign tie_hi_4 = 4'b1111;
    assign tie_low  = 1'b0;


    assign HRESPS1  = i_hrespS1[0];

    assign i_hrespM1 = {tie_low, HRESPM1};
    assign i_hrespM2 = {tie_low, HRESPM2};
    assign i_hrespM3 = {tie_low, HRESPM3};
    assign i_hrespM4 = {tie_low, HRESPM4};
    assign i_hrespM5 = {tie_low, HRESPM5};
    assign i_hrespM6 = {tie_low, HRESPM6};
    assign i_hrespM7 = {tie_low, HRESPM7};

// BusMatrix instance
  L1AhbMtx uL1AhbMtx (
    .HCLK       (HCLK),
    .HRESETn    (HRESETn),
    .REMAP      (REMAP),

    // Input port SI0 signals
    .HSELS1       (tie_hi),
    .HADDRS1      (HADDRS1),
    .HTRANSS1     (HTRANSS1),
    .HWRITES1     (HWRITES1),
    .HSIZES1      (HSIZES1),
    .HBURSTS1     (HBURSTS1),
    .HPROTS1      (HPROTS1),
    .HWDATAS1     (HWDATAS1),
    .HMASTLOCKS1  (HMASTLOCKS1),
    .HMASTERS1    (tie_hi_4),
    .HREADYS1     (HREADYS1),
    .HRDATAS1     (HRDATAS1),
    .HREADYOUTS1  (HREADYS1),
    .HRESPS1      (i_hrespS1),


    // Output port MI0 signals
    .HSELM1       (HSELM1),
    .HADDRM1      (HADDRM1),
    .HTRANSM1     (HTRANSM1),
    .HWRITEM1     (HWRITEM1),
    .HSIZEM1      (HSIZEM1),
    .HBURSTM1     (HBURSTM1),
    .HPROTM1      (HPROTM1),
    .HWDATAM1     (HWDATAM1),
    .HMASTERM1    (i_hmasterM1),
    .HMASTLOCKM1  (HMASTLOCKM1),
    .HREADYMUXM1  (HREADYMUXM1),
    .HRDATAM1     (HRDATAM1),
    .HREADYOUTM1  (HREADYOUTM1),
    .HRESPM1      (i_hrespM1),

    // Output port MI1 signals
    .HSELM2       (HSELM2),
    .HADDRM2      (HADDRM2),
    .HTRANSM2     (HTRANSM2),
    .HWRITEM2     (HWRITEM2),
    .HSIZEM2      (HSIZEM2),
    .HBURSTM2     (HBURSTM2),
    .HPROTM2      (HPROTM2),
    .HWDATAM2     (HWDATAM2),
    .HMASTERM2    (i_hmasterM2),
    .HMASTLOCKM2  (HMASTLOCKM2),
    .HREADYMUXM2  (HREADYMUXM2),
    .HRDATAM2     (HRDATAM2),
    .HREADYOUTM2  (HREADYOUTM2),
    .HRESPM2      (i_hrespM2),

    // Output port MI2 signals
    .HSELM3       (HSELM3),
    .HADDRM3      (HADDRM3),
    .HTRANSM3     (HTRANSM3),
    .HWRITEM3     (HWRITEM3),
    .HSIZEM3      (HSIZEM3),
    .HBURSTM3     (HBURSTM3),
    .HPROTM3      (HPROTM3),
    .HWDATAM3     (HWDATAM3),
    .HMASTERM3    (i_hmasterM3),
    .HMASTLOCKM3  (HMASTLOCKM3),
    .HREADYMUXM3  (HREADYMUXM3),
    .HRDATAM3     (HRDATAM3),
    .HREADYOUTM3  (HREADYOUTM3),
    .HRESPM3      (i_hrespM3),

    // Output port MI3 signals
    .HSELM4       (HSELM4),
    .HADDRM4      (HADDRM4),
    .HTRANSM4     (HTRANSM4),
    .HWRITEM4     (HWRITEM4),
    .HSIZEM4      (HSIZEM4),
    .HBURSTM4     (HBURSTM4),
    .HPROTM4      (HPROTM4),
    .HWDATAM4     (HWDATAM4),
    .HMASTERM4    (i_hmasterM4),
    .HMASTLOCKM4  (HMASTLOCKM4),
    .HREADYMUXM4  (HREADYMUXM4),
    .HRDATAM4     (HRDATAM4),
    .HREADYOUTM4  (HREADYOUTM4),
    .HRESPM4      (i_hrespM4),

    // Output port MI4 signals
    .HSELM5       (HSELM5),
    .HADDRM5      (HADDRM5),
    .HTRANSM5     (HTRANSM5),
    .HWRITEM5     (HWRITEM5),
    .HSIZEM5      (HSIZEM5),
    .HBURSTM5     (HBURSTM5),
    .HPROTM5      (HPROTM5),
    .HWDATAM5     (HWDATAM5),
    .HMASTERM5    (i_hmasterM5),
    .HMASTLOCKM5  (HMASTLOCKM5),
    .HREADYMUXM5  (HREADYMUXM5),
    .HRDATAM5     (HRDATAM5),
    .HREADYOUTM5  (HREADYOUTM5),
    .HRESPM5      (i_hrespM5),

    // Output port MI5 signals
    .HSELM6       (HSELM6),
    .HADDRM6      (HADDRM6),
    .HTRANSM6     (HTRANSM6),
    .HWRITEM6     (HWRITEM6),
    .HSIZEM6      (HSIZEM6),
    .HBURSTM6     (HBURSTM6),
    .HPROTM6      (HPROTM6),
    .HWDATAM6     (HWDATAM6),
    .HMASTERM6    (i_hmasterM6),
    .HMASTLOCKM6  (HMASTLOCKM6),
    .HREADYMUXM6  (HREADYMUXM6),
    .HRDATAM6     (HRDATAM6),
    .HREADYOUTM6  (HREADYOUTM6),
    .HRESPM6      (i_hrespM6),

    // Output port MI6 signals
    .HSELM7       (HSELM7),
    .HADDRM7      (HADDRM7),
    .HTRANSM7     (HTRANSM7),
    .HWRITEM7     (HWRITEM7),
    .HSIZEM7      (HSIZEM7),
    .HBURSTM7     (HBURSTM7),
    .HPROTM7      (HPROTM7),
    .HWDATAM7     (HWDATAM7),
    .HMASTERM7    (i_hmasterM7),
    .HMASTLOCKM7  (HMASTLOCKM7),
    .HREADYMUXM7  (HREADYMUXM7),
    .HRDATAM7     (HRDATAM7),
    .HREADYOUTM7  (HREADYOUTM7),
    .HRESPM7      (i_hrespM7),


    // Scan test dummy signals; not connected until scan insertion
    .SCANENABLE            (SCANENABLE),
    .SCANINHCLK            (SCANINHCLK),
    .SCANOUTHCLK           (SCANOUTHCLK)
  );


endmodule
//-----------------------------------------------------------------------------
// The confidential and proprietary information contained in this file may
// only be used by a person authorised under and to the extent permitted
// by a subsisting licensing agreement from ARM Limited.
//
//            (C) COPYRIGHT 2001-2013-2025 ARM Limited.
//                ALL RIGHTS RESERVED
//
// This entire notice must be reproduced on all copies of this file
// and copies of this file may only be made by a person if such person is
// permitted to do so under the terms of a subsisting license agreement
// from ARM Limited.
//
//      SVN Information
//
//      Checked In          : $Date: 2012-10-15 18:01:36 +0100 (Mon, 15 Oct 2012) $
//
//      Revision            : $Revision: 225465 $
//
//      Release Information : Cortex-M System Design Kit-r1p0-01rel0
//
//-----------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
//  Abstract            : BusMatrix is the top-level which connects together
//                        the required Input Stages, MatrixDecodes, Output
//                        Stages and Output Arbitration blocks.
//
//                        Supports the following configured options:
//
//                         - Architecture type 'ahb2',
//                         - 1 slave ports (connecting to masters),
//                         - 7 master ports (connecting to slaves),
//                         - Routing address width of 32 bits,
//                         - Routing data width of 32 bits,
//                         - Arbiter type 'burst',
//                         - Connectivity mapping:
//                             S<0..0> -> M<0..6>,
//                         - Connectivity type 'full'.
//
//------------------------------------------------------------------------------

`timescale 1ns/1ps

module L1AhbMtx (

    // Common AHB signals
    HCLK,
    HRESETn,

    // System address remapping control
    REMAP,

    // Input port SI0 (inputs from master 0)
    HSELS1,
    HADDRS1,
    HTRANSS1,
    HWRITES1,
    HSIZES1,
    HBURSTS1,
    HPROTS1,
    HMASTERS1,
    HWDATAS1,
    HMASTLOCKS1,
    HREADYS1,

    // Output port MI0 (inputs from slave 0)
    HRDATAM1,
    HREADYOUTM1,
    HRESPM1,

    // Output port MI1 (inputs from slave 1)
    HRDATAM2,
    HREADYOUTM2,
    HRESPM2,

    // Output port MI2 (inputs from slave 2)
    HRDATAM3,
    HREADYOUTM3,
    HRESPM3,

    // Output port MI3 (inputs from slave 3)
    HRDATAM4,
    HREADYOUTM4,
    HRESPM4,

    // Output port MI4 (inputs from slave 4)
    HRDATAM5,
    HREADYOUTM5,
    HRESPM5,

    // Output port MI5 (inputs from slave 5)
    HRDATAM6,
    HREADYOUTM6,
    HRESPM6,

    // Output port MI6 (inputs from slave 6)
    HRDATAM7,
    HREADYOUTM7,
    HRESPM7,

    // Scan test dummy signals; not connected until scan insertion
    SCANENABLE,   // Scan Test Mode Enable
    SCANINHCLK,   // Scan Chain Input


    // Output port MI0 (outputs to slave 0)
    HSELM1,
    HADDRM1,
    HTRANSM1,
    HWRITEM1,
    HSIZEM1,
    HBURSTM1,
    HPROTM1,
    HMASTERM1,
    HWDATAM1,
    HMASTLOCKM1,
    HREADYMUXM1,

    // Output port MI1 (outputs to slave 1)
    HSELM2,
    HADDRM2,
    HTRANSM2,
    HWRITEM2,
    HSIZEM2,
    HBURSTM2,
    HPROTM2,
    HMASTERM2,
    HWDATAM2,
    HMASTLOCKM2,
    HREADYMUXM2,

    // Output port MI2 (outputs to slave 2)
    HSELM3,
    HADDRM3,
    HTRANSM3,
    HWRITEM3,
    HSIZEM3,
    HBURSTM3,
    HPROTM3,
    HMASTERM3,
    HWDATAM3,
    HMASTLOCKM3,
    HREADYMUXM3,

    // Output port MI3 (outputs to slave 3)
    HSELM4,
    HADDRM4,
    HTRANSM4,
    HWRITEM4,
    HSIZEM4,
    HBURSTM4,
    HPROTM4,
    HMASTERM4,
    HWDATAM4,
    HMASTLOCKM4,
    HREADYMUXM4,

    // Output port MI4 (outputs to slave 4)
    HSELM5,
    HADDRM5,
    HTRANSM5,
    HWRITEM5,
    HSIZEM5,
    HBURSTM5,
    HPROTM5,
    HMASTERM5,
    HWDATAM5,
    HMASTLOCKM5,
    HREADYMUXM5,

    // Output port MI5 (outputs to slave 5)
    HSELM6,
    HADDRM6,
    HTRANSM6,
    HWRITEM6,
    HSIZEM6,
    HBURSTM6,
    HPROTM6,
    HMASTERM6,
    HWDATAM6,
    HMASTLOCKM6,
    HREADYMUXM6,

    // Output port MI6 (outputs to slave 6)
    HSELM7,
    HADDRM7,
    HTRANSM7,
    HWRITEM7,
    HSIZEM7,
    HBURSTM7,
    HPROTM7,
    HMASTERM7,
    HWDATAM7,
    HMASTLOCKM7,
    HREADYMUXM7,

    // Input port SI0 (outputs to master 0)
    HRDATAS1,
    HREADYOUTS1,
    HRESPS1,

    // Scan test dummy signals; not connected until scan insertion
    SCANOUTHCLK   // Scan Chain Output

    );


// -----------------------------------------------------------------------------
// Input and Output declarations
// -----------------------------------------------------------------------------

    // Common AHB signals
    input         HCLK;            // AHB System Clock
    input         HRESETn;         // AHB System Reset

    // System address remapping control
    input   [3:0] REMAP;           // REMAP input

    // Input port SI0 (inputs from master 0)
    input         HSELS1;          // Slave Select
    input  [31:0] HADDRS1;         // Address bus
    input   [1:0] HTRANSS1;        // Transfer type
    input         HWRITES1;        // Transfer direction
    input   [2:0] HSIZES1;         // Transfer size
    input   [2:0] HBURSTS1;        // Burst type
    input   [3:0] HPROTS1;         // Protection control
    input   [3:0] HMASTERS1;       // Master select
    input  [31:0] HWDATAS1;        // Write data
    input         HMASTLOCKS1;     // Locked Sequence
    input         HREADYS1;        // Transfer done

    // Output port MI0 (inputs from slave 0)
    input  [31:0] HRDATAM1;        // Read data bus
    input         HREADYOUTM1;     // HREADY feedback
    input   [1:0] HRESPM1;         // Transfer response

    // Output port MI1 (inputs from slave 1)
    input  [31:0] HRDATAM2;        // Read data bus
    input         HREADYOUTM2;     // HREADY feedback
    input   [1:0] HRESPM2;         // Transfer response

    // Output port MI2 (inputs from slave 2)
    input  [31:0] HRDATAM3;        // Read data bus
    input         HREADYOUTM3;     // HREADY feedback
    input   [1:0] HRESPM3;         // Transfer response

    // Output port MI3 (inputs from slave 3)
    input  [31:0] HRDATAM4;        // Read data bus
    input         HREADYOUTM4;     // HREADY feedback
    input   [1:0] HRESPM4;         // Transfer response

    // Output port MI4 (inputs from slave 4)
    input  [31:0] HRDATAM5;        // Read data bus
    input         HREADYOUTM5;     // HREADY feedback
    input   [1:0] HRESPM5;         // Transfer response

    // Output port MI5 (inputs from slave 5)
    input  [31:0] HRDATAM6;        // Read data bus
    input         HREADYOUTM6;     // HREADY feedback
    input   [1:0] HRESPM6;         // Transfer response

    // Output port MI6 (inputs from slave 6)
    input  [31:0] HRDATAM7;        // Read data bus
    input         HREADYOUTM7;     // HREADY feedback
    input   [1:0] HRESPM7;         // Transfer response

    // Scan test dummy signals; not connected until scan insertion
    input         SCANENABLE;      // Scan enable signal
    input         SCANINHCLK;      // HCLK scan input


    // Output port MI0 (outputs to slave 0)
    output        HSELM1;          // Slave Select
    output [31:0] HADDRM1;         // Address bus
    output  [1:0] HTRANSM1;        // Transfer type
    output        HWRITEM1;        // Transfer direction
    output  [2:0] HSIZEM1;         // Transfer size
    output  [2:0] HBURSTM1;        // Burst type
    output  [3:0] HPROTM1;         // Protection control
    output  [3:0] HMASTERM1;       // Master select
    output [31:0] HWDATAM1;        // Write data
    output        HMASTLOCKM1;     // Locked Sequence
    output        HREADYMUXM1;     // Transfer done

    // Output port MI1 (outputs to slave 1)
    output        HSELM2;          // Slave Select
    output [31:0] HADDRM2;         // Address bus
    output  [1:0] HTRANSM2;        // Transfer type
    output        HWRITEM2;        // Transfer direction
    output  [2:0] HSIZEM2;         // Transfer size
    output  [2:0] HBURSTM2;        // Burst type
    output  [3:0] HPROTM2;         // Protection control
    output  [3:0] HMASTERM2;       // Master select
    output [31:0] HWDATAM2;        // Write data
    output        HMASTLOCKM2;     // Locked Sequence
    output        HREADYMUXM2;     // Transfer done

    // Output port MI2 (outputs to slave 2)
    output        HSELM3;          // Slave Select
    output [31:0] HADDRM3;         // Address bus
    output  [1:0] HTRANSM3;        // Transfer type
    output        HWRITEM3;        // Transfer direction
    output  [2:0] HSIZEM3;         // Transfer size
    output  [2:0] HBURSTM3;        // Burst type
    output  [3:0] HPROTM3;         // Protection control
    output  [3:0] HMASTERM3;       // Master select
    output [31:0] HWDATAM3;        // Write data
    output        HMASTLOCKM3;     // Locked Sequence
    output        HREADYMUXM3;     // Transfer done

    // Output port MI3 (outputs to slave 3)
    output        HSELM4;          // Slave Select
    output [31:0] HADDRM4;         // Address bus
    output  [1:0] HTRANSM4;        // Transfer type
    output        HWRITEM4;        // Transfer direction
    output  [2:0] HSIZEM4;         // Transfer size
    output  [2:0] HBURSTM4;        // Burst type
    output  [3:0] HPROTM4;         // Protection control
    output  [3:0] HMASTERM4;       // Master select
    output [31:0] HWDATAM4;        // Write data
    output        HMASTLOCKM4;     // Locked Sequence
    output        HREADYMUXM4;     // Transfer done

    // Output port MI4 (outputs to slave 4)
    output        HSELM5;          // Slave Select
    output [31:0] HADDRM5;         // Address bus
    output  [1:0] HTRANSM5;        // Transfer type
    output        HWRITEM5;        // Transfer direction
    output  [2:0] HSIZEM5;         // Transfer size
    output  [2:0] HBURSTM5;        // Burst type
    output  [3:0] HPROTM5;         // Protection control
    output  [3:0] HMASTERM5;       // Master select
    output [31:0] HWDATAM5;        // Write data
    output        HMASTLOCKM5;     // Locked Sequence
    output        HREADYMUXM5;     // Transfer done

    // Output port MI5 (outputs to slave 5)
    output        HSELM6;          // Slave Select
    output [31:0] HADDRM6;         // Address bus
    output  [1:0] HTRANSM6;        // Transfer type
    output        HWRITEM6;        // Transfer direction
    output  [2:0] HSIZEM6;         // Transfer size
    output  [2:0] HBURSTM6;        // Burst type
    output  [3:0] HPROTM6;         // Protection control
    output  [3:0] HMASTERM6;       // Master select
    output [31:0] HWDATAM6;        // Write data
    output        HMASTLOCKM6;     // Locked Sequence
    output        HREADYMUXM6;     // Transfer done

    // Output port MI6 (outputs to slave 6)
    output        HSELM7;          // Slave Select
    output [31:0] HADDRM7;         // Address bus
    output  [1:0] HTRANSM7;        // Transfer type
    output        HWRITEM7;        // Transfer direction
    output  [2:0] HSIZEM7;         // Transfer size
    output  [2:0] HBURSTM7;        // Burst type
    output  [3:0] HPROTM7;         // Protection control
    output  [3:0] HMASTERM7;       // Master select
    output [31:0] HWDATAM7;        // Write data
    output        HMASTLOCKM7;     // Locked Sequence
    output        HREADYMUXM7;     // Transfer done

    // Input port SI0 (outputs to master 0)
    output [31:0] HRDATAS1;        // Read data bus
    output        HREADYOUTS1;     // HREADY feedback
    output  [1:0] HRESPS1;         // Transfer response

    // Scan test dummy signals; not connected until scan insertion
    output        SCANOUTHCLK;     // Scan Chain Output


// -----------------------------------------------------------------------------
// Wire declarations
// -----------------------------------------------------------------------------

    // Common AHB signals
    wire         HCLK;            // AHB System Clock
    wire         HRESETn;         // AHB System Reset

    // System address remapping control
    wire   [3:0] REMAP;           // REMAP signal

    // Input Port SI0
    wire         HSELS1;          // Slave Select
    wire  [31:0] HADDRS1;         // Address bus
    wire   [1:0] HTRANSS1;        // Transfer type
    wire         HWRITES1;        // Transfer direction
    wire   [2:0] HSIZES1;         // Transfer size
    wire   [2:0] HBURSTS1;        // Burst type
    wire   [3:0] HPROTS1;         // Protection control
    wire   [3:0] HMASTERS1;       // Master select
    wire  [31:0] HWDATAS1;        // Write data
    wire         HMASTLOCKS1;     // Locked Sequence
    wire         HREADYS1;        // Transfer done

    wire  [31:0] HRDATAS1;        // Read data bus
    wire         HREADYOUTS1;     // HREADY feedback
    wire   [1:0] HRESPS1;         // Transfer response

    // Output Port MI0
    wire         HSELM1;          // Slave Select
    wire  [31:0] HADDRM1;         // Address bus
    wire   [1:0] HTRANSM1;        // Transfer type
    wire         HWRITEM1;        // Transfer direction
    wire   [2:0] HSIZEM1;         // Transfer size
    wire   [2:0] HBURSTM1;        // Burst type
    wire   [3:0] HPROTM1;         // Protection control
    wire   [3:0] HMASTERM1;       // Master select
    wire  [31:0] HWDATAM1;        // Write data
    wire         HMASTLOCKM1;     // Locked Sequence
    wire         HREADYMUXM1;     // Transfer done

    wire  [31:0] HRDATAM1;        // Read data bus
    wire         HREADYOUTM1;     // HREADY feedback
    wire   [1:0] HRESPM1;         // Transfer response

    // Output Port MI1
    wire         HSELM2;          // Slave Select
    wire  [31:0] HADDRM2;         // Address bus
    wire   [1:0] HTRANSM2;        // Transfer type
    wire         HWRITEM2;        // Transfer direction
    wire   [2:0] HSIZEM2;         // Transfer size
    wire   [2:0] HBURSTM2;        // Burst type
    wire   [3:0] HPROTM2;         // Protection control
    wire   [3:0] HMASTERM2;       // Master select
    wire  [31:0] HWDATAM2;        // Write data
    wire         HMASTLOCKM2;     // Locked Sequence
    wire         HREADYMUXM2;     // Transfer done

    wire  [31:0] HRDATAM2;        // Read data bus
    wire         HREADYOUTM2;     // HREADY feedback
    wire   [1:0] HRESPM2;         // Transfer response

    // Output Port MI2
    wire         HSELM3;          // Slave Select
    wire  [31:0] HADDRM3;         // Address bus
    wire   [1:0] HTRANSM3;        // Transfer type
    wire         HWRITEM3;        // Transfer direction
    wire   [2:0] HSIZEM3;         // Transfer size
    wire   [2:0] HBURSTM3;        // Burst type
    wire   [3:0] HPROTM3;         // Protection control
    wire   [3:0] HMASTERM3;       // Master select
    wire  [31:0] HWDATAM3;        // Write data
    wire         HMASTLOCKM3;     // Locked Sequence
    wire         HREADYMUXM3;     // Transfer done

    wire  [31:0] HRDATAM3;        // Read data bus
    wire         HREADYOUTM3;     // HREADY feedback
    wire   [1:0] HRESPM3;         // Transfer response

    // Output Port MI3
    wire         HSELM4;          // Slave Select
    wire  [31:0] HADDRM4;         // Address bus
    wire   [1:0] HTRANSM4;        // Transfer type
    wire         HWRITEM4;        // Transfer direction
    wire   [2:0] HSIZEM4;         // Transfer size
    wire   [2:0] HBURSTM4;        // Burst type
    wire   [3:0] HPROTM4;         // Protection control
    wire   [3:0] HMASTERM4;       // Master select
    wire  [31:0] HWDATAM4;        // Write data
    wire         HMASTLOCKM4;     // Locked Sequence
    wire         HREADYMUXM4;     // Transfer done

    wire  [31:0] HRDATAM4;        // Read data bus
    wire         HREADYOUTM4;     // HREADY feedback
    wire   [1:0] HRESPM4;         // Transfer response

    // Output Port MI4
    wire         HSELM5;          // Slave Select
    wire  [31:0] HADDRM5;         // Address bus
    wire   [1:0] HTRANSM5;        // Transfer type
    wire         HWRITEM5;        // Transfer direction
    wire   [2:0] HSIZEM5;         // Transfer size
    wire   [2:0] HBURSTM5;        // Burst type
    wire   [3:0] HPROTM5;         // Protection control
    wire   [3:0] HMASTERM5;       // Master select
    wire  [31:0] HWDATAM5;        // Write data
    wire         HMASTLOCKM5;     // Locked Sequence
    wire         HREADYMUXM5;     // Transfer done

    wire  [31:0] HRDATAM5;        // Read data bus
    wire         HREADYOUTM5;     // HREADY feedback
    wire   [1:0] HRESPM5;         // Transfer response

    // Output Port MI5
    wire         HSELM6;          // Slave Select
    wire  [31:0] HADDRM6;         // Address bus
    wire   [1:0] HTRANSM6;        // Transfer type
    wire         HWRITEM6;        // Transfer direction
    wire   [2:0] HSIZEM6;         // Transfer size
    wire   [2:0] HBURSTM6;        // Burst type
    wire   [3:0] HPROTM6;         // Protection control
    wire   [3:0] HMASTERM6;       // Master select
    wire  [31:0] HWDATAM6;        // Write data
    wire         HMASTLOCKM6;     // Locked Sequence
    wire         HREADYMUXM6;     // Transfer done

    wire  [31:0] HRDATAM6;        // Read data bus
    wire         HREADYOUTM6;     // HREADY feedback
    wire   [1:0] HRESPM6;         // Transfer response

    // Output Port MI6
    wire         HSELM7;          // Slave Select
    wire  [31:0] HADDRM7;         // Address bus
    wire   [1:0] HTRANSM7;        // Transfer type
    wire         HWRITEM7;        // Transfer direction
    wire   [2:0] HSIZEM7;         // Transfer size
    wire   [2:0] HBURSTM7;        // Burst type
    wire   [3:0] HPROTM7;         // Protection control
    wire   [3:0] HMASTERM7;       // Master select
    wire  [31:0] HWDATAM7;        // Write data
    wire         HMASTLOCKM7;     // Locked Sequence
    wire         HREADYMUXM7;     // Transfer done

    wire  [31:0] HRDATAM7;        // Read data bus
    wire         HREADYOUTM7;     // HREADY feedback
    wire   [1:0] HRESPM7;         // Transfer response


// -----------------------------------------------------------------------------
// Signal declarations
// -----------------------------------------------------------------------------

    // Bus-switch input SI0
    wire         i_sel0;            // HSEL signal
    wire  [31:0] i_addr0;           // HADDR signal
    wire   [1:0] i_trans0;          // HTRANS signal
    wire         i_write0;          // HWRITE signal
    wire   [2:0] i_size0;           // HSIZE signal
    wire   [2:0] i_burst0;          // HBURST signal
    wire   [3:0] i_prot0;           // HPROTS signal
    wire   [3:0] i_master0;         // HMASTER signal
    wire         i_mastlock0;       // HMASTLOCK signal
    wire         i_active0;         // Active signal
    wire         i_held_tran0;       // HeldTran signal
    wire         i_readyout0;       // Readyout signal
    wire   [1:0] i_resp0;           // Response signal

    // Bus-switch SI0 to MI0 signals
    wire         i_sel0to0;         // Routing selection signal
    wire         i_active0to0;      // Active signal

    // Bus-switch SI0 to MI1 signals
    wire         i_sel0to1;         // Routing selection signal
    wire         i_active0to1;      // Active signal

    // Bus-switch SI0 to MI2 signals
    wire         i_sel0to2;         // Routing selection signal
    wire         i_active0to2;      // Active signal

    // Bus-switch SI0 to MI3 signals
    wire         i_sel0to3;         // Routing selection signal
    wire         i_active0to3;      // Active signal

    // Bus-switch SI0 to MI4 signals
    wire         i_sel0to4;         // Routing selection signal
    wire         i_active0to4;      // Active signal

    // Bus-switch SI0 to MI5 signals
    wire         i_sel0to5;         // Routing selection signal
    wire         i_active0to5;      // Active signal

    // Bus-switch SI0 to MI6 signals
    wire         i_sel0to6;         // Routing selection signal
    wire         i_active0to6;      // Active signal

    wire         i_hready_mux_m1;    // Internal HREADYMUXM for MI0
    wire         i_hready_mux_m2;    // Internal HREADYMUXM for MI1
    wire         i_hready_mux_m3;    // Internal HREADYMUXM for MI2
    wire         i_hready_mux_m4;    // Internal HREADYMUXM for MI3
    wire         i_hready_mux_m5;    // Internal HREADYMUXM for MI4
    wire         i_hready_mux_m6;    // Internal HREADYMUXM for MI5
    wire         i_hready_mux_m7;    // Internal HREADYMUXM for MI6


// -----------------------------------------------------------------------------
// Beginning of main code
// -----------------------------------------------------------------------------

  // Input stage for SI0
  L1AhbMtxInStg u_L1AhbMtxInStg_0 (

    // Common AHB signals
    .HCLK       (HCLK),
    .HRESETn    (HRESETn),

    // Input Port Address/Control Signals
    .HSELS      (HSELS1),
    .HADDRS     (HADDRS1),
    .HTRANSS    (HTRANSS1),
    .HWRITES    (HWRITES1),
    .HSIZES     (HSIZES1),
    .HBURSTS    (HBURSTS1),
    .HPROTS     (HPROTS1),
    .HMASTERS   (HMASTERS1),
    .HMASTLOCKS (HMASTLOCKS1),
    .HREADYS    (HREADYS1),

    // Internal Response
    .active_ip     (i_active0),
    .readyout_ip   (i_readyout0),
    .resp_ip       (i_resp0),

    // Input Port Response
    .HREADYOUTS (HREADYOUTS1),
    .HRESPS     (HRESPS1),

    // Internal Address/Control Signals
    .sel_ip        (i_sel0),
    .addr_ip       (i_addr0),
    .trans_ip      (i_trans0),
    .write_ip      (i_write0),
    .size_ip       (i_size0),
    .burst_ip      (i_burst0),
    .prot_ip       (i_prot0),
    .master_ip     (i_master0),
    .mastlock_ip   (i_mastlock0),
    .held_tran_ip   (i_held_tran0)

    );


  // Matrix decoder for SI0
  L1AhbMtxDecS1 u_l1ahbmtxdecs1 (

    // Common AHB signals
    .HCLK       (HCLK),
    .HRESETn    (HRESETn),

    // Signals from Input stage SI0
    .HREADYS    (HREADYS1),
    .sel_dec        (i_sel0),
    .decode_addr_dec (i_addr0[31:10]),   // HADDR[9:0] is not decoded
    .trans_dec      (i_trans0),

    // Control/Response for Output Stage MI0
    .active_dec0    (i_active0to0),
    .readyout_dec0  (i_hready_mux_m1),
    .resp_dec0      (HRESPM1),
    .rdata_dec0     (HRDATAM1),

    // Control/Response for Output Stage MI1
    .active_dec1    (i_active0to1),
    .readyout_dec1  (i_hready_mux_m2),
    .resp_dec1      (HRESPM2),
    .rdata_dec1     (HRDATAM2),

    // Control/Response for Output Stage MI2
    .active_dec2    (i_active0to2),
    .readyout_dec2  (i_hready_mux_m3),
    .resp_dec2      (HRESPM3),
    .rdata_dec2     (HRDATAM3),

    // Control/Response for Output Stage MI3
    .active_dec3    (i_active0to3),
    .readyout_dec3  (i_hready_mux_m4),
    .resp_dec3      (HRESPM4),
    .rdata_dec3     (HRDATAM4),

    // Control/Response for Output Stage MI4
    .active_dec4    (i_active0to4),
    .readyout_dec4  (i_hready_mux_m5),
    .resp_dec4      (HRESPM5),
    .rdata_dec4     (HRDATAM5),

    // Control/Response for Output Stage MI5
    .active_dec5    (i_active0to5),
    .readyout_dec5  (i_hready_mux_m6),
    .resp_dec5      (HRESPM6),
    .rdata_dec5     (HRDATAM6),

    // Control/Response for Output Stage MI6
    .active_dec6    (i_active0to6),
    .readyout_dec6  (i_hready_mux_m7),
    .resp_dec6      (HRESPM7),
    .rdata_dec6     (HRDATAM7),

    .sel_dec0       (i_sel0to0),
    .sel_dec1       (i_sel0to1),
    .sel_dec2       (i_sel0to2),
    .sel_dec3       (i_sel0to3),
    .sel_dec4       (i_sel0to4),
    .sel_dec5       (i_sel0to5),
    .sel_dec6       (i_sel0to6),

    .active_dec     (i_active0),
    .HREADYOUTS (i_readyout0),
    .HRESPS     (i_resp0),
    .HRDATAS    (HRDATAS1)

    );


  // Output stage for MI0
  L1AhbMtxOutStg u_l1ahbmtxoutstg_0 (

    // Common AHB signals
    .HCLK       (HCLK),
    .HRESETn    (HRESETn),

    // Port 0 Signals
    .sel_op0       (i_sel0to0),
    .addr_op0      (i_addr0),
    .trans_op0     (i_trans0),
    .write_op0     (i_write0),
    .size_op0      (i_size0),
    .burst_op0     (i_burst0),
    .prot_op0      (i_prot0),
    .master_op0    (i_master0),
    .mastlock_op0  (i_mastlock0),
    .wdata_op0     (HWDATAS1),
    .held_tran_op0  (i_held_tran0),

    // Slave read data and response
    .HREADYOUTM (HREADYOUTM1),

    .active_op0    (i_active0to0),

    // Slave Address/Control Signals
    .HSELM      (HSELM1),
    .HADDRM     (HADDRM1),
    .HTRANSM    (HTRANSM1),
    .HWRITEM    (HWRITEM1),
    .HSIZEM     (HSIZEM1),
    .HBURSTM    (HBURSTM1),
    .HPROTM     (HPROTM1),
    .HMASTERM   (HMASTERM1),
    .HMASTLOCKM (HMASTLOCKM1),
    .HREADYMUXM (i_hready_mux_m1),
    .HWDATAM    (HWDATAM1)

    );

  // Drive output with internal version
  assign HREADYMUXM1 = i_hready_mux_m1;


  // Output stage for MI1
  L1AhbMtxOutStg u_l1ahbmtxoutstg_1 (

    // Common AHB signals
    .HCLK       (HCLK),
    .HRESETn    (HRESETn),

    // Port 0 Signals
    .sel_op0       (i_sel0to1),
    .addr_op0      (i_addr0),
    .trans_op0     (i_trans0),
    .write_op0     (i_write0),
    .size_op0      (i_size0),
    .burst_op0     (i_burst0),
    .prot_op0      (i_prot0),
    .master_op0    (i_master0),
    .mastlock_op0  (i_mastlock0),
    .wdata_op0     (HWDATAS1),
    .held_tran_op0  (i_held_tran0),

    // Slave read data and response
    .HREADYOUTM (HREADYOUTM2),

    .active_op0    (i_active0to1),

    // Slave Address/Control Signals
    .HSELM      (HSELM2),
    .HADDRM     (HADDRM2),
    .HTRANSM    (HTRANSM2),
    .HWRITEM    (HWRITEM2),
    .HSIZEM     (HSIZEM2),
    .HBURSTM    (HBURSTM2),
    .HPROTM     (HPROTM2),
    .HMASTERM   (HMASTERM2),
    .HMASTLOCKM (HMASTLOCKM2),
    .HREADYMUXM (i_hready_mux_m2),
    .HWDATAM    (HWDATAM2)

    );

  // Drive output with internal version
  assign HREADYMUXM2 = i_hready_mux_m2;


  // Output stage for MI2
  L1AhbMtxOutStg u_l1ahbmtxoutstg_2 (

    // Common AHB signals
    .HCLK       (HCLK),
    .HRESETn    (HRESETn),

    // Port 0 Signals
    .sel_op0       (i_sel0to2),
    .addr_op0      (i_addr0),
    .trans_op0     (i_trans0),
    .write_op0     (i_write0),
    .size_op0      (i_size0),
    .burst_op0     (i_burst0),
    .prot_op0      (i_prot0),
    .master_op0    (i_master0),
    .mastlock_op0  (i_mastlock0),
    .wdata_op0     (HWDATAS1),
    .held_tran_op0  (i_held_tran0),

    // Slave read data and response
    .HREADYOUTM (HREADYOUTM3),

    .active_op0    (i_active0to2),

    // Slave Address/Control Signals
    .HSELM      (HSELM3),
    .HADDRM     (HADDRM3),
    .HTRANSM    (HTRANSM3),
    .HWRITEM    (HWRITEM3),
    .HSIZEM     (HSIZEM3),
    .HBURSTM    (HBURSTM3),
    .HPROTM     (HPROTM3),
    .HMASTERM   (HMASTERM3),
    .HMASTLOCKM (HMASTLOCKM3),
    .HREADYMUXM (i_hready_mux_m3),
    .HWDATAM    (HWDATAM3)

    );

  // Drive output with internal version
  assign HREADYMUXM3 = i_hready_mux_m3;


  // Output stage for MI3
  L1AhbMtxOutStg u_l1ahbmtxoutstg_3 (

    // Common AHB signals
    .HCLK       (HCLK),
    .HRESETn    (HRESETn),

    // Port 0 Signals
    .sel_op0       (i_sel0to3),
    .addr_op0      (i_addr0),
    .trans_op0     (i_trans0),
    .write_op0     (i_write0),
    .size_op0      (i_size0),
    .burst_op0     (i_burst0),
    .prot_op0      (i_prot0),
    .master_op0    (i_master0),
    .mastlock_op0  (i_mastlock0),
    .wdata_op0     (HWDATAS1),
    .held_tran_op0  (i_held_tran0),

    // Slave read data and response
    .HREADYOUTM (HREADYOUTM4),

    .active_op0    (i_active0to3),

    // Slave Address/Control Signals
    .HSELM      (HSELM4),
    .HADDRM     (HADDRM4),
    .HTRANSM    (HTRANSM4),
    .HWRITEM    (HWRITEM4),
    .HSIZEM     (HSIZEM4),
    .HBURSTM    (HBURSTM4),
    .HPROTM     (HPROTM4),
    .HMASTERM   (HMASTERM4),
    .HMASTLOCKM (HMASTLOCKM4),
    .HREADYMUXM (i_hready_mux_m4),
    .HWDATAM    (HWDATAM4)

    );

  // Drive output with internal version
  assign HREADYMUXM4 = i_hready_mux_m4;


  // Output stage for MI4
  L1AhbMtxOutStg u_l1ahbmtxoutstg_4 (

    // Common AHB signals
    .HCLK       (HCLK),
    .HRESETn    (HRESETn),

    // Port 0 Signals
    .sel_op0       (i_sel0to4),
    .addr_op0      (i_addr0),
    .trans_op0     (i_trans0),
    .write_op0     (i_write0),
    .size_op0      (i_size0),
    .burst_op0     (i_burst0),
    .prot_op0      (i_prot0),
    .master_op0    (i_master0),
    .mastlock_op0  (i_mastlock0),
    .wdata_op0     (HWDATAS1),
    .held_tran_op0  (i_held_tran0),

    // Slave read data and response
    .HREADYOUTM (HREADYOUTM5),

    .active_op0    (i_active0to4),

    // Slave Address/Control Signals
    .HSELM      (HSELM5),
    .HADDRM     (HADDRM5),
    .HTRANSM    (HTRANSM5),
    .HWRITEM    (HWRITEM5),
    .HSIZEM     (HSIZEM5),
    .HBURSTM    (HBURSTM5),
    .HPROTM     (HPROTM5),
    .HMASTERM   (HMASTERM5),
    .HMASTLOCKM (HMASTLOCKM5),
    .HREADYMUXM (i_hready_mux_m5),
    .HWDATAM    (HWDATAM5)

    );

  // Drive output with internal version
  assign HREADYMUXM5 = i_hready_mux_m5;


  // Output stage for MI5
  L1AhbMtxOutStg u_l1ahbmtxoutstg_5 (

    // Common AHB signals
    .HCLK       (HCLK),
    .HRESETn    (HRESETn),

    // Port 0 Signals
    .sel_op0       (i_sel0to5),
    .addr_op0      (i_addr0),
    .trans_op0     (i_trans0),
    .write_op0     (i_write0),
    .size_op0      (i_size0),
    .burst_op0     (i_burst0),
    .prot_op0      (i_prot0),
    .master_op0    (i_master0),
    .mastlock_op0  (i_mastlock0),
    .wdata_op0     (HWDATAS1),
    .held_tran_op0  (i_held_tran0),

    // Slave read data and response
    .HREADYOUTM (HREADYOUTM6),

    .active_op0    (i_active0to5),

    // Slave Address/Control Signals
    .HSELM      (HSELM6),
    .HADDRM     (HADDRM6),
    .HTRANSM    (HTRANSM6),
    .HWRITEM    (HWRITEM6),
    .HSIZEM     (HSIZEM6),
    .HBURSTM    (HBURSTM6),
    .HPROTM     (HPROTM6),
    .HMASTERM   (HMASTERM6),
    .HMASTLOCKM (HMASTLOCKM6),
    .HREADYMUXM (i_hready_mux_m6),
    .HWDATAM    (HWDATAM6)

    );

  // Drive output with internal version
  assign HREADYMUXM6 = i_hready_mux_m6;


  // Output stage for MI6
  L1AhbMtxOutStg u_l1ahbmtxoutstg_6 (

    // Common AHB signals
    .HCLK       (HCLK),
    .HRESETn    (HRESETn),

    // Port 0 Signals
    .sel_op0       (i_sel0to6),
    .addr_op0      (i_addr0),
    .trans_op0     (i_trans0),
    .write_op0     (i_write0),
    .size_op0      (i_size0),
    .burst_op0     (i_burst0),
    .prot_op0      (i_prot0),
    .master_op0    (i_master0),
    .mastlock_op0  (i_mastlock0),
    .wdata_op0     (HWDATAS1),
    .held_tran_op0  (i_held_tran0),

    // Slave read data and response
    .HREADYOUTM (HREADYOUTM7),

    .active_op0    (i_active0to6),

    // Slave Address/Control Signals
    .HSELM      (HSELM7),
    .HADDRM     (HADDRM7),
    .HTRANSM    (HTRANSM7),
    .HWRITEM    (HWRITEM7),
    .HSIZEM     (HSIZEM7),
    .HBURSTM    (HBURSTM7),
    .HPROTM     (HPROTM7),
    .HMASTERM   (HMASTERM7),
    .HMASTLOCKM (HMASTLOCKM7),
    .HREADYMUXM (i_hready_mux_m7),
    .HWDATAM    (HWDATAM7)

    );

  // Drive output with internal version
  assign HREADYMUXM7 = i_hready_mux_m7;


endmodule

// --================================= End ===================================--
//-----------------------------------------------------------------------------
// The confidential and proprietary information contained in this file may
// only be used by a person authorised under and to the extent permitted
// by a subsisting licensing agreement from ARM Limited.
//
//            (C) COPYRIGHT 2001-2013-2025 ARM Limited.
//                ALL RIGHTS RESERVED
//
// This entire notice must be reproduced on all copies of this file
// and copies of this file may only be made by a person if such person is
// permitted to do so under the terms of a subsisting license agreement
// from ARM Limited.
//
//      SVN Information
//
//      Checked In          : $Date: 2013-01-23 11:45:45 +0000 (Wed, 23 Jan 2013) $
//
//      Revision            : $Revision: 234562 $
//
//      Release Information : Cortex-M System Design Kit-r1p0-01rel0
//
//-----------------------------------------------------------------------------
//
// -----------------------------------------------------------------------------
// Abstract            : Default slave used to drive the slave response signals
//                       when there are no other slaves selected.
//-----------------------------------------------------------------------------

`timescale 1ns/1ps

module L1AhbMtx_default_slave (

    // Common AHB signals
    HCLK,
    HRESETn,

    // AHB control input signals
    HSEL,
    HTRANS,
    HREADY,

    // AHB control output signals
    HREADYOUT,
    HRESP

    );


// -----------------------------------------------------------------------------
// Input and Output declarations
// -----------------------------------------------------------------------------

    // Common AHB signals
    input         HCLK;           // AHB System Clock
    input         HRESETn;        // AHB System Reset

    // AHB control input signals
    input         HSEL;           // Slave Select
    input  [1:0]  HTRANS;         // Transfer type
    input         HREADY;         // Transfer done

    // AHB control output signals
    output        HREADYOUT;      // HREADY feedback
    output  [1:0] HRESP;          // Transfer response


// -----------------------------------------------------------------------------
// Constant declarations
// -----------------------------------------------------------------------------

// HRESP transfer response signal encoding
`define RSP_OKAY    2'b00      // OKAY response
`define RSP_ERROR   2'b01     // ERROR response
`define RSP_RETRY   2'b10     // RETRY response
`define RSP_SPLIT   2'b11     // SPLIT response


// -----------------------------------------------------------------------------
// Input and Output declarations
// -----------------------------------------------------------------------------

    // Common AHB signals
    wire          HCLK;           // AHB System Clock
    wire          HRESETn;        // AHB System Reset

    // AHB control input signals
    wire          HSEL;           // Slave Select
    wire    [1:0] HTRANS;         // Transfer type
    wire          HREADY;         // Transfer done

    // AHB control output signals
    wire          HREADYOUT;      // HREADY feedback
    wire    [1:0] HRESP;          // Transfer response


// -----------------------------------------------------------------------------
// Signal declarations
// -----------------------------------------------------------------------------

    wire          invalid;    // Set during invalid transfer
    wire          hready_next; // Controls generation of HREADYOUT output
    reg           i_hreadyout; // HREADYOUT register
    wire    [1:0] hresp_next;  // Generated response
    reg     [1:0] i_hresp;     // HRESP register


// -----------------------------------------------------------------------------
// Beginning of main code
// -----------------------------------------------------------------------------

  assign invalid = ( HREADY & HSEL & HTRANS[1] );
  assign hready_next = i_hreadyout ?  ~invalid : 1'b1 ;
  assign hresp_next = invalid ? `RSP_ERROR : `RSP_OKAY;

  always @(negedge HRESETn or posedge HCLK)
    begin : p_resp_seq
      if (~HRESETn)
        begin
          i_hreadyout <= 1'b1;
          i_hresp     <= `RSP_OKAY;
        end
      else
        begin
          i_hreadyout <= hready_next;

          if (i_hreadyout)
            i_hresp <= hresp_next;
        end
    end

  // Drive outputs with internal versions
  assign HREADYOUT = i_hreadyout;
  assign HRESP     = i_hresp;


endmodule

// --================================= End ===================================--
//-----------------------------------------------------------------------------
// The confidential and proprietary information contained in this file may
// only be used by a person authorised under and to the extent permitted
// by a subsisting licensing agreement from ARM Limited.
//
//            (C) COPYRIGHT 2001-2013-2025 ARM Limited.
//                ALL RIGHTS RESERVED
//
// This entire notice must be reproduced on all copies of this file
// and copies of this file may only be made by a person if such person is
// permitted to do so under the terms of a subsisting license agreement
// from ARM Limited.
//
//      SVN Information
//
//      Checked In          : $Date: 2012-10-15 18:01:36 +0100 (Mon, 15 Oct 2012) $
//
//      Revision            : $Revision: 225465 $
//
//      Release Information : Cortex-M System Design Kit-r1p0-01rel0
//
//-----------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
//  Abstract            : The Output Arbitration is used to determine which
//                        of the input stages will be given access to the
//                        shared slave.
//
//  Notes               : The bus matrix has full connectivity.
//
//-----------------------------------------------------------------------------

`timescale 1ns/1ps

module L1AhbMtxArb (

    // Common AHB signals
    HCLK ,
    HRESETn,

    // Input port request signals
    req_port0,

    HREADYM,
    HSELM,
    HTRANSM,
    HBURSTM,
    HMASTLOCKM,

    // Arbiter outputs
    addr_in_port,
    no_port

    );


// -----------------------------------------------------------------------------
// Input and Output declarations
// -----------------------------------------------------------------------------

    // Common AHB signals
    input        HCLK;         // AHB system clock
    input        HRESETn;      // AHB system reset
    input        req_port0;     // Port 0 request signal
    input        HREADYM;      // Transfer done
    input        HSELM;        // Slave select line
    input  [1:0] HTRANSM;      // Transfer type
    input  [2:0] HBURSTM;      // Burst type
    input        HMASTLOCKM;   // Locked transfer
    output [0:0] addr_in_port;   // Port address input
    output       no_port;       // No port selected signal


// -----------------------------------------------------------------------------
// Constant declarations
// -----------------------------------------------------------------------------

// HTRANS transfer type signal encoding
`define TRN_IDLE   2'b00       // Idle transfer
`define TRN_BUSY   2'b01       // Busy transfer
`define TRN_NONSEQ 2'b10       // NonSequential transfer
`define TRN_SEQ    2'b11       // Sequential transfer

// HBURST transfer type signal encoding
`define BUR_SINGLE 3'b000       // Single
`define BUR_INCR   3'b001       // Incremental
`define BUR_WRAP4  3'b010       // 4-beat wrap
`define BUR_INCR4  3'b011       // 4-beat Incr
`define BUR_WRAP8  3'b100       // 8-beat wrap
`define BUR_INCR8  3'b101       // 8-beat Incr
`define BUR_WRAP16 3'b110       // 16-beat Wrap
`define BUR_INCR16 3'b111       // 16-beat Incr


// -----------------------------------------------------------------------------
// Wire declarations
// -----------------------------------------------------------------------------
    wire       HCLK;           // AHB system clock
    wire       HRESETn;        // AHB system reset
    wire       req_port0;       // Port 0 request signal
    wire       HREADYM;        // Transfer done
    wire       HSELM;          // Slave select line
    wire [1:0] HTRANSM;        // Transfer type
    wire [2:0] HBURSTM;        // Burst type
    wire       HMASTLOCKM;     // Locked transfer
    wire [0:0] addr_in_port;     // Address input port
    reg        no_port;         // No port selected signal

// -----------------------------------------------------------------------------
// Signal declarations
// -----------------------------------------------------------------------------
    reg  [0:0] addr_in_port_next; // D-input of addr_in_port
    reg  [0:0] i_addr_in_port;    // Internal version of addr_in_port
    reg        no_port_next;       // D-input of no_port
    reg  [3:0] next_burst_count;   // D-input of reg_burst_count
    reg  [3:0] reg_burst_count;    // Burst counter
    reg        next_burst_hold;    // D-input of reg_burst_hold
    reg        reg_burst_hold;     // Burst hold signal


// -----------------------------------------------------------------------------
// Beginning of main code
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
// BURST TRANSFER COUNTER
// -----------------------------------------------------------------------------
//
// The Burst counter is used to count down from the number of transfers the
// master should perform and when the counter reaches zero the bus may be
// passed to another master.
//
// reg_burst_count indicates the number of transfers remaining in the
// current fixed length burst.
// reg_burst_hold is actually a decode of reg_burst_count=0 but is driven from a register
// to improve timing

  always @ (HREADYM or HTRANSM or HSELM or HBURSTM or reg_burst_count or reg_burst_hold)
    begin : p_next_burst_count_comb
      // Only update the counters on a valid phase
      if (!HREADYM)
        begin
          next_burst_count = reg_burst_count;
          next_burst_hold  = reg_burst_hold;
        end

      // Force the Burst logic to reset if this port is de-selected.  This can
      // happen for two reasons:
      //   1. The master performs 2 fixed-length bursts back-to-back, but the
      //      second is to an alternate output port
      //   2. The master is performing a fixed-length burst but is de-granted mid-
      //      way by a local AHB Arbiter
      else if (!HSELM)
        begin
          next_burst_count = 4'b0000;
          next_burst_hold  = 1'b0;
        end

      // Burst logic is initialised on a NONSEQ transfer (i.e. start of burst)
      // IDLE transfers cause the logic to reset
      // BUSY transfers pause the decrementer
      // SEQ transfers decrement the counter
      else
        case (HTRANSM)

          `TRN_NONSEQ : begin
            case (HBURSTM)
              `BUR_INCR16, `BUR_WRAP16 : begin
                next_burst_count = 4'b1111;
                next_burst_hold = 1'b1;
              end // case: BUR_INCR16 | BUR_WRAP16

              `BUR_INCR8, `BUR_WRAP8 : begin
                next_burst_count = 4'b0111;
                next_burst_hold = 1'b1;
              end // case: BUR_INCR8  | BUR_WRAP8

              `BUR_INCR4, `BUR_WRAP4 : begin
                next_burst_count = 4'b0011;
                next_burst_hold = 1'b1;
              end // case: BUR_INCR4  | BUR_WRAP4

              `BUR_SINGLE, `BUR_INCR : begin
                next_burst_count = 4'b0000;
                next_burst_hold = 1'b0;
              end // case: BUR_SINGLE | BUR_INCR

              default : begin
                next_burst_count = 4'bxxxx;
                next_burst_hold = 1'bx;
              end // case: default
            endcase // case(HBURSTM)
          end // case: `TRN_NONSEQ

          `TRN_SEQ : begin
            next_burst_count = reg_burst_count - 1'b1;
            if (reg_burst_count == 4'b0001)
              next_burst_hold = 1'b0;
            else
              next_burst_hold = reg_burst_hold;
          end // case: `TRN_SEQ

          `TRN_BUSY : begin
            next_burst_count = reg_burst_count;
            next_burst_hold = reg_burst_hold;
          end // case: `TRN_BUSY

          `TRN_IDLE : begin
            next_burst_count = 4'b0000;
            next_burst_hold = 1'b0;
          end // case: `TRN_IDLE

          default : begin
            next_burst_count = 4'bxxxx;
            next_burst_hold = 1'bx;
          end // case: default

        endcase // case(HTRANSM)
    end // block: p_next_burst_countComb


  // Sequential process
  always @ (negedge HRESETn or posedge HCLK)
    begin : p_burst_seq
      if (!HRESETn)
        begin
          reg_burst_count <= 4'b0000;
          reg_burst_hold  <= 1'b0;
        end // if (HRESETn == 1'b0)
      else
        begin
          reg_burst_count <= next_burst_count;
          reg_burst_hold  <= next_burst_hold;
        end
    end // block: p_burst_seq


// -----------------------------------------------------------------------------
// Port Selection
// -----------------------------------------------------------------------------
// The Output Arbitration function looks at all the requests to use the
//  output port and determines which is the highest priority request. This
//  version of the arbitration logic uses a fixed priority scheme that is
//  gated by a tracking function of the burst boundary. Input port 0 is the
//  highest priority, input port 1 is the second highest priority, etc.
// If none of the input ports are requesting then the current port will
//  remain active if it is performing IDLE transfers to the selected slave. If
//  this is not the case then the no_port signal will be asserted which
//  indicates that no input port should be selected.

  always @ (
             req_port0 or
             HSELM or HTRANSM or HMASTLOCKM or next_burst_hold or i_addr_in_port
           )
    begin : p_sel_port_comb
      // Default values are used for addr_in_port_next and no_port_next
      no_port_next = 1'b0;
      addr_in_port_next = i_addr_in_port;

      if ( HMASTLOCKM | next_burst_hold )
        addr_in_port_next = i_addr_in_port;
      else if ( req_port0 | ( (i_addr_in_port == 1'b0) & HSELM &
                              (HTRANSM != 2'b00) ) )
        addr_in_port_next = 1'b0;
      else if (HSELM)
        addr_in_port_next = i_addr_in_port;
      else
        no_port_next = 1'b1;
    end // block: p_sel_port_comb


  // Sequential process
  always @ (negedge HRESETn or posedge HCLK)
    begin : p_addr_in_port_reg
      if (!HRESETn)
        begin
          no_port        <= 1'b1;
          i_addr_in_port <= {1{1'b0}};
        end
      else
        if (HREADYM)
          begin
            no_port        <= no_port_next;
            i_addr_in_port <= addr_in_port_next;
          end
    end // block: p_addr_in_port_reg

  // Drive output with internal version
  assign addr_in_port = i_addr_in_port;


endmodule

// --================================= End ===================================--
//-----------------------------------------------------------------------------
// The confidential and proprietary information contained in this file may
// only be used by a person authorised under and to the extent permitted
// by a subsisting licensing agreement from ARM Limited.
//
//            (C) COPYRIGHT 2001-2013-2025 ARM Limited.
//                ALL RIGHTS RESERVED
//
// This entire notice must be reproduced on all copies of this file
// and copies of this file may only be made by a person if such person is
// permitted to do so under the terms of a subsisting license agreement
// from ARM Limited.
//
//      SVN Information
//
//      Checked In          : $Date: 2013-01-23 11:45:45 +0000 (Wed, 23 Jan 2013) $
//
//      Revision            : $Revision: 234562 $
//
//      Release Information : Cortex-M System Design Kit-r1p0-01rel0
//
//-----------------------------------------------------------------------------
//
//-----------------------------------------------------------------------------
//  Abstract             : The MatrixDecode is used to determine which output
//                         stage is required for a particular access. Addresses
//                         that do not map to an Output port are diverted to
//                         the local default slave.
//
//  Notes               : The bus matrix has full connectivity.
//
//-----------------------------------------------------------------------------

`timescale 1ns/1ps

module L1AhbMtxDecS1 (

    // Common AHB signals
    HCLK,
    HRESETn,

    // Signals from the Input stage
    HREADYS,
    sel_dec,
    decode_addr_dec,
    trans_dec,

    // Bus-switch output 0
    active_dec0,
    readyout_dec0,
    resp_dec0,
    rdata_dec0,

    // Bus-switch output 1
    active_dec1,
    readyout_dec1,
    resp_dec1,
    rdata_dec1,

    // Bus-switch output 2
    active_dec2,
    readyout_dec2,
    resp_dec2,
    rdata_dec2,

    // Bus-switch output 3
    active_dec3,
    readyout_dec3,
    resp_dec3,
    rdata_dec3,

    // Bus-switch output 4
    active_dec4,
    readyout_dec4,
    resp_dec4,
    rdata_dec4,

    // Bus-switch output 5
    active_dec5,
    readyout_dec5,
    resp_dec5,
    rdata_dec5,

    // Bus-switch output 6
    active_dec6,
    readyout_dec6,
    resp_dec6,
    rdata_dec6,

    // Output port selection signals
    sel_dec0,
    sel_dec1,
    sel_dec2,
    sel_dec3,
    sel_dec4,
    sel_dec5,
    sel_dec6,

    // Selected Output port data and control signals
    active_dec,
    HREADYOUTS,
    HRESPS,
    HRDATAS

    );


// -----------------------------------------------------------------------------
// Input and Output declarations
// -----------------------------------------------------------------------------

    // Common AHB signals
    input         HCLK;           // AHB System Clock
    input         HRESETn;        // AHB System Reset

    // Signals from the Input stage
    input         HREADYS;        // Transfer done
    input         sel_dec;            // HSEL input
    input [31:10] decode_addr_dec;     // HADDR decoder input
    input   [1:0] trans_dec;          // Input port HTRANS signal

    // Bus-switch output MI0
    input         active_dec0;        // Output stage MI0 active_dec signal
    input         readyout_dec0;      // HREADYOUT input
    input   [1:0] resp_dec0;          // HRESP input
    input  [31:0] rdata_dec0;         // HRDATA input

    // Bus-switch output MI1
    input         active_dec1;        // Output stage MI1 active_dec signal
    input         readyout_dec1;      // HREADYOUT input
    input   [1:0] resp_dec1;          // HRESP input
    input  [31:0] rdata_dec1;         // HRDATA input

    // Bus-switch output MI2
    input         active_dec2;        // Output stage MI2 active_dec signal
    input         readyout_dec2;      // HREADYOUT input
    input   [1:0] resp_dec2;          // HRESP input
    input  [31:0] rdata_dec2;         // HRDATA input

    // Bus-switch output MI3
    input         active_dec3;        // Output stage MI3 active_dec signal
    input         readyout_dec3;      // HREADYOUT input
    input   [1:0] resp_dec3;          // HRESP input
    input  [31:0] rdata_dec3;         // HRDATA input

    // Bus-switch output MI4
    input         active_dec4;        // Output stage MI4 active_dec signal
    input         readyout_dec4;      // HREADYOUT input
    input   [1:0] resp_dec4;          // HRESP input
    input  [31:0] rdata_dec4;         // HRDATA input

    // Bus-switch output MI5
    input         active_dec5;        // Output stage MI5 active_dec signal
    input         readyout_dec5;      // HREADYOUT input
    input   [1:0] resp_dec5;          // HRESP input
    input  [31:0] rdata_dec5;         // HRDATA input

    // Bus-switch output MI6
    input         active_dec6;        // Output stage MI6 active_dec signal
    input         readyout_dec6;      // HREADYOUT input
    input   [1:0] resp_dec6;          // HRESP input
    input  [31:0] rdata_dec6;         // HRDATA input

    // Output port selection signals
    output        sel_dec0;           // HSEL output
    output        sel_dec1;           // HSEL output
    output        sel_dec2;           // HSEL output
    output        sel_dec3;           // HSEL output
    output        sel_dec4;           // HSEL output
    output        sel_dec5;           // HSEL output
    output        sel_dec6;           // HSEL output

    // Selected Output port data and control signals
    output        active_dec;         // Combinatorial active_dec O/P
    output        HREADYOUTS;     // HREADY feedback output
    output  [1:0] HRESPS;         // Transfer response
    output [31:0] HRDATAS;        // Read Data


// -----------------------------------------------------------------------------
// Wire declarations
// -----------------------------------------------------------------------------

    // Common AHB signals
    wire          HCLK;            // AHB System Clock
    wire          HRESETn;         // AHB System Reset

    // Signals from the Input stage
    wire          HREADYS;         // Transfer done
    wire          sel_dec;             // HSEL input
    wire  [31:10] decode_addr_dec;      // HADDR input
    wire    [1:0] trans_dec;           // Input port HTRANS signal

    // Bus-switch output MI0
    wire          active_dec0;         // active_dec signal
    wire          readyout_dec0;       // HREADYOUT input
    wire    [1:0] resp_dec0;           // HRESP input
    wire   [31:0] rdata_dec0;          // HRDATA input
    reg           sel_dec0;            // HSEL output

    // Bus-switch output MI1
    wire          active_dec1;         // active_dec signal
    wire          readyout_dec1;       // HREADYOUT input
    wire    [1:0] resp_dec1;           // HRESP input
    wire   [31:0] rdata_dec1;          // HRDATA input
    reg           sel_dec1;            // HSEL output

    // Bus-switch output MI2
    wire          active_dec2;         // active_dec signal
    wire          readyout_dec2;       // HREADYOUT input
    wire    [1:0] resp_dec2;           // HRESP input
    wire   [31:0] rdata_dec2;          // HRDATA input
    reg           sel_dec2;            // HSEL output

    // Bus-switch output MI3
    wire          active_dec3;         // active_dec signal
    wire          readyout_dec3;       // HREADYOUT input
    wire    [1:0] resp_dec3;           // HRESP input
    wire   [31:0] rdata_dec3;          // HRDATA input
    reg           sel_dec3;            // HSEL output

    // Bus-switch output MI4
    wire          active_dec4;         // active_dec signal
    wire          readyout_dec4;       // HREADYOUT input
    wire    [1:0] resp_dec4;           // HRESP input
    wire   [31:0] rdata_dec4;          // HRDATA input
    reg           sel_dec4;            // HSEL output

    // Bus-switch output MI5
    wire          active_dec5;         // active_dec signal
    wire          readyout_dec5;       // HREADYOUT input
    wire    [1:0] resp_dec5;           // HRESP input
    wire   [31:0] rdata_dec5;          // HRDATA input
    reg           sel_dec5;            // HSEL output

    // Bus-switch output MI6
    wire          active_dec6;         // active_dec signal
    wire          readyout_dec6;       // HREADYOUT input
    wire    [1:0] resp_dec6;           // HRESP input
    wire   [31:0] rdata_dec6;          // HRDATA input
    reg           sel_dec6;            // HSEL output


// -----------------------------------------------------------------------------
// Signal declarations
// -----------------------------------------------------------------------------

    // Selected Output port data and control signals
    reg           active_dec;          // Combinatorial active_dec O/P signal
    reg           HREADYOUTS;      // Combinatorial HREADYOUT signal
    reg     [1:0] HRESPS;          // Combinatorial HRESPS signal
    reg    [31:0] HRDATAS;         // Read data bus

    reg     [3:0] addr_out_port;     // Address output ports
    reg     [3:0] data_out_port;     // Data output ports

    // Default slave signals
    reg           sel_dft_slv;       // HSEL signal
    wire          readyout_dft_slv;  // HREADYOUT signal
    wire    [1:0] resp_dft_slv;      // Combinatorial HRESPS signal


// -----------------------------------------------------------------------------
// Beginning of main code
// -----------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Default slave (accessed when HADDR is unmapped)
//------------------------------------------------------------------------------

  L1AhbMtx_default_slave u_L1AhbMtx_default_slave (

    // Common AHB signals
    .HCLK        (HCLK),
    .HRESETn     (HRESETn),

    // AHB Control signals
    .HSEL        (sel_dft_slv),
    .HTRANS      (trans_dec),
    .HREADY      (HREADYS),
    .HREADYOUT   (readyout_dft_slv),
    .HRESP       (resp_dft_slv)

    );


//------------------------------------------------------------------------------
// Address phase signals
//------------------------------------------------------------------------------

// The address decode is done in two stages. This is so that the address
//  decode occurs in only one process, p_addr_out_portComb, and then the select
//  signal is factored in.
//
// Note that the hexadecimal address values are reformatted to align with the
//  lower bound of decode_addr_dec[31:10], which is not a hex character boundary

  always @ (
             decode_addr_dec or data_out_port or trans_dec
           )
    begin : p_addr_out_port_comb
      // Address region 0x00000000-0x0000ffff
      if (((decode_addr_dec >= 22'h000000) & (decode_addr_dec <= 22'h00003f))
                           | ((data_out_port == 4'b0000) & (trans_dec == 2'b00)))
        addr_out_port = 4'b0000;  // Select Output port MI0

      // Address region 0x00010000-0x0001ffff
      else if (((decode_addr_dec >= 22'h000040) & (decode_addr_dec <= 22'h00007f))
                           | ((data_out_port == 4'b0001) & (trans_dec == 2'b00)))
        addr_out_port = 4'b0001;  // Select Output port MI1

      // Address region 0x00020000-0x0002ffff
      else if (((decode_addr_dec >= 22'h000080) & (decode_addr_dec <= 22'h0000bf))
                           | ((data_out_port == 4'b0010) & (trans_dec == 2'b00)))
        addr_out_port = 4'b0010;  // Select Output port MI2

      // Address region 0x00030000-0x0003ffff
      else if (((decode_addr_dec >= 22'h0000c0) & (decode_addr_dec <= 22'h0000ff))
                           | ((data_out_port == 4'b0011) & (trans_dec == 2'b00)))
        addr_out_port = 4'b0011;  // Select Output port MI3

      // Address region 0x00040000-0x0fffffff
      else if (((decode_addr_dec >= 22'h000100) & (decode_addr_dec <= 22'h03ffff))
                           | ((data_out_port == 4'b0100) & (trans_dec == 2'b00)))
        addr_out_port = 4'b0100;  // Select Output port MI4

      // Address region 0x10000000-0x14ffffff
      else if (((decode_addr_dec >= 22'h040000) & (decode_addr_dec <= 22'h053fff))
                           | ((data_out_port == 4'b0101) & (trans_dec == 2'b00)))
        addr_out_port = 4'b0101;  // Select Output port MI5

      // Address region 0x15000000-0x150fffff
      else if (((decode_addr_dec >= 22'h054000) & (decode_addr_dec <= 22'h0543ff))
                           | ((data_out_port == 4'b0110) & (trans_dec == 2'b00)))
        addr_out_port = 4'b0110;  // Select Output port MI6

      else
        addr_out_port = 4'b1000;   // Select the default slave
    end // block: p_addr_out_port_comb

  // Select signal decode
  always @ (sel_dec or addr_out_port)
    begin : p_sel_comb
      sel_dec0 = 1'b0;
      sel_dec1 = 1'b0;
      sel_dec2 = 1'b0;
      sel_dec3 = 1'b0;
      sel_dec4 = 1'b0;
      sel_dec5 = 1'b0;
      sel_dec6 = 1'b0;
      sel_dft_slv = 1'b0;

      if (sel_dec)
        case (addr_out_port)
          4'b0000 : sel_dec0 = 1'b1;
          4'b0001 : sel_dec1 = 1'b1;
          4'b0010 : sel_dec2 = 1'b1;
          4'b0011 : sel_dec3 = 1'b1;
          4'b0100 : sel_dec4 = 1'b1;
          4'b0101 : sel_dec5 = 1'b1;
          4'b0110 : sel_dec6 = 1'b1;
          4'b1000 : sel_dft_slv = 1'b1;    // Select the default slave
          default : begin
            sel_dec0 = 1'bx;
            sel_dec1 = 1'bx;
            sel_dec2 = 1'bx;
            sel_dec3 = 1'bx;
            sel_dec4 = 1'bx;
            sel_dec5 = 1'bx;
            sel_dec6 = 1'bx;
            sel_dft_slv = 1'bx;
          end
        endcase // case(addr_out_port)
    end // block: p_sel_comb

// The decoder selects the appropriate active_dec signal depending on which
//  output stage is required for the transfer.
  always @ (
             active_dec0 or
             active_dec1 or
             active_dec2 or
             active_dec3 or
             active_dec4 or
             active_dec5 or
             active_dec6 or
             addr_out_port
           )
    begin : p_active_comb
      case (addr_out_port)
        4'b0000 : active_dec = active_dec0;
        4'b0001 : active_dec = active_dec1;
        4'b0010 : active_dec = active_dec2;
        4'b0011 : active_dec = active_dec3;
        4'b0100 : active_dec = active_dec4;
        4'b0101 : active_dec = active_dec5;
        4'b0110 : active_dec = active_dec6;
        4'b1000 : active_dec = 1'b1;         // Select the default slave
        default : active_dec = 1'bx;
      endcase // case(addr_out_port)
    end // block: p_active_comb


//------------------------------------------------------------------------------
// Data phase signals
//------------------------------------------------------------------------------

// The data_out_port needs to be updated when HREADY from the input stage is high.
//  Note: HREADY must be used, not HREADYOUT, because there are occaisions
//  (namely when the holding register gets loaded) when HREADYOUT may be low
//  but HREADY is high, and in this case it is important that the data_out_port
//  gets updated.

  always @ (negedge HRESETn or posedge HCLK)
    begin : p_data_out_port_seq
      if (~HRESETn)
        data_out_port <= {4{1'b0}};
      else
        if (HREADYS)
          data_out_port <= addr_out_port;
    end // block: p_data_out_port_seq

  // HREADYOUTS output decode
  always @ (
             readyout_dft_slv or
             readyout_dec0 or
             readyout_dec1 or
             readyout_dec2 or
             readyout_dec3 or
             readyout_dec4 or
             readyout_dec5 or
             readyout_dec6 or
             data_out_port
           )
  begin : p_ready_comb
    case (data_out_port)
      4'b0000 : HREADYOUTS = readyout_dec0;
      4'b0001 : HREADYOUTS = readyout_dec1;
      4'b0010 : HREADYOUTS = readyout_dec2;
      4'b0011 : HREADYOUTS = readyout_dec3;
      4'b0100 : HREADYOUTS = readyout_dec4;
      4'b0101 : HREADYOUTS = readyout_dec5;
      4'b0110 : HREADYOUTS = readyout_dec6;
      4'b1000 : HREADYOUTS = readyout_dft_slv;    // Select the default slave
      default : HREADYOUTS = 1'bx;
    endcase // case(data_out_port)
  end // block: p_ready_comb

  // HRESPS output decode
  always @ (
             resp_dft_slv or
             resp_dec0 or
             resp_dec1 or
             resp_dec2 or
             resp_dec3 or
             resp_dec4 or
             resp_dec5 or
             resp_dec6 or
             data_out_port
           )
  begin : p_resp_comb
    case (data_out_port)
      4'b0000 : HRESPS = resp_dec0;
      4'b0001 : HRESPS = resp_dec1;
      4'b0010 : HRESPS = resp_dec2;
      4'b0011 : HRESPS = resp_dec3;
      4'b0100 : HRESPS = resp_dec4;
      4'b0101 : HRESPS = resp_dec5;
      4'b0110 : HRESPS = resp_dec6;
      4'b1000 : HRESPS = resp_dft_slv;     // Select the default slave
      default : HRESPS = {2{1'bx}};
    endcase // case (data_out_port)
  end // block: p_resp_comb

  // HRDATAS output decode
  always @ (
             rdata_dec0 or
             rdata_dec1 or
             rdata_dec2 or
             rdata_dec3 or
             rdata_dec4 or
             rdata_dec5 or
             rdata_dec6 or
             data_out_port
           )
  begin : p_rdata_comb
    case (data_out_port)
      4'b0000 : HRDATAS = rdata_dec0;
      4'b0001 : HRDATAS = rdata_dec1;
      4'b0010 : HRDATAS = rdata_dec2;
      4'b0011 : HRDATAS = rdata_dec3;
      4'b0100 : HRDATAS = rdata_dec4;
      4'b0101 : HRDATAS = rdata_dec5;
      4'b0110 : HRDATAS = rdata_dec6;
      4'b1000 : HRDATAS = {32{1'b0}};   // Select the default slave
      default : HRDATAS = {32{1'bx}};
    endcase // case (data_out_port)
  end // block: p_rdata_comb


endmodule

// --================================= End ===================================--
//-----------------------------------------------------------------------------
// The confidential and proprietary information contained in this file may
// only be used by a person authorised under and to the extent permitted
// by a subsisting licensing agreement from ARM Limited.
//
//            (C) COPYRIGHT 2001-2013-2025 ARM Limited.
//                ALL RIGHTS RESERVED
//
// This entire notice must be reproduced on all copies of this file
// and copies of this file may only be made by a person if such person is
// permitted to do so under the terms of a subsisting license agreement
// from ARM Limited.
//
//      SVN Information
//
//      Checked In          : $Date: 2012-10-15 18:01:36 +0100 (Mon, 15 Oct 2012) $
//
//      Revision            : $Revision: 225465 $
//
//      Release Information : Cortex-M System Design Kit-r1p0-01rel0
//
//-----------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
//  Abstract            : The Input Stage is used to hold a pending transfer
//                        when the required output stage is not available.
//
//-----------------------------------------------------------------------------

`timescale 1ns/1ps

module L1AhbMtxInStg (

    // Common AHB signals
    HCLK,
    HRESETn,

    // Input Port Address/Control Signals
    HSELS,
    HADDRS,
    HTRANSS,
    HWRITES,
    HSIZES,
    HBURSTS,
    HPROTS,
    HMASTERS,
    HMASTLOCKS,
    HREADYS,

    // Internal Response
    active_ip,
    readyout_ip,
    resp_ip,

    // Input Port Response
    HREADYOUTS,
    HRESPS,

    // Internal Address/Control Signals
    sel_ip,
    addr_ip,
    trans_ip,
    write_ip,
    size_ip,
    burst_ip,
    prot_ip,
    master_ip,
    mastlock_ip,
    held_tran_ip

    );


// -----------------------------------------------------------------------------
// Input and Output declarations
// -----------------------------------------------------------------------------

    input         HCLK;            // AHB System Clock
    input         HRESETn;         // AHB System Reset
    input         HSELS;           // Slave Select from AHB
    input  [31:0] HADDRS;          // Address bus from AHB
    input   [1:0] HTRANSS;         // Transfer type from AHB
    input         HWRITES;         // Transfer direction from AHB
    input   [2:0] HSIZES;          // Transfer size from AHB
    input   [2:0] HBURSTS;         // Burst type from AHB
    input   [3:0] HPROTS;          // Protection control from AHB
    input   [3:0] HMASTERS;        // Master number from AHB
    input         HMASTLOCKS;      // Locked Sequence  from AHB
    input         HREADYS;         // Transfer done from AHB
    input         active_ip;          // active_ip signal
    input         readyout_ip;        // HREADYOUT input
    input   [1:0] resp_ip;            // HRESP input

    output        HREADYOUTS;      // HREADY feedback to AHB
    output  [1:0] HRESPS;          // Transfer response to AHB
    output        sel_ip;             // HSEL output
    output [31:0] addr_ip;            // HADDR output
    output  [1:0] trans_ip;           // HTRANS output
    output        write_ip;           // HWRITE output
    output  [2:0] size_ip;            // HSIZE output
    output  [2:0] burst_ip;           // HBURST output
    output  [3:0] prot_ip;            // HPROT output
    output [3:0]  master_ip;          // HMASTER output
    output        mastlock_ip;        // HMASTLOCK output
    output        held_tran_ip;        // Holding register active flag


// -----------------------------------------------------------------------------
// Constant declarations
// -----------------------------------------------------------------------------

// HTRANS transfer type signal encoding
`define TRN_IDLE    2'b00     // Idle Transfer
`define TRN_BUSY    2'b01     // Busy Transfer
`define TRN_NONSEQ  2'b10     // Nonsequential transfer
`define TRN_SEQ     2'b11     // Sequential transfer

// HBURST transfer type signal encoding
`define BUR_SINGLE  3'b000    // Single BURST
`define BUR_INCR    3'b001    // Incremental BURSTS
`define BUR_WRAP4   3'b010    // 4-beat wrap
`define BUR_INCR4   3'b011    // 4-beat incr
`define BUR_WRAP8   3'b100    // 8-beat wrap
`define BUR_INCR8   3'b101    // 8-beat incr
`define BUR_WRAP16  3'b110    // 16-beat wrap
`define BUR_INCR16  3'b111    // 16-beat incr

// HRESP signal encoding
`define RSP_OKAY    2'b00      // OKAY response
`define RSP_ERROR   2'b01     // ERROR response
`define RSP_RETRY   2'b10     // RETRY response
`define RSP_SPLIT   2'b11     // SPLIT response


// -----------------------------------------------------------------------------
// Wire declarations
// -----------------------------------------------------------------------------

    wire        HCLK;            // AHB System Clock
    wire        HRESETn;         // AHB System Reset
    wire        HSELS;           // Slave Select from AHB
    wire [31:0] HADDRS;          // Address bus from AHB
    wire  [1:0] HTRANSS;         // Transfer type from AHB
    wire        HWRITES;         // Transfer direction from AHB
    wire  [2:0] HSIZES;          // Transfer size from AHB
    wire  [2:0] HBURSTS;         // Burst type from AHB
    wire  [3:0] HPROTS;          // Protection control from AHB
    wire  [3:0] HMASTERS;        // Master number from AHB
    wire        HMASTLOCKS;      // Locked Sequence  from AHB
    wire        HREADYS;         // Transfer done from AHB
    reg         HREADYOUTS;      // HREADY feedback to AHB
    reg   [1:0] HRESPS;          // Transfer response to AHB
    reg         sel_ip;             // HSEL output
    reg  [31:0] addr_ip;            // HADDR output
    wire  [1:0] trans_ip;           // HTRANS output
    reg         write_ip;           // HWRITE output
    reg   [2:0] size_ip;            // HSIZE output
    wire  [2:0] burst_ip;           // HBURST output
    reg   [3:0] prot_ip;            // HPROT output
    reg   [3:0] master_ip;          // HMASTER output
    reg         mastlock_ip;        // HMASTLOCK output
    wire        held_tran_ip;        // Holding register active flag
    wire        active_ip;          // active_ip signal
    wire        readyout_ip;        // HREADYOUT input
    wire  [1:0] resp_ip;            // HRESP input


// -----------------------------------------------------------------------------
// Signal declarations
// -----------------------------------------------------------------------------

    wire        load_reg;             // Holding register load flag
    wire        pend_tran;            // An active transfer cannot complete
    reg         pend_tran_reg;         // Registered version of pend_tran
    wire        addr_valid;           // Indicates address phase of
                                     // valid transfer
    reg         data_valid;           // Indicates data phase of
                                     // valid transfer
    reg   [1:0] reg_trans;            // Registered HTRANSS
    reg  [31:0] reg_addr;             // Registered HADDRS
    reg         reg_write;            // Registered HWRITES
    reg   [2:0] reg_size;             // Registered HSIZES
    reg   [2:0] reg_burst;            // Registered HBURSTS
    reg   [3:0] reg_prot;             // Registered HPROTS
    reg   [3:0] reg_master;           // Registerd HMASTERS
    reg         reg_mastlock;         // Registered HMASTLOCKS
    reg   [1:0] transb;               // HTRANS output used for burst information
    reg   [1:0] trans_int;            // HTRANS output
    reg   [2:0] burst_int;            // HBURST output
    reg   [3:0] offset_addr;          // Address offset for boundary logic
    reg   [3:0] check_addr;           // Address check for wrapped bursts
    reg         burst_override;       // Registered burst_override_next
    wire        burst_override_next;  // Indicates burst has been over-ridden
    reg         bound;                // Registered version of bound_next
    wire        bound_next;           // Indicates boundary wrapping
    wire        bound_en;             // Clock-enable for bound register


// -----------------------------------------------------------------------------
// Beginning of main code
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
// Holding Registers
// -----------------------------------------------------------------------------
// Each input port has a holding register associated with it and a mux to
//  select between the register and the direct input path. The control of
//  the mux is done simply by selecting the holding register when it is loaded
//  with a pending transfer, otherwise the straight through path is used.

  always @ (negedge HRESETn or posedge HCLK)
    begin : p_holding_reg_seq1
      if (~HRESETn)
        begin
          reg_trans    <= 2'b00;
          reg_addr     <= {32{1'b0}};
          reg_write    <= 1'b0 ;
          reg_size     <= 3'b000;
          reg_burst    <= 3'b000;
          reg_prot     <= {4{1'b0}};
          reg_master   <= 4'b0000;
          reg_mastlock <= 1'b0 ;
        end
      else
        if (load_reg)
          begin
            reg_trans    <= HTRANSS;
            reg_addr     <= HADDRS;
            reg_write    <= HWRITES;
            reg_size     <= HSIZES;
            reg_burst    <= HBURSTS;
            reg_prot     <= HPROTS;
            reg_master   <= HMASTERS;
            reg_mastlock <= HMASTLOCKS;
          end
    end

  // addr_valid indicates the address phase of an active (non-BUSY/IDLE)
  // transfer to this slave port
  assign addr_valid = ( HSELS & HTRANSS[1] );

  // The holding register is loaded whenever there is a transfer on the input
  // port which is validated by active HREADYS
  assign load_reg = ( addr_valid & HREADYS );

  // data_valid register
  // addr_valid indicates the data phase of an active (non-BUSY/IDLE)
  // transfer to this slave port. A valid response (HREADY, HRESP) must be
  // generated
  always @ (negedge HRESETn or posedge HCLK)
    begin : p_data_valid
      if (~HRESETn)
        data_valid <= 1'b0;
      else
       if (HREADYS)
        data_valid  <= addr_valid;
    end

// -----------------------------------------------------------------------------
// Generate HeldTran
// -----------------------------------------------------------------------------
// The HeldTran signal is used to indicate when there is an active transfer
// being presented to the output stage, either passing straight through or from
// the holding register.

  // pend_tran indicates that an active transfer presented to this
  // slave cannot complete immediately.  It is always set after the
  // load_reg signal has been active. When set, it is cleared when the
  // transfer is being driven onto the selected slave (as indicated by
  // active_ip being high) and HREADY from the selected slave is high.
  assign pend_tran = (load_reg & (~active_ip)) ? 1'b1 :
                    (active_ip & readyout_ip) ? 1'b0 : pend_tran_reg;

  // pend_tran_reg indicates that an active transfer was accepted by the input
  // stage,but not by the output stage, and so the holding registers should be
  // used
  always @ (negedge HRESETn or posedge HCLK)
    begin : p_pend_tran_reg
      if (~HRESETn)
        pend_tran_reg <= 1'b0;
      else
        pend_tran_reg <= pend_tran;
    end

  // held_tran_ip indicates an active transfer, and is held whilst that transfer is
  // in the holding registers.  It passes to the output stage where it acts as
  // a request line to the arbitration scheme
  assign  held_tran_ip  = (load_reg | pend_tran_reg);

  // The output from this stage is selected from the holding register when
  //  there is a held transfer. Otherwise the direct path is used.

  always @ ( pend_tran_reg or HSELS or HTRANSS or HADDRS or HWRITES or
             HSIZES or HBURSTS or HPROTS or HMASTERS or HMASTLOCKS or
             reg_addr or reg_write or reg_size or reg_burst or reg_prot or
             reg_master or reg_mastlock
           )
    begin : p_mux_comb
      if (~pend_tran_reg)
        begin
          sel_ip      = HSELS;
          trans_int   = HTRANSS;
          addr_ip     = HADDRS;
          write_ip    = HWRITES;
          size_ip     = HSIZES;
          burst_int   = HBURSTS;
          prot_ip     = HPROTS;
          master_ip   = HMASTERS;
          mastlock_ip = HMASTLOCKS;
        end
      else
        begin
          sel_ip      = 1'b1;
          trans_int   = `TRN_NONSEQ;
          addr_ip     = reg_addr;
          write_ip    = reg_write;
          size_ip     = reg_size;
          burst_int   = reg_burst;
          prot_ip     = reg_prot;
          master_ip   = reg_master;
          mastlock_ip = reg_mastlock;
        end
    end

  // The transb output is used to select the correct Burst value when completing
  // an interrupted defined-lenght burst.

  always @ (pend_tran_reg or HTRANSS or reg_trans)
    begin : p_transb_comb
      if (~pend_tran_reg)
        transb = HTRANSS;
      else
        transb = reg_trans;
    end // block: p_transb_comb


  // Convert SEQ->NONSEQ and BUSY->IDLE when an address boundary is crossed
  // whilst the burst type is being over-ridden, i.e. when completing an
  // interrupted wrapping burst.
  assign trans_ip = (burst_override & bound) ? {trans_int[1], 1'b0}
               : trans_int;

  assign burst_ip = (burst_override & (transb != `TRN_NONSEQ)) ? `BUR_INCR
               : burst_int;

// -----------------------------------------------------------------------------
// HREADYOUT Generation
// -----------------------------------------------------------------------------
// There are three possible sources for the HREADYOUT signal.
//  - It is driven LOW when there is a held transfer.
//  - It is driven HIGH when not Selected or for Idle/Busy transfers.
//  - At all other times it is driven from the appropriate shared
//    slave.

  always @ (data_valid or pend_tran_reg or readyout_ip or resp_ip)
    begin : p_ready_comb
      if (~data_valid)
        begin
          HREADYOUTS = 1'b1;
          HRESPS     = `RSP_OKAY;
        end
      else if (pend_tran_reg)
        begin
          HREADYOUTS = 1'b0;
          HRESPS     = `RSP_OKAY;
        end
      else
        begin
          HREADYOUTS = readyout_ip;
          HRESPS     = resp_ip;
        end
    end // block: p_ready_comb

// -----------------------------------------------------------------------------
// Early Burst Termination
// -----------------------------------------------------------------------------
// There are times when the output stage will switch to another input port
//  without allowing the current burst to complete. In these cases the HTRANS
//  and HBURST signals need to be overriden to ensure that the transfers
//  reaching the output port meet the AHB specification.

  assign burst_override_next  = ( (HTRANSS == `TRN_NONSEQ) |
                                (HTRANSS == `TRN_IDLE) ) ? 1'b0
                              : ( (HTRANSS ==`TRN_SEQ) &
                                   load_reg &
                                   (~active_ip) ) ? 1'b1
                                  : burst_override;

  // burst_override register
  always @ (negedge HRESETn or posedge HCLK)
    begin : p_burst_overrideseq
      if (~HRESETn)
        burst_override <= 1'b0;
      else
        if (HREADYS)
          burst_override <= burst_override_next;
    end // block: p_burst_overrideseq

// -----------------------------------------------------------------------------
// Boundary Checking Logic
// -----------------------------------------------------------------------------
  // offset_addr
  always @ (HADDRS or HSIZES)
    begin : p_offset_addr_comb
      case (HSIZES)
        3'b000 : offset_addr = HADDRS[3:0];
        3'b001 : offset_addr = HADDRS[4:1];
        3'b010 : offset_addr = HADDRS[5:2];
        3'b011 : offset_addr = HADDRS[6:3];

        3'b100, 3'b101, 3'b110, 3'b111 :
          offset_addr = HADDRS[3:0];      // Sizes >= 128-bits are not supported

        default : offset_addr = 4'bxxxx;
      endcase
    end

  // check_addr
  always @ (offset_addr or HBURSTS)
    begin : p_check_addr_comb
      case (HBURSTS)
        `BUR_WRAP4 : begin
          check_addr[1:0] = offset_addr[1:0];
          check_addr[3:2] = 2'b11;
        end

        `BUR_WRAP8 : begin
          check_addr[2:0] = offset_addr[2:0];
          check_addr[3]   = 1'b1;
        end

        `BUR_WRAP16 :
          check_addr[3:0] = offset_addr[3:0];

        `BUR_SINGLE, `BUR_INCR, `BUR_INCR4, `BUR_INCR8, `BUR_INCR16 :
          check_addr[3:0] = 4'b0000;

        default : check_addr[3:0] = 4'bxxxx;
      endcase
    end

  assign bound_next = ( check_addr == 4'b1111 );

  assign bound_en = ( HTRANSS[1] & HREADYS );

  // bound register
  always @ (negedge HRESETn or posedge HCLK)
    begin : p_bound_seq
      if (~HRESETn)
        bound <= 1'b0;
      else
        if (bound_en)
          bound <= bound_next;
    end


endmodule

// --================================= End ===================================--
//-----------------------------------------------------------------------------
// The confidential and proprietary information contained in this file may
// only be used by a person authorised under and to the extent permitted
// by a subsisting licensing agreement from ARM Limited.
//
//            (C) COPYRIGHT 2001-2013-2025 ARM Limited.
//                ALL RIGHTS RESERVED
//
// This entire notice must be reproduced on all copies of this file
// and copies of this file may only be made by a person if such person is
// permitted to do so under the terms of a subsisting license agreement
// from ARM Limited.
//
//      SVN Information
//
//      Checked In          : $Date: 2012-10-15 18:01:36 +0100 (Mon, 15 Oct 2012) $
//
//      Revision            : $Revision: 225465 $
//
//      Release Information : Cortex-M System Design Kit-r1p0-01rel0
//
//-----------------------------------------------------------------------------
//
//-----------------------------------------------------------------------------
//  Abstract            : The Output Stage is used to route the required input
//                        stage to the shared slave output.
//
//  Notes               : The bus matrix has full connectivity,
//                         and has a burst arbiter scheme.
//
//-----------------------------------------------------------------------------

`timescale 1ns/1ps

module L1AhbMtxOutStg (

    // Common AHB signals
    HCLK,
    HRESETn,

    // Port 0 Signals
    sel_op0,
    addr_op0,
    trans_op0,
    write_op0,
    size_op0,
    burst_op0,
    prot_op0,
    master_op0,
    mastlock_op0,
    wdata_op0,
    held_tran_op0,

    // Slave read data and response
    HREADYOUTM,

    active_op0,

    // Slave Address/Control Signals
    HSELM,
    HADDRM,
    HTRANSM,
    HWRITEM,
    HSIZEM,
    HBURSTM,
    HPROTM,
    HMASTERM,
    HMASTLOCKM,
    HREADYMUXM,
    HWDATAM

    );


// -----------------------------------------------------------------------------
// Input and Output declarations
// -----------------------------------------------------------------------------

    // Common AHB signals
    input         HCLK;       // AHB system clock
    input         HRESETn;    // AHB system reset

    // Bus-switch input 0
    input         sel_op0;       // Port 0 HSEL signal
    input [31:0]  addr_op0;      // Port 0 HADDR signal
    input  [1:0]  trans_op0;     // Port 0 HTRANS signal
    input         write_op0;     // Port 0 HWRITE signal
    input  [2:0]  size_op0;      // Port 0 HSIZE signal
    input  [2:0]  burst_op0;     // Port 0 HBURST signal
    input  [3:0]  prot_op0;      // Port 0 HPROT signal
    input  [3:0]  master_op0;    // Port 0 HMASTER signal
    input         mastlock_op0;  // Port 0 HMASTLOCK signal
    input [31:0]  wdata_op0;     // Port 0 HWDATA signal
    input         held_tran_op0;  // Port 0 HeldTran signal

    input         HREADYOUTM; // HREADY feedback

    output        active_op0;    // Port 0 Active signal

    // Slave Address/Control Signals
    output        HSELM;      // Slave select line
    output [31:0] HADDRM;     // Address
    output  [1:0] HTRANSM;    // Transfer type
    output        HWRITEM;    // Transfer direction
    output  [2:0] HSIZEM;     // Transfer size
    output  [2:0] HBURSTM;    // Burst type
    output  [3:0] HPROTM;     // Protection control
    output  [3:0] HMASTERM;   // Master ID
    output        HMASTLOCKM; // Locked transfer
    output        HREADYMUXM; // Transfer done
    output [31:0] HWDATAM;    // Write data


// -----------------------------------------------------------------------------
// Wire declarations
// -----------------------------------------------------------------------------
    wire        HCLK;       // AHB system clock
    wire        HRESETn;    // AHB system reset

    // Bus-switch input 0
    wire        sel_op0;       // Port 0 HSEL signal
    wire [31:0] addr_op0;      // Port 0 HADDR signal
    wire  [1:0] trans_op0;     // Port 0 HTRANS signal
    wire        write_op0;     // Port 0 HWRITE signal
    wire  [2:0] size_op0;      // Port 0 HSIZE signal
    wire  [2:0] burst_op0;     // Port 0 HBURST signal
    wire  [3:0] prot_op0;      // Port 0 HPROT signal
    wire  [3:0] master_op0;    // Port 0 HMASTER signal
    wire        mastlock_op0;  // Port 0 HMASTLOCK signal
    wire [31:0] wdata_op0;     // Port 0 HWDATA signal
    wire        held_tran_op0;  // Port 0 HeldTran signal
    reg         active_op0;    // Port 0 Active signal

    // Slave Address/Control Signals
    wire        HSELM;      // Slave select line
    reg  [31:0] HADDRM;     // Address
    wire  [1:0] HTRANSM;    // Transfer type
    reg         HWRITEM;    // Transfer direction
    reg   [2:0] HSIZEM;     // Transfer size
    wire  [2:0] HBURSTM;    // Burst type
    reg   [3:0] HPROTM;     // Protection control
    reg   [3:0] HMASTERM;   // Master ID
    wire        HMASTLOCKM; // Locked transfer
    wire        HREADYMUXM; // Transfer done
    reg  [31:0] HWDATAM;    // Write data
    wire        HREADYOUTM; // HREADY feedback


// -----------------------------------------------------------------------------
// Signal declarations
// -----------------------------------------------------------------------------
    wire        req_port0;     // Port 0 request signal

    wire  [0:0] addr_in_port;   // Address input port
    reg   [0:0] data_in_port;   // Data input port
    wire        no_port;       // No port selected signal
    reg         slave_sel;     // Slave select signal

    reg         hsel_lock;     // Held HSELS during locked sequence
    wire        next_hsel_lock; // Pre-registered hsel_lock
    wire        hlock_arb;     // HMASTLOCK modified by HSEL for arbitration

    reg         i_hselm;       // Internal HSELM
    reg   [1:0] i_htransm;     // Internal HTRANSM
    reg   [2:0] i_hburstm;     // Internal HBURSTM
    wire        i_hreadymuxm;  // Internal HREADYMUXM
    reg         i_hmastlockm;  // Internal HMASTLOCKM


// -----------------------------------------------------------------------------
// Beginning of main code
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
// Port Selection
// -----------------------------------------------------------------------------

  assign req_port0 = held_tran_op0 & sel_op0;

  // Arbiter instance for resolving requests to this output stage
  L1AhbMtxArb u_output_arb (

    .HCLK       (HCLK),
    .HRESETn    (HRESETn),

    .req_port0   (req_port0),

    .HREADYM    (i_hreadymuxm),
    .HSELM      (i_hselm),
    .HTRANSM    (i_htransm),
    .HBURSTM    (i_hburstm),
    .HMASTLOCKM (hlock_arb),

    .addr_in_port (addr_in_port),
    .no_port     (no_port)

    );


  // Active signal combinatorial decode
  always @ (addr_in_port or no_port)
    begin : p_active_comb
      // Default value(s)
      active_op0 = 1'b0;

      // Decode selection when enabled
      if (~no_port)
        case (addr_in_port)
          1'b0 : active_op0 = 1'b1;
          default : begin
            active_op0 = 1'bx;
          end
        endcase // case(addr_in_port)
    end // block: p_active_comb


  //  Address/control output decode
  always @ (
             sel_op0 or addr_op0 or trans_op0 or write_op0 or
             size_op0 or burst_op0 or prot_op0 or
             master_op0 or mastlock_op0 or
             addr_in_port or no_port
           )
    begin : p_addr_mux
      // Default values
      i_hselm     = 1'b0;
      HADDRM      = {32{1'b0}};
      i_htransm   = 2'b00;
      HWRITEM     = 1'b0;
      HSIZEM      = 3'b000;
      i_hburstm   = 3'b000;
      HPROTM      = {4{1'b0}};
      HMASTERM    = 4'b0000;
      i_hmastlockm= 1'b0;

      // Decode selection when enabled
      if (~no_port)
        case (addr_in_port)
          // Bus-switch input 0
          1'b0 :
            begin
              i_hselm     = sel_op0;
              HADDRM      = addr_op0;
              i_htransm   = trans_op0;
              HWRITEM     = write_op0;
              HSIZEM      = size_op0;
              i_hburstm   = burst_op0;
              HPROTM      = prot_op0;
              HMASTERM    = master_op0;
              i_hmastlockm= mastlock_op0;
            end // case: 4'b0

          default :
            begin
              i_hselm     = 1'bx;
              HADDRM      = {32{1'bx}};
              i_htransm   = 2'bxx;
              HWRITEM     = 1'bx;
              HSIZEM      = 3'bxxx;
              i_hburstm   = 3'bxxx;
              HPROTM      = {4{1'bx}};
              HMASTERM    = 4'bxxxx;
              i_hmastlockm= 1'bx;
            end // case: default
        endcase // case(addr_in_port)
    end // block: p_addr_mux

  // hsel_lock provides support for AHB masters that address other
  // slave regions in the middle of a locked sequence (i.e. HSEL is
  // de-asserted during the locked sequence).  Unless HMASTLOCK is
  // held during these intermediate cycles, the OutputArb scheme will
  // lose track of the locked sequence and may allow another input
  // port to access the output port which should be locked
  assign next_hsel_lock = (i_hselm & i_htransm[1] & i_hmastlockm) ? 1'b1 :
                         (i_hmastlockm == 1'b0) ? 1'b0 :
                          hsel_lock;

  // Register hsel_lock
  always @ (negedge HRESETn or posedge HCLK)
    begin : p_hsel_lock
      if (~HRESETn)
        hsel_lock <= 1'b0;
      else
        if (i_hreadymuxm)
          hsel_lock <= next_hsel_lock;
    end

  // Version of HMASTLOCK which is masked when not selected, unless a
  // locked sequence has already begun through this port
  assign hlock_arb = i_hmastlockm & (hsel_lock | i_hselm);

  assign HTRANSM    = i_htransm;
  assign HBURSTM    = i_hburstm;
  assign HSELM      = i_hselm;
  assign HMASTLOCKM = i_hmastlockm;

  // Dataport register
  always @ (negedge HRESETn or posedge HCLK)
    begin : p_data_in_port_reg
      if (~HRESETn)
        data_in_port <= {1{1'b0}};
      else
        if (i_hreadymuxm)
          data_in_port <= addr_in_port;
    end

  // HWDATAM output decode
  always @ (
             wdata_op0 or
             data_in_port
           )
    begin : p_data_mux
      // Default value
      HWDATAM = {32{1'b0}};

      // Decode selection
      case (data_in_port)
        1'b0 : HWDATAM  = wdata_op0;
        default : HWDATAM = {32{1'bx}};
      endcase // case(data_in_port)
    end // block: p_data_mux


  // ---------------------------------------------------------------------------
  // HREADYMUXM generation
  // ---------------------------------------------------------------------------
  // The HREADY signal on the shared slave is generated directly from
  //  the shared slave HREADYOUTS if the slave is selected, otherwise
  //  it mirrors the HREADY signal of the appropriate input port
  always @ (negedge HRESETn or posedge HCLK)
    begin : p_slave_sel_reg
      if (~HRESETn)
        slave_sel <= 1'b0;
      else
        if (i_hreadymuxm)
          slave_sel  <= i_hselm;
    end

  // HREADYMUXM output selection
  assign i_hreadymuxm = (slave_sel) ? HREADYOUTM : 1'b1;

  // Drive output with internal version of the signal
  assign HREADYMUXM = i_hreadymuxm;


endmodule

// --================================= End ===================================--
module ahb_to_sram
#(
    parameter ADDR_WIDTH    = 32   ,   // 10~64
    parameter DATA_WIDTH    = 32  ,   // 8,16,32,64,128,256,512,1024
    parameter HBURST_WIDTH  = 3  ,   // 0,3
    parameter HPROT_WIDTH   = 0   ,   // 0,4,7
    parameter HMASTER_WIDTH = 8     // 0~8
)
(
    // ------ From system ------ //
    input                           HCLK        ,
    input                           HRESETn     ,
    // ------ From SRAM --------//
    input                           sram_rdy    ,
    input [DATA_WIDTH-1:0]          rdata       ,
    // ------ To SRAM ----------//
    output logic [ADDR_WIDTH-1:0]   raddr       ,  
    output logic                    re          ,
    output logic[DATA_WIDTH/8-1:0]  rsel        ,  
    output logic[ADDR_WIDTH-1:0]    waddr       ,
    output logic                    we          ,  
    output logic[DATA_WIDTH-1:0]    wdata       ,  
    output logic[DATA_WIDTH/8-1:0]  wsel        ,   
    // ------ From master ------ //
    input [ADDR_WIDTH-1:0]          HADDR       ,
    input [HBURST_WIDTH-1:0]        HBURST      ,
    input                           HMASTLOCK ,
    input [HPROT_WIDTH-1:0]         HPROT       ,
    input [2:0]                     HSIZE       ,
    input [1:0]                     HTRANS      ,
    input [DATA_WIDTH-1:0]          HWDATA      ,
    input                           HWRITE      ,
    // ------ From interconnect ------ //
    input                           HSEL        ,
    input                           HREADY      ,
    // ------ To interconnect ------ //
    output logic [DATA_WIDTH-1:0]   HRDATA      ,
    output logic                    HREADYOUT   ,
    output logic                    HRESP       
);
// AHB的从机一般是一些高速设备，比如memory之类；
// 此处代码假设slave是memory
// 以下定义了mem接口
/*
    we
    mem_ce
    raddr
    wdata
    rdata
*/
logic mem_ce;
logic transfer_on;
assign transfer_on = HSEL && HREADY && HTRANS[1]; 
//assign transfer_on = HSEL && HTRANS[1];   // HTRANS = NONSEQ/SEQ
typedef enum logic [1:0] {IDLE,MEM_W,MEM_R} state_t;
state_t state_c,state_n;
always_ff@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn)
        state_c <= IDLE;
    else
        state_c <= state_n;
end
always@(*) begin
    if(~HRESETn)
        state_n = IDLE;
    else begin
        state_n = IDLE;
        case(state_c) 
            IDLE: begin
                state_n = (transfer_on && HWRITE)?  MEM_W :
                          (transfer_on && ~HWRITE)? MEM_R :
                          IDLE;
            end
            MEM_W: begin
                state_n = transfer_on?    (HWRITE?    MEM_W : MEM_R) : IDLE;
            end
            MEM_R: begin
                state_n = transfer_on?    (HWRITE?    MEM_W : MEM_R) : IDLE;
            end
            default:    state_n = IDLE;
        endcase
    end
end
/*
always_ff@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn)
        mem_ce <= 1'b0;
    else if(state_n == MEM_W | state_n == MEM_R)
        mem_ce <= 1'b1;
    else 
        mem_ce <= 1'b0;
end
always_ff@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn)
        we <= 1'b0;
    else if(state_n == MEM_W)
        we <= 1'b1;
    else 
        we <= 1'b0;
end
always_ff@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn)
        re <= 1'b0;
    else if(state_n == MEM_R)
        re <= 1'b1;
    else 
        re <= 1'b0;
end
always_ff@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn)
        raddr <= 'b0;
    else if(state_n == MEM_R)
        raddr <= HADDR;
    //else 
    //    raddr <= 'b0;
end
always_ff@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn)
        rsel <= 4'b1111;
    else if(state_n == MEM_R)
        rsel <= 4'b1111;
    else 
        rsel <= 'b0;
end
*/
logic [ADDR_WIDTH-1:0] waddr_lat;
always@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn)
        waddr_lat <= 'd0;
    else if(transfer_on && HWRITE)
        waddr_lat <= HADDR;
end
logic [2:0] hsize_lat;
always@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn)
        hsize_lat <= 'd0;
    else if(transfer_on && HWRITE)
        hsize_lat <= HSIZE;
end

assign we = (state_c == MEM_W);
assign re = (state_n == MEM_R);
assign raddr = (state_n == MEM_R)?  HADDR : 'd0;
assign rsel = (state_n == MEM_R)?   4'b1111 : 'd0;
//assign waddr = {HADDR[ADDR_WIDTH-1:2],2'b0};
assign waddr = {waddr_lat[ADDR_WIDTH-1:2],2'b0};
assign wdata = HWDATA;

always@(negedge HCLK) begin
    if(we && re == 1) begin
      $display("Warning: RAW issue!! WA=0x%h, RA=0x%h",waddr,raddr);
    end
end

/*
always_ff@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn)
        waddr <= 'b0;
    else if(state_n == MEM_W)
        waddr <= {HADDR[ADDR_WIDTH-1:2],2'b0};
    //else 
    //    waddr <= 'b0;
end
*/
//always_ff@(posedge HCLK or negedge HRESETn) begin
//    if(~HRESETn)
//        wsel <= 'b0;
//    else if(state_n == MEM_W) begin
always@(*) begin
        if(hsize_lat==3'b000) begin
            wsel <= {4'b0001 << waddr_lat[1:0]};
        end
        else if(hsize_lat==3'b001) begin
            wsel <= {4'b0011 << waddr_lat[1:0]};
        end
        else begin
            wsel <= 4'b1111;
        end
    end
    //else
    //    wsel <= 'b0;
//end


always_ff@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn)
        HRDATA <= 'b0;
    else if(re)
        HRDATA <= rdata;
end

logic [DATA_WIDTH-1:0] rdata_merge;

//assign HRDATA = rdata;
//assign HRDATA = rdata;
assign HRESP = 1'b0;


always_ff@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn)
        HREADYOUT <= 1'b1;
    else //if(state_c == MEM_W | state_c == MEM_R)    // can add condition to extend transfer
        HREADYOUT <= sram_rdy;
end


//assign HREADYOUT = (state_c == MEM_W | state_c == MEM_R) && sram_rdy;

endmodule
module ahb_to_slave
#(
    parameter ADDR_WIDTH    = 32   ,   // 10~64
    parameter DATA_WIDTH    = 32  ,   // 8,16,32,64,128,256,512,1024
    parameter HBURST_WIDTH  = 3  ,   // 0,3
    parameter HPROT_WIDTH   = 0   ,   // 0,4,7
    parameter HMASTER_WIDTH = 8     // 0~8
)
(
    // ------ From system ------ //
    input                           HCLK        ,
    input                           HRESETn     ,
    // ------ From SRAM --------//
    input                           slave_rdy   ,
    input [DATA_WIDTH-1:0]          rdata       ,
    // ------ To SRAM ----------//
    output logic [ADDR_WIDTH-1:0]   raddr       ,  
    output logic                    re          ,
    output logic[DATA_WIDTH/8-1:0]  rsel        ,  
    output logic[ADDR_WIDTH-1:0]    waddr       ,
    output logic                    we          ,  
    output logic[DATA_WIDTH-1:0]    wdata       ,  
    output logic[DATA_WIDTH/8-1:0]  wsel        ,   
    // ------ From master ------ //
    input [ADDR_WIDTH-1:0]          HADDR       ,
    input [HBURST_WIDTH-1:0]        HBURST      ,
    input                           HMASTLOCK   ,
    input [HPROT_WIDTH-1:0]         HPROT       ,
    input [2:0]                     HSIZE       ,
    input [1:0]                     HTRANS      ,
    input [DATA_WIDTH-1:0]          HWDATA      ,
    input                           HWRITE      ,
    // ------ From interconnect ------ //
    input                           HSEL        ,
    input                           HREADY      ,
    // ------ To interconnect ------ //
    output logic [DATA_WIDTH-1:0]   HRDATA      ,
    output logic                    HREADYOUT   ,
    output logic                    HRESP       
);




logic transfer_on;
//assign transfer_on = HSEL && HREADY && HTRANS[1];   // HTRANS = NONSEQ/SEQ
assign transfer_on = HSEL && HTRANS[1] & HREADY;   // HTRANS = NONSEQ/SEQ

typedef enum logic [1:0] {IDLE,SLV_W,SLV_R} state_t;
state_t state_c,state_n;
always_ff@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn)
        state_c <= IDLE;
    else
        state_c <= state_n;
end
always@(*) begin
    if(~HRESETn)
        state_n = IDLE;
    else begin
        state_n = IDLE;
        case(state_c) 
            IDLE: begin
                state_n = (transfer_on && HWRITE)?  SLV_W :
                          (transfer_on && ~HWRITE)? SLV_R :
                          IDLE;
            end
            SLV_W: begin
                state_n = transfer_on?    (HWRITE?    SLV_W : SLV_R) : IDLE;
            end
            SLV_R: begin
                state_n = transfer_on?    (HWRITE?    SLV_W : SLV_R) : IDLE;
            end
            default:    state_n = IDLE;
        endcase
    end
end
/*
assign we = (state_n == SLV_W);
assign re = (state_n == SLV_R);
assign raddr = (state_n == SLV_R)?  HADDR : 'd0;
assign rsel = (state_n == SLV_R)?   4'b1111 : 'd0;
assign waddr = {HADDR[ADDR_WIDTH-1:2],2'b0};
assign wdata = HWDATA;
*/
logic [ADDR_WIDTH-1:0] waddr_lat;
always@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn)
        waddr_lat <= 'd0;
    else if(transfer_on && HWRITE)
        waddr_lat <= HADDR;
end
logic [2:0] hsize_lat;
always@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn)
        hsize_lat <= 'd0;
    else if(transfer_on && HWRITE)
        hsize_lat <= HSIZE;
end

assign we = (state_c == SLV_W);
assign re = (state_n == SLV_R);
assign raddr = (state_n == SLV_R)?  HADDR : 'd0;
assign rsel = (state_n == SLV_R)?   4'b1111 : 'd0;
//assign waddr = {HADDR[ADDR_WIDTH-1:2],2'b0};
assign waddr = {waddr_lat[ADDR_WIDTH-1:2],2'b0};
assign wdata = HWDATA;

always@(negedge HCLK) begin
    if(we && re == 1) begin
      $display("Warning: RAW issue!! WA=0x%h, RA=0x%h",waddr,raddr);
    end
end

always@(*) begin
        if(hsize_lat==3'b000) begin
            wsel <= {4'b0001 << waddr_lat[1:0]};
        end
        else if(hsize_lat==3'b001) begin
            wsel <= {4'b0011 << waddr_lat[1:0]};
        end
        else begin
            wsel <= 4'b1111;
        end
    end

//assign HRDATA = rdata;
always_ff@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn)
        HRDATA <= 'b0;
    else if(re)
        HRDATA <= rdata;
end

assign HRESP = 1'b0;


always_ff@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn)
        HREADYOUT <= 1'b1;
    else 
        HREADYOUT <= slave_rdy;
end


//assign HREADYOUT = slave_rdy;

endmodule
module ahb2apb_bridge #(
    parameter ADDR_WIDTH    = 32    ,  // max = 32
    parameter DATA_WIDTH    = 32    ,  // 8,16,32
    parameter HBURST_WIDTH  = 3     ,
    parameter HPROT_WIDTH   = 4
)
(
    // with AHB
    input                           HCLK                    ,   // From AHB
    input                           HRESETn                 ,   // From AHB
    input [ADDR_WIDTH-1:0]          HADDR                   ,   // From AHB
    input [HBURST_WIDTH-1:0]        HBURST                  ,   // From AHB
    input [2:0]                     HSIZE                   ,   // From AHB
    input [1:0]                     HTRANS                  ,   // From AHB
    input [DATA_WIDTH-1:0]          HWDATA                  ,   // From AHB
    
    //input [DATA_WIDTH/8-1:0]        hwstrb                  ,   // From AHB
    input                           HWRITE                  ,   // From AHB
    input                           HSEL                    ,   // From AHB
    input                           HREADY                  ,   // From AHB
    output logic                    HREADYOUT               ,   // To AHB
    output logic                    HRESP                   ,   // To AHB
    //output logic                    hexokay_o               ,   // To AHB
    output logic [DATA_WIDTH-1:0]   HRDATA                  ,   // To AHB
    input                           HMASTLOCK               ,
    input [HPROT_WIDTH-1:0]         HPROT                   ,    // with APB         
    input                           PCLK                    ,   // From APB
    input                           PRESETn                 ,   // From APB
    input                           PCLKen                  ,
    //output logic [ADDR_WIDTH-1:0]   PADDR                   ,   // To APB
    //output logic                    PENABLE                 ,   // To APB
    //output logic                    PWRITE                  ,   // To APB
    //output logic [DATA_WIDTH-1:0]   PWDATA                  ,   // To APB
    //output logic [DATA_WIDTH/8-1:0] pstrb                   ,   // To APB
    //input                           PREADY [0:PSLV_LEN]     ,   // From APBs
    //input [DATA_WIDTH-1:0]          PRDATA [0:PSLV_LEN]      // From APBs
    output logic                    PSELM1                    ,   // To APB
    output logic                    PSELM2                    , 
    output logic                    PSELM3                    , 
    output logic                    PSELM4                    , 
    output logic                    PSELM5                    , 
    output logic [ADDR_WIDTH-1:0]   PADDRM1                   ,
    output logic [ADDR_WIDTH-1:0]   PADDRM2                   ,
    output logic [ADDR_WIDTH-1:0]   PADDRM3                   ,
    output logic [ADDR_WIDTH-1:0]   PADDRM4                   ,
    output logic [ADDR_WIDTH-1:0]   PADDRM5                   ,
    output logic                    PENABLEM1                 ,
    output logic                    PENABLEM2                 ,
    output logic                    PENABLEM3                 ,
    output logic                    PENABLEM4                 ,
    output logic                    PENABLEM5                 ,
    output logic                    PWRITEM1                  ,
    output logic                    PWRITEM2                  ,
    output logic                    PWRITEM3                  ,
    output logic                    PWRITEM4                  ,
    output logic                    PWRITEM5                  ,
    output logic [DATA_WIDTH-1:0]   PWDATAM1                  ,
    output logic [DATA_WIDTH-1:0]   PWDATAM2                  ,
    output logic [DATA_WIDTH-1:0]   PWDATAM3                  ,
    output logic [DATA_WIDTH-1:0]   PWDATAM4                  ,
    output logic [DATA_WIDTH-1:0]   PWDATAM5                  ,
    input                           PREADYM1                  ,   // From APBs
    input [DATA_WIDTH-1:0]          PRDATAM1                  , // From APBs
    input                           PREADYM2                  ,
    input [DATA_WIDTH-1:0]          PRDATAM2                  ,
    input                           PREADYM3                  ,
    input [DATA_WIDTH-1:0]          PRDATAM3                  ,
    input                           PREADYM4                  ,
    input [DATA_WIDTH-1:0]          PRDATAM4                  ,
    input                           PREADYM5                  ,
    input [DATA_WIDTH-1:0]          PRDATAM5                  
);

/* APB Slave List */
// 0. UART                               0x40000000~0x4000ffff
// 1. SPI                                0x40010000~0x4001ffff
// 2. I2C                                0x40020000~0x4002ffff  
// 3. Memory                             0x40030000~0x4003ffff  
// 4. LED                                0x40040000~0x4004ffff   
// Reserved
/*
logic [PSLV_NUM-1:0] PSEL_tmp;
always@(*) begin
    case(HADDR[ADDR_WIDTH+11:ADDR_WIDTH])
        12'h0:  PSEL_tmp = 'b1;
        12'h1:  PSEL_tmp = 'b10;
        12'h2:  PSEL_tmp = 'b100;
        12'h3:  PSEL_tmp = 'b1000;
        12'h4:  PSEL_tmp = 'b10000;
        default:PSEL_tmp = 'b0;
    endcase
end
*/
logic PSEL;
logic PENABLE;
logic [ADDR_WIDTH-1:0] PADDR;
logic PWRITE;
logic [DATA_WIDTH-1:0] PWDATA;

logic [ADDR_WIDTH-1:0] haddr_lat;
logic hwrite_lat;
logic [DATA_WIDTH-1:0] hwdata_lat;

logic pready_mux;
logic [DATA_WIDTH-1:0] prdata_mux;

logic apb_sel;
assign apb_sel = HSEL && HREADY && HTRANS[1];

assign pready_mux = PSELM1?  PREADYM1 :
                    PSELM2?  PREADYM2 :
                    PSELM3?  PREADYM3 :
                    PSELM4?  PREADYM4 :
                    PSELM5?  PREADYM5 : 1'b0;

assign prdata_mux = PSELM1?  PRDATAM1 :
                    PSELM2?  PRDATAM2 :
                    PSELM3?  PRDATAM3 :
                    PSELM4?  PRDATAM4 :
                    PSELM5?  PRDATAM5 : 32'bx;

typedef enum logic [1:0] {IDLE,SETUP,ACCESS,WAIT} state_t;
state_t state_c,state_n;
always_ff@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn)
        state_c <= IDLE;
    else 
        state_c <= state_n;
end

always@(*) begin
    state_n = IDLE;
    case(state_c)
        IDLE:   state_n = apb_sel?   SETUP:IDLE;
        SETUP:  state_n = (PCLKen)? ACCESS : SETUP;
        //ACCESS: state_n = (HSEL)?   ACCESS:IDLE;
        ACCESS: state_n = (pready_mux && PCLKen)?   (apb_sel?  WAIT : IDLE):ACCESS;
        WAIT:   state_n = (apb_sel?  SETUP : IDLE);
        default:state_n = IDLE;
    endcase
end

always_ff@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn) begin
        haddr_lat <= 'd0;
        hwrite_lat <= 1'b0;
        hwdata_lat <= 'd0;
    end
    else if(HSEL) begin
        haddr_lat <= HADDR;
        hwrite_lat <= HWRITE;
        hwdata_lat <= HWDATA;
    end
end

always_ff@(posedge PCLK or negedge PRESETn) begin
    if(~PRESETn) begin
        PSEL <= 'd0;
        PENABLE <= 'd0;
        PADDR <= 'd0;
        PWRITE <= 'd0;
        PWDATA <= 'd0;
    end
    else if(state_c == IDLE)   begin
        PSEL <= 'd0;
        PENABLE <= 'd0;
        PADDR <= 'd0;
        PWRITE <= 'd0;
        PWDATA <= 'd0;
    end
    else if(state_c == SETUP)  begin    // Lock AHB information
        PSEL <= 1'b1;
        PENABLE <= 1'b0;
        PADDR  <= haddr_lat; 
        PWRITE <= hwrite_lat;
        PWDATA <= hwdata_lat;
    end
    else if(state_c == ACCESS) begin
        PENABLE <= 1'b1;
    end
    else if(state_c == WAIT) begin
      PENABLE <= 1'b0;
    end
end

//assign PADDR = HADDR[ADDR_WIDTH-1:0];
//assign PSEL = (state_c != IDLE)? ({{(PSLV_NUM-1){1'b0}},1'b1} << HADDR[ADDR_WIDTH+11:ADDR_WIDTH]) : 'd0;
//assign PSEL = PSEL_tmp;
//logic apb_psel;
//assign apb_psel = (state_c == ACCESS);
//assign apb_psel = (state_c == SETUP) || (state_c == ACCESS);
assign PSELM1 = PSEL && (PADDR >= 32'h10000000 && PADDR <= 32'h1000FFFF);
assign PSELM2 = PSEL && (PADDR >= 32'h10010000 && PADDR <= 32'h1001FFFF);
assign PSELM3 = PSEL && (PADDR >= 32'h10020000 && PADDR <= 32'h1002FFFF);
assign PSELM4 = PSEL && (PADDR >= 32'h10030000 && PADDR <= 32'h1003FFFF);
assign PSELM5 = PSEL && (PADDR >= 32'h10040000 && PADDR <= 32'h1004FFFF);

//assign PENABLE = (state_c == ACCESS);
//assign PWRITE = HWRITE;
//assign PWDATA = HWDATA;
//assign pstrb = hwstrb;
/*
always_ff@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn)
        HRDATA <= 'd0;
    else if(state_c == ACCESS)
        HRDATA <= PRDATA[PSEL];
end
*/

assign HRDATA = prdata_mux;
/*
always_ff@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn)
        HREADYOUT <= 1'b1;
    else if(state_c == ACCESS)
        HREADYOUT <= PREADY[PSEL];
    else 
        HREADYOUT <= 1'b1;
end
*/
//assign HREADYOUT = PCLKen & prdata_mux & (state_n == SETUP);
assign HREADYOUT =  (state_c == IDLE || state_c == WAIT)?   1'b1 :
                    (state_c == SETUP)?  1'b0 : (PCLKen & pready_mux);

assign HRESP   = 1'b0;
//assign hexokay_o = 1'b1;

assign PADDRM1     = PADDR;
assign PADDRM2     = PADDR;
assign PADDRM3     = PADDR;
assign PADDRM4     = PADDR;
assign PADDRM5     = PADDR;
assign PENABLEM1   = PENABLE;
assign PENABLEM2   = PENABLE;
assign PENABLEM3   = PENABLE;
assign PENABLEM4   = PENABLE;
assign PENABLEM5   = PENABLE;
assign PWRITEM1    = PWRITE;
assign PWRITEM2    = PWRITE;
assign PWRITEM3    = PWRITE;
assign PWRITEM4    = PWRITE;
assign PWRITEM5    = PWRITE;
assign PWDATAM1    = PWDATA;
assign PWDATAM2    = PWDATA;
assign PWDATAM3    = PWDATA;
assign PWDATAM4    = PWDATA;
assign PWDATAM5    = PWDATA;


endmodule
module timer
#(
    parameter ADDR_WIDTH    = 32   ,
    parameter DATA_WIDTH    = 32   
)
(
    input                    clk               ,
    input                    rstn              ,
    output logic             slave_rdy         ,
    output logic             timer_int         ,
    input [ADDR_WIDTH-1:0]   raddr             ,
    input                    re                ,
    output logic  [DATA_WIDTH-1:0]   rdata             ,
    input [DATA_WIDTH/8-1:0] rsel              ,
    input [ADDR_WIDTH-1:0]   waddr             ,
    input                    we                ,
    input [DATA_WIDTH-1:0]   wdata             ,
    input [DATA_WIDTH/8-1:0] wsel   
);

logic    [2 :0]  timer_1_control_reg;   
logic    [31:0]  timer_1_load_count;    
logic            timer_1_raw_int_status; 
logic    [2 :0]  timer_2_control_reg;   
logic    [31:0]  timer_2_load_count;    
logic            timer_2_raw_int_status; 
logic    [2 :0]  timer_3_control_reg;   
logic    [31:0]  timer_3_load_count;    
logic            timer_3_raw_int_status; 
logic    [2 :0]  timer_4_control_reg;   
logic    [31:0]  timer_4_load_count;    
logic            timer_4_raw_int_status; 


logic    [31:0]  timer_1_current_value; 
logic            timer_1_int_clear;     
logic            timer_1_int_status;    
logic            timer_1_interrupt;     
logic    [31:0]  timer_2_current_value; 
logic            timer_2_int_clear;     
logic            timer_2_int_status;    
logic            timer_2_interrupt;     
logic    [31:0]  timer_3_current_value; 
logic            timer_3_int_clear;     
logic            timer_3_int_status;    
logic            timer_3_interrupt;     
logic    [31:0]  timer_4_current_value; 
logic            timer_4_int_clear;     
logic            timer_4_int_status;    
logic            timer_4_interrupt;     
logic    [3 :0]  timer_int;             
logic            timers_int_clear;  

logic first_trans;
logic re_d1;
logic we_d1;
logic re_pos;
logic we_pos;

counter  timer_1 (
  .interrupt              (timer_1_interrupt     ),
  .clk                    (clk                   ),
  .rstn                   (rstn                  ),
  .timer_current_value    (timer_1_current_value ),
  .timer_enable           (timer_1_control_reg[0]),
  .timer_load_count       (timer_1_load_count    ),
  .timer_mode             (timer_1_control_reg[1])
);

counter  timer_2 (
  .interrupt              (timer_2_interrupt     ),
  .clk                    (clk                   ),
  .rstn                   (rstn                  ),
  .timer_current_value    (timer_2_current_value ),
  .timer_enable           (timer_2_control_reg[0]),
  .timer_load_count       (timer_2_load_count    ),
  .timer_mode             (timer_2_control_reg[1])
);

counter  timer_3 (
  .interrupt              (timer_3_interrupt     ),
  .clk                    (clk                   ),
  .rstn                   (rstn                  ),
  .timer_current_value    (timer_3_current_value ),
  .timer_enable           (timer_3_control_reg[0]),
  .timer_load_count       (timer_3_load_count    ),
  .timer_mode             (timer_3_control_reg[1])
);

counter  timer_4 (
  .interrupt              (timer_4_interrupt     ),
  .clk                    (clk                   ),
  .rstn                   (rstn                  ),
  .timer_current_value    (timer_4_current_value ),
  .timer_enable           (timer_4_control_reg[0]),
  .timer_load_count       (timer_4_load_count    ),
  .timer_mode             (timer_4_control_reg[1])
);


always @(posedge clk or negedge rstn)
begin
  if(!rstn)
  begin
    timer_1_load_count <= 32'b0;
    timer_1_control_reg <= 3'b0;
    timer_2_load_count <= 32'b0;
    timer_2_control_reg <= 3'b0;
    timer_3_load_count <= 32'b0;
    timer_3_control_reg <= 3'b0;
    timer_4_load_count <= 32'b0;
    timer_4_control_reg <= 3'b0;
  end
  else
  begin // todo with wsel
    if(we)
    begin
      case(waddr[7:2])
        6'b000000:
        begin
          timer_1_load_count <= wdata;
        end
        6'b000010:
        begin
          timer_1_control_reg <= wdata[2:0];
        end
        6'b000101:
        begin
          timer_2_load_count <= wdata;
        end
        6'b000111:
        begin
          timer_2_control_reg <= wdata[2:0];
        end
        6'b001010:
        begin
          timer_3_load_count <= wdata;
        end
        6'b001100:
        begin
          timer_3_control_reg <= wdata[2:0];
        end
        6'b001111:
        begin
          timer_4_load_count <= wdata;
        end
        6'b010001:
        begin
          timer_4_control_reg <= wdata[2:0];
        end
      endcase
    end
  end
end
    

always @(posedge clk)
begin
  if(re)  begin
    case(raddr[7:2])
    6'b000000:
    begin
      rdata <= timer_1_load_count;
    end
    6'b000001:
    begin
      rdata <= timer_1_current_value;
    end
    6'b000010:
    begin
      rdata <= {29'b0, timer_1_control_reg};
    end
    6'b000011:
    begin
      rdata <= 32'b0;
    end
    6'b000100:
    begin
      rdata <= {31'b0, timer_1_int_status};
    end
    6'b000101:
    begin
      rdata <= timer_2_load_count;
    end
    6'b000110:
    begin
      rdata <= timer_2_current_value;
    end
    6'b000111:
    begin
      rdata <= {29'b0, timer_2_control_reg};
    end
    6'b001000:
    begin
      rdata <= 32'b0;
    end
    6'b001001:
    begin
      rdata <= {31'b0, timer_2_int_status};
    end
    6'b001010:
    begin
      rdata <= timer_3_load_count;
    end
    6'b001011:
    begin
      rdata <= timer_3_current_value;
    end
    6'b001100:
    begin
      rdata <= {29'b0, timer_3_control_reg};
    end
    6'b001101:
    begin
      rdata <= 32'b0;
    end
    6'b001110:
    begin
      rdata <= {31'b0, timer_3_int_status};
    end
    6'b001111:
    begin
      rdata <= timer_4_load_count;
    end
    6'b010000:
    begin
      rdata <= timer_4_current_value;
    end
    6'b010001:
    begin
      rdata <= {29'b0, timer_4_control_reg};
    end
    6'b010010:
    begin
      rdata <= 32'b0;
    end
    6'b010011:
    begin
      rdata <= {31'b0, timer_4_int_status};
    end
    6'b101000:
    begin
      rdata <= {28'b0, timer_int};
    end
    6'b101001:
    begin
      rdata <= 32'b0;
    end
    6'b101010:
    begin
      rdata <= {28'b0, timer_4_raw_int_status, timer_3_raw_int_status, timer_2_raw_int_status, timer_1_raw_int_status};
    end
    default:
    begin
      rdata <= 32'bx;
    end
    endcase
  end
  else
	begin
	  rdata <= 32'bx;
	end
end

assign timer_int = {timer_4_int_status, timer_3_int_status, timer_2_int_status, timer_1_int_status};

assign first_trans = re_pos || we_pos;
assign re_pos = re && ~re_d1;
assign we_pos = we && ~we_d1;

always @(posedge clk or negedge rstn) begin
    if(!rstn)
        re_d1 <= 1'b0;
    else
        re_d1 <= re;
end

always @(posedge clk or negedge rstn) begin
    if(!rstn)
        we_d1 <= 1'b0;
    else
        we_d1 <= we;
end

assign slave_rdy = ~first_trans;
//always @(posedge clk or negedge rstn) begin
//    if(!rstn)
//        slave_rdy <= 1'b1;
//    else if(first_trans)
//        slave_rdy <= 1'b0;
//    else
//        slave_rdy <= 1'b1;
//end


assign timer_1_int_clear = (raddr[7:2] == 6'b000011) && re;
assign timer_2_int_clear = (raddr[7:2] == 6'b001000) && re;
assign timer_3_int_clear = (raddr[7:2] == 6'b001101) && re;
assign timer_4_int_clear = (raddr[7:2] == 6'b010010) && re;
assign timers_int_clear =  (raddr[7:2] == 6'b101001) && re;




assign timer_1_int_status = timer_1_raw_int_status && !timer_1_control_reg[2];
always @(posedge clk or negedge rstn)
begin
  if(!rstn)
  begin
    timer_1_raw_int_status <= 1'b0;
  end
  else
  begin
    if(timer_1_control_reg[0])
    begin
      if(timer_1_interrupt)
      begin
        timer_1_raw_int_status <= 1'b1;
      end
      else
      begin
        if(timer_1_int_clear || timers_int_clear)
        begin
          timer_1_raw_int_status <= 1'b0;
        end
      end
    end
    else
    begin
      timer_1_raw_int_status <= 1'b0;
    end
  end
end




assign timer_2_int_status = timer_2_raw_int_status && !timer_2_control_reg[2];
always @(posedge clk or negedge rstn)
begin
  if(!rstn)
  begin
    timer_2_raw_int_status <= 1'b0;
  end
  else
  begin
    if(timer_2_control_reg[0])
    begin
      if(timer_2_interrupt)
      begin
        timer_2_raw_int_status <= 1'b1;
      end
      else
      begin
        if(timer_2_int_clear || timers_int_clear)
        begin
          timer_2_raw_int_status <= 1'b0;
        end
      end
    end
    else
    begin
      timer_2_raw_int_status <= 1'b0;
    end
  end
end




assign timer_3_int_status = timer_3_raw_int_status && !timer_3_control_reg[2];
always @(posedge clk or negedge rstn)
begin
  if(!rstn)
  begin
    timer_3_raw_int_status <= 1'b0;
  end
  else
  begin
    if(timer_3_control_reg[0])
    begin
      if(timer_3_interrupt)
      begin
        timer_3_raw_int_status <= 1'b1;
      end
      else
      begin
        if(timer_3_int_clear || timers_int_clear)
        begin
          timer_3_raw_int_status <= 1'b0;
        end
      end
    end
    else
    begin
      timer_3_raw_int_status <= 1'b0;
    end
  end
end




assign timer_4_int_status = timer_4_raw_int_status && !timer_4_control_reg[2];
always @(posedge clk or negedge rstn)
begin
  if(!rstn)
  begin
    timer_4_raw_int_status <= 1'b0;
  end
  else
  begin
    if(timer_4_control_reg[0])
    begin
      if(timer_4_interrupt)
      begin
        timer_4_raw_int_status <= 1'b1;
      end
      else
      begin
        if(timer_4_int_clear || timers_int_clear)
        begin
          timer_4_raw_int_status <= 1'b0;
        end
      end
    end
    else
    begin
      timer_4_raw_int_status <= 1'b0;
    end
  end
end


endmodule
module counter(
  input                 clk                 ,
  input                 rstn                ,
  input                 timer_enable        ,
  input                 timer_mode          ,
  input        [31:0]   timer_load_count    ,
  output logic [31:0]   timer_current_value ,
  output logic          interrupt           
);


logic     [31:0]  counter;            
logic             timer_enable_flop;  

logic            load_cnt_en;        


always @(posedge clk or negedge rstn)
begin
  if(!rstn)
  begin
      timer_enable_flop <=0;
  end
  else
  begin
      timer_enable_flop <=timer_enable ;
  end
end

assign load_cnt_en= (timer_enable && !timer_enable_flop)
                    || !(|counter[31:0]);

always @(posedge clk or negedge rstn)
begin
  if(!rstn)
  begin
      counter[31:0] <=32'hffffffff;
  end
  else if(load_cnt_en)
  begin
    if(timer_mode)
    begin
      counter[31:0]<=timer_load_count[31:0];
    end
    else 
    begin
      counter[31:0]<=32'hffffffff;
    end
  end
  else if (timer_enable)
  begin
    counter[31:0]<=counter[31:0]-1'b1;
  end
  else
  begin
    counter[31:0]<=counter[31:0];
  end
end

assign timer_current_value[31:0]=counter[31:0];


always @(posedge clk or negedge rstn)
begin
  if(!rstn)
  begin
    interrupt <=1'b0;
  end
  else if(!{|counter[31:0]})
  begin
    interrupt <=1'b1;
  end
  else
  begin
    interrupt<=1'b0 ;
  end
end


endmodule
module apb_to_slave
#(
    parameter ADDR_WIDTH    = 32   ,   // 10~64
    parameter DATA_WIDTH    = 32     // 8,16,32,64,128,256,512,1024
)
(
    // ------ From system ------ //
    input                           PCLK        ,
    input                           PRESETn     ,
    // ------ From slave --------//
    input                           slave_rdy   ,
    input [DATA_WIDTH-1:0]          rdata       ,
    // ------ To SRAM ----------//
    output logic [ADDR_WIDTH-1:0]   raddr       ,  
    output logic                    re          ,
    output logic[DATA_WIDTH/8-1:0]  rsel        ,  
    output logic[ADDR_WIDTH-1:0]    waddr       ,
    output logic                    we          ,  
    output logic[DATA_WIDTH-1:0]    wdata       ,  
    output logic[DATA_WIDTH/8-1:0]  wsel        ,   
    // ------ From master ------ //
    input [ADDR_WIDTH-1:0]          PADDR       ,
    input                           PENABLE     ,
    input [DATA_WIDTH-1:0]          PWDATA      ,
    input                           PWRITE      ,
    // ------ From interconnect ------ //
    input                           PSEL        ,
    // ------ To interconnect ------ //
    output logic [DATA_WIDTH-1:0]   PRDATA      ,
    output logic                    PREADY      ,
    output logic                    PSLVERR       
);




logic transfer_on;
//assign transfer_on = HSEL && HREADY && HTRANS[1];   // HTRANS = NONSEQ/SEQ
assign transfer_on = PENABLE;   // HTRANS = NONSEQ/SEQ

typedef enum logic [1:0] {IDLE,SLV_W,SLV_R} state_t;
state_t state_c,state_n;
always_ff@(posedge PCLK or negedge PRESETn) begin
    if(~PRESETn)
        state_c <= IDLE;
    else
        state_c <= state_n;
end
always@(*) begin
    if(~PRESETn)
        state_n = IDLE;
    else begin
        state_n = IDLE;
        case(state_c) 
            IDLE: begin
                state_n = (transfer_on && PWRITE)?  SLV_W :
                          (transfer_on && ~PWRITE)? SLV_R :
                          IDLE;
            end
            SLV_W: begin
                state_n = transfer_on?    (PWRITE?    SLV_W : SLV_R) : IDLE;
            end
            SLV_R: begin
                state_n = transfer_on?    (PWRITE?    SLV_W : SLV_R) : IDLE;
            end
            default:    state_n = IDLE;
        endcase
    end
end

assign we = (state_n == SLV_W);
assign re = (state_n == SLV_R);
assign raddr = (state_n == SLV_R)?  PADDR : 'd0;
assign rsel = (state_n == SLV_R)?   4'b1111 : 'd0;
assign waddr = {PADDR[ADDR_WIDTH-1:2],2'b0};
assign wdata = PWDATA;

always@(*) begin
    wsel <= 4'b1111;
end

assign PRDATA = rdata;

assign PSLVERR = 1'b0;

/*
always_ff@(posedge PCLK or negedge PRESETn) begin
    if(~PRESETn)
        PREADY <= 1'b0;
    else if(state_c == IDLE)
        PREADY <= 1'b0;
    else 
        PREADY <= slave_rdy;
end
*/
assign PREADY = (state_c == IDLE)?  1'b0 : slave_rdy;

//assign HREADYOUT = slave_rdy;

endmodule
