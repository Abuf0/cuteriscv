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