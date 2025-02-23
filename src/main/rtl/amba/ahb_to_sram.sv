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
        if(HSIZE==3'b000) begin
            wsel <= {4'b0001 << HADDR[1:0]};
        end
        else if(HSIZE==3'b001) begin
            wsel <= {4'b0011 << HADDR[1:0]};
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