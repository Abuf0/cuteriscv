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