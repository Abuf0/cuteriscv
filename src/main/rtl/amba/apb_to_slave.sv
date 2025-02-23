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


always_ff@(posedge PCLK or negedge PRESETn) begin
    if(~PRESETn)
        PREADY <= 1'b0;
    else if(state_c == IDLE)
        PREADY <= 1'b0;
    else 
        PREADY <= slave_rdy;
end


//assign HREADYOUT = slave_rdy;

endmodule