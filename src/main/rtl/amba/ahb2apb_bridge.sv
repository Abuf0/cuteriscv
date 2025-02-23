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

typedef enum logic [1:0] {IDLE,SETUP,ACCESS} state_t;
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
        SETUP:  state_n = PCLKen? ACCESS:SETUP;
        //ACCESS: state_n = (HSEL)?   ACCESS:IDLE;
        ACCESS: state_n = (pready_mux && PCLKen)?   (apb_sel?  SETUP : IDLE):ACCESS;
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
        PENABLE <= 1'B0;
        PADDR  <= haddr_lat; 
        PWRITE <= hwrite_lat;
        PWDATA <= hwdata_lat;
    end
    else if(state_c == ACCESS) begin
        PENABLE <= 1'b1;
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
assign HREADYOUT =  (state_c == IDLE)?   1'b1 :
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