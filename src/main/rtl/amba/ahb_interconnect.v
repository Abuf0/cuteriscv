//-----------------------------------------------------------------------------
// The confidential and proprietary information contained in this file may
// only be used by a person authorised under and to the extent permitted
// by a subsisting licensing agreement from ARM Limited.
//
//            (C) COPYRIGHT 2001-2013-2024 ARM Limited.
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

    // Output port MI1 (inputs from slave 1)
    HRDATAM3,
    HREADYOUTM3,
    HRESPM3,

    // Output port MI1 (inputs from slave 1)
    HRDATAM4,
    HREADYOUTM4,
    HRESPM4,

    // Output port MI1 (inputs from slave 1)
    HRDATAM5,
    HREADYOUTM5,
    HRESPM5,

    // Output port MI1 (inputs from slave 1)
    HRDATAM6,
    HREADYOUTM6,
    HRESPM6,

    // Output port MI1 (inputs from slave 1)
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

    // Output port MI1 (outputs to slave 1)
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

    // Output port MI1 (outputs to slave 1)
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

    // Output port MI1 (outputs to slave 1)
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

    // Output port MI1 (outputs to slave 1)
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

    // Output port MI1 (outputs to slave 1)
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

    // each slave first trans pending
    trans_pend,

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
    input         HRESPM1; 

    // Output port MI1 (inputs from slave 1)
    input  [31:0] HRDATAM2;        // Read data bus
    input         HREADYOUTM2;     // HREADY feedback
    input         HRESPM2;

    // Output port MI2 (inputs from slave 2)
    input  [31:0] HRDATAM3;        // Read data bus
    input         HREADYOUTM3;     // HREADY feedback
    input         HRESPM3;

    // Output port MI1 (inputs from slave 1)
    input  [31:0] HRDATAM4;        // Read data bus
    input         HREADYOUTM4;     // HREADY feedback
    input         HRESPM4;

    // Output port MI1 (inputs from slave 1)
    input  [31:0] HRDATAM5;        // Read data bus
    input         HREADYOUTM5;     // HREADY feedback
    input         HRESPM5;

    // Output port MI1 (inputs from slave 1)
    input  [31:0] HRDATAM6;        // Read data bus
    input         HREADYOUTM6;     // HREADY feedback
    input         HRESPM6;

    // Output port MI1 (inputs from slave 1)
    input  [31:0] HRDATAM7;        // Read data bus
    input         HREADYOUTM7;     // HREADY feedback
    input         HRESPM7;


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

    // Output port MI1 (outputs to slave 1)
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

    // Output port MI1 (outputs to slave 1)
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

    // Output port MI1 (outputs to slave 1)
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

    // Output port MI1 (outputs to slave 1)
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

    // Output port MI1 (outputs to slave 1)
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

    output wire   trans_pend;
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

    wire         HSELM3;        
    wire  [31:0] HADDRM3;      
    wire   [1:0] HTRANSM3;     
    wire         HWRITEM3;     
    wire   [2:0] HSIZEM3;      
    wire   [2:0] HBURSTM3;     
    wire   [3:0] HPROTM3;      
    wire  [31:0] HWDATAM3;     
    wire         HMASTLOCKM3;  
    wire         HREADYMUXM3;  
    wire         HSELM4;       
    wire  [31:0] HADDRM4;      
    wire   [1:0] HTRANSM4;     
    wire         HWRITEM4;     
    wire   [2:0] HSIZEM4;      
    wire   [2:0] HBURSTM4;     
    wire   [3:0] HPROTM4;      
    wire  [31:0] HWDATAM4;     
    wire         HMASTLOCKM4;  
    wire         HREADYMUXM4;  
    wire         HSELM5;       
    wire  [31:0] HADDRM5;      
    wire   [1:0] HTRANSM5;     
    wire         HWRITEM5;     
    wire   [2:0] HSIZEM5;      
    wire   [2:0] HBURSTM5;     
    wire   [3:0] HPROTM5;      
    wire  [31:0] HWDATAM5;     
    wire         HMASTLOCKM5;  
    wire         HREADYMUXM5;  
    wire         HSELM6;       
    wire  [31:0] HADDRM6;      
    wire   [1:0] HTRANSM6;     
    wire         HWRITEM6;     
    wire   [2:0] HSIZEM6;      
    wire   [2:0] HBURSTM6;     
    wire   [3:0] HPROTM6;      
    wire  [31:0] HWDATAM6;     
    wire         HMASTLOCKM6;  
    wire         HREADYMUXM6;  
    wire         HSELM7;       
    wire  [31:0] HADDRM7;      
    wire   [1:0] HTRANSM7;     
    wire         HWRITEM7;     
    wire   [2:0] HSIZEM7;      
    wire   [2:0] HBURSTM7;     
    wire   [3:0] HPROTM7;      
    wire  [31:0] HWDATAM7;     
    wire         HMASTLOCKM7;  
    wire         HREADYMUXM7;  
    wire   [31:0] HRDATAM3;       
    wire          HREADYOUTM3;    
    wire          HRESPM3;
    wire   [31:0] HRDATAM4;       
    wire          HREADYOUTM4;    
    wire          HRESPM4;
    wire   [31:0] HRDATAM5;       
    wire          HREADYOUTM5;    
    wire          HRESPM5;
    wire   [31:0] HRDATAM6;       
    wire          HREADYOUTM6;    
    wire          HRESPM6;
    wire   [31:0] HRDATAM7;       
    wire          HREADYOUTM7;    
    wire          HRESPM7;

// -----------------------------------------------------------------------------
// Signal declarations
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
// Beginning of main code
assign HSELM1       = (HADDRS1>=0 && HADDRS1<=32'h0000FFFF);
assign HADDRM1      =  HADDRS1;        
assign HTRANSM1     =  HTRANSS1;    
assign HWRITEM1     =  HWRITES1;     
assign HSIZEM1      =  HSIZES1;     
assign HBURSTM1     =  HBURSTS1;    
assign HPROTM1      =  HPROTS1;     
assign HWDATAM1     =  HWDATAS1;    
assign HMASTLOCKM1  =  HMASTLOCKS1;
assign HREADYMUXM1  =  HREADYOUTM1;

assign HSELM2       = (HADDRS1>=32'h00010000 && HADDRS1<=32'h0001FFFF);
//assign HADDRM2      =  HADDRS1;    
assign HADDRM2      =  HSELM2?   (HADDRS1 - 32'h00010000) : HADDRS1;    // todo
assign HTRANSM2     =  HTRANSS1;    
assign HWRITEM2     =  HWRITES1;     
assign HSIZEM2      =  HSIZES1;     
assign HBURSTM2     =  HBURSTS1;    
assign HPROTM2      =  HPROTS1;     
assign HWDATAM2     =  HWDATAS1;    
assign HMASTLOCKM2  =  HMASTLOCKS1;
assign HREADYMUXM2  =  HREADYOUTM2;

assign HSELM3       = (HADDRS1>=32'h00020000 && HADDRS1<=32'h0002FFFF);
assign HADDRM3      =  HADDRS1;        
assign HTRANSM3     =  HTRANSS1;   
assign HWRITEM3     =  HWRITES1;    
assign HSIZEM3      =  HSIZES1;    
assign HBURSTM3     =  HBURSTS1;   
assign HPROTM3      =  HPROTS1;    
assign HWDATAM3     =  HWDATAS1;   
assign HMASTLOCKM3  =  HMASTLOCKS1;
assign HREADYMUXM3  =  HREADYOUTM3;

assign HSELM4       = (HADDRS1>=32'h00030000 && HADDRS1<=32'h0003FFFF);
assign HADDRM4      =  HADDRS1;        
assign HTRANSM4     =  HTRANSS1;   
assign HWRITEM4     =  HWRITES1;     
assign HSIZEM4      =  HSIZES1;    
assign HBURSTM4     =  HBURSTS1;   
assign HPROTM4      =  HPROTS1;    
assign HWDATAM4     =  HWDATAS1;   
assign HMASTLOCKM4  =  HMASTLOCKS1;
assign HREADYMUXM4  =  HREADYOUTM4;

assign HSELM5       = (HADDRS1>=32'h00040000 && HADDRS1<=32'h0FFFFFFF);
assign HADDRM5      =  HADDRS1;         
assign HTRANSM5     =  HTRANSS1;    
assign HWRITEM5     =  HWRITES1;      
assign HSIZEM5      =  HSIZES1;     
assign HBURSTM5     =  HBURSTS1;    
assign HPROTM5      =  HPROTS1;     
assign HWDATAM5     =  HWDATAS1;    
assign HMASTLOCKM5  =  HMASTLOCKS1;
assign HREADYMUXM5  =  HREADYOUTM5;

assign HSELM6       = (HADDRS1>=32'h10000000 && HADDRS1<=32'h14FFFFFF);
assign HADDRM6      =  HADDRS1;         
assign HTRANSM6     =  HTRANSS1;    
assign HWRITEM6     =  HWRITES1;      
assign HSIZEM6      =  HSIZES1;     
assign HBURSTM6     =  HBURSTS1;    
assign HPROTM6      =  HPROTS1;     
assign HWDATAM6     =  HWDATAS1;    
assign HMASTLOCKM6  =  HMASTLOCKS1;
assign HREADYMUXM6  =  HREADYOUTM6;

assign HSELM7       = (HADDRS1>=32'h15000000 && HADDRS1<=32'h150FFFFF);
assign HADDRM7      =  HADDRS1;         
assign HTRANSM7     =  HTRANSS1;    
assign HWRITEM7     =  HWRITES1;      
assign HSIZEM7      =  HSIZES1;     
assign HBURSTM7     =  HBURSTS1;    
assign HPROTM7      =  HPROTS1;     
assign HWDATAM7     =  HWDATAS1;    
assign HMASTLOCKM7  =  HMASTLOCKS1;
assign HREADYMUXM7  =  HREADYOUTM7;

assign HRDATAS1 = HSELM1?   HRDATAM1 :
                  HSELM2?   HRDATAM2 :
                  HSELM3?   HRDATAM3 :
                  HSELM4?   HRDATAM4 :
                  HSELM5?   HRDATAM5 :
                  HSELM6?   HRDATAM6 :
                  HSELM7?   HRDATAM7 : 'd0;
assign HREADYS1 = HSELM1?   HREADYMUXM1 :
                  HSELM2?   HREADYMUXM2 :
                  HSELM3?   HREADYMUXM3 :
                  HSELM4?   HREADYMUXM4 :
                  HSELM5?   HREADYMUXM5 :
                  HSELM6?   HREADYMUXM6 :
                  HSELM7?   HREADYMUXM7 : 'd0;
assign HRESPS1 =  HSELM1?   HRESPM1 :
                  HSELM2?   HRESPM2 :
                  HSELM3?   HRESPM3 :
                  HSELM4?   HRESPM4 :
                  HSELM5?   HRESPM5 :
                  HSELM6?   HRESPM6 :
                  HSELM7?   HRESPM7 : 'd0;
// ----------------------------------------------------------------------------
reg [6:0] hselmx_d1;
wire [6:0] hselmx;
wire [6:0] hselmx_pos;
assign hselmx = {HSELM7,HSELM6,HSELM5,HSELM4,HSELM3,HSELM2,HSELM1};
always@(posedge HCLK or negedge HRESETn) begin
    if(~HRESETn)
        hselmx_d1 <= 'd0;
    else
        hselmx_d1 <= hselmx;
end
assign hselmx_pos = hselmx & ~hselmx_d1;
assign trans_pend = |hselmx_pos;

endmodule
