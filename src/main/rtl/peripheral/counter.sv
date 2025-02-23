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
