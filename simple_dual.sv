`include "define.v"
module simple_dual(input                        clka ,
                   input                        ena  ,
                   input      [`RAM_WEA_WIDTH-1:0]  wea  ,
                   input      [`RAM_ADDR_WIDTH-1:0] addra,
                   input      [`RAM_WIDTH-1:0]      dina ,
                   input                        clkb ,
                   input                        enb  ,              
                   input      [`RAM_ADDR_WIDTH-1:0] addrb,
                   output reg [`RAM_WIDTH-1:0]      doutb);
reg [`RAM_WIDTH-1:0] ram[0:`RAM_DEPTH-1]; 

generate
    genvar i;
    for (i = 0; i < `RAM_WEA_WIDTH; i = i + 1) begin : gen_block
        always @(posedge clka) begin
	  if(ena) begin
            if (wea[`RAM_WEA_WIDTH - i - 1]) begin
                ram[addra][`RAM_WIDTH - 8*(i+1) +: 8] <= dina[`RAM_WIDTH - 8*(i+1) +: 8];
            end
          end
        end
    end
endgenerate

always @(posedge clkb) begin
  if (enb) begin
      doutb <= ram[addrb];
  end
end
endmodule
