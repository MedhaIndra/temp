\m5_TLV_version 1d: tl-x.org
\m5
   / A competition template for:
   /
   / /----------------------------------------------------------------------------\
   / | The First Annual Makerchip ASIC Design Showdown, Summer 2025, Space Battle |
   / \----------------------------------------------------------------------------/
   /
   / Each player or team modifies this template to provide their own custom spacecraft
   / control circuitry. This template is for teams using Verilog. A TL-Verilog-based
   / template is provided separately. Monitor the Showdown Slack channel for updates.
   / Use the latest template for submission.
   /
   / Just 3 steps:
   /   - Replace all YOUR_GITHUB_ID and YOUR_TEAM_NAME.
   /   - Code your logic in the module below.
   /   - Submit by Sun. July 26, 11 PM IST/1:30 PM EDT.
   /
   / Showdown details: https://www.redwoodeda.com/showdown-info and in the reposotory README.
   
   use(m5-1.0)
   
   var(viz_mode, devel)  /// Enables VIZ for development.
                         /// Use "devel" or "demo". ("demo" will be used in competition.)


   macro(team_YOUR_GITHUB_ID_module, ['
      module team_YOUR_GITHUB_ID (
         // Inputs:
         input logic clk, input logic reset,
         input logic signed [7:0] x [m5_SHIP_RANGE], input logic signed [7:0] y [m5_SHIP_RANGE],   // Positions of your ships, as affected by last cycle's acceleration.
         input logic [7:0] energy [m5_SHIP_RANGE],   // The energy supply of each ship, as affected by last cycle's actions.
         input logic [m5_SHIP_RANGE] destroyed,   // Asserted if and when the ships are destroyed.
         input logic signed [7:0] enemy_x_p [m5_SHIP_RANGE], input logic signed [7:0] enemy_y_p [m5_SHIP_RANGE],   // Positions of enemy ships as affected by their acceleration last cycle.
         input logic [m5_SHIP_RANGE] enemy_cloaked,   // Whether the enemy ships are cloaked, in which case their enemy_x_p and enemy_y_p will not update.
         input logic [m5_SHIP_RANGE] enemy_destroyed, // Whether the enemy ships have been destroyed.
         // Outputs:
         output logic signed [3:0] x_a [m5_SHIP_RANGE], output logic signed [3:0] y_a [m5_SHIP_RANGE],  // Attempted acceleration for each of your ships; capped by max_acceleration (see showdown_lib.tlv).
         output logic [m5_SHIP_RANGE] attempt_fire, output logic [m5_SHIP_RANGE] attempt_shield, output logic [m5_SHIP_RANGE] attempt_cloak,  // Attempted actions for each of your ships.
         output logic [1:0] fire_dir [m5_SHIP_RANGE]   // Direction to fire (if firing). (For the first player: 0 = right, 1 = down, 2 = left, 3 = up)
      );

      // Parameters defining the valid ranges of input/output values can be found near the top of "showdown_lib.tlv".

      // /------------------------------\
      // | Your Verilog logic goes here |
      // \------------------------------/

      // E.g.:
      localparam signed [7:0] BORDER = 32;
localparam signed [7:0] MARGIN = 2;
localparam [15:0] FIRE_RANGE_SQ = 2500;
localparam FIRE_COST   = 30;
localparam SHIELD_COST = 25;
localparam CLOAK_COST  = 15;

// ====== State to track previous enemy positions ======
logic signed [7:0] prev_ex [m5_SHIP_RANGE], prev_ey [m5_SHIP_RANGE];
logic [1:0] prev_vx_sign [m5_SHIP_RANGE], prev_vy_sign [m5_SHIP_RANGE];
      
function [7:0] abs;
  input signed [7:0] val;
  begin
    abs = (val < 0) ? -val : val;
  end
endfunction


always_ff @(posedge clk) begin
  if (reset) begin
    for (integer j = 0; j < 3; j++) begin
      prev_ex[j]       <= 0;
      prev_ey[j]       <= 0;
      prev_vx_sign[j]  <= 2;
      prev_vy_sign[j]  <= 2;
    end
  end else begin
    for (integer j = 0; j < 3; j++) begin
      logic signed [7:0] vx = enemy_x_p[j] - prev_ex[j];
      logic signed [7:0] vy = enemy_y_p[j] - prev_ey[j];
      prev_vx_sign[j] <= (vx > 0) ? 1 : (vx <= 0) ? 0 : 2;
      prev_vy_sign[j] <= (vy > 0) ? 1 : (vy <= 0) ? 0 : 2;
      prev_ex[j] <= enemy_x_p[j];
      prev_ey[j] <= enemy_y_p[j];
    end
  end
end

// ====== Stateless helpers ======
function logic is_approaching;
  input signed [7:0] dx_now, dy_now, dx_prev, dy_prev;
  begin
    is_approaching =
      ((dx_now*dx_now + dy_now*dy_now) <
       (dx_prev*dx_prev + dy_prev*dy_prev));
  end
endfunction

function logic is_dir_approach;
  input signed [7:0] dx, dy;
  input [1:0] vx_s, vy_s;
  begin
    is_dir_approach =
      ((vx_s == 1 && dx >  0) ||
       (vx_s == 0 && dx <  0) ||
       (vx_s == 2 && dx != 0)) ||
      ((vy_s == 1 && dy >  0) ||
       (vy_s == 0 && dy <  0) ||
       (vy_s == 2 && dy != 0));
  end
endfunction

// ====== Per-ship decision logic ======
genvar i;
generate
  for (i = 0; i < 3; i++) begin : SHIP
    // Calculate deltas: current + previous
    wire signed [7:0] dx0 = enemy_x_p[0] - x[i], dy0 = enemy_y_p[0] - y[i];
    wire signed [7:0] dx1 = enemy_x_p[1] - x[i], dy1 = enemy_y_p[1] - y[i];
    wire signed [7:0] dx2 = enemy_x_p[2] - x[i], dy2 = enemy_y_p[2] - y[i];
    
    wire signed [7:0] dx0p = prev_ex[0] - x[i], dy0p = prev_ey[0] - y[i];
    wire signed [7:0] dx1p = prev_ex[1] - x[i], dy1p = prev_ey[1] - y[i];
    wire signed [7:0] dx2p = prev_ex[2] - x[i], dy2p = prev_ey[2] - y[i];

    wire [15:0] dsq0 = dx0*dx0 + dy0*dy0;
    wire [15:0] dsq1 = dx1*dx1 + dy1*dy1;
    wire [15:0] dsq2 = dx2*dx2 + dy2*dy2;

    wire ok0 = !enemy_destroyed[0] && !enemy_cloaked[0];
    wire ok1 = !enemy_destroyed[1] && !enemy_cloaked[1];
    wire ok2 = !enemy_destroyed[2] && !enemy_cloaked[2];

    // Should we fire on that ship?
    wire fire0 = ok0 && (is_approaching(dx0,dy0,dx0p,dy0p) || is_dir_approach(dx0,dy0, prev_vx_sign[0], prev_vy_sign[0]) || dsq0 <= FIRE_RANGE_SQ);
    wire fire1 = ok1 && (is_approaching(dx1,dy1,dx1p,dy1p) || is_dir_approach(dx1,dy1, prev_vx_sign[1], prev_vy_sign[1]) || dsq1 <= FIRE_RANGE_SQ);
    wire fire2 = ok2 && (is_approaching(dx2,dy2,dx2p,dy2p) || is_dir_approach(dx2,dy2, prev_vx_sign[2], prev_vy_sign[2]) || dsq2 <= FIRE_RANGE_SQ);

    // Choose nearest threat
    wire [15:0] s0 = fire0 ? dsq0 : 16'hFFFF;
    wire [15:0] s1 = fire1 ? dsq1 : 16'hFFFF;
    wire [15:0] s2 = fire2 ? dsq2 : 16'hFFFF;

    wire [1:0] target =
      (s0 <= s1 && s0 <= s2) ? 2'd0 :
      (s1 <= s2)              ? 2'd1 : 2'd2;

    wire signed [7:0] dxf = (target==0 ? dx0 : target==1 ? dx1 : dx2);
    wire signed [7:0] dyf = (target==0 ? dy0 : target==1 ? dy1 : dy2);

    assign attempt_fire[i]   = (s0 < 16'hFFFF || s1 < 16'hFFFF || s2 < 16'hFFFF) && (energy[i] >= FIRE_COST);
    wire signed [7:0] dx_fire = enemy_x_p[target] - x[i];
    wire signed [7:0] dy_fire = enemy_y_p[target] - y[i];
    assign fire_dir[i] = ( (dx_fire > dy_fire) && (dx_fire > -dy_fire) ) ? 2'd0 :
                         ( (dx_fire < dy_fire) && (dx_fire > -dy_fire) ) ? 2'd3 :
                         ( (dx_fire < dy_fire) && (dx_fire < -dy_fire) ) ? 2'd2 :
                                                                          2'd1 ;
    assign attempt_shield[i] = ((is_approaching(dx0,dy0,dx0p,dy0p) || is_approaching(dx1,dy1,dx1p,dy1p) || is_approaching(dx2,dy2,dx2p,dy2p) || (dsq0 <= FIRE_RANGE_SQ) || (dsq1 <= FIRE_RANGE_SQ) || (dsq2 <= FIRE_RANGE_SQ) && energy[i] >= SHIELD_COST));
    //assign attempt_shield[i] = (((dsq0 <= FIRE_RANGE_SQ) || (dsq1 <= FIRE_RANGE_SQ) || (dsq2 <= FIRE_RANGE_SQ))  && (energy[i] >= SHIELD_COST));
     //assign attempt_shield[i] = (((fire0) || (fire1) || (fire2) ) && (energy[i] >= SHIELD_COST ));
     
    // Cloak if enemy very close
    //wire close0 = ok0 && ( (abs(dx0)+abs(dy0)) <= (BORDER / 4) );
    //wire close1 = ok1 && ( (abs(dx1)+abs(dy1)) <= (BORDER / 4) );t
    //wire close2 = ok2 && ( (abs(dx2)+abs(dy2)) <= (BORDER / 4) );
    //assign attempt_cloak[i] = (close0 || close1 || close2) && (energy[i] >= CLOAK_COST);

    // Medha's movement logic
    wire [15:0] best_dist_sq = 
      (ok0 && (!ok1 || dsq0 <= dsq1) && (!ok2 || dsq0 <= dsq2)) ? dsq0 :
      (ok1 && (!ok2 || dsq1 <= dsq2)) ? dsq1 :
      (ok2) ? dsq2 : 16'hFFFF;

    wire signed [7:0] mv_dx =
      (ok0 && (dsq0 == best_dist_sq)) ? dx0 :
      (ok1 && (dsq1 == best_dist_sq)) ? dx1 :
      (ok2 && (dsq2 == best_dist_sq)) ? dx2 : 8'd0;

    wire signed [7:0] mv_dy =
      (ok0 && (dsq0 == best_dist_sq)) ? dy0 :
      (ok1 && (dsq1 == best_dist_sq)) ? dy1 :
      (ok2 && (dsq2 == best_dist_sq)) ? dy2 : 8'd0;

    // Step size logic: move +/-2 or +/-1 depending on magnitude
    wire signed [2:0] step_x = 
      (mv_dx > 2)  ? 2 : (mv_dx < -2) ? -2 : mv_dx[2:0];
    wire signed [2:0] step_y = 
      (mv_dy > 2)  ? 2 : (mv_dy < -2) ? -2 : mv_dy[2:0];

    // Border logic: clamp as before
    assign x_a[i] = (x[i] >= BORDER - MARGIN) ? -2 :
                    (x[i] <= -BORDER + MARGIN) ? 2 :
                    (i==2) ? -step_x :
                    step_x;

    assign y_a[i] = (y[i] >= BORDER - MARGIN) ? -2 :
                    (y[i] <= -BORDER + MARGIN) ? 2 :
                    (i==2) ? -step_y :
                    step_y;
  end
endgenerate

      endmodule
   '])

\SV
   // Include the showdown framework.
   m4_include_lib(https://raw.githubusercontent.com/rweda/showdown-2025-space-battle/a211a27da91c5dda590feac280f067096c96e721/showdown_lib.tlv)


// [Optional]
// Visualization of your logic for each ship.
\TLV team_YOUR_GITHUB_ID_viz(/_top, _team_num)
   m5+io_viz(/_top, _team_num)   /// Visualization of your IOs.
   \viz_js
      m5_DefaultTeamVizBoxAndWhere()
      // Add your own visualization of your own logic here, if you like, within the bounds {left: 0..100, top: 0..100}.
      render() {
         // ... draw using fabric.js and signal values. (See VIZ docs under "LEARN" menu.)
         // For example...
         const destroyed = (this.sigVal("team_YOUR_GITHUB_ID.destroyed").asInt() >> this.getIndex("ship")) & 1;
         return [
            new fabric.Text(destroyed ? "I''m dead! ☹️" : "I''m alive! 😊", {
               left: 10, top: 50, originY: "center", fill: "black", fontSize: 10,
            })
         ];
      },


\TLV team_YOUR_GITHUB_ID(/_top)
   m5+verilog_wrapper(/_top, YOUR_GITHUB_ID)



// Compete!
// This defines the competition to simulate (for development).
// When this file is included as a library (for competition), this code is ignored.
\SV
   m5_makerchip_module
\TLV
   // Enlist teams for battle.
   
   // Your team as the first player. Provide:
   //   - your GitHub ID, (as in your \TLV team_* macro, above)
   //   - your team name--anything you like (that isn't crude or disrespectful)
   m5_team(YOUR_GITHUB_ID, YOUR_TEAM_NAME)
   
   // Choose your opponent.
   // Note that inactive teams must be commented with "///", not "//", to prevent M5 macro evaluation.
   m5_team(random, Random)
   ///m5_team(sitting_duck, Sitting Duck)
   ///m5_team(demo1, Test 1)
   
   
   // Instantiate the Showdown environment.
   m5+showdown(/top, /secret)
   
   *passed = /secret$passed || *cyc_cnt > 100;   // Defines max cycles, up to ~600.
   *failed = /secret$failed;
\SV
   endmodule
   // Declare Verilog modules.
   m4_ifdef(['m5']_team_\m5_get_ago(github_id, 0)_module, ['m5_call(team_\m5_get_ago(github_id, 0)_module)'])
   m4_ifdef(['m5']_team_\m5_get_ago(github_id, 1)_module, ['m5_call(team_\m5_get_ago(github_id, 1)_module)'])
