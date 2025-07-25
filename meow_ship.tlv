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


   macro(team_bogus1_module, ['
      
      // Parameters defining the valid ranges of input/output values can be found near the top of "showdown_lib.tlv".

         localparam signed [7:0] BORDER = 32;
         localparam signed [7:0] MARGIN = 2;
         localparam [15:0] FIRE_RANGE_SQ = 2500;

         genvar i;
         generate
         for (i = 0; i < 3; i++) begin : ship_logic
            // dx and dy
            wire signed [7:0] dx0 = enemy_x_p[0] - x[i];
            wire signed [7:0] dy0 = enemy_y_p[0] - y[i];
            wire signed [7:0] dx1 = enemy_x_p[1] - x[i];
            wire signed [7:0] dy1 = enemy_y_p[1] - y[i];
            wire signed [7:0] dx2 = enemy_x_p[2] - x[i];
            wire signed [7:0] dy2 = enemy_y_p[2] - y[i];

            // Unsigned squared distance
            wire [15:0] dist_sq0 = dx0 * dx0 + dy0 * dy0;
            wire [15:0] dist_sq1 = dx1 * dx1 + dy1 * dy1;
            wire [15:0] dist_sq2 = dx2 * dx2 + dy2 * dy2;

            // Valid targets
            wire valid0 = !enemy_destroyed[0] && !enemy_cloaked[0];
            wire valid1 = !enemy_destroyed[1] && !enemy_cloaked[1];
            wire valid2 = !enemy_destroyed[2] && !enemy_cloaked[2];

            // Manhattan distances
            wire [7:0] abs_dx0 = dx0[7] ? -dx0 : dx0;
            wire [7:0] abs_dy0 = dy0[7] ? -dy0 : dy0;
            wire [7:0] abs_dx1 = dx1[7] ? -dx1 : dx1;
            wire [7:0] abs_dy1 = dy1[7] ? -dy1 : dy1;
            wire [7:0] abs_dx2 = dx2[7] ? -dx2 : dx2;
            wire [7:0] abs_dy2 = dy2[7] ? -dy2 : dy2;

            wire [8:0] sum0 = abs_dx0 + abs_dy0;
            wire [8:0] sum1 = abs_dx1 + abs_dy1;
            wire [8:0] sum2 = abs_dx2 + abs_dy2;

            // Direction selection (flipped 180°)
            function [1:0] select_dir;
               input signed [7:0] dx;
               input signed [7:0] dy;
               reg [7:0] abs_dx, abs_dy;
               begin
               abs_dx = dx[7] ? -dx : dx;
               abs_dy = dy[7] ? -dy : dy;

               if (abs_dx >= abs_dy) begin
                  select_dir = (dx > 0) ? 2'd0 : 2'd2;  // Left : Right (flipped)
               end else begin
                  select_dir = (dy > 0) ? 2'd3 : 2'd1;  // Down : Up (flipped)
               end
               end
            endfunction


            wire [1:0] best_dir =
            (valid0 && (sum0 <= sum1) && (sum0 <= sum2)) ? select_dir(dx0, dy0) :
            (valid1 && (sum1 <= sum2))                   ? select_dir(dx1, dy1) :
            (valid2)                                     ? select_dir(dx2, dy2) : 2'd0;

            assign fire_dir[i] = best_dir;
            assign attempt_fire[i] =
            ((valid0 && (dist_sq0 <= FIRE_RANGE_SQ)) || (valid1 && (dist_sq1 <= FIRE_RANGE_SQ)) || (valid2 && (dist_sq2 <= FIRE_RANGE_SQ))) && (energy[i] > 30);
            assign attempt_shield[i] = (i!==2) ? (((valid0 && (dist_sq0 <= FIRE_RANGE_SQ)) || (valid1 && (dist_sq1 <= FIRE_RANGE_SQ)) || (valid2 && (dist_sq2 <= FIRE_RANGE_SQ))) && (energy[i] > 25)): 1;

            // Move toward nearest valid enemy logic
            // Select nearest valid enemy
            wire [15:0] best_dist_sq = (valid0 && (!valid1 || dist_sq0 <= dist_sq1) && (!valid2 || dist_sq0 <= dist_sq2)) ? dist_sq0 : (valid1 && (!valid2 || dist_sq1 <= dist_sq2)) ? dist_sq1 : (valid2) ? dist_sq2 : 16'hFFFF;

            wire signed [7:0] mv_dx =
            (valid0 && (dist_sq0 == best_dist_sq)) ? dx0 :
            (valid1 && (dist_sq1 == best_dist_sq)) ? dx1 :
            (valid2 && (dist_sq2 == best_dist_sq)) ? dx2 : 8'd0;

            wire signed [7:0] mv_dy =
            (valid0 && (dist_sq0 == best_dist_sq)) ? dy0 :
            (valid1 && (dist_sq1 == best_dist_sq)) ? dy1 :
            (valid2 && (dist_sq2 == best_dist_sq)) ? dy2 : 8'd0;

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
\TLV team_bogus1_viz(/_top, _team_num)
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


\TLV team_bogus1(/_top)
   m5+verilog_wrapper(/_top, bogus1)



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
   m5_team(bogus1, MEOW)
   
   // Choose your opponent.
   // Note that inactive teams must be commented with "///", not "//", to prevent M5 macro evaluation.
   ///m5_team(random, Random)
   ///m5_team(sitting_duck, Sitting Duck)
   ///m5_team(demo1, Test 1)
   
   
   // Instantiate the Showdown environment.
   m5+showdown(/top, /secret)
   
   *passed = /secret$passed || *cyc_cnt > 600;   // Defines max cycles, up to ~600.
   *failed = /secret$failed;
\SV
   endmodule
   // Declare Verilog modules.
   m4_ifdef(['m5']_team_\m5_get_ago(github_id, 0)_module, ['m5_call(team_\m5_get_ago(github_id, 0)_module)'])
   m4_ifdef(['m5']_team_\m5_get_ago(github_id, 1)_module, ['m5_call(team_\m5_get_ago(github_id, 1)_module)'])
