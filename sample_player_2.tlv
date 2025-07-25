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

   macro(team_bogus2_module, ['
      module team_bogus2 (
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
      for (genvar ship = 0; ship < 3; ship++) begin
         always @(posedge clk) begin
            x_a[ship] = $random;   // Default acceleration to zero.
            y_a[ship] = $random;   // Default acceleration to zero.
            attempt_fire[ship] = $random;   // Default to not firing.
            fire_dir[ship] = $random;   // Default fire direction to right.
            attempt_cloak[ship] = $random;   // Default to not cloaking.
            attempt_shield[ship] = $random;   // Default to not shielding.
         end
      end

      endmodule
   '])

\SV
   // Include the showdown framework.
   m4_include_lib(https://raw.githubusercontent.com/rweda/showdown-2025-space-battle/a211a27da91c5dda590feac280f067096c96e721/showdown_lib.tlv)


// [Optional]
// Visualization of your logic for each ship.
\TLV team_bogus2_viz(/_top, _team_num)
   m5+io_viz(/_top, _team_num)   /// Visualization of your IOs.
   \viz_js
      m5_DefaultTeamVizBoxAndWhere()
      // Add your own visualization of your own logic here, if you like, within the bounds {left: 0..100, top: 0..100}.
      render() {
         // ... draw using fabric.js and signal values. (See VIZ docs under "LEARN" menu.)
         // For example...
         const destroyed = (this.sigVal("team_bogus2.destroyed").asInt() >> this.getIndex("ship")) & 1;
         return [
            new fabric.Text(destroyed ? "I''m dead! ☹️" : "I''m alive! 😊", {
               left: 10, top: 50, originY: "center", fill: "black", fontSize: 10,
            })
         ];
      },


\TLV team_bogus2(/_top)
   m5+verilog_wrapper(/_top, bogus2)



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
   m5_team(bogus2, Blind Squirrel)
   
   // Choose your opponent.
   // Note that inactive teams must be commented with "///", not "//", to prevent M5 macro evaluation.
   ///m5_team(random, Random)
   ///m5_team(sitting_duck, Sitting Duck)
   m5_team(demo1, Test 1)
   
   
   // Instantiate the Showdown environment.
   m5+showdown(/top, /secret)
   
   *passed = /secret$passed || *cyc_cnt > 100;   // Defines max cycles, up to ~600.
   *failed = /secret$failed;
\SV
   endmodule
   // Declare Verilog modules.
   m4_ifdef(['m5']_team_\m5_get_ago(github_id, 0)_module, ['m5_call(team_\m5_get_ago(github_id, 0)_module)'])
   m4_ifdef(['m5']_team_\m5_get_ago(github_id, 1)_module, ['m5_call(team_\m5_get_ago(github_id, 1)_module)'])
