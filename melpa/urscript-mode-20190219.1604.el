;;; urscript-mode.el --- major mode for editing URScript. -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Guido Schmidt (git@guidoschmidt.cc)
;; Created: 26 Jun 2015
;; Keywords: languages
;; Package-Version: 20190219.1604
;; Package-Commit: b341f96b129ead8fb74d680cb4f546985bf110a9
;; Homepage: https://github.com/guidoschmidt/urscript-mode
;; Version: 0.0.2
;; Package-Requires: ((emacs "24.4"))

;; MIT License

;; Copyright (c) 2019 Guido Schmidt

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Simple major mode for URScript scripts.
;; For details about the scripting language, check out:
;; - https://s3-eu-west-1.amazonaws.com/ur-support-site/46196/scriptManual.pdf
;; - https://universal-robots.com

;;; Code:
(require 'python)

(defconst urscript--constants
  '("True" "False")
  "URScript constants.")

(defconst urscript--regexp-constants
  (regexp-opt urscript--constants 'symbols))

(defconst urscript--regexp-types
  (concat "-?[0-9]+"              ;; int
          "\\|" "-?[0-9]+.[0-9]+" ;; float
          "\\|" "none")           ;; none
  "URScript builtin types.")

(defconst urscript--keywords
  '("def" "sec" "end"
    "if" "elif" "else"
    "global" "local"
    "while" "break" "continue"
    "thread" "join" "run" "kill" "enter_critical" "exit_critical"
    "halt" "return"
    "or" "and" "not")
  "URScript reserved keywords.")

(defconst urscript--regexp-keywords
  (regexp-opt urscript--keywords 'symbols)
  "Regular expression representing reserved keywords.")

(defconst urscript--regexp-operators
  (concat "\\(" "`[^`]+`"
          "\\|" "\\B\\\\"
          "\\|" "[-+*/\\\\|<>!$%&,]+"
          "\\|" "=="
          "\\)")
  "A regular expression representing operators inside expressions.")

(defconst urscript--builtin-funtions
  '("rpc_factory" "assert" "popup" "sync" "sleep"
    "conveyor_puse_decode" "encode_enable_pulse_decode"
    "encoder_enable_set_tick_count" "encoder_get_tick_count"
    "encoder_set_tick_count" "end_force_mode" "end_freedrive_mode"
    "end_teach_mode" "force_mode" "force_mode_example" "force_mode_set_damping"
    "force_mode_set_gain_scaling" "freedrive_mode" "get_conveyor_tick_count"
    "movec" "movej" "movel" "movep" "position_deviation_warning"
    "reset_revolution_counter" "servoc" "servoj" "set_conveyor_tick_count"
    "set_pos" "set_safety_mode_transition_hardness" "speedj" "speedl"
    "stop_conveyor_tracking" "stopj" "stopl" "teach_mode"
    "track_conveyor_circular" "track_conveyor_linear"
    "force" "get_actual_joint_positions" "get_actual_joint_speeds"
    "get_actual_tcp_pose" "get_actual_tcp_speed"
    "get_actual_tool_flange_pose" "get_controller_temp" "get_forward_kin"
    "get_inverse_kin" "get_joint_temp" "get_joint_torques" "get_steptime"
    "get_target_joint_positions" "get_target_joint_speeds"
    "get_target_payload" "get_target_payload_cog" "get_target_tcp_pose"
    "get_target_tc_speed" "get_tcp_force" "get_tcp_offset"
    "get_tool_accelerometer_reading" "get_tool_current" "is_steady"
    "is_within_safety_limits" "powerdown" "set_gravity" "set_payload"
    "set_payload_cog" "set_payload_mass" "set_tcp" "str_at" "str_cat"
    "str_empty" "str_find" "str_len" "str_sub" "textmsg" "to_num" "to_str"
    "acos" "asin" "atan" "atan2" "binary_list_to_integer"
    "ceil" "cos" "d2r" "floor" "get_list_length" "integer_to_binary_list"
    "interpolate_pose" "length" "log" "norm" "point_dist" "pose_add"
    "pose_dist" "pose_inv" "pose_sub" "pose_trans" "pow"
    "r2d" "random" "rotvec2rpy" "rpy2rotvec" "sin" "sqrt" "tan"
    "wrench_trans" "get_analog_in" "get_analog_out"
    "get_configurable_digital_in" "get_configurable_digital_out"
    "get_digital_in" "get_digital_out" "get_flag" "get_standard_analog_in"
    "get_standard_analog_out" "get_standard_digital_in"
    "get_standard_digital_out" "get_tool_analog_in" "get_tool_digital_in"
    "get_tool_digital_out" "get_tool_digital_output_mode"
    "get_tool_output_mode" "modbus_add_signal" "mobus_delete_signal"
    "modbus_get_signal_status" "modbus_send_custom_command"
    "modbus_set_digital_input_action" "modbus_set_output_register"
    "modbus_set_output_signal" "modbus_set_signal_update_frequency"
    "read_input_boolean_register" "read_input_float_register"
    "read_input_integer_register" "read_output_boolean_register"
    "read_output_float_register" "read_output_integer_register"
    "read_port_bit" "read_port_register" "rtde_set_watchdog"
    "set_analog_intputrange" "set_analog_out" "set_configurable_digital_out"
    "set_digital_out" "set_flag" "set_standard_analog_out"
    "set_standard_digital_out" "set_tool_communication" "set_tool_digital_out"
    "set_tool_digital_output_mode" "set_tool_output_mode" "set_tool_voltage"
    "socket_close" "socket_get_var" "socket_open" "socket_read_ascii_float"
    "socket_read_binary_integer" "socket_read_byte_list" "socket_read_line"
    "socket_read_string" "socket_send_byte" "socket_send_int"
    "socket_send_line" "socket_send_string" "socket_set_var"
    "write_output_boolean_register" "write_output_float_register"
    "write_output_integer_register" "write_port_bit" "write_port_register"
    "zero_ftsensor" "modbus_set_runstate_dependent_choice"
    "set_analog_outputdomain" "set_configurable_digital_input_action"
    "set_gp_boolean_input_action" "set_input_actions_to_default"
    "set_runstate_configurable_digital_output_to_value"
    "set_runstate_gp_boolean_output_to_value"
    "set_runstate_standard_analog_output_to_value"
    "set_runstate_standard_digital_output_to_value"
    "set_runstate_tool_digital_output_to_value"
    "set_standard_analog_input_domain" "set_standard_digital_input_action"
    "set_tool_analog_input_domain" "set_tool_digital_input_action")
  "Builtin functions.")

(defconst urscript--regexp-builtin-functions
  (regexp-opt urscript--builtin-funtions 'symbols))

(defconst urscript--regexp-assignment
  "[A-Za-z]+\\(\_[A-Za-z]+\\)*\\( \\)*="
  "A regular expression representing types.")

(defconst urscript--highlighting
  `((,urscript--regexp-keywords          . font-lock-function-name-face)
    (,urscript--regexp-builtin-functions . font-lock-builtin-face)
    (,urscript--regexp-constants         . font-lock-constant-face)
    (,urscript--regexp-types             . font-lock-constant-face)
    (,urscript--regexp-operators         . font-lock-keyword-face)
    (,urscript--regexp-assignment        . font-lock-variable-name-face)))

(define-derived-mode urscript-mode python-mode "URScript"
  "Major mode for editing URScript source code."
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2)
  (setq-local python-indent-offset 2)
  (setq font-lock-defaults '(urscript--highlighting)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.urscript\\'" . urscript-mode))

;; add the mode to the `features' list
(provide 'urscript-mode)

;;; urscript-mode.el ends here
