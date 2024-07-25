[@@@ocamlformat "disable"]
[@@@ocaml.warning "-33"]
module Core_codec =
  struct
    open Protocol.Core
    let decode_char2b _buf = ()
    let decode_point _buf = ()
    let decode_rectangle _buf = ()
    let decode_arc _buf = ()
    let decode_format _buf = ()
    let decode_visualtype _buf = ()
    let decode_depth _buf = ()
    let decode_screen _buf = ()
    let decode_setup_request _buf = ()
    let decode_setup_failed _buf = ()
    let decode_setup_authenticate _buf = ()
    let decode_setup _buf = ()
    let decode_timecoord _buf = ()
    let decode_fontprop _buf = ()
    let decode_charinfo _buf = ()
    let decode_str _buf = ()
    let decode_segment _buf = ()
    let decode_coloritem _buf = ()
    let decode_rgb _buf = ()
    let decode_host _buf = ()
  end
module Bigreq_codec = struct open Protocol.Bigreq end
module Render_codec =
  struct
    open Protocol.Render
    let decode_directformat _buf = ()
    let decode_pictforminfo _buf = ()
    let decode_pictvisual _buf = ()
    let decode_pictdepth _buf = ()
    let decode_pictscreen _buf = ()
    let decode_indexvalue _buf = ()
    let decode_color _buf = ()
    let decode_pointfix _buf = ()
    let decode_linefix _buf = ()
    let decode_triangle _buf = ()
    let decode_trapezoid _buf = ()
    let decode_glyphinfo _buf = ()
    let decode_transform _buf = ()
    let decode_animcursorelt _buf = ()
    let decode_spanfix _buf = ()
    let decode_trap _buf = ()
  end
module Shape_codec = struct open Protocol.Shape end
module Xfixes_codec = struct open Protocol.Xfixes end
module Composite_codec = struct open Protocol.Composite end
module Damage_codec = struct open Protocol.Damage end
module Dpms_codec = struct open Protocol.Dpms end
module Dri2_codec =
  struct
    open Protocol.Dri2
    let decode_dri2_buffer _buf = ()
    let decode_attach_format _buf = ()
  end
module Dri3_codec = struct open Protocol.Dri3 end
module Ge_codec = struct open Protocol.Ge end
module Glx_codec = struct open Protocol.Glx end
module Randr_codec =
  struct
    open Protocol.Randr
    let decode_screen_size _buf = ()
    let decode_refresh_rates _buf = ()
    let decode_mode_info _buf = ()
    let decode_crtc_change _buf = ()
    let decode_output_change _buf = ()
    let decode_output_property _buf = ()
    let decode_provider_change _buf = ()
    let decode_provider_property _buf = ()
    let decode_resource_change _buf = ()
    let decode_monitor_info _buf = ()
    let decode_lease_notify _buf = ()
  end
module Sync_codec =
  struct
    open Protocol.Sync
    let decode_int64 _buf = ()
    let decode_systemcounter _buf = ()
    let decode_trigger _buf = ()
    let decode_waitcondition _buf = ()
  end
module Present_codec =
  struct open Protocol.Present
         let decode_notify _buf = () end
module Record_codec =
  struct
    open Protocol.Record
    let decode_range8 _buf = ()
    let decode_range16 _buf = ()
    let decode_ext_range _buf = ()
    let decode_range _buf = ()
    let decode_client_info _buf = ()
  end
module Res_codec =
  struct
    open Protocol.Res
    let decode_client _buf = ()
    let decode_type _buf = ()
    let decode_client_id_spec _buf = ()
    let decode_client_id_value _buf = ()
    let decode_resource_id_spec _buf = ()
    let decode_resource_size_spec _buf = ()
    let decode_resource_size_value _buf = ()
  end
module Screensaver_codec = struct open Protocol.Screensaver end
module Shm_codec = struct open Protocol.Shm end
module Xc_misc_codec = struct open Protocol.Xc_misc end
module Xevie_codec = struct open Protocol.Xevie
                            let decode_event _buf = () end
module Xf86dri_codec =
  struct open Protocol.Xf86dri
         let decode_drm_clip_rect _buf = () end
module Xf86vidmode_codec =
  struct open Protocol.Xf86vidmode
         let decode_mode_info _buf = () end
module Xinerama_codec =
  struct open Protocol.Xinerama
         let decode_screen_info _buf = () end
module Xinput_codec =
  struct
    open Protocol.Xinput
    let decode_fp3232 _buf = ()
    let decode_device_info _buf = ()
    let decode_key_info _buf = ()
    let decode_button_info _buf = ()
    let decode_axis_info _buf = ()
    let decode_valuator_info _buf = ()
    let decode_input_info _buf = ()
    let decode_device_name _buf = ()
    let decode_input_class_info _buf = ()
    let decode_device_time_coord _buf = ()
    let decode_kbd_feedback_state _buf = ()
    let decode_ptr_feedback_state _buf = ()
    let decode_integer_feedback_state _buf = ()
    let decode_string_feedback_state _buf = ()
    let decode_bell_feedback_state _buf = ()
    let decode_led_feedback_state _buf = ()
    let decode_feedback_state _buf = ()
    let decode_kbd_feedback_ctl _buf = ()
    let decode_ptr_feedback_ctl _buf = ()
    let decode_integer_feedback_ctl _buf = ()
    let decode_string_feedback_ctl _buf = ()
    let decode_bell_feedback_ctl _buf = ()
    let decode_led_feedback_ctl _buf = ()
    let decode_feedback_ctl _buf = ()
    let decode_key_state _buf = ()
    let decode_button_state _buf = ()
    let decode_valuator_state _buf = ()
    let decode_input_state _buf = ()
    let decode_device_resolution_state _buf = ()
    let decode_device_abs_calib_state _buf = ()
    let decode_device_abs_area_state _buf = ()
    let decode_device_core_state _buf = ()
    let decode_device_enable_state _buf = ()
    let decode_device_state _buf = ()
    let decode_device_resolution_ctl _buf = ()
    let decode_device_abs_calib_ctl _buf = ()
    let decode_device_abs_area_ctrl _buf = ()
    let decode_device_core_ctrl _buf = ()
    let decode_device_enable_ctrl _buf = ()
    let decode_device_ctl _buf = ()
    let decode_group_info _buf = ()
    let decode_modifier_info _buf = ()
    let decode_add_master _buf = ()
    let decode_remove_master _buf = ()
    let decode_attach_slave _buf = ()
    let decode_detach_slave _buf = ()
    let decode_hierarchy_change _buf = ()
    let decode_event_mask _buf = ()
    let decode_button_class _buf = ()
    let decode_key_class _buf = ()
    let decode_scroll_class _buf = ()
    let decode_touch_class _buf = ()
    let decode_valuator_class _buf = ()
    let decode_device_class _buf = ()
    let decode_xi_device_info _buf = ()
    let decode_grab_modifier_info _buf = ()
    let decode_barrier_release_pointer_info _buf = ()
    let decode_hierarchy_info _buf = ()
  end
module Xprint_codec =
  struct open Protocol.Xprint
         let decode_printer _buf = () end
module Xselinux_codec =
  struct open Protocol.Xselinux
         let decode_list_item _buf = () end
module Xtest_codec = struct open Protocol.Xtest end
module Xv_codec =
  struct
    open Protocol.Xv
    let decode_rational _buf = ()
    let decode_format _buf = ()
    let decode_adaptor_info _buf = ()
    let decode_encoding_info _buf = ()
    let decode_image _buf = ()
    let decode_attribute_info _buf = ()
    let decode_image_format_info _buf = ()
  end
module Xvmc_codec =
  struct open Protocol.Xvmc
         let decode_surface_info _buf = () end
