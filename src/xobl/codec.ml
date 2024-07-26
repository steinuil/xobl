[@@@ocamlformat "disable"]
[@@@ocaml.warning "-33"]
open Util
open Protocol
module Core_codec =
  struct
    open Protocol.Core
    let decode_char2b buf =
      let byte1 = Decode.u8 buf in let byte2 = Decode.u8 buf in ()
    let decode_point buf =
      let x = Decode.i16 buf in let y = Decode.i16 buf in ()
    let decode_rectangle buf =
      let x = Decode.i16 buf in
      let y = Decode.i16 buf in
      let width = Decode.u16 buf in let height = Decode.u16 buf in ()
    let decode_arc buf =
      let x = Decode.i16 buf in
      let y = Decode.i16 buf in
      let width = Decode.u16 buf in
      let height = Decode.u16 buf in
      let angle1 = Decode.i16 buf in let angle2 = Decode.i16 buf in ()
    let decode_format buf =
      let depth = Decode.u8 buf in
      let bits_per_pixel = Decode.u8 buf in
      let scanline_pad = Decode.u8 buf in Decode.pad buf 5; ()
    let decode_visualtype buf =
      let visual_id = Decode.u32 buf in
      let class_ =
        ((Decode.u8 %> Conv.To_int.u8) %> Visual_class_enum.of_int) buf in
      let bits_per_rgb_value = Decode.u8 buf in
      let colormap_entries = Decode.u16 buf in
      let red_mask = Decode.u32 buf in
      let green_mask = Decode.u32 buf in
      let blue_mask = Decode.u32 buf in Decode.pad buf 4; ()
    let decode_depth buf =
      let depth = Decode.u8 buf in
      Decode.pad buf 1;
      (let visuals_len = Conv.To_int.u16 (Decode.u16 buf) in
       Decode.pad buf 4;
       (let visuals =
          (Decode.list ~item:decode_visualtype) ~len:visuals_len buf in
        ()))
    let decode_screen buf =
      let root = Decode.xid buf in
      let default_colormap = Decode.xid buf in
      let white_pixel = Decode.u32 buf in
      let black_pixel = Decode.u32 buf in
      let current_input_masks =
        ((Decode.u32 %> Conv.To_i32.u32) %> Event_mask.of_int32) buf in
      let width_in_pixels = Decode.u16 buf in
      let height_in_pixels = Decode.u16 buf in
      let width_in_millimeters = Decode.u16 buf in
      let height_in_millimeters = Decode.u16 buf in
      let min_installed_maps = Decode.u16 buf in
      let max_installed_maps = Decode.u16 buf in
      let root_visual = Decode.u32 buf in
      let backing_stores =
        ((Decode.byte %> Conv.To_int.byte) %> Backing_store_enum.of_int) buf in
      let save_unders = Decode.bool buf in
      let root_depth = Decode.u8 buf in
      let allowed_depths_len = Conv.To_int.u8 (Decode.u8 buf) in
      let allowed_depths =
        (Decode.list ~item:decode_depth) ~len:allowed_depths_len buf in
      ()
    let decode_setup_request buf =
      let byte_order = Decode.u8 buf in
      Decode.pad buf 1;
      (let protocol_major_version = Decode.u16 buf in
       let protocol_minor_version = Decode.u16 buf in
       let authorization_protocol_name_len = Conv.To_int.u16 (Decode.u16 buf) in
       let authorization_protocol_data_len = Conv.To_int.u16 (Decode.u16 buf) in
       Decode.pad buf 2;
       (let authorization_protocol_name =
          Decode.string ~len:authorization_protocol_name_len buf in
        Decode.pad buf 4;
        (let authorization_protocol_data =
           Decode.string ~len:authorization_protocol_data_len buf in
         Decode.pad buf 4; ())))
    let decode_setup_failed buf =
      let status = Decode.u8 buf in
      let reason_len = Conv.To_int.u8 (Decode.u8 buf) in
      let protocol_major_version = Decode.u16 buf in
      let protocol_minor_version = Decode.u16 buf in
      let length = Decode.u16 buf in
      let reason = Decode.string ~len:reason_len buf in ()
    let decode_setup_authenticate buf =
      let status = Decode.u8 buf in
      Decode.pad buf 5;
      (let length = Conv.To_int.u16 (Decode.u16 buf) in
       let length = length / 4 in
       let reason = Decode.string ~len:length buf in ())
    let decode_setup buf =
      let status = Decode.u8 buf in
      Decode.pad buf 1;
      (let protocol_major_version = Decode.u16 buf in
       let protocol_minor_version = Decode.u16 buf in
       let length = Decode.u16 buf in
       let release_number = Decode.u32 buf in
       let resource_id_base = Decode.u32 buf in
       let resource_id_mask = Decode.u32 buf in
       let motion_buffer_size = Decode.u32 buf in
       let vendor_len = Conv.To_int.u16 (Decode.u16 buf) in
       let maximum_request_length = Decode.u16 buf in
       let roots_len = Conv.To_int.u8 (Decode.u8 buf) in
       let pixmap_formats_len = Conv.To_int.u8 (Decode.u8 buf) in
       let image_byte_order =
         ((Decode.u8 %> Conv.To_int.u8) %> Image_order_enum.of_int) buf in
       let bitmap_format_bit_order =
         ((Decode.u8 %> Conv.To_int.u8) %> Image_order_enum.of_int) buf in
       let bitmap_format_scanline_unit = Decode.u8 buf in
       let bitmap_format_scanline_pad = Decode.u8 buf in
       let min_keycode = Decode.u8 buf in
       let max_keycode = Decode.u8 buf in
       Decode.pad buf 4;
       (let vendor = Decode.string ~len:vendor_len buf in
        Decode.pad buf 4;
        (let pixmap_formats =
           (Decode.list ~item:decode_format) ~len:pixmap_formats_len buf in
         let roots = (Decode.list ~item:decode_screen) ~len:roots_len buf in
         ())))
    let decode_timecoord buf =
      let time = Decode.u32 buf in
      let x = Decode.i16 buf in let y = Decode.i16 buf in ()
    let decode_fontprop buf =
      let name = Decode.xid buf in let value = Decode.u32 buf in ()
    let decode_charinfo buf =
      let left_side_bearing = Decode.i16 buf in
      let right_side_bearing = Decode.i16 buf in
      let character_width = Decode.i16 buf in
      let ascent = Decode.i16 buf in
      let descent = Decode.i16 buf in let attributes = Decode.u16 buf in ()
    let decode_str buf =
      let name_len = Conv.To_int.u8 (Decode.u8 buf) in
      let name = Decode.string ~len:name_len buf in ()
    let decode_segment buf =
      let x1 = Decode.i16 buf in
      let y1 = Decode.i16 buf in
      let x2 = Decode.i16 buf in let y2 = Decode.i16 buf in ()
    let decode_coloritem buf =
      let pixel = Decode.u32 buf in
      let red = Decode.u16 buf in
      let green = Decode.u16 buf in
      let blue = Decode.u16 buf in
      let flags =
        ((Decode.byte %> Conv.To_i32.byte) %> Color_flag_mask.of_int32) buf in
      Decode.pad buf 1; ()
    let decode_rgb buf =
      let red = Decode.u16 buf in
      let green = Decode.u16 buf in
      let blue = Decode.u16 buf in Decode.pad buf 2; ()
    let decode_host buf =
      let family = ((Decode.u8 %> Conv.To_int.u8) %> Family_enum.of_int) buf in
      Decode.pad buf 1;
      (let address_len = Conv.To_int.u16 (Decode.u16 buf) in
       let address = (Decode.list ~item:Decode.byte) ~len:address_len buf in
       Decode.pad buf 4; ())
  end
module Bigreq_codec = struct open Protocol.Bigreq end
module Render_codec =
  struct
    open Protocol.Render
    let decode_directformat buf =
      let red_shift = Decode.u16 buf in
      let red_mask = Decode.u16 buf in
      let green_shift = Decode.u16 buf in
      let green_mask = Decode.u16 buf in
      let blue_shift = Decode.u16 buf in
      let blue_mask = Decode.u16 buf in
      let alpha_shift = Decode.u16 buf in
      let alpha_mask = Decode.u16 buf in ()
    let decode_pictforminfo buf =
      let id = Decode.xid buf in
      let type_ =
        ((Decode.u8 %> Conv.To_int.u8) %> Pict_type_enum.of_int) buf in
      let depth = Decode.u8 buf in
      Decode.pad buf 2;
      (let direct = decode_directformat buf in
       let colormap = Decode.xid buf in ())
    let decode_pictvisual buf =
      let visual = Decode.u32 buf in let format = Decode.xid buf in ()
    let decode_pictdepth buf =
      let depth = Decode.u8 buf in
      Decode.pad buf 1;
      (let num_visuals = Conv.To_int.u16 (Decode.u16 buf) in
       Decode.pad buf 4;
       (let visuals =
          (Decode.list ~item:decode_pictvisual) ~len:num_visuals buf in
        ()))
    let decode_pictscreen buf =
      let num_depths = Conv.To_int.u32 (Decode.u32 buf) in
      let fallback = Decode.xid buf in
      let depths = (Decode.list ~item:decode_pictdepth) ~len:num_depths buf in
      ()
    let decode_indexvalue buf =
      let pixel = Decode.u32 buf in
      let red = Decode.u16 buf in
      let green = Decode.u16 buf in
      let blue = Decode.u16 buf in let alpha = Decode.u16 buf in ()
    let decode_color buf =
      let red = Decode.u16 buf in
      let green = Decode.u16 buf in
      let blue = Decode.u16 buf in let alpha = Decode.u16 buf in ()
    let decode_pointfix buf =
      let x = Decode.i32 buf in let y = Decode.i32 buf in ()
    let decode_linefix buf =
      let p1 = decode_pointfix buf in let p2 = decode_pointfix buf in ()
    let decode_triangle buf =
      let p1 = decode_pointfix buf in
      let p2 = decode_pointfix buf in let p3 = decode_pointfix buf in ()
    let decode_trapezoid buf =
      let top = Decode.i32 buf in
      let bottom = Decode.i32 buf in
      let left = decode_linefix buf in let right = decode_linefix buf in ()
    let decode_glyphinfo buf =
      let width = Decode.u16 buf in
      let height = Decode.u16 buf in
      let x = Decode.i16 buf in
      let y = Decode.i16 buf in
      let x_off = Decode.i16 buf in let y_off = Decode.i16 buf in ()
    let decode_transform buf =
      let matrix11 = Decode.i32 buf in
      let matrix12 = Decode.i32 buf in
      let matrix13 = Decode.i32 buf in
      let matrix21 = Decode.i32 buf in
      let matrix22 = Decode.i32 buf in
      let matrix23 = Decode.i32 buf in
      let matrix31 = Decode.i32 buf in
      let matrix32 = Decode.i32 buf in let matrix33 = Decode.i32 buf in ()
    let decode_animcursorelt buf =
      let cursor = Decode.xid buf in let delay = Decode.u32 buf in ()
    let decode_spanfix buf =
      let l = Decode.i32 buf in
      let r = Decode.i32 buf in let y = Decode.i32 buf in ()
    let decode_trap buf =
      let top = decode_spanfix buf in let bot = decode_spanfix buf in ()
  end
module Shape_codec = struct open Protocol.Shape end
module Xfixes_codec = struct open Protocol.Xfixes end
module Composite_codec = struct open Protocol.Composite end
module Damage_codec = struct open Protocol.Damage end
module Dpms_codec = struct open Protocol.Dpms end
module Dri2_codec =
  struct
    open Protocol.Dri2
    let decode_dri2_buffer buf =
      let attachment =
        ((Decode.u32 %> Conv.To_int.u32) %> Attachment_enum.of_int) buf in
      let name = Decode.u32 buf in
      let pitch = Decode.u32 buf in
      let cpp = Decode.u32 buf in let flags = Decode.u32 buf in ()
    let decode_attach_format buf =
      let attachment =
        ((Decode.u32 %> Conv.To_int.u32) %> Attachment_enum.of_int) buf in
      let format = Decode.u32 buf in ()
  end
module Dri3_codec = struct open Protocol.Dri3 end
module Ge_codec = struct open Protocol.Ge end
module Glx_codec = struct open Protocol.Glx end
module Randr_codec =
  struct
    open Protocol.Randr
    let decode_screen_size buf =
      let width = Decode.u16 buf in
      let height = Decode.u16 buf in
      let mwidth = Decode.u16 buf in let mheight = Decode.u16 buf in ()
    let decode_refresh_rates buf =
      let n_rates = Conv.To_int.u16 (Decode.u16 buf) in
      let rates = (Decode.list ~item:Decode.u16) ~len:n_rates buf in ()
    let decode_mode_info buf =
      let id = Decode.u32 buf in
      let width = Decode.u16 buf in
      let height = Decode.u16 buf in
      let dot_clock = Decode.u32 buf in
      let hsync_start = Decode.u16 buf in
      let hsync_end = Decode.u16 buf in
      let htotal = Decode.u16 buf in
      let hskew = Decode.u16 buf in
      let vsync_start = Decode.u16 buf in
      let vsync_end = Decode.u16 buf in
      let vtotal = Decode.u16 buf in
      let name_len = Decode.u16 buf in
      let mode_flags =
        ((Decode.u32 %> Conv.To_i32.u32) %> Mode_flag_mask.of_int32) buf in
      ()
    let decode_crtc_change buf =
      let timestamp = Decode.u32 buf in
      let window = Decode.xid buf in
      let crtc = Decode.xid buf in
      let mode = Decode.xid buf in
      let rotation =
        ((Decode.u16 %> Conv.To_i32.u16) %> Rotation_mask.of_int32) buf in
      Decode.pad buf 2;
      (let x = Decode.i16 buf in
       let y = Decode.i16 buf in
       let width = Decode.u16 buf in let height = Decode.u16 buf in ())
    let decode_output_change buf =
      let timestamp = Decode.u32 buf in
      let config_timestamp = Decode.u32 buf in
      let window = Decode.xid buf in
      let output = Decode.xid buf in
      let crtc = Decode.xid buf in
      let mode = Decode.xid buf in
      let rotation =
        ((Decode.u16 %> Conv.To_i32.u16) %> Rotation_mask.of_int32) buf in
      let connection =
        ((Decode.u8 %> Conv.To_int.u8) %> Connection_enum.of_int) buf in
      let subpixel_order =
        ((Decode.u8 %> Conv.To_int.u8) %> Render.Sub_pixel_enum.of_int) buf in
      ()
    let decode_output_property buf =
      let window = Decode.xid buf in
      let output = Decode.xid buf in
      let atom = Decode.xid buf in
      let timestamp = Decode.u32 buf in
      let status =
        ((Decode.u8 %> Conv.To_int.u8) %> Core.Property_enum.of_int) buf in
      Decode.pad buf 11; ()
    let decode_provider_change buf =
      let timestamp = Decode.u32 buf in
      let window = Decode.xid buf in
      let provider = Decode.xid buf in Decode.pad buf 16; ()
    let decode_provider_property buf =
      let window = Decode.xid buf in
      let provider = Decode.xid buf in
      let atom = Decode.xid buf in
      let timestamp = Decode.u32 buf in
      let state = Decode.u8 buf in Decode.pad buf 11; ()
    let decode_resource_change buf =
      let timestamp = Decode.u32 buf in
      let window = Decode.xid buf in Decode.pad buf 20; ()
    let decode_monitor_info buf =
      let name = Decode.xid buf in
      let primary = Decode.bool buf in
      let automatic = Decode.bool buf in
      let n_output = Conv.To_int.u16 (Decode.u16 buf) in
      let x = Decode.i16 buf in
      let y = Decode.i16 buf in
      let width = Decode.u16 buf in
      let height = Decode.u16 buf in
      let width_in_millimeters = Decode.u32 buf in
      let height_in_millimeters = Decode.u32 buf in
      let outputs = (Decode.list ~item:Decode.xid) ~len:n_output buf in ()
    let decode_lease_notify buf =
      let timestamp = Decode.u32 buf in
      let window = Decode.xid buf in
      let lease = Decode.xid buf in
      let created = Decode.u8 buf in Decode.pad buf 15; ()
  end
module Sync_codec =
  struct
    open Protocol.Sync
    let decode_int64 buf =
      let hi = Decode.i32 buf in let lo = Decode.u32 buf in ()
    let decode_systemcounter buf =
      let counter = Decode.xid buf in
      let resolution = decode_int64 buf in
      let name_len = Conv.To_int.u16 (Decode.u16 buf) in
      let name = Decode.string ~len:name_len buf in Decode.pad buf 4; ()
    let decode_trigger buf =
      let counter = Decode.xid buf in
      let wait_type =
        ((Decode.u32 %> Conv.To_int.u32) %> Valuetype_enum.of_int) buf in
      let wait_value = decode_int64 buf in
      let test_type =
        ((Decode.u32 %> Conv.To_int.u32) %> Testtype_enum.of_int) buf in
      ()
    let decode_waitcondition buf =
      let trigger = decode_trigger buf in
      let event_threshold = decode_int64 buf in ()
  end
module Present_codec =
  struct
    open Protocol.Present
    let decode_notify buf =
      let window = Decode.xid buf in let serial = Decode.u32 buf in ()
  end
module Record_codec =
  struct
    open Protocol.Record
    let decode_range8 buf =
      let first = Decode.u8 buf in let last = Decode.u8 buf in ()
    let decode_range16 buf =
      let first = Decode.u16 buf in let last = Decode.u16 buf in ()
    let decode_ext_range buf =
      let major = decode_range8 buf in let minor = decode_range16 buf in ()
    let decode_range buf =
      let core_requests = decode_range8 buf in
      let core_replies = decode_range8 buf in
      let ext_requests = decode_ext_range buf in
      let ext_replies = decode_ext_range buf in
      let delivered_events = decode_range8 buf in
      let device_events = decode_range8 buf in
      let errors = decode_range8 buf in
      let client_started = Decode.bool buf in
      let client_died = Decode.bool buf in ()
    let decode_client_info buf =
      let client_resource = Decode.u32 buf in
      let num_ranges = Conv.To_int.u32 (Decode.u32 buf) in
      let ranges = (Decode.list ~item:decode_range) ~len:num_ranges buf in ()
  end
module Res_codec =
  struct
    open Protocol.Res
    let decode_client buf =
      let resource_base = Decode.u32 buf in
      let resource_mask = Decode.u32 buf in ()
    let decode_type buf =
      let resource_type = Decode.xid buf in let count = Decode.u32 buf in ()
    let decode_client_id_spec buf =
      let client = Decode.u32 buf in
      let mask =
        ((Decode.u32 %> Conv.To_i32.u32) %> Client_id_mask.of_int32) buf in
      ()
    let decode_client_id_value buf =
      let spec = decode_client_id_spec buf in
      let length = Conv.To_int.u32 (Decode.u32 buf) in
      let length = length * 4 in
      let value = (Decode.list ~item:Decode.u32) ~len:length buf in ()
    let decode_resource_id_spec buf =
      let resource = Decode.u32 buf in let type_ = Decode.u32 buf in ()
    let decode_resource_size_spec buf =
      let spec = decode_resource_id_spec buf in
      let bytes = Decode.u32 buf in
      let ref_count = Decode.u32 buf in let use_count = Decode.u32 buf in ()
    let decode_resource_size_value buf =
      let size = decode_resource_size_spec buf in
      let num_cross_references = Conv.To_int.u32 (Decode.u32 buf) in
      let cross_references =
        (Decode.list ~item:decode_resource_size_spec)
          ~len:num_cross_references buf in
      ()
  end
module Screensaver_codec = struct open Protocol.Screensaver end
module Shm_codec = struct open Protocol.Shm end
module Xc_misc_codec = struct open Protocol.Xc_misc end
module Xevie_codec =
  struct open Protocol.Xevie
         let decode_event buf = Decode.pad buf 32; () end
module Xf86dri_codec =
  struct
    open Protocol.Xf86dri
    let decode_drm_clip_rect buf =
      let x1 = Decode.i16 buf in
      let y1 = Decode.i16 buf in
      let x2 = Decode.i16 buf in let x3 = Decode.i16 buf in ()
  end
module Xf86vidmode_codec =
  struct
    open Protocol.Xf86vidmode
    let decode_mode_info buf =
      let dotclock = Decode.u32 buf in
      let hdisplay = Decode.u16 buf in
      let hsyncstart = Decode.u16 buf in
      let hsyncend = Decode.u16 buf in
      let htotal = Decode.u16 buf in
      let hskew = Decode.u32 buf in
      let vdisplay = Decode.u16 buf in
      let vsyncstart = Decode.u16 buf in
      let vsyncend = Decode.u16 buf in
      let vtotal = Decode.u16 buf in
      Decode.pad buf 4;
      (let flags =
         ((Decode.u32 %> Conv.To_i32.u32) %> Mode_flag_mask.of_int32) buf in
       Decode.pad buf 12; (let privsize = Decode.u32 buf in ()))
  end
module Xinerama_codec =
  struct
    open Protocol.Xinerama
    let decode_screen_info buf =
      let x_org = Decode.i16 buf in
      let y_org = Decode.i16 buf in
      let width = Decode.u16 buf in let height = Decode.u16 buf in ()
  end
module Xinput_codec =
  struct
    open Protocol.Xinput
    let decode_fp3232 buf =
      let integral = Decode.i32 buf in let frac = Decode.u32 buf in ()
    let decode_device_info buf =
      let device_type = Decode.xid buf in
      let device_id = Decode.u8 buf in
      let num_class_info = Decode.u8 buf in
      let device_use =
        ((Decode.u8 %> Conv.To_int.u8) %> Device_use_enum.of_int) buf in
      Decode.pad buf 1; ()
    let decode_key_info buf =
      let class_id =
        ((Decode.u8 %> Conv.To_int.u8) %> Input_class_enum.of_int) buf in
      let len = Decode.u8 buf in
      let min_keycode = Decode.u8 buf in
      let max_keycode = Decode.u8 buf in
      let num_keys = Decode.u16 buf in Decode.pad buf 2; ()
    let decode_button_info buf =
      let class_id =
        ((Decode.u8 %> Conv.To_int.u8) %> Input_class_enum.of_int) buf in
      let len = Decode.u8 buf in let num_buttons = Decode.u16 buf in ()
    let decode_axis_info buf =
      let resolution = Decode.u32 buf in
      let minimum = Decode.i32 buf in let maximum = Decode.i32 buf in ()
    let decode_valuator_info buf =
      let class_id =
        ((Decode.u8 %> Conv.To_int.u8) %> Input_class_enum.of_int) buf in
      let len = Decode.u8 buf in
      let axes_len = Conv.To_int.u8 (Decode.u8 buf) in
      let mode =
        ((Decode.u8 %> Conv.To_int.u8) %> Valuator_mode_enum.of_int) buf in
      let motion_size = Decode.u32 buf in
      let axes = (Decode.list ~item:decode_axis_info) ~len:axes_len buf in ()
    let decode_input_info buf =
      let info_tag = Decode.u8 buf in
      let len = Decode.u8 buf in
      let info = decode_input_class_variant ~tag:info_tag buf in ()
    let decode_device_name buf =
      let len = Conv.To_int.u8 (Decode.u8 buf) in
      let string = Decode.string ~len buf in ()
    let decode_input_class_info buf =
      let class_id =
        ((Decode.u8 %> Conv.To_int.u8) %> Input_class_enum.of_int) buf in
      let event_type_base = Decode.u8 buf in ()
    let decode_kbd_feedback_state buf =
      let class_id =
        ((Decode.u8 %> Conv.To_int.u8) %> Feedback_class_enum.of_int) buf in
      let feedback_id = Decode.u8 buf in
      let len = Decode.u16 buf in
      let pitch = Decode.u16 buf in
      let duration = Decode.u16 buf in
      let led_mask = Decode.u32 buf in
      let led_values = Decode.u32 buf in
      let global_auto_repeat = Decode.bool buf in
      let click = Decode.u8 buf in
      let percent = Decode.u8 buf in
      Decode.pad buf 1;
      (let auto_repeats = (Decode.list ~item:Decode.u8) ~len:32 buf in ())
    let decode_ptr_feedback_state buf =
      let class_id =
        ((Decode.u8 %> Conv.To_int.u8) %> Feedback_class_enum.of_int) buf in
      let feedback_id = Decode.u8 buf in
      let len = Decode.u16 buf in
      Decode.pad buf 2;
      (let accel_num = Decode.u16 buf in
       let accel_denom = Decode.u16 buf in
       let threshold = Decode.u16 buf in ())
    let decode_integer_feedback_state buf =
      let class_id =
        ((Decode.u8 %> Conv.To_int.u8) %> Feedback_class_enum.of_int) buf in
      let feedback_id = Decode.u8 buf in
      let len = Decode.u16 buf in
      let resolution = Decode.u32 buf in
      let min_value = Decode.i32 buf in let max_value = Decode.i32 buf in ()
    let decode_string_feedback_state buf =
      let class_id =
        ((Decode.u8 %> Conv.To_int.u8) %> Feedback_class_enum.of_int) buf in
      let feedback_id = Decode.u8 buf in
      let len = Decode.u16 buf in
      let max_symbols = Decode.u16 buf in
      let num_keysyms = Conv.To_int.u16 (Decode.u16 buf) in
      let keysyms = (Decode.list ~item:Decode.u32) ~len:num_keysyms buf in ()
    let decode_bell_feedback_state buf =
      let class_id =
        ((Decode.u8 %> Conv.To_int.u8) %> Feedback_class_enum.of_int) buf in
      let feedback_id = Decode.u8 buf in
      let len = Decode.u16 buf in
      let percent = Decode.u8 buf in
      Decode.pad buf 3;
      (let pitch = Decode.u16 buf in let duration = Decode.u16 buf in ())
    let decode_led_feedback_state buf =
      let class_id =
        ((Decode.u8 %> Conv.To_int.u8) %> Feedback_class_enum.of_int) buf in
      let feedback_id = Decode.u8 buf in
      let len = Decode.u16 buf in
      let led_mask = Decode.u32 buf in let led_values = Decode.u32 buf in ()
    let decode_feedback_state buf =
      let data_tag = Decode.u8 buf in
      let feedback_id = Decode.u8 buf in
      let len = Decode.u16 buf in
      let data = decode_feedback_class_variant ~tag:data_tag buf in ()
    let decode_kbd_feedback_ctl buf =
      let class_id =
        ((Decode.u8 %> Conv.To_int.u8) %> Feedback_class_enum.of_int) buf in
      let feedback_id = Decode.u8 buf in
      let len = Decode.u16 buf in
      let key = Decode.u8 buf in
      let auto_repeat_mode = Decode.u8 buf in
      let key_click_percent = Decode.i8 buf in
      let bell_percent = Decode.i8 buf in
      let bell_pitch = Decode.i16 buf in
      let bell_duration = Decode.i16 buf in
      let led_mask = Decode.u32 buf in let led_values = Decode.u32 buf in ()
    let decode_ptr_feedback_ctl buf =
      let class_id =
        ((Decode.u8 %> Conv.To_int.u8) %> Feedback_class_enum.of_int) buf in
      let feedback_id = Decode.u8 buf in
      let len = Decode.u16 buf in
      Decode.pad buf 2;
      (let num = Decode.i16 buf in
       let denom = Decode.i16 buf in let threshold = Decode.i16 buf in ())
    let decode_integer_feedback_ctl buf =
      let class_id =
        ((Decode.u8 %> Conv.To_int.u8) %> Feedback_class_enum.of_int) buf in
      let feedback_id = Decode.u8 buf in
      let len = Decode.u16 buf in let int_to_display = Decode.i32 buf in ()
    let decode_string_feedback_ctl buf =
      let class_id =
        ((Decode.u8 %> Conv.To_int.u8) %> Feedback_class_enum.of_int) buf in
      let feedback_id = Decode.u8 buf in
      let len = Decode.u16 buf in
      Decode.pad buf 2;
      (let num_keysyms = Conv.To_int.u16 (Decode.u16 buf) in
       let keysyms = (Decode.list ~item:Decode.u32) ~len:num_keysyms buf in
       ())
    let decode_bell_feedback_ctl buf =
      let class_id =
        ((Decode.u8 %> Conv.To_int.u8) %> Feedback_class_enum.of_int) buf in
      let feedback_id = Decode.u8 buf in
      let len = Decode.u16 buf in
      let percent = Decode.i8 buf in
      Decode.pad buf 3;
      (let pitch = Decode.i16 buf in let duration = Decode.i16 buf in ())
    let decode_led_feedback_ctl buf =
      let class_id =
        ((Decode.u8 %> Conv.To_int.u8) %> Feedback_class_enum.of_int) buf in
      let feedback_id = Decode.u8 buf in
      let len = Decode.u16 buf in
      let led_mask = Decode.u32 buf in let led_values = Decode.u32 buf in ()
    let decode_feedback_ctl buf =
      let data_tag = Decode.u8 buf in
      let feedback_id = Decode.u8 buf in
      let len = Decode.u16 buf in
      let data = decode_feedback_class_variant ~tag:data_tag buf in ()
    let decode_key_state buf =
      let class_id =
        ((Decode.u8 %> Conv.To_int.u8) %> Input_class_enum.of_int) buf in
      let len = Decode.u8 buf in
      let num_keys = Decode.u8 buf in
      Decode.pad buf 1;
      (let keys = (Decode.list ~item:Decode.u8) ~len:32 buf in ())
    let decode_button_state buf =
      let class_id =
        ((Decode.u8 %> Conv.To_int.u8) %> Input_class_enum.of_int) buf in
      let len = Decode.u8 buf in
      let num_buttons = Decode.u8 buf in
      Decode.pad buf 1;
      (let buttons = (Decode.list ~item:Decode.u8) ~len:32 buf in ())
    let decode_valuator_state buf =
      let class_id =
        ((Decode.u8 %> Conv.To_int.u8) %> Input_class_enum.of_int) buf in
      let len = Decode.u8 buf in
      let num_valuators = Conv.To_int.u8 (Decode.u8 buf) in
      let mode =
        ((Decode.u8 %> Conv.To_i32.u8) %> Valuator_state_mode_mask.of_int32)
          buf in
      let valuators = (Decode.list ~item:Decode.i32) ~len:num_valuators buf in
      ()
    let decode_input_state buf =
      let data_tag = Decode.u8 buf in
      let len = Decode.u8 buf in
      let data = decode_input_class_variant ~tag:data_tag buf in ()
    let decode_device_resolution_state buf =
      let control_id =
        ((Decode.u16 %> Conv.To_int.u16) %> Device_control_enum.of_int) buf in
      let len = Decode.u16 buf in
      let num_valuators = Conv.To_int.u32 (Decode.u32 buf) in
      let resolution_values =
        (Decode.list ~item:Decode.u32) ~len:num_valuators buf in
      let resolution_min =
        (Decode.list ~item:Decode.u32) ~len:num_valuators buf in
      let resolution_max =
        (Decode.list ~item:Decode.u32) ~len:num_valuators buf in
      ()
    let decode_device_abs_calib_state buf =
      let control_id =
        ((Decode.u16 %> Conv.To_int.u16) %> Device_control_enum.of_int) buf in
      let len = Decode.u16 buf in
      let min_x = Decode.i32 buf in
      let max_x = Decode.i32 buf in
      let min_y = Decode.i32 buf in
      let max_y = Decode.i32 buf in
      let flip_x = Decode.u32 buf in
      let flip_y = Decode.u32 buf in
      let rotation = Decode.u32 buf in
      let button_threshold = Decode.u32 buf in ()
    let decode_device_abs_area_state buf =
      let control_id =
        ((Decode.u16 %> Conv.To_int.u16) %> Device_control_enum.of_int) buf in
      let len = Decode.u16 buf in
      let offset_x = Decode.u32 buf in
      let offset_y = Decode.u32 buf in
      let width = Decode.u32 buf in
      let height = Decode.u32 buf in
      let screen = Decode.u32 buf in let following = Decode.u32 buf in ()
    let decode_device_core_state buf =
      let control_id =
        ((Decode.u16 %> Conv.To_int.u16) %> Device_control_enum.of_int) buf in
      let len = Decode.u16 buf in
      let status = Decode.u8 buf in
      let iscore = Decode.u8 buf in Decode.pad buf 2; ()
    let decode_device_enable_state buf =
      let control_id =
        ((Decode.u16 %> Conv.To_int.u16) %> Device_control_enum.of_int) buf in
      let len = Decode.u16 buf in
      let enable = Decode.u8 buf in Decode.pad buf 3; ()
    let decode_device_state buf =
      let data_tag = Decode.u16 buf in
      let len = Decode.u16 buf in
      let data = decode_device_control_variant ~tag:data_tag buf in ()
    let decode_device_resolution_ctl buf =
      let control_id =
        ((Decode.u16 %> Conv.To_int.u16) %> Device_control_enum.of_int) buf in
      let len = Decode.u16 buf in
      let first_valuator = Decode.u8 buf in
      let num_valuators = Conv.To_int.u8 (Decode.u8 buf) in
      Decode.pad buf 2;
      (let resolution_values =
         (Decode.list ~item:Decode.u32) ~len:num_valuators buf in
       ())
    let decode_device_abs_calib_ctl buf =
      let control_id =
        ((Decode.u16 %> Conv.To_int.u16) %> Device_control_enum.of_int) buf in
      let len = Decode.u16 buf in
      let min_x = Decode.i32 buf in
      let max_x = Decode.i32 buf in
      let min_y = Decode.i32 buf in
      let max_y = Decode.i32 buf in
      let flip_x = Decode.u32 buf in
      let flip_y = Decode.u32 buf in
      let rotation = Decode.u32 buf in
      let button_threshold = Decode.u32 buf in ()
    let decode_device_abs_area_ctrl buf =
      let control_id =
        ((Decode.u16 %> Conv.To_int.u16) %> Device_control_enum.of_int) buf in
      let len = Decode.u16 buf in
      let offset_x = Decode.u32 buf in
      let offset_y = Decode.u32 buf in
      let width = Decode.i32 buf in
      let height = Decode.i32 buf in
      let screen = Decode.i32 buf in let following = Decode.u32 buf in ()
    let decode_device_core_ctrl buf =
      let control_id =
        ((Decode.u16 %> Conv.To_int.u16) %> Device_control_enum.of_int) buf in
      let len = Decode.u16 buf in
      let status = Decode.u8 buf in Decode.pad buf 3; ()
    let decode_device_enable_ctrl buf =
      let control_id =
        ((Decode.u16 %> Conv.To_int.u16) %> Device_control_enum.of_int) buf in
      let len = Decode.u16 buf in
      let enable = Decode.u8 buf in Decode.pad buf 3; ()
    let decode_device_ctl buf =
      let data_tag = Decode.u16 buf in
      let len = Decode.u16 buf in
      let data = decode_device_control_variant ~tag:data_tag buf in ()
    let decode_group_info buf =
      let base = Decode.u8 buf in
      let latched = Decode.u8 buf in
      let locked = Decode.u8 buf in let effective = Decode.u8 buf in ()
    let decode_modifier_info buf =
      let base = Decode.u32 buf in
      let latched = Decode.u32 buf in
      let locked = Decode.u32 buf in let effective = Decode.u32 buf in ()
    let decode_add_master buf =
      let type_ =
        ((Decode.u16 %> Conv.To_int.u16) %> Hierarchy_change_type_enum.of_int)
          buf in
      let len = Decode.u16 buf in
      let name_len = Conv.To_int.u16 (Decode.u16 buf) in
      let send_core = Decode.bool buf in
      let enable = Decode.bool buf in
      let name = Decode.string ~len:name_len buf in Decode.pad buf 4; ()
    let decode_remove_master buf =
      let type_ =
        ((Decode.u16 %> Conv.To_int.u16) %> Hierarchy_change_type_enum.of_int)
          buf in
      let len = Decode.u16 buf in
      let deviceid =
        (Decode.u16 %>
           (Conv.alt_enum ~enum_of_int:Device_enum.of_int
              ~int_of_t:Conv.To_int.u16)) buf in
      let return_mode =
        ((Decode.u8 %> Conv.To_int.u8) %> Change_mode_enum.of_int) buf in
      Decode.pad buf 1;
      (let return_pointer =
         (Decode.u16 %>
            (Conv.alt_enum ~enum_of_int:Device_enum.of_int
               ~int_of_t:Conv.To_int.u16)) buf in
       let return_keyboard =
         (Decode.u16 %>
            (Conv.alt_enum ~enum_of_int:Device_enum.of_int
               ~int_of_t:Conv.To_int.u16)) buf in
       ())
    let decode_attach_slave buf =
      let type_ =
        ((Decode.u16 %> Conv.To_int.u16) %> Hierarchy_change_type_enum.of_int)
          buf in
      let len = Decode.u16 buf in
      let deviceid =
        (Decode.u16 %>
           (Conv.alt_enum ~enum_of_int:Device_enum.of_int
              ~int_of_t:Conv.To_int.u16)) buf in
      let master =
        (Decode.u16 %>
           (Conv.alt_enum ~enum_of_int:Device_enum.of_int
              ~int_of_t:Conv.To_int.u16)) buf in
      ()
    let decode_detach_slave buf =
      let type_ =
        ((Decode.u16 %> Conv.To_int.u16) %> Hierarchy_change_type_enum.of_int)
          buf in
      let len = Decode.u16 buf in
      let deviceid =
        (Decode.u16 %>
           (Conv.alt_enum ~enum_of_int:Device_enum.of_int
              ~int_of_t:Conv.To_int.u16)) buf in
      Decode.pad buf 2; ()
    let decode_hierarchy_change buf =
      let data_tag = Decode.u16 buf in
      let len = Decode.u16 buf in
      let data = decode_hierarchy_change_type_variant ~tag:data_tag buf in ()
    let decode_event_mask buf =
      let deviceid =
        (Decode.u16 %>
           (Conv.alt_enum ~enum_of_int:Device_enum.of_int
              ~int_of_t:Conv.To_int.u16)) buf in
      let mask_len = Conv.To_int.u16 (Decode.u16 buf) in
      let mask =
        (Decode.list
           ~item:((Decode.u32 %> Conv.To_i32.u32) %> Xi_event_mask.of_int32))
          ~len:mask_len buf in
      ()
    let decode_button_class buf =
      let type_ =
        ((Decode.u16 %> Conv.To_int.u16) %> Device_class_type_enum.of_int)
          buf in
      let len = Decode.u16 buf in
      let sourceid = Decode.u16 buf in
      let num_buttons = Conv.To_int.u16 (Decode.u16 buf) in
      let state =
        (Decode.list ~item:Decode.u32) ~len:((num_buttons + 31) / 32) buf in
      let labels = (Decode.list ~item:Decode.xid) ~len:num_buttons buf in ()
    let decode_key_class buf =
      let type_ =
        ((Decode.u16 %> Conv.To_int.u16) %> Device_class_type_enum.of_int)
          buf in
      let len = Decode.u16 buf in
      let sourceid = Decode.u16 buf in
      let num_keys = Conv.To_int.u16 (Decode.u16 buf) in
      let keys = (Decode.list ~item:Decode.u32) ~len:num_keys buf in ()
    let decode_scroll_class buf =
      let type_ =
        ((Decode.u16 %> Conv.To_int.u16) %> Device_class_type_enum.of_int)
          buf in
      let len = Decode.u16 buf in
      let sourceid = Decode.u16 buf in
      let number = Decode.u16 buf in
      let scroll_type =
        ((Decode.u16 %> Conv.To_int.u16) %> Scroll_type_enum.of_int) buf in
      Decode.pad buf 2;
      (let flags =
         ((Decode.u32 %> Conv.To_i32.u32) %> Scroll_flags_mask.of_int32) buf in
       let increment = decode_fp3232 buf in ())
    let decode_touch_class buf =
      let type_ =
        ((Decode.u16 %> Conv.To_int.u16) %> Device_class_type_enum.of_int)
          buf in
      let len = Decode.u16 buf in
      let sourceid = Decode.u16 buf in
      let mode =
        ((Decode.u8 %> Conv.To_int.u8) %> Touch_mode_enum.of_int) buf in
      let num_touches = Decode.u8 buf in ()
    let decode_valuator_class buf =
      let type_ =
        ((Decode.u16 %> Conv.To_int.u16) %> Device_class_type_enum.of_int)
          buf in
      let len = Decode.u16 buf in
      let sourceid = Decode.u16 buf in
      let number = Decode.u16 buf in
      let label = Decode.xid buf in
      let min = decode_fp3232 buf in
      let max = decode_fp3232 buf in
      let value = decode_fp3232 buf in
      let resolution = Decode.u32 buf in
      let mode =
        ((Decode.u8 %> Conv.To_int.u8) %> Valuator_mode_enum.of_int) buf in
      Decode.pad buf 3; ()
    let decode_device_class buf =
      let data_tag = Decode.u16 buf in
      let len = Decode.u16 buf in
      let sourceid = Decode.u16 buf in
      let data = decode_device_class_type_variant ~tag:data_tag buf in ()
    let decode_xi_device_info buf =
      let deviceid =
        (Decode.u16 %>
           (Conv.alt_enum ~enum_of_int:Device_enum.of_int
              ~int_of_t:Conv.To_int.u16)) buf in
      let type_ =
        ((Decode.u16 %> Conv.To_int.u16) %> Device_type_enum.of_int) buf in
      let attachment =
        (Decode.u16 %>
           (Conv.alt_enum ~enum_of_int:Device_enum.of_int
              ~int_of_t:Conv.To_int.u16)) buf in
      let num_classes = Conv.To_int.u16 (Decode.u16 buf) in
      let name_len = Conv.To_int.u16 (Decode.u16 buf) in
      let enabled = Decode.bool buf in
      Decode.pad buf 1;
      (let name = Decode.string ~len:name_len buf in
       Decode.pad buf 4;
       (let classes =
          (Decode.list ~item:decode_device_class) ~len:num_classes buf in
        ()))
    let decode_grab_modifier_info buf =
      let modifiers =
        ((Decode.u32 %> Conv.To_i32.u32) %> Modifier_mask.of_int32) buf in
      let status =
        ((Decode.u8 %> Conv.To_int.u8) %> Core.Grab_status_enum.of_int) buf in
      Decode.pad buf 3; ()
    let decode_barrier_release_pointer_info buf =
      let deviceid = Decode.u16 buf in
      Decode.pad buf 2;
      (let barrier = Decode.xid buf in let eventid = Decode.u32 buf in ())
    let decode_hierarchy_info buf =
      let deviceid =
        (Decode.u16 %>
           (Conv.alt_enum ~enum_of_int:Device_enum.of_int
              ~int_of_t:Conv.To_int.u16)) buf in
      let attachment =
        (Decode.u16 %>
           (Conv.alt_enum ~enum_of_int:Device_enum.of_int
              ~int_of_t:Conv.To_int.u16)) buf in
      let type_ =
        ((Decode.u8 %> Conv.To_int.u8) %> Device_type_enum.of_int) buf in
      let enabled = Decode.bool buf in
      Decode.pad buf 2;
      (let flags =
         ((Decode.u32 %> Conv.To_i32.u32) %> Hierarchy_mask.of_int32) buf in
       ())
  end
module Xprint_codec =
  struct
    open Protocol.Xprint
    let decode_printer buf =
      let name_len = Conv.To_int.u32 (Decode.u32 buf) in
      let name = Decode.string ~len:name_len buf in
      Decode.pad buf 4;
      (let desc_len = Conv.To_int.u32 (Decode.u32 buf) in
       let description = Decode.string ~len:desc_len buf in
       Decode.pad buf 4; ())
  end
module Xselinux_codec =
  struct
    open Protocol.Xselinux
    let decode_list_item buf =
      let name = Decode.xid buf in
      let object_context_len = Conv.To_int.u32 (Decode.u32 buf) in
      let data_context_len = Conv.To_int.u32 (Decode.u32 buf) in
      let object_context = Decode.string ~len:object_context_len buf in
      Decode.pad buf 4;
      (let data_context = Decode.string ~len:data_context_len buf in
       Decode.pad buf 4; ())
  end
module Xtest_codec = struct open Protocol.Xtest end
module Xv_codec =
  struct
    open Protocol.Xv
    let decode_rational buf =
      let numerator = Decode.i32 buf in
      let denominator = Decode.i32 buf in ()
    let decode_format buf =
      let visual = Decode.u32 buf in
      let depth = Decode.u8 buf in Decode.pad buf 3; ()
    let decode_adaptor_info buf =
      let base_id = Decode.xid buf in
      let name_size = Conv.To_int.u16 (Decode.u16 buf) in
      let num_ports = Decode.u16 buf in
      let num_formats = Conv.To_int.u16 (Decode.u16 buf) in
      let type_ = ((Decode.u8 %> Conv.To_i32.u8) %> Type_mask.of_int32) buf in
      Decode.pad buf 1;
      (let name = Decode.string ~len:name_size buf in
       Decode.pad buf 4;
       (let formats = (Decode.list ~item:decode_format) ~len:num_formats buf in
        ()))
    let decode_encoding_info buf =
      let encoding = Decode.xid buf in
      let name_size = Conv.To_int.u16 (Decode.u16 buf) in
      let width = Decode.u16 buf in
      let height = Decode.u16 buf in
      Decode.pad buf 2;
      (let rate = decode_rational buf in
       let name = Decode.string ~len:name_size buf in Decode.pad buf 4; ())
    let decode_image buf =
      let id = Decode.u32 buf in
      let width = Decode.u16 buf in
      let height = Decode.u16 buf in
      let data_size = Conv.To_int.u32 (Decode.u32 buf) in
      let num_planes = Conv.To_int.u32 (Decode.u32 buf) in
      let pitches = (Decode.list ~item:Decode.u32) ~len:num_planes buf in
      let offsets = (Decode.list ~item:Decode.u32) ~len:num_planes buf in
      let data = (Decode.list ~item:Decode.u8) ~len:data_size buf in ()
    let decode_attribute_info buf =
      let flags =
        ((Decode.u32 %> Conv.To_i32.u32) %> Attribute_flag_mask.of_int32) buf in
      let min = Decode.i32 buf in
      let max = Decode.i32 buf in
      let size = Conv.To_int.u32 (Decode.u32 buf) in
      let name = Decode.string ~len:size buf in Decode.pad buf 4; ()
    let decode_image_format_info buf =
      let id = Decode.u32 buf in
      let type_ =
        ((Decode.u8 %> Conv.To_int.u8) %> Image_format_info_type_enum.of_int)
          buf in
      let byte_order =
        ((Decode.u8 %> Conv.To_int.u8) %> Core.Image_order_enum.of_int) buf in
      Decode.pad buf 2;
      (let guid = (Decode.list ~item:Decode.u8) ~len:16 buf in
       let bpp = Decode.u8 buf in
       let num_planes = Decode.u8 buf in
       Decode.pad buf 2;
       (let depth = Decode.u8 buf in
        Decode.pad buf 3;
        (let red_mask = Decode.u32 buf in
         let green_mask = Decode.u32 buf in
         let blue_mask = Decode.u32 buf in
         let format =
           ((Decode.u8 %> Conv.To_int.u8) %>
              Image_format_info_format_enum.of_int) buf in
         Decode.pad buf 3;
         (let y_sample_bits = Decode.u32 buf in
          let u_sample_bits = Decode.u32 buf in
          let v_sample_bits = Decode.u32 buf in
          let vhorz_y_period = Decode.u32 buf in
          let vhorz_u_period = Decode.u32 buf in
          let vhorz_v_period = Decode.u32 buf in
          let vvert_y_period = Decode.u32 buf in
          let vvert_u_period = Decode.u32 buf in
          let vvert_v_period = Decode.u32 buf in
          let vcomp_order = (Decode.list ~item:Decode.u8) ~len:32 buf in
          let vscanline_order =
            ((Decode.u8 %> Conv.To_int.u8) %> Scanline_order_enum.of_int) buf in
          Decode.pad buf 11; ()))))
  end
module Xvmc_codec =
  struct
    open Protocol.Xvmc
    let decode_surface_info buf =
      let id = Decode.xid buf in
      let chroma_format = Decode.u16 buf in
      let pad0 = Decode.u16 buf in
      let max_width = Decode.u16 buf in
      let max_height = Decode.u16 buf in
      let subpicture_max_width = Decode.u16 buf in
      let subpicture_max_height = Decode.u16 buf in
      let mc_type = Decode.u32 buf in let flags = Decode.u32 buf in ()
  end
