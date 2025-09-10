[@@@ocamlformat "disable"]
[@@@ocaml.warning "-33"]
[@@@ocaml.warning "-27"]
open Util
open Protocol
module Core_codec =
  struct
    open Protocol.Core
    let decode_char2b buf =
      (let byte1 = Decode.u8 buf in
       let byte2 = Decode.u8 buf in { byte1; byte2 } : char2b)
    let decode_point buf =
      (let x = Decode.i16 buf in let y = Decode.i16 buf in { x; y } : 
      point)
    let decode_rectangle buf =
      (let x = Decode.i16 buf in
       let y = Decode.i16 buf in
       let width = Decode.u16 buf in
       let height = Decode.u16 buf in { x; y; width; height } : rectangle)
    let decode_arc buf =
      (let x = Decode.i16 buf in
       let y = Decode.i16 buf in
       let width = Decode.u16 buf in
       let height = Decode.u16 buf in
       let angle1 = Decode.i16 buf in
       let angle2 = Decode.i16 buf in { x; y; width; height; angle1; angle2 } : 
      arc)
    let decode_format buf =
      (let depth = Decode.u8 buf in
       let bits_per_pixel = Decode.u8 buf in
       let scanline_pad = Decode.u8 buf in
       Decode.pad buf 5; { depth; bits_per_pixel; scanline_pad } : format)
    let decode_visualtype buf =
      (let visual_id = Decode.u32 buf in
       let class_ =
         ((Decode.u8 %> Conv.To_int.u8) %> Visual_class_enum.of_int) buf in
       let bits_per_rgb_value = Decode.u8 buf in
       let colormap_entries = Decode.u16 buf in
       let red_mask = Decode.u32 buf in
       let green_mask = Decode.u32 buf in
       let blue_mask = Decode.u32 buf in
       Decode.pad buf 4;
       {
         visual_id;
         class_;
         bits_per_rgb_value;
         colormap_entries;
         red_mask;
         green_mask;
         blue_mask
       } : visualtype)
    let decode_depth buf =
      (let depth = Decode.u8 buf in
       Decode.pad buf 1;
       (let visuals_len = Decode.u16 buf in
        Decode.pad buf 4;
        (let visuals =
           (Decode.list ~item:decode_visualtype) ~len:visuals_len buf in
         { depth; visuals })) : depth)
    let decode_screen buf =
      (let root = Decode.xid buf in
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
       let allowed_depths_len = Decode.u8 buf in
       let allowed_depths =
         (Decode.list ~item:decode_depth) ~len:allowed_depths_len buf in
       {
         root;
         default_colormap;
         white_pixel;
         black_pixel;
         current_input_masks;
         width_in_pixels;
         height_in_pixels;
         width_in_millimeters;
         height_in_millimeters;
         min_installed_maps;
         max_installed_maps;
         root_visual;
         backing_stores;
         save_unders;
         root_depth;
         allowed_depths
       } : screen)
    let decode_setup_request buf =
      (let byte_order = Decode.u8 buf in
       Decode.pad buf 1;
       (let protocol_major_version = Decode.u16 buf in
        let protocol_minor_version = Decode.u16 buf in
        let authorization_protocol_name_len = Decode.u16 buf in
        let authorization_protocol_data_len = Decode.u16 buf in
        Decode.pad buf 2;
        (let authorization_protocol_name =
           Decode.string ~len:authorization_protocol_name_len buf in
         Decode.align buf 4;
         (let authorization_protocol_data =
            Decode.string ~len:authorization_protocol_data_len buf in
          Decode.align buf 4;
          {
            byte_order;
            protocol_major_version;
            protocol_minor_version;
            authorization_protocol_name;
            authorization_protocol_data
          }))) : setup_request)
    let decode_setup_failed buf =
      (let status = Decode.u8 buf in
       let reason_len = Decode.u8 buf in
       let protocol_major_version = Decode.u16 buf in
       let protocol_minor_version = Decode.u16 buf in
       let length = Decode.u16 buf in
       let reason = Decode.string ~len:reason_len buf in
       {
         status;
         protocol_major_version;
         protocol_minor_version;
         length;
         reason
       } : setup_failed)
    let decode_setup_authenticate buf =
      (let status = Decode.u8 buf in
       Decode.pad buf 5;
       (let length = Decode.u16 buf in
        let length = length / 4 in
        let reason = Decode.string ~len:length buf in { status; reason }) : 
      setup_authenticate)
    let decode_setup buf =
      (let status = Decode.u8 buf in
       Decode.pad buf 1;
       (let protocol_major_version = Decode.u16 buf in
        let protocol_minor_version = Decode.u16 buf in
        let length = Decode.u16 buf in
        let release_number = Decode.u32 buf in
        let resource_id_base = Decode.u32 buf in
        let resource_id_mask = Decode.u32 buf in
        let motion_buffer_size = Decode.u32 buf in
        let vendor_len = Decode.u16 buf in
        let maximum_request_length = Decode.u16 buf in
        let roots_len = Decode.u8 buf in
        let pixmap_formats_len = Decode.u8 buf in
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
         Decode.align buf 4;
         (let pixmap_formats =
            (Decode.list ~item:decode_format) ~len:pixmap_formats_len buf in
          let roots = (Decode.list ~item:decode_screen) ~len:roots_len buf in
          {
            status;
            protocol_major_version;
            protocol_minor_version;
            length;
            release_number;
            resource_id_base;
            resource_id_mask;
            motion_buffer_size;
            maximum_request_length;
            image_byte_order;
            bitmap_format_bit_order;
            bitmap_format_scanline_unit;
            bitmap_format_scanline_pad;
            min_keycode;
            max_keycode;
            vendor;
            pixmap_formats;
            roots
          }))) : setup)
    let decode_key_press_event buf =
      (Decode.pad buf 1;
       (let detail = Decode.u8 buf in
        Decode.pad buf 2;
        (let time = Decode.u32 buf in
         let root = Decode.xid buf in
         let event = Decode.xid buf in
         let child =
           (Decode.xid %>
              (Conv.alt_enum ~enum_of_int:Window_enum.of_int
                 ~int_of_t:Conv.To_int.xid)) buf in
         let root_x = Decode.i16 buf in
         let root_y = Decode.i16 buf in
         let event_x = Decode.i16 buf in
         let event_y = Decode.i16 buf in
         let state =
           ((Decode.u16 %> Conv.To_i32.u16) %> Key_but_mask.of_int32) buf in
         let same_screen = Decode.bool buf in
         Decode.pad buf 1;
         {
           detail;
           time;
           root;
           event;
           child;
           root_x;
           root_y;
           event_x;
           event_y;
           state;
           same_screen
         })) : Event.Key_press.t)
    let decode_key_release_event = decode_key_press_event
    let decode_button_press_event buf =
      (Decode.pad buf 1;
       (let detail = Decode.u8 buf in
        Decode.pad buf 2;
        (let time = Decode.u32 buf in
         let root = Decode.xid buf in
         let event = Decode.xid buf in
         let child =
           (Decode.xid %>
              (Conv.alt_enum ~enum_of_int:Window_enum.of_int
                 ~int_of_t:Conv.To_int.xid)) buf in
         let root_x = Decode.i16 buf in
         let root_y = Decode.i16 buf in
         let event_x = Decode.i16 buf in
         let event_y = Decode.i16 buf in
         let state =
           ((Decode.u16 %> Conv.To_i32.u16) %> Key_but_mask.of_int32) buf in
         let same_screen = Decode.bool buf in
         Decode.pad buf 1;
         {
           detail;
           time;
           root;
           event;
           child;
           root_x;
           root_y;
           event_x;
           event_y;
           state;
           same_screen
         })) : Event.Button_press.t)
    let decode_button_release_event = decode_button_press_event
    let decode_motion_notify_event buf =
      (Decode.pad buf 1;
       (let detail =
          ((Decode.byte %> Conv.To_int.byte) %> Motion_enum.of_int) buf in
        Decode.pad buf 2;
        (let time = Decode.u32 buf in
         let root = Decode.xid buf in
         let event = Decode.xid buf in
         let child =
           (Decode.xid %>
              (Conv.alt_enum ~enum_of_int:Window_enum.of_int
                 ~int_of_t:Conv.To_int.xid)) buf in
         let root_x = Decode.i16 buf in
         let root_y = Decode.i16 buf in
         let event_x = Decode.i16 buf in
         let event_y = Decode.i16 buf in
         let state =
           ((Decode.u16 %> Conv.To_i32.u16) %> Key_but_mask.of_int32) buf in
         let same_screen = Decode.bool buf in
         Decode.pad buf 1;
         {
           detail;
           time;
           root;
           event;
           child;
           root_x;
           root_y;
           event_x;
           event_y;
           state;
           same_screen
         })) : Event.Motion_notify.t)
    let decode_enter_notify_event buf =
      (Decode.pad buf 1;
       (let detail =
          ((Decode.byte %> Conv.To_int.byte) %> Notify_detail_enum.of_int)
            buf in
        Decode.pad buf 2;
        (let time = Decode.u32 buf in
         let root = Decode.xid buf in
         let event = Decode.xid buf in
         let child =
           (Decode.xid %>
              (Conv.alt_enum ~enum_of_int:Window_enum.of_int
                 ~int_of_t:Conv.To_int.xid)) buf in
         let root_x = Decode.i16 buf in
         let root_y = Decode.i16 buf in
         let event_x = Decode.i16 buf in
         let event_y = Decode.i16 buf in
         let state =
           ((Decode.u16 %> Conv.To_i32.u16) %> Key_but_mask.of_int32) buf in
         let mode =
           ((Decode.byte %> Conv.To_int.byte) %> Notify_mode_enum.of_int) buf in
         let same_screen_focus = Decode.byte buf in
         {
           detail;
           time;
           root;
           event;
           child;
           root_x;
           root_y;
           event_x;
           event_y;
           state;
           mode;
           same_screen_focus
         })) : Event.Enter_notify.t)
    let decode_leave_notify_event = decode_enter_notify_event
    let decode_focus_in_event buf =
      (Decode.pad buf 1;
       (let detail =
          ((Decode.byte %> Conv.To_int.byte) %> Notify_detail_enum.of_int)
            buf in
        Decode.pad buf 2;
        (let event = Decode.xid buf in
         let mode =
           ((Decode.byte %> Conv.To_int.byte) %> Notify_mode_enum.of_int) buf in
         Decode.pad buf 3; { detail; event; mode })) : Event.Focus_in.t)
    let decode_focus_out_event = decode_focus_in_event
    let decode_keymap_notify_event buf =
      (Decode.pad buf 1;
       (let keys = (Decode.list ~item:Decode.u8) ~len:31 buf in keys) : 
      Event.Keymap_notify.t)
    let decode_expose_event buf =
      (Decode.pad buf 4;
       (let window = Decode.xid buf in
        let x = Decode.u16 buf in
        let y = Decode.u16 buf in
        let width = Decode.u16 buf in
        let height = Decode.u16 buf in
        let count = Decode.u16 buf in
        Decode.pad buf 2; { window; x; y; width; height; count }) : Event.Expose.t)
    let decode_graphics_exposure_event buf =
      (Decode.pad buf 4;
       (let drawable = Decode.xid buf in
        let x = Decode.u16 buf in
        let y = Decode.u16 buf in
        let width = Decode.u16 buf in
        let height = Decode.u16 buf in
        let minor_opcode = Decode.u16 buf in
        let count = Decode.u16 buf in
        let major_opcode = Decode.u8 buf in
        Decode.pad buf 3;
        { drawable; x; y; width; height; minor_opcode; count; major_opcode }) : 
      Event.Graphics_exposure.t)
    let decode_no_exposure_event buf =
      (Decode.pad buf 4;
       (let drawable = Decode.xid buf in
        let minor_opcode = Decode.u16 buf in
        let major_opcode = Decode.u8 buf in
        Decode.pad buf 1; { drawable; minor_opcode; major_opcode }) : 
      Event.No_exposure.t)
    let decode_visibility_notify_event buf =
      (Decode.pad buf 4;
       (let window = Decode.xid buf in
        let state =
          ((Decode.byte %> Conv.To_int.byte) %> Visibility_enum.of_int) buf in
        Decode.pad buf 3; { window; state }) : Event.Visibility_notify.t)
    let decode_create_notify_event buf =
      (Decode.pad buf 4;
       (let parent = Decode.xid buf in
        let window = Decode.xid buf in
        let x = Decode.i16 buf in
        let y = Decode.i16 buf in
        let width = Decode.u16 buf in
        let height = Decode.u16 buf in
        let border_width = Decode.u16 buf in
        let override_redirect = Decode.bool buf in
        Decode.pad buf 1;
        {
          parent;
          window;
          x;
          y;
          width;
          height;
          border_width;
          override_redirect
        }) : Event.Create_notify.t)
    let decode_destroy_notify_event buf =
      (Decode.pad buf 4;
       (let event = Decode.xid buf in
        let window = Decode.xid buf in { event; window }) : Event.Destroy_notify.t)
    let decode_unmap_notify_event buf =
      (Decode.pad buf 4;
       (let event = Decode.xid buf in
        let window = Decode.xid buf in
        let from_configure = Decode.bool buf in
        Decode.pad buf 3; { event; window; from_configure }) : Event.Unmap_notify.t)
    let decode_map_notify_event buf =
      (Decode.pad buf 4;
       (let event = Decode.xid buf in
        let window = Decode.xid buf in
        let override_redirect = Decode.bool buf in
        Decode.pad buf 3; { event; window; override_redirect }) : Event.Map_notify.t)
    let decode_map_request_event buf =
      (Decode.pad buf 4;
       (let parent = Decode.xid buf in
        let window = Decode.xid buf in { parent; window }) : Event.Map_request.t)
    let decode_reparent_notify_event buf =
      (Decode.pad buf 4;
       (let event = Decode.xid buf in
        let window = Decode.xid buf in
        let parent = Decode.xid buf in
        let x = Decode.i16 buf in
        let y = Decode.i16 buf in
        let override_redirect = Decode.bool buf in
        Decode.pad buf 3; { event; window; parent; x; y; override_redirect }) : 
      Event.Reparent_notify.t)
    let decode_configure_notify_event buf =
      (Decode.pad buf 4;
       (let event = Decode.xid buf in
        let window = Decode.xid buf in
        let above_sibling =
          (Decode.xid %>
             (Conv.alt_enum ~enum_of_int:Window_enum.of_int
                ~int_of_t:Conv.To_int.xid)) buf in
        let x = Decode.i16 buf in
        let y = Decode.i16 buf in
        let width = Decode.u16 buf in
        let height = Decode.u16 buf in
        let border_width = Decode.u16 buf in
        let override_redirect = Decode.bool buf in
        Decode.pad buf 1;
        {
          event;
          window;
          above_sibling;
          x;
          y;
          width;
          height;
          border_width;
          override_redirect
        }) : Event.Configure_notify.t)
    let decode_configure_request_event buf =
      (Decode.pad buf 1;
       (let stack_mode =
          ((Decode.byte %> Conv.To_int.byte) %> Stack_mode_enum.of_int) buf in
        Decode.pad buf 2;
        (let parent = Decode.xid buf in
         let window = Decode.xid buf in
         let sibling =
           (Decode.xid %>
              (Conv.alt_enum ~enum_of_int:Window_enum.of_int
                 ~int_of_t:Conv.To_int.xid)) buf in
         let x = Decode.i16 buf in
         let y = Decode.i16 buf in
         let width = Decode.u16 buf in
         let height = Decode.u16 buf in
         let border_width = Decode.u16 buf in
         let value_mask =
           ((Decode.u16 %> Conv.To_i32.u16) %> Config_window_mask.of_int32)
             buf in
         {
           stack_mode;
           parent;
           window;
           sibling;
           x;
           y;
           width;
           height;
           border_width;
           value_mask
         })) : Event.Configure_request.t)
    let decode_gravity_notify_event buf =
      (Decode.pad buf 4;
       (let event = Decode.xid buf in
        let window = Decode.xid buf in
        let x = Decode.i16 buf in
        let y = Decode.i16 buf in { event; window; x; y }) : Event.Gravity_notify.t)
    let decode_resize_request_event buf =
      (Decode.pad buf 4;
       (let window = Decode.xid buf in
        let width = Decode.u16 buf in
        let height = Decode.u16 buf in { window; width; height }) : Event.Resize_request.t)
    let decode_circulate_notify_event buf =
      (Decode.pad buf 4;
       (let event = Decode.xid buf in
        let window = Decode.xid buf in
        Decode.pad buf 4;
        (let place =
           ((Decode.byte %> Conv.To_int.byte) %> Place_enum.of_int) buf in
         Decode.pad buf 3; { event; window; place })) : Event.Circulate_notify.t)
    let decode_circulate_request_event = decode_circulate_notify_event
    let decode_property_notify_event buf =
      (Decode.pad buf 4;
       (let window = Decode.xid buf in
        let atom = Decode.xid buf in
        let time = Decode.u32 buf in
        let state =
          ((Decode.byte %> Conv.To_int.byte) %> Property_enum.of_int) buf in
        Decode.pad buf 3; { window; atom; time; state }) : Event.Property_notify.t)
    let decode_selection_clear_event buf =
      (Decode.pad buf 4;
       (let time = Decode.u32 buf in
        let owner = Decode.xid buf in
        let selection = Decode.xid buf in { time; owner; selection }) : 
      Event.Selection_clear.t)
    let decode_selection_request_event buf =
      (Decode.pad buf 4;
       (let time =
          (Decode.u32 %>
             (Conv.alt_enum ~enum_of_int:Time_enum.of_int
                ~int_of_t:Conv.To_int.u32)) buf in
        let owner = Decode.xid buf in
        let requestor = Decode.xid buf in
        let selection = Decode.xid buf in
        let target = Decode.xid buf in
        let property =
          (Decode.xid %>
             (Conv.alt_enum ~enum_of_int:Atom_enum.of_int
                ~int_of_t:Conv.To_int.xid)) buf in
        { time; owner; requestor; selection; target; property }) : Event.Selection_request.t)
    let decode_selection_notify_event buf =
      (Decode.pad buf 4;
       (let time =
          (Decode.u32 %>
             (Conv.alt_enum ~enum_of_int:Time_enum.of_int
                ~int_of_t:Conv.To_int.u32)) buf in
        let requestor = Decode.xid buf in
        let selection = Decode.xid buf in
        let target = Decode.xid buf in
        let property =
          (Decode.xid %>
             (Conv.alt_enum ~enum_of_int:Atom_enum.of_int
                ~int_of_t:Conv.To_int.xid)) buf in
        { time; requestor; selection; target; property }) : Event.Selection_notify.t)
    let decode_colormap_notify_event buf =
      (Decode.pad buf 4;
       (let window = Decode.xid buf in
        let colormap =
          (Decode.xid %>
             (Conv.alt_enum ~enum_of_int:Colormap_enum.of_int
                ~int_of_t:Conv.To_int.xid)) buf in
        let new_ = Decode.bool buf in
        let state =
          ((Decode.byte %> Conv.To_int.byte) %> Colormap_state_enum.of_int)
            buf in
        Decode.pad buf 2; { window; colormap; new_; state }) : Event.Colormap_notify.t)
    let decode_client_message_data_format_variant buf ~tag  =
      (match tag with
       | 8 ->
           let data8 = (Decode.list ~item:Decode.u8) ~len:20 buf in
           `Data8 data8
       | 16 ->
           let data16 = (Decode.list ~item:Decode.u16) ~len:10 buf in
           `Data16 data16
       | 32 ->
           let data32 = (Decode.list ~item:Decode.u32) ~len:5 buf in
           `Data32 data32
       | n -> invalid_arg ("Invalid enum value: " ^ (string_of_int n)) : 
      Client_message_data_format.t)
    let decode_client_message_event buf =
      (Decode.pad buf 1;
       (let data_tag = Decode.u8 buf in
        Decode.pad buf 2;
        (let window = Decode.xid buf in
         let type_ = Decode.xid buf in
         let data =
           decode_client_message_data_format_variant ~tag:data_tag buf in
         { window; type_; data })) : Event.Client_message.t)
    let decode_mapping_notify_event buf =
      (Decode.pad buf 4;
       (let request =
          ((Decode.byte %> Conv.To_int.byte) %> Mapping_enum.of_int) buf in
        let first_keycode = Decode.u8 buf in
        let count = Decode.u8 buf in
        Decode.pad buf 1; { request; first_keycode; count }) : Event.Mapping_notify.t)
    let decode_ge_generic_event buf =
      (Decode.pad buf 25; () : Event.Ge_generic.t)
    let decode_request_error buf =
      (let bad_value = Decode.u32 buf in
       Decode.pad buf 4; Decode.align buf 32; bad_value : Error.Request.t)
    let decode_value_error buf =
      (let bad_value = Decode.u32 buf in
       Decode.pad buf 4; Decode.align buf 32; bad_value : Error.Value.t)
    let decode_window_error = decode_value_error
    let decode_pixmap_error = decode_value_error
    let decode_atom_error = decode_value_error
    let decode_cursor_error = decode_value_error
    let decode_font_error = decode_value_error
    let decode_match_error = decode_request_error
    let decode_drawable_error = decode_value_error
    let decode_access_error = decode_request_error
    let decode_alloc_error = decode_request_error
    let decode_colormap_error = decode_value_error
    let decode_g_context_error = decode_value_error
    let decode_id_choice_error = decode_value_error
    let decode_name_error = decode_request_error
    let decode_length_error = decode_request_error
    let decode_implementation_error = decode_request_error
    let decode_get_window_attributes_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let backing_store =
          ((Decode.u8 %> Conv.To_int.u8) %> Backing_store_enum.of_int) buf in
        Decode.pad buf 6;
        (let visual = Decode.u32 buf in
         let class_ =
           ((Decode.u16 %> Conv.To_int.u16) %> Window_class_enum.of_int) buf in
         let bit_gravity =
           ((Decode.u8 %> Conv.To_int.u8) %> Bit_gravity_enum.of_int) buf in
         let win_gravity =
           ((Decode.u8 %> Conv.To_int.u8) %> Win_gravity_enum.of_int) buf in
         let backing_planes = Decode.u32 buf in
         let backing_pixel = Decode.u32 buf in
         let save_under = Decode.bool buf in
         let map_is_installed = Decode.bool buf in
         let map_state =
           ((Decode.u8 %> Conv.To_int.u8) %> Map_state_enum.of_int) buf in
         let override_redirect = Decode.bool buf in
         let colormap =
           (Decode.xid %>
              (Conv.alt_enum ~enum_of_int:Colormap_enum.of_int
                 ~int_of_t:Conv.To_int.xid)) buf in
         let all_event_masks =
           ((Decode.u32 %> Conv.To_i32.u32) %> Event_mask.of_int32) buf in
         let your_event_mask =
           ((Decode.u32 %> Conv.To_i32.u32) %> Event_mask.of_int32) buf in
         let do_not_propagate_mask =
           ((Decode.u16 %> Conv.To_i32.u16) %> Event_mask.of_int32) buf in
         Decode.pad buf 2;
         {
           backing_store;
           visual;
           class_;
           bit_gravity;
           win_gravity;
           backing_planes;
           backing_pixel;
           save_under;
           map_is_installed;
           map_state;
           override_redirect;
           colormap;
           all_event_masks;
           your_event_mask;
           do_not_propagate_mask
         })) : Get_window_attributes.Reply.t)
    let decode_get_geometry_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let depth = Decode.u8 buf in
        Decode.pad buf 6;
        (let root = Decode.xid buf in
         let x = Decode.i16 buf in
         let y = Decode.i16 buf in
         let width = Decode.u16 buf in
         let height = Decode.u16 buf in
         let border_width = Decode.u16 buf in
         Decode.pad buf 2; { depth; root; x; y; width; height; border_width })) : 
      Get_geometry.Reply.t)
    let decode_query_tree_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let root = Decode.xid buf in
        let parent =
          (Decode.xid %>
             (Conv.alt_enum ~enum_of_int:Window_enum.of_int
                ~int_of_t:Conv.To_int.xid)) buf in
        let children_len = Decode.u16 buf in
        Decode.pad buf 14;
        (let children = (Decode.list ~item:Decode.xid) ~len:children_len buf in
         { root; parent; children })) : Query_tree.Reply.t)
    let decode_intern_atom_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let atom =
          (Decode.xid %>
             (Conv.alt_enum ~enum_of_int:Atom_enum.of_int
                ~int_of_t:Conv.To_int.xid)) buf in
        atom) : Intern_atom.Reply.t)
    let decode_get_atom_name_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let name_len = Decode.u16 buf in
        Decode.pad buf 22;
        (let name = Decode.string ~len:name_len buf in name)) : Get_atom_name.Reply.t)
    let decode_get_property_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let format = Decode.u8 buf in
        Decode.pad buf 6;
        (let type_ = Decode.xid buf in
         let bytes_after = Decode.u32 buf in
         let value_len = Decode.u32 buf in
         Decode.pad buf 12;
         (let value = Decode.string ~len:(value_len * (format / 8)) buf in
          { format; type_; bytes_after; value_len; value }))) : Get_property.Reply.t)
    let decode_list_properties_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let atoms_len = Decode.u16 buf in
        Decode.pad buf 22;
        (let atoms = (Decode.list ~item:Decode.xid) ~len:atoms_len buf in
         atoms)) : List_properties.Reply.t)
    let decode_get_selection_owner_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let owner =
          (Decode.xid %>
             (Conv.alt_enum ~enum_of_int:Window_enum.of_int
                ~int_of_t:Conv.To_int.xid)) buf in
        owner) : Get_selection_owner.Reply.t)
    let decode_grab_pointer_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let status =
          ((Decode.byte %> Conv.To_int.byte) %> Grab_status_enum.of_int) buf in
        Decode.pad buf 6; status) : Grab_pointer.Reply.t)
    let decode_grab_keyboard_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let status =
          ((Decode.byte %> Conv.To_int.byte) %> Grab_status_enum.of_int) buf in
        Decode.pad buf 6; status) : Grab_keyboard.Reply.t)
    let decode_query_pointer_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let same_screen = Decode.bool buf in
        Decode.pad buf 6;
        (let root = Decode.xid buf in
         let child =
           (Decode.xid %>
              (Conv.alt_enum ~enum_of_int:Window_enum.of_int
                 ~int_of_t:Conv.To_int.xid)) buf in
         let root_x = Decode.i16 buf in
         let root_y = Decode.i16 buf in
         let win_x = Decode.i16 buf in
         let win_y = Decode.i16 buf in
         let mask =
           ((Decode.u16 %> Conv.To_i32.u16) %> Key_but_mask.of_int32) buf in
         Decode.pad buf 2;
         { same_screen; root; child; root_x; root_y; win_x; win_y; mask })) : 
      Query_pointer.Reply.t)
    let decode_timecoord buf =
      (let time = Decode.u32 buf in
       let x = Decode.i16 buf in let y = Decode.i16 buf in { time; x; y } : 
      timecoord)
    let decode_get_motion_events_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let events_len = Decode.u32 buf in
        Decode.pad buf 20;
        (let events =
           (Decode.list ~item:decode_timecoord) ~len:events_len buf in
         events)) : Get_motion_events.Reply.t)
    let decode_translate_coordinates_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let same_screen = Decode.bool buf in
        Decode.pad buf 6;
        (let child =
           (Decode.xid %>
              (Conv.alt_enum ~enum_of_int:Window_enum.of_int
                 ~int_of_t:Conv.To_int.xid)) buf in
         let dst_x = Decode.i16 buf in
         let dst_y = Decode.i16 buf in { same_screen; child; dst_x; dst_y })) : 
      Translate_coordinates.Reply.t)
    let decode_get_input_focus_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let revert_to =
          ((Decode.u8 %> Conv.To_int.u8) %> Input_focus_enum.of_int) buf in
        Decode.pad buf 6;
        (let focus =
           (Decode.xid %>
              (Conv.alt_enum ~enum_of_int:Input_focus_enum.of_int
                 ~int_of_t:Conv.To_int.xid)) buf in
         { revert_to; focus })) : Get_input_focus.Reply.t)
    let decode_query_keymap_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let keys = (Decode.list ~item:Decode.u8) ~len:32 buf in keys) : 
      Query_keymap.Reply.t)
    let decode_fontprop buf =
      (let name = Decode.xid buf in
       let value = Decode.u32 buf in { name; value } : fontprop)
    let decode_charinfo buf =
      (let left_side_bearing = Decode.i16 buf in
       let right_side_bearing = Decode.i16 buf in
       let character_width = Decode.i16 buf in
       let ascent = Decode.i16 buf in
       let descent = Decode.i16 buf in
       let attributes = Decode.u16 buf in
       {
         left_side_bearing;
         right_side_bearing;
         character_width;
         ascent;
         descent;
         attributes
       } : charinfo)
    let decode_query_font_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let min_bounds = decode_charinfo buf in
        Decode.pad buf 4;
        (let max_bounds = decode_charinfo buf in
         Decode.pad buf 4;
         (let min_char_or_byte2 = Decode.u16 buf in
          let max_char_or_byte2 = Decode.u16 buf in
          let default_char = Decode.u16 buf in
          let properties_len = Decode.u16 buf in
          let draw_direction =
            ((Decode.byte %> Conv.To_int.byte) %> Font_draw_enum.of_int) buf in
          let min_byte1 = Decode.u8 buf in
          let max_byte1 = Decode.u8 buf in
          let all_chars_exist = Decode.bool buf in
          let font_ascent = Decode.i16 buf in
          let font_descent = Decode.i16 buf in
          let char_infos_len = Decode.u32 buf in
          let properties =
            (Decode.list ~item:decode_fontprop) ~len:properties_len buf in
          let char_infos =
            (Decode.list ~item:decode_charinfo) ~len:char_infos_len buf in
          {
            min_bounds;
            max_bounds;
            min_char_or_byte2;
            max_char_or_byte2;
            default_char;
            draw_direction;
            min_byte1;
            max_byte1;
            all_chars_exist;
            font_ascent;
            font_descent;
            properties;
            char_infos
          }))) : Query_font.Reply.t)
    let decode_query_text_extents_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let draw_direction =
          ((Decode.byte %> Conv.To_int.byte) %> Font_draw_enum.of_int) buf in
        Decode.pad buf 6;
        (let font_ascent = Decode.i16 buf in
         let font_descent = Decode.i16 buf in
         let overall_ascent = Decode.i16 buf in
         let overall_descent = Decode.i16 buf in
         let overall_width = Decode.i32 buf in
         let overall_left = Decode.i32 buf in
         let overall_right = Decode.i32 buf in
         {
           draw_direction;
           font_ascent;
           font_descent;
           overall_ascent;
           overall_descent;
           overall_width;
           overall_left;
           overall_right
         })) : Query_text_extents.Reply.t)
    let decode_str buf =
      (let name_len = Decode.u8 buf in
       let name = Decode.string ~len:name_len buf in name : str)
    let decode_list_fonts_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let names_len = Decode.u16 buf in
        Decode.pad buf 22;
        (let names = (Decode.list ~item:decode_str) ~len:names_len buf in
         names)) : List_fonts.Reply.t)
    let decode_list_fonts_with_info_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let name_len = Decode.u8 buf in
        Decode.pad buf 6;
        (let min_bounds = decode_charinfo buf in
         Decode.pad buf 4;
         (let max_bounds = decode_charinfo buf in
          Decode.pad buf 4;
          (let min_char_or_byte2 = Decode.u16 buf in
           let max_char_or_byte2 = Decode.u16 buf in
           let default_char = Decode.u16 buf in
           let properties_len = Decode.u16 buf in
           let draw_direction =
             ((Decode.byte %> Conv.To_int.byte) %> Font_draw_enum.of_int) buf in
           let min_byte1 = Decode.u8 buf in
           let max_byte1 = Decode.u8 buf in
           let all_chars_exist = Decode.bool buf in
           let font_ascent = Decode.i16 buf in
           let font_descent = Decode.i16 buf in
           let replies_hint = Decode.u32 buf in
           let properties =
             (Decode.list ~item:decode_fontprop) ~len:properties_len buf in
           let name = Decode.string ~len:name_len buf in
           {
             min_bounds;
             max_bounds;
             min_char_or_byte2;
             max_char_or_byte2;
             default_char;
             draw_direction;
             min_byte1;
             max_byte1;
             all_chars_exist;
             font_ascent;
             font_descent;
             replies_hint;
             properties;
             name
           })))) : List_fonts_with_info.Reply.t)
    let decode_get_font_path_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let path_len = Decode.u16 buf in
        Decode.pad buf 22;
        (let path = (Decode.list ~item:decode_str) ~len:path_len buf in path)) : 
      Get_font_path.Reply.t)
    let decode_segment buf =
      (let x1 = Decode.i16 buf in
       let y1 = Decode.i16 buf in
       let x2 = Decode.i16 buf in
       let y2 = Decode.i16 buf in { x1; y1; x2; y2 } : segment)
    let decode_get_image_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let depth = Decode.u8 buf in
        Decode.pad buf 6;
        (let visual = Decode.u32 buf in
         Decode.pad buf 20;
         (let data = (Decode.list ~item:Decode.byte) ~len:(length * 4) buf in
          { depth; visual; data }))) : Get_image.Reply.t)
    let decode_list_installed_colormaps_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let cmaps_len = Decode.u16 buf in
        Decode.pad buf 22;
        (let cmaps = (Decode.list ~item:Decode.xid) ~len:cmaps_len buf in
         cmaps)) : List_installed_colormaps.Reply.t)
    let decode_alloc_color_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let red = Decode.u16 buf in
        let green = Decode.u16 buf in
        let blue = Decode.u16 buf in
        Decode.pad buf 2;
        (let pixel = Decode.u32 buf in { red; green; blue; pixel })) : 
      Alloc_color.Reply.t)
    let decode_alloc_named_color_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let pixel = Decode.u32 buf in
        let exact_red = Decode.u16 buf in
        let exact_green = Decode.u16 buf in
        let exact_blue = Decode.u16 buf in
        let visual_red = Decode.u16 buf in
        let visual_green = Decode.u16 buf in
        let visual_blue = Decode.u16 buf in
        {
          pixel;
          exact_red;
          exact_green;
          exact_blue;
          visual_red;
          visual_green;
          visual_blue
        }) : Alloc_named_color.Reply.t)
    let decode_alloc_color_cells_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let pixels_len = Decode.u16 buf in
        let masks_len = Decode.u16 buf in
        Decode.pad buf 20;
        (let pixels = (Decode.list ~item:Decode.u32) ~len:pixels_len buf in
         let masks = (Decode.list ~item:Decode.u32) ~len:masks_len buf in
         { pixels; masks })) : Alloc_color_cells.Reply.t)
    let decode_alloc_color_planes_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let pixels_len = Decode.u16 buf in
        Decode.pad buf 2;
        (let red_mask = Decode.u32 buf in
         let green_mask = Decode.u32 buf in
         let blue_mask = Decode.u32 buf in
         Decode.pad buf 8;
         (let pixels = (Decode.list ~item:Decode.u32) ~len:pixels_len buf in
          { red_mask; green_mask; blue_mask; pixels }))) : Alloc_color_planes.Reply.t)
    let decode_coloritem buf =
      (let pixel = Decode.u32 buf in
       let red = Decode.u16 buf in
       let green = Decode.u16 buf in
       let blue = Decode.u16 buf in
       let flags =
         ((Decode.byte %> Conv.To_i32.byte) %> Color_flag_mask.of_int32) buf in
       Decode.pad buf 1; { pixel; red; green; blue; flags } : coloritem)
    let decode_rgb buf =
      (let red = Decode.u16 buf in
       let green = Decode.u16 buf in
       let blue = Decode.u16 buf in Decode.pad buf 2; { red; green; blue } : 
      rgb)
    let decode_query_colors_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let colors_len = Decode.u16 buf in
        Decode.pad buf 22;
        (let colors = (Decode.list ~item:decode_rgb) ~len:colors_len buf in
         colors)) : Query_colors.Reply.t)
    let decode_lookup_color_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let exact_red = Decode.u16 buf in
        let exact_green = Decode.u16 buf in
        let exact_blue = Decode.u16 buf in
        let visual_red = Decode.u16 buf in
        let visual_green = Decode.u16 buf in
        let visual_blue = Decode.u16 buf in
        {
          exact_red;
          exact_green;
          exact_blue;
          visual_red;
          visual_green;
          visual_blue
        }) : Lookup_color.Reply.t)
    let decode_query_best_size_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let width = Decode.u16 buf in
        let height = Decode.u16 buf in { width; height }) : Query_best_size.Reply.t)
    let decode_query_extension_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let present = Decode.bool buf in
        let major_opcode = Decode.u8 buf in
        let first_event = Decode.u8 buf in
        let first_error = Decode.u8 buf in
        { present; major_opcode; first_event; first_error }) : Query_extension.Reply.t)
    let decode_list_extensions_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let names_len = Decode.u8 buf in
        Decode.pad buf 30;
        (let names = (Decode.list ~item:decode_str) ~len:names_len buf in
         names)) : List_extensions.Reply.t)
    let decode_get_keyboard_mapping_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let keysyms_per_keycode = Decode.byte buf in
        Decode.pad buf 30;
        (let keysyms = (Decode.list ~item:Decode.u32) ~len:length buf in
         { keysyms_per_keycode; keysyms })) : Get_keyboard_mapping.Reply.t)
    let decode_get_keyboard_control_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let global_auto_repeat =
          ((Decode.byte %> Conv.To_int.byte) %> Auto_repeat_mode_enum.of_int)
            buf in
        Decode.pad buf 6;
        (let led_mask = Decode.u32 buf in
         let key_click_percent = Decode.u8 buf in
         let bell_percent = Decode.u8 buf in
         let bell_pitch = Decode.u16 buf in
         let bell_duration = Decode.u16 buf in
         Decode.pad buf 2;
         (let auto_repeats = (Decode.list ~item:Decode.u8) ~len:32 buf in
          {
            global_auto_repeat;
            led_mask;
            key_click_percent;
            bell_percent;
            bell_pitch;
            bell_duration;
            auto_repeats
          }))) : Get_keyboard_control.Reply.t)
    let decode_get_pointer_control_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let acceleration_numerator = Decode.u16 buf in
        let acceleration_denominator = Decode.u16 buf in
        let threshold = Decode.u16 buf in
        Decode.pad buf 18;
        { acceleration_numerator; acceleration_denominator; threshold }) : 
      Get_pointer_control.Reply.t)
    let decode_get_screen_saver_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let timeout = Decode.u16 buf in
        let interval = Decode.u16 buf in
        let prefer_blanking =
          ((Decode.byte %> Conv.To_int.byte) %> Blanking_enum.of_int) buf in
        let allow_exposures =
          ((Decode.byte %> Conv.To_int.byte) %> Exposures_enum.of_int) buf in
        Decode.pad buf 18;
        { timeout; interval; prefer_blanking; allow_exposures }) : Get_screen_saver.Reply.t)
    let decode_host buf =
      (let family = ((Decode.u8 %> Conv.To_int.u8) %> Family_enum.of_int) buf in
       Decode.pad buf 1;
       (let address_len = Decode.u16 buf in
        let address = (Decode.list ~item:Decode.byte) ~len:address_len buf in
        Decode.align buf 4; { family; address }) : host)
    let decode_list_hosts_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let mode =
          ((Decode.byte %> Conv.To_int.byte) %> Access_control_enum.of_int)
            buf in
        Decode.pad buf 6;
        (let hosts_len = Decode.u16 buf in
         Decode.pad buf 22;
         (let hosts = (Decode.list ~item:decode_host) ~len:hosts_len buf in
          { mode; hosts }))) : List_hosts.Reply.t)
    let decode_set_pointer_mapping_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let status =
          ((Decode.byte %> Conv.To_int.byte) %> Mapping_status_enum.of_int)
            buf in
        Decode.pad buf 6; status) : Set_pointer_mapping.Reply.t)
    let decode_get_pointer_mapping_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let map_len = Decode.u8 buf in
        Decode.pad buf 30;
        (let map = (Decode.list ~item:Decode.u8) ~len:map_len buf in map)) : 
      Get_pointer_mapping.Reply.t)
    let decode_set_modifier_mapping_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let status =
          ((Decode.byte %> Conv.To_int.byte) %> Mapping_status_enum.of_int)
            buf in
        Decode.pad buf 6; status) : Set_modifier_mapping.Reply.t)
    let decode_get_modifier_mapping_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let keycodes_per_modifier = Decode.u8 buf in
        let keycodes_per_modifier = keycodes_per_modifier / 8 in
        Decode.pad buf 30;
        (let keycodes =
           (Decode.list ~item:Decode.u8) ~len:keycodes_per_modifier buf in
         keycodes)) : Get_modifier_mapping.Reply.t)
    let decode_error ~number  buf =
      (Decode.pad buf 4;
       (match number with
        | 1 -> let error = decode_request_error buf in `Request error
        | 2 -> let error = decode_value_error buf in `Value error
        | 3 -> let error = decode_window_error buf in `Window error
        | 4 -> let error = decode_pixmap_error buf in `Pixmap error
        | 5 -> let error = decode_atom_error buf in `Atom error
        | 6 -> let error = decode_cursor_error buf in `Cursor error
        | 7 -> let error = decode_font_error buf in `Font error
        | 8 -> let error = decode_match_error buf in `Match_ error
        | 9 -> let error = decode_drawable_error buf in `Drawable error
        | 10 -> let error = decode_access_error buf in `Access error
        | 11 -> let error = decode_alloc_error buf in `Alloc error
        | 12 -> let error = decode_colormap_error buf in `Colormap error
        | 13 -> let error = decode_g_context_error buf in `G_context error
        | 14 -> let error = decode_id_choice_error buf in `Id_choice error
        | 15 -> let error = decode_name_error buf in `Name error
        | 16 -> let error = decode_length_error buf in `Length error
        | 17 ->
            let error = decode_implementation_error buf in
            `Implementation error
        | n -> invalid_arg ("Invalid error number: " ^ (string_of_int n))) : 
      Error.t)
  end
module Bigreq_codec =
  struct
    open Protocol.Bigreq
    let decode_enable_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let maximum_request_length = Decode.u32 buf in maximum_request_length) : 
      Enable.Reply.t)
    let decode_error ~number  buf =
      (Decode.pad buf 4;
       (match number with
        | n -> invalid_arg ("Invalid error number: " ^ (string_of_int n))) : 
      Error.t)
  end
module Render_codec =
  struct
    open Protocol.Render
    let decode_pict_format_error buf =
      (Decode.align buf 32; () : Error.Pict_format.t)
    let decode_picture_error buf =
      (Decode.align buf 32; () : Error.Picture.t)
    let decode_pict_op_error buf =
      (Decode.align buf 32; () : Error.Pict_op.t)
    let decode_glyph_set_error buf =
      (Decode.align buf 32; () : Error.Glyph_set.t)
    let decode_glyph_error buf = (Decode.align buf 32; () : Error.Glyph.t)
    let decode_directformat buf =
      (let red_shift = Decode.u16 buf in
       let red_mask = Decode.u16 buf in
       let green_shift = Decode.u16 buf in
       let green_mask = Decode.u16 buf in
       let blue_shift = Decode.u16 buf in
       let blue_mask = Decode.u16 buf in
       let alpha_shift = Decode.u16 buf in
       let alpha_mask = Decode.u16 buf in
       {
         red_shift;
         red_mask;
         green_shift;
         green_mask;
         blue_shift;
         blue_mask;
         alpha_shift;
         alpha_mask
       } : directformat)
    let decode_pictforminfo buf =
      (let id = Decode.xid buf in
       let type_ =
         ((Decode.u8 %> Conv.To_int.u8) %> Pict_type_enum.of_int) buf in
       let depth = Decode.u8 buf in
       Decode.pad buf 2;
       (let direct = decode_directformat buf in
        let colormap = Decode.xid buf in
        { id; type_; depth; direct; colormap }) : pictforminfo)
    let decode_pictvisual buf =
      (let visual = Decode.u32 buf in
       let format = Decode.xid buf in { visual; format } : pictvisual)
    let decode_pictdepth buf =
      (let depth = Decode.u8 buf in
       Decode.pad buf 1;
       (let num_visuals = Decode.u16 buf in
        Decode.pad buf 4;
        (let visuals =
           (Decode.list ~item:decode_pictvisual) ~len:num_visuals buf in
         { depth; visuals })) : pictdepth)
    let decode_pictscreen buf =
      (let num_depths = Decode.u32 buf in
       let fallback = Decode.xid buf in
       let depths = (Decode.list ~item:decode_pictdepth) ~len:num_depths buf in
       { fallback; depths } : pictscreen)
    let decode_indexvalue buf =
      (let pixel = Decode.u32 buf in
       let red = Decode.u16 buf in
       let green = Decode.u16 buf in
       let blue = Decode.u16 buf in
       let alpha = Decode.u16 buf in { pixel; red; green; blue; alpha } : 
      indexvalue)
    let decode_color buf =
      (let red = Decode.u16 buf in
       let green = Decode.u16 buf in
       let blue = Decode.u16 buf in
       let alpha = Decode.u16 buf in { red; green; blue; alpha } : color)
    let decode_pointfix buf =
      (let x = Decode.i32 buf in let y = Decode.i32 buf in { x; y } : 
      pointfix)
    let decode_linefix buf =
      (let p1 = decode_pointfix buf in
       let p2 = decode_pointfix buf in { p1; p2 } : linefix)
    let decode_triangle buf =
      (let p1 = decode_pointfix buf in
       let p2 = decode_pointfix buf in
       let p3 = decode_pointfix buf in { p1; p2; p3 } : triangle)
    let decode_trapezoid buf =
      (let top = Decode.i32 buf in
       let bottom = Decode.i32 buf in
       let left = decode_linefix buf in
       let right = decode_linefix buf in { top; bottom; left; right } : 
      trapezoid)
    let decode_glyphinfo buf =
      (let width = Decode.u16 buf in
       let height = Decode.u16 buf in
       let x = Decode.i16 buf in
       let y = Decode.i16 buf in
       let x_off = Decode.i16 buf in
       let y_off = Decode.i16 buf in { width; height; x; y; x_off; y_off } : 
      glyphinfo)
    let decode_query_version_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let major_version = Decode.u32 buf in
        let minor_version = Decode.u32 buf in
        Decode.pad buf 16; { major_version; minor_version }) : Query_version.Reply.t)
    let decode_query_pict_formats_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let num_formats = Decode.u32 buf in
        let num_screens = Decode.u32 buf in
        let num_depths = Decode.u32 buf in
        let num_visuals = Decode.u32 buf in
        let num_subpixel = Decode.u32 buf in
        Decode.pad buf 4;
        (let formats =
           (Decode.list ~item:decode_pictforminfo) ~len:num_formats buf in
         let screens =
           (Decode.list ~item:decode_pictscreen) ~len:num_screens buf in
         let subpixels =
           (Decode.list
              ~item:((Decode.u32 %> Conv.To_int.u32) %> Sub_pixel_enum.of_int))
             ~len:num_subpixel buf in
         { num_depths; num_visuals; formats; screens; subpixels })) : 
      Query_pict_formats.Reply.t)
    let decode_query_pict_index_values_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let num_values = Decode.u32 buf in
        Decode.pad buf 20;
        (let values =
           (Decode.list ~item:decode_indexvalue) ~len:num_values buf in
         values)) : Query_pict_index_values.Reply.t)
    let decode_transform buf =
      (let matrix11 = Decode.i32 buf in
       let matrix12 = Decode.i32 buf in
       let matrix13 = Decode.i32 buf in
       let matrix21 = Decode.i32 buf in
       let matrix22 = Decode.i32 buf in
       let matrix23 = Decode.i32 buf in
       let matrix31 = Decode.i32 buf in
       let matrix32 = Decode.i32 buf in
       let matrix33 = Decode.i32 buf in
       {
         matrix11;
         matrix12;
         matrix13;
         matrix21;
         matrix22;
         matrix23;
         matrix31;
         matrix32;
         matrix33
       } : transform)
    let decode_query_filters_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let num_aliases = Decode.u32 buf in
        let num_filters = Decode.u32 buf in
        Decode.pad buf 16;
        (let aliases = (Decode.list ~item:Decode.u16) ~len:num_aliases buf in
         let filters =
           (Decode.list ~item:Core_codec.decode_str) ~len:num_filters buf in
         { aliases; filters })) : Query_filters.Reply.t)
    let decode_animcursorelt buf =
      (let cursor = Decode.xid buf in
       let delay = Decode.u32 buf in { cursor; delay } : animcursorelt)
    let decode_spanfix buf =
      (let l = Decode.i32 buf in
       let r = Decode.i32 buf in let y = Decode.i32 buf in { l; r; y } : 
      spanfix)
    let decode_trap buf =
      (let top = decode_spanfix buf in
       let bot = decode_spanfix buf in { top; bot } : trap)
    let decode_error ~number  buf =
      (Decode.pad buf 4;
       (match number with
        | 0 -> let error = decode_pict_format_error buf in `Pict_format error
        | 1 -> let error = decode_picture_error buf in `Picture error
        | 2 -> let error = decode_pict_op_error buf in `Pict_op error
        | 3 -> let error = decode_glyph_set_error buf in `Glyph_set error
        | 4 -> let error = decode_glyph_error buf in `Glyph error
        | n -> invalid_arg ("Invalid error number: " ^ (string_of_int n))) : 
      Error.t)
  end
module Shape_codec =
  struct
    open Protocol.Shape
    let decode_notify_event buf =
      (Decode.pad buf 1;
       (let shape_kind =
          ((Decode.u8 %> Conv.To_int.u8) %> Sk_enum.of_int) buf in
        Decode.pad buf 2;
        (let affected_window = Decode.xid buf in
         let extents_x = Decode.i16 buf in
         let extents_y = Decode.i16 buf in
         let extents_width = Decode.u16 buf in
         let extents_height = Decode.u16 buf in
         let server_time = Decode.u32 buf in
         let shaped = Decode.bool buf in
         Decode.pad buf 11;
         {
           shape_kind;
           affected_window;
           extents_x;
           extents_y;
           extents_width;
           extents_height;
           server_time;
           shaped
         })) : Event.Notify.t)
    let decode_query_version_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let major_version = Decode.u16 buf in
        let minor_version = Decode.u16 buf in
        { major_version; minor_version }) : Query_version.Reply.t)
    let decode_query_extents_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let bounding_shaped = Decode.bool buf in
        let clip_shaped = Decode.bool buf in
        Decode.pad buf 2;
        (let bounding_shape_extents_x = Decode.i16 buf in
         let bounding_shape_extents_y = Decode.i16 buf in
         let bounding_shape_extents_width = Decode.u16 buf in
         let bounding_shape_extents_height = Decode.u16 buf in
         let clip_shape_extents_x = Decode.i16 buf in
         let clip_shape_extents_y = Decode.i16 buf in
         let clip_shape_extents_width = Decode.u16 buf in
         let clip_shape_extents_height = Decode.u16 buf in
         {
           bounding_shaped;
           clip_shaped;
           bounding_shape_extents_x;
           bounding_shape_extents_y;
           bounding_shape_extents_width;
           bounding_shape_extents_height;
           clip_shape_extents_x;
           clip_shape_extents_y;
           clip_shape_extents_width;
           clip_shape_extents_height
         })) : Query_extents.Reply.t)
    let decode_input_selected_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let enabled = Decode.bool buf in Decode.pad buf 6; enabled) : 
      Input_selected.Reply.t)
    let decode_get_rectangles_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let ordering =
          ((Decode.byte %> Conv.To_int.byte) %>
             Core.Clip_ordering_enum.of_int) buf in
        Decode.pad buf 6;
        (let rectangles_len = Decode.u32 buf in
         Decode.pad buf 20;
         (let rectangles =
            (Decode.list ~item:Core_codec.decode_rectangle)
              ~len:rectangles_len buf in
          { ordering; rectangles }))) : Get_rectangles.Reply.t)
    let decode_error ~number  buf =
      (Decode.pad buf 4;
       (match number with
        | n -> invalid_arg ("Invalid error number: " ^ (string_of_int n))) : 
      Error.t)
  end
module Xfixes_codec =
  struct
    open Protocol.Xfixes
    let decode_query_version_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let major_version = Decode.u32 buf in
        let minor_version = Decode.u32 buf in
        Decode.pad buf 16; { major_version; minor_version }) : Query_version.Reply.t)
    let decode_selection_notify_event buf =
      (Decode.pad buf 1;
       (let subtype =
          ((Decode.u8 %> Conv.To_int.u8) %> Selection_event_enum.of_int) buf in
        Decode.pad buf 2;
        (let window = Decode.xid buf in
         let owner = Decode.xid buf in
         let selection = Decode.xid buf in
         let timestamp = Decode.u32 buf in
         let selection_timestamp = Decode.u32 buf in
         Decode.pad buf 8;
         { subtype; window; owner; selection; timestamp; selection_timestamp
         })) : Event.Selection_notify.t)
    let decode_cursor_notify_event buf =
      (Decode.pad buf 1;
       (let subtype =
          ((Decode.u8 %> Conv.To_int.u8) %> Cursor_notify_enum.of_int) buf in
        Decode.pad buf 2;
        (let window = Decode.xid buf in
         let cursor_serial = Decode.u32 buf in
         let timestamp = Decode.u32 buf in
         let name =
           (Decode.xid %>
              (Conv.alt_enum ~enum_of_int:Core.Atom_enum.of_int
                 ~int_of_t:Conv.To_int.xid)) buf in
         Decode.pad buf 12;
         { subtype; window; cursor_serial; timestamp; name })) : Event.Cursor_notify.t)
    let decode_get_cursor_image_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let x = Decode.i16 buf in
        let y = Decode.i16 buf in
        let width = Decode.u16 buf in
        let height = Decode.u16 buf in
        let xhot = Decode.u16 buf in
        let yhot = Decode.u16 buf in
        let cursor_serial = Decode.u32 buf in
        Decode.pad buf 8;
        (let cursor_image =
           (Decode.list ~item:Decode.u32) ~len:(width * height) buf in
         { x; y; width; height; xhot; yhot; cursor_serial; cursor_image })) : 
      Get_cursor_image.Reply.t)
    let decode_bad_region_error buf =
      (Decode.align buf 32; () : Error.Bad_region.t)
    let decode_fetch_region_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let extents = Core_codec.decode_rectangle buf in
        Decode.pad buf 16;
        (let rectangles =
           (Decode.list ~item:Core_codec.decode_rectangle) ~len:(length / 2)
             buf in
         { extents; rectangles })) : Fetch_region.Reply.t)
    let decode_get_cursor_name_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let atom =
          (Decode.xid %>
             (Conv.alt_enum ~enum_of_int:Core.Atom_enum.of_int
                ~int_of_t:Conv.To_int.xid)) buf in
        let nbytes = Decode.u16 buf in
        Decode.pad buf 18;
        (let name = Decode.string ~len:nbytes buf in { atom; name })) : 
      Get_cursor_name.Reply.t)
    let decode_get_cursor_image_and_name_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let x = Decode.i16 buf in
        let y = Decode.i16 buf in
        let width = Decode.u16 buf in
        let height = Decode.u16 buf in
        let xhot = Decode.u16 buf in
        let yhot = Decode.u16 buf in
        let cursor_serial = Decode.u32 buf in
        let cursor_atom =
          (Decode.xid %>
             (Conv.alt_enum ~enum_of_int:Core.Atom_enum.of_int
                ~int_of_t:Conv.To_int.xid)) buf in
        let nbytes = Decode.u16 buf in
        Decode.pad buf 2;
        (let cursor_image =
           (Decode.list ~item:Decode.u32) ~len:(width * height) buf in
         let name = Decode.string ~len:nbytes buf in
         {
           x;
           y;
           width;
           height;
           xhot;
           yhot;
           cursor_serial;
           cursor_atom;
           cursor_image;
           name
         })) : Get_cursor_image_and_name.Reply.t)
    let decode_error ~number  buf =
      (Decode.pad buf 4;
       (match number with
        | 0 -> let error = decode_bad_region_error buf in `Bad_region error
        | n -> invalid_arg ("Invalid error number: " ^ (string_of_int n))) : 
      Error.t)
  end
module Composite_codec =
  struct
    open Protocol.Composite
    let decode_query_version_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let major_version = Decode.u32 buf in
        let minor_version = Decode.u32 buf in
        Decode.pad buf 16; { major_version; minor_version }) : Query_version.Reply.t)
    let decode_get_overlay_window_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let overlay_win = Decode.xid buf in Decode.pad buf 20; overlay_win) : 
      Get_overlay_window.Reply.t)
    let decode_error ~number  buf =
      (Decode.pad buf 4;
       (match number with
        | n -> invalid_arg ("Invalid error number: " ^ (string_of_int n))) : 
      Error.t)
  end
module Damage_codec =
  struct
    open Protocol.Damage
    let decode_bad_damage_error buf =
      (Decode.align buf 32; () : Error.Bad_damage.t)
    let decode_query_version_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let major_version = Decode.u32 buf in
        let minor_version = Decode.u32 buf in
        Decode.pad buf 16; { major_version; minor_version }) : Query_version.Reply.t)
    let decode_notify_event buf =
      (Decode.pad buf 1;
       (let level =
          ((Decode.u8 %> Conv.To_int.u8) %> Report_level_enum.of_int) buf in
        Decode.pad buf 2;
        (let drawable = Decode.xid buf in
         let damage = Decode.xid buf in
         let timestamp = Decode.u32 buf in
         let area = Core_codec.decode_rectangle buf in
         let geometry = Core_codec.decode_rectangle buf in
         { level; drawable; damage; timestamp; area; geometry })) : Event.Notify.t)
    let decode_error ~number  buf =
      (Decode.pad buf 4;
       (match number with
        | 0 -> let error = decode_bad_damage_error buf in `Bad_damage error
        | n -> invalid_arg ("Invalid error number: " ^ (string_of_int n))) : 
      Error.t)
  end
module Dpms_codec =
  struct
    open Protocol.Dpms
    let decode_get_version_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let server_major_version = Decode.u16 buf in
        let server_minor_version = Decode.u16 buf in
        { server_major_version; server_minor_version }) : Get_version.Reply.t)
    let decode_capable_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let capable = Decode.bool buf in Decode.pad buf 23; capable) : 
      Capable.Reply.t)
    let decode_get_timeouts_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let standby_timeout = Decode.u16 buf in
        let suspend_timeout = Decode.u16 buf in
        let off_timeout = Decode.u16 buf in
        Decode.pad buf 18; { standby_timeout; suspend_timeout; off_timeout }) : 
      Get_timeouts.Reply.t)
    let decode_info_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let power_level =
          ((Decode.u16 %> Conv.To_int.u16) %> Dpms_mode_enum.of_int) buf in
        let state = Decode.bool buf in
        Decode.pad buf 21; { power_level; state }) : Info.Reply.t)
    let decode_error ~number  buf =
      (Decode.pad buf 4;
       (match number with
        | n -> invalid_arg ("Invalid error number: " ^ (string_of_int n))) : 
      Error.t)
  end
module Dri2_codec =
  struct
    open Protocol.Dri2
    let decode_dri2_buffer buf =
      (let attachment =
         ((Decode.u32 %> Conv.To_int.u32) %> Attachment_enum.of_int) buf in
       let name = Decode.u32 buf in
       let pitch = Decode.u32 buf in
       let cpp = Decode.u32 buf in
       let flags = Decode.u32 buf in { attachment; name; pitch; cpp; flags } : 
      dri2_buffer)
    let decode_attach_format buf =
      (let attachment =
         ((Decode.u32 %> Conv.To_int.u32) %> Attachment_enum.of_int) buf in
       let format = Decode.u32 buf in { attachment; format } : attach_format)
    let decode_query_version_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let major_version = Decode.u32 buf in
        let minor_version = Decode.u32 buf in
        { major_version; minor_version }) : Query_version.Reply.t)
    let decode_connect_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let driver_name_length = Decode.u32 buf in
        let device_name_length = Decode.u32 buf in
        Decode.pad buf 16;
        (let driver_name = Decode.string ~len:driver_name_length buf in
         let alignment_pad =
           Decode.string
             ~len:(((driver_name_length + 3) land (lnot 3)) -
                     driver_name_length) buf in
         let device_name = Decode.string ~len:device_name_length buf in
         { driver_name; alignment_pad; device_name })) : Connect.Reply.t)
    let decode_authenticate_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let authenticated = Decode.u32 buf in authenticated) : Authenticate.Reply.t)
    let decode_get_buffers_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let width = Decode.u32 buf in
        let height = Decode.u32 buf in
        let count = Decode.u32 buf in
        Decode.pad buf 12;
        (let buffers = (Decode.list ~item:decode_dri2_buffer) ~len:count buf in
         { width; height; buffers })) : Get_buffers.Reply.t)
    let decode_copy_region_reply ~length:(length : int)  buf =
      (Decode.pad buf 8; () : Copy_region.Reply.t)
    let decode_get_buffers_with_format_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let width = Decode.u32 buf in
        let height = Decode.u32 buf in
        let count = Decode.u32 buf in
        Decode.pad buf 12;
        (let buffers = (Decode.list ~item:decode_dri2_buffer) ~len:count buf in
         { width; height; buffers })) : Get_buffers_with_format.Reply.t)
    let decode_swap_buffers_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let swap_hi = Decode.u32 buf in
        let swap_lo = Decode.u32 buf in { swap_hi; swap_lo }) : Swap_buffers.Reply.t)
    let decode_get_msc_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let ust_hi = Decode.u32 buf in
        let ust_lo = Decode.u32 buf in
        let msc_hi = Decode.u32 buf in
        let msc_lo = Decode.u32 buf in
        let sbc_hi = Decode.u32 buf in
        let sbc_lo = Decode.u32 buf in
        { ust_hi; ust_lo; msc_hi; msc_lo; sbc_hi; sbc_lo }) : Get_msc.Reply.t)
    let decode_wait_msc_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let ust_hi = Decode.u32 buf in
        let ust_lo = Decode.u32 buf in
        let msc_hi = Decode.u32 buf in
        let msc_lo = Decode.u32 buf in
        let sbc_hi = Decode.u32 buf in
        let sbc_lo = Decode.u32 buf in
        { ust_hi; ust_lo; msc_hi; msc_lo; sbc_hi; sbc_lo }) : Wait_msc.Reply.t)
    let decode_wait_sbc_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let ust_hi = Decode.u32 buf in
        let ust_lo = Decode.u32 buf in
        let msc_hi = Decode.u32 buf in
        let msc_lo = Decode.u32 buf in
        let sbc_hi = Decode.u32 buf in
        let sbc_lo = Decode.u32 buf in
        { ust_hi; ust_lo; msc_hi; msc_lo; sbc_hi; sbc_lo }) : Wait_sbc.Reply.t)
    let decode_get_param_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let is_param_recognized = Decode.bool buf in
        Decode.pad buf 6;
        (let value_hi = Decode.u32 buf in
         let value_lo = Decode.u32 buf in
         { is_param_recognized; value_hi; value_lo })) : Get_param.Reply.t)
    let decode_buffer_swap_complete_event buf =
      (Decode.pad buf 4;
       (let event_type =
          ((Decode.u16 %> Conv.To_int.u16) %> Event_type_enum.of_int) buf in
        Decode.pad buf 2;
        (let drawable = Decode.xid buf in
         let ust_hi = Decode.u32 buf in
         let ust_lo = Decode.u32 buf in
         let msc_hi = Decode.u32 buf in
         let msc_lo = Decode.u32 buf in
         let sbc = Decode.u32 buf in
         { event_type; drawable; ust_hi; ust_lo; msc_hi; msc_lo; sbc })) : 
      Event.Buffer_swap_complete.t)
    let decode_invalidate_buffers_event buf =
      (Decode.pad buf 4; (let drawable = Decode.xid buf in drawable) : 
      Event.Invalidate_buffers.t)
    let decode_error ~number  buf =
      (Decode.pad buf 4;
       (match number with
        | n -> invalid_arg ("Invalid error number: " ^ (string_of_int n))) : 
      Error.t)
  end
module Dri3_codec =
  struct
    open Protocol.Dri3
    let decode_query_version_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let major_version = Decode.u32 buf in
        let minor_version = Decode.u32 buf in
        { major_version; minor_version }) : Query_version.Reply.t)
    let decode_open_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let nfd = Decode.u8 buf in
        Decode.pad buf 6;
        (let device_fd = Decode.file_descr buf in
         Decode.pad buf 24; { nfd; device_fd })) : Open_.Reply.t)
    let decode_buffer_from_pixmap_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let nfd = Decode.u8 buf in
        Decode.pad buf 6;
        (let size = Decode.u32 buf in
         let width = Decode.u16 buf in
         let height = Decode.u16 buf in
         let stride = Decode.u16 buf in
         let depth = Decode.u8 buf in
         let bpp = Decode.u8 buf in
         let pixmap_fd = Decode.file_descr buf in
         Decode.pad buf 12;
         { nfd; size; width; height; stride; depth; bpp; pixmap_fd })) : 
      Buffer_from_pixmap.Reply.t)
    let decode_fd_from_fence_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let nfd = Decode.u8 buf in
        Decode.pad buf 6;
        (let fence_fd = Decode.file_descr buf in
         Decode.pad buf 24; { nfd; fence_fd })) : Fd_from_fence.Reply.t)
    let decode_get_supported_modifiers_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let num_window_modifiers = Decode.u32 buf in
        let num_screen_modifiers = Decode.u32 buf in
        Decode.pad buf 16;
        (let window_modifiers =
           (Decode.list ~item:Decode.u64) ~len:num_window_modifiers buf in
         let screen_modifiers =
           (Decode.list ~item:Decode.u64) ~len:num_screen_modifiers buf in
         { window_modifiers; screen_modifiers })) : Get_supported_modifiers.Reply.t)
    let decode_buffers_from_pixmap_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let nfd = Decode.u8 buf in
        Decode.pad buf 6;
        (let width = Decode.u16 buf in
         let height = Decode.u16 buf in
         Decode.pad buf 4;
         (let modifier = Decode.u64 buf in
          let depth = Decode.u8 buf in
          let bpp = Decode.u8 buf in
          Decode.pad buf 6;
          (let strides = (Decode.list ~item:Decode.u32) ~len:nfd buf in
           let offsets = (Decode.list ~item:Decode.u32) ~len:nfd buf in
           let buffers = (Decode.list ~item:Decode.file_descr) ~len:nfd buf in
           { width; height; modifier; depth; bpp; strides; offsets; buffers })))) : 
      Buffers_from_pixmap.Reply.t)
    let decode_error ~number  buf =
      (Decode.pad buf 4;
       (match number with
        | n -> invalid_arg ("Invalid error number: " ^ (string_of_int n))) : 
      Error.t)
  end
module Ge_codec =
  struct
    open Protocol.Ge
    let decode_query_version_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let major_version = Decode.u16 buf in
        let minor_version = Decode.u16 buf in
        Decode.pad buf 20; { major_version; minor_version }) : Query_version.Reply.t)
    let decode_error ~number  buf =
      (Decode.pad buf 4;
       (match number with
        | n -> invalid_arg ("Invalid error number: " ^ (string_of_int n))) : 
      Error.t)
  end
module Glx_codec =
  struct
    open Protocol.Glx
    let decode_generic_error buf =
      (let bad_value = Decode.u32 buf in
       Decode.pad buf 24; Decode.align buf 32; bad_value : Error.Generic.t)
    let decode_bad_context_error = decode_generic_error
    let decode_bad_context_state_error = decode_generic_error
    let decode_bad_drawable_error = decode_generic_error
    let decode_bad_pixmap_error = decode_generic_error
    let decode_bad_context_tag_error = decode_generic_error
    let decode_bad_current_window_error = decode_generic_error
    let decode_bad_render_request_error = decode_generic_error
    let decode_bad_large_request_error = decode_generic_error
    let decode_unsupported_private_request_error = decode_generic_error
    let decode_bad_fb_config_error = decode_generic_error
    let decode_bad_pbuffer_error = decode_generic_error
    let decode_bad_current_drawable_error = decode_generic_error
    let decode_bad_window_error = decode_generic_error
    let decode_glx_bad_profile_arb_error = decode_generic_error
    let decode_pbuffer_clobber_event buf =
      (Decode.pad buf 4;
       (let event_type = Decode.u16 buf in
        let draw_type = Decode.u16 buf in
        let drawable = Decode.xid buf in
        let b_mask = Decode.u32 buf in
        let aux_buffer = Decode.u16 buf in
        let x = Decode.u16 buf in
        let y = Decode.u16 buf in
        let width = Decode.u16 buf in
        let height = Decode.u16 buf in
        let count = Decode.u16 buf in
        Decode.pad buf 4;
        {
          event_type;
          draw_type;
          drawable;
          b_mask;
          aux_buffer;
          x;
          y;
          width;
          height;
          count
        }) : Event.Pbuffer_clobber.t)
    let decode_buffer_swap_complete_event buf =
      (Decode.pad buf 4;
       (let event_type = Decode.u16 buf in
        Decode.pad buf 2;
        (let drawable = Decode.xid buf in
         let ust_hi = Decode.u32 buf in
         let ust_lo = Decode.u32 buf in
         let msc_hi = Decode.u32 buf in
         let msc_lo = Decode.u32 buf in
         let sbc = Decode.u32 buf in
         { event_type; drawable; ust_hi; ust_lo; msc_hi; msc_lo; sbc })) : 
      Event.Buffer_swap_complete.t)
    let decode_make_current_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let context_tag = Decode.u32 buf in Decode.pad buf 20; context_tag) : 
      Make_current.Reply.t)
    let decode_is_direct_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let is_direct = Decode.bool buf in Decode.pad buf 23; is_direct) : 
      Is_direct.Reply.t)
    let decode_query_version_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let major_version = Decode.u32 buf in
        let minor_version = Decode.u32 buf in
        Decode.pad buf 16; { major_version; minor_version }) : Query_version.Reply.t)
    let decode_get_visual_configs_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let num_visuals = Decode.u32 buf in
        let num_properties = Decode.u32 buf in
        Decode.pad buf 16;
        (let property_list = (Decode.list ~item:Decode.u32) ~len:length buf in
         { num_visuals; num_properties; property_list })) : Get_visual_configs.Reply.t)
    let decode_vendor_private_with_reply_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let retval = Decode.u32 buf in
        let data1 = (Decode.list ~item:Decode.byte) ~len:24 buf in
        let data2 = (Decode.list ~item:Decode.byte) ~len:(length * 4) buf in
        { retval; data1; data2 }) : Vendor_private_with_reply.Reply.t)
    let decode_query_extensions_string_reply ~length:(length : int)  buf =
      (Decode.pad buf 12; (let n = Decode.u32 buf in Decode.pad buf 16; n) : 
      Query_extensions_string.Reply.t)
    let decode_query_server_string_reply ~length:(length : int)  buf =
      (Decode.pad buf 12;
       (let str_len = Decode.u32 buf in
        Decode.pad buf 16;
        (let string = Decode.string ~len:str_len buf in string)) : Query_server_string.Reply.t)
    let decode_get_fb_configs_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let num_f_b_configs = Decode.u32 buf in
        let num_properties = Decode.u32 buf in
        Decode.pad buf 16;
        (let property_list = (Decode.list ~item:Decode.u32) ~len:length buf in
         { num_f_b_configs; num_properties; property_list })) : Get_fb_configs.Reply.t)
    let decode_query_context_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let num_attribs = Decode.u32 buf in
        let num_attribs = num_attribs / 2 in
        Decode.pad buf 20;
        (let attribs = (Decode.list ~item:Decode.u32) ~len:num_attribs buf in
         attribs)) : Query_context.Reply.t)
    let decode_make_context_current_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let context_tag = Decode.u32 buf in Decode.pad buf 20; context_tag) : 
      Make_context_current.Reply.t)
    let decode_get_drawable_attributes_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let num_attribs = Decode.u32 buf in
        let num_attribs = num_attribs / 2 in
        Decode.pad buf 20;
        (let attribs = (Decode.list ~item:Decode.u32) ~len:num_attribs buf in
         attribs)) : Get_drawable_attributes.Reply.t)
    let decode_gen_lists_reply ~length:(length : int)  buf =
      (Decode.pad buf 8; (let ret_val = Decode.u32 buf in ret_val) : 
      Gen_lists.Reply.t)
    let decode_render_mode_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let ret_val = Decode.u32 buf in
        let n = Decode.u32 buf in
        let new_mode = Decode.u32 buf in
        Decode.pad buf 12;
        (let data = (Decode.list ~item:Decode.u32) ~len:n buf in
         { ret_val; new_mode; data })) : Render_mode.Reply.t)
    let decode_finish_reply ~length:(length : int)  buf =
      (Decode.pad buf 8; () : Finish.Reply.t)
    let decode_read_pixels_reply ~length:(length : int)  buf =
      (Decode.pad buf 32;
       (let data = (Decode.list ~item:Decode.byte) ~len:(length * 4) buf in
        data) : Read_pixels.Reply.t)
    let decode_get_booleanv_reply ~length:(length : int)  buf =
      (Decode.pad buf 12;
       (let n = Decode.u32 buf in
        let datum = Decode.bool buf in
        Decode.pad buf 15;
        (let data = (Decode.list ~item:Decode.bool) ~len:n buf in
         { datum; data })) : Get_booleanv.Reply.t)
    let decode_get_clip_plane_reply ~length:(length : int)  buf =
      (Decode.pad buf 32;
       (let data = (Decode.list ~item:Decode.double) ~len:(length / 2) buf in
        data) : Get_clip_plane.Reply.t)
    let decode_get_doublev_reply ~length:(length : int)  buf =
      (Decode.pad buf 12;
       (let n = Decode.u32 buf in
        let datum = Decode.double buf in
        Decode.pad buf 8;
        (let data = (Decode.list ~item:Decode.double) ~len:n buf in
         { datum; data })) : Get_doublev.Reply.t)
    let decode_get_error_reply ~length:(length : int)  buf =
      (Decode.pad buf 8; (let error = Decode.i32 buf in error) : Get_error.Reply.t)
    let decode_get_floatv_reply ~length:(length : int)  buf =
      (Decode.pad buf 12;
       (let n = Decode.u32 buf in
        let datum = Decode.float buf in
        Decode.pad buf 12;
        (let data = (Decode.list ~item:Decode.float) ~len:n buf in
         { datum; data })) : Get_floatv.Reply.t)
    let decode_get_integerv_reply ~length:(length : int)  buf =
      (Decode.pad buf 12;
       (let n = Decode.u32 buf in
        let datum = Decode.i32 buf in
        Decode.pad buf 12;
        (let data = (Decode.list ~item:Decode.i32) ~len:n buf in
         { datum; data })) : Get_integerv.Reply.t)
    let decode_get_lightfv_reply ~length:(length : int)  buf =
      (Decode.pad buf 12;
       (let n = Decode.u32 buf in
        let datum = Decode.float buf in
        Decode.pad buf 12;
        (let data = (Decode.list ~item:Decode.float) ~len:n buf in
         { datum; data })) : Get_lightfv.Reply.t)
    let decode_get_lightiv_reply ~length:(length : int)  buf =
      (Decode.pad buf 12;
       (let n = Decode.u32 buf in
        let datum = Decode.i32 buf in
        Decode.pad buf 12;
        (let data = (Decode.list ~item:Decode.i32) ~len:n buf in
         { datum; data })) : Get_lightiv.Reply.t)
    let decode_get_mapdv_reply ~length:(length : int)  buf =
      (Decode.pad buf 12;
       (let n = Decode.u32 buf in
        let datum = Decode.double buf in
        Decode.pad buf 8;
        (let data = (Decode.list ~item:Decode.double) ~len:n buf in
         { datum; data })) : Get_mapdv.Reply.t)
    let decode_get_mapfv_reply ~length:(length : int)  buf =
      (Decode.pad buf 12;
       (let n = Decode.u32 buf in
        let datum = Decode.float buf in
        Decode.pad buf 12;
        (let data = (Decode.list ~item:Decode.float) ~len:n buf in
         { datum; data })) : Get_mapfv.Reply.t)
    let decode_get_mapiv_reply ~length:(length : int)  buf =
      (Decode.pad buf 12;
       (let n = Decode.u32 buf in
        let datum = Decode.i32 buf in
        Decode.pad buf 12;
        (let data = (Decode.list ~item:Decode.i32) ~len:n buf in
         { datum; data })) : Get_mapiv.Reply.t)
    let decode_get_materialfv_reply ~length:(length : int)  buf =
      (Decode.pad buf 12;
       (let n = Decode.u32 buf in
        let datum = Decode.float buf in
        Decode.pad buf 12;
        (let data = (Decode.list ~item:Decode.float) ~len:n buf in
         { datum; data })) : Get_materialfv.Reply.t)
    let decode_get_materialiv_reply ~length:(length : int)  buf =
      (Decode.pad buf 12;
       (let n = Decode.u32 buf in
        let datum = Decode.i32 buf in
        Decode.pad buf 12;
        (let data = (Decode.list ~item:Decode.i32) ~len:n buf in
         { datum; data })) : Get_materialiv.Reply.t)
    let decode_get_pixel_mapfv_reply ~length:(length : int)  buf =
      (Decode.pad buf 12;
       (let n = Decode.u32 buf in
        let datum = Decode.float buf in
        Decode.pad buf 12;
        (let data = (Decode.list ~item:Decode.float) ~len:n buf in
         { datum; data })) : Get_pixel_mapfv.Reply.t)
    let decode_get_pixel_mapuiv_reply ~length:(length : int)  buf =
      (Decode.pad buf 12;
       (let n = Decode.u32 buf in
        let datum = Decode.u32 buf in
        Decode.pad buf 12;
        (let data = (Decode.list ~item:Decode.u32) ~len:n buf in
         { datum; data })) : Get_pixel_mapuiv.Reply.t)
    let decode_get_pixel_mapusv_reply ~length:(length : int)  buf =
      (Decode.pad buf 12;
       (let n = Decode.u32 buf in
        let datum = Decode.u16 buf in
        Decode.pad buf 16;
        (let data = (Decode.list ~item:Decode.u16) ~len:n buf in
         { datum; data })) : Get_pixel_mapusv.Reply.t)
    let decode_get_polygon_stipple_reply ~length:(length : int)  buf =
      (Decode.pad buf 32;
       (let data = (Decode.list ~item:Decode.byte) ~len:(length * 4) buf in
        data) : Get_polygon_stipple.Reply.t)
    let decode_get_string_reply ~length:(length : int)  buf =
      (Decode.pad buf 12;
       (let n = Decode.u32 buf in
        Decode.pad buf 16; (let string = Decode.string ~len:n buf in string)) : 
      Get_string.Reply.t)
    let decode_get_tex_envfv_reply ~length:(length : int)  buf =
      (Decode.pad buf 12;
       (let n = Decode.u32 buf in
        let datum = Decode.float buf in
        Decode.pad buf 12;
        (let data = (Decode.list ~item:Decode.float) ~len:n buf in
         { datum; data })) : Get_tex_envfv.Reply.t)
    let decode_get_tex_enviv_reply ~length:(length : int)  buf =
      (Decode.pad buf 12;
       (let n = Decode.u32 buf in
        let datum = Decode.i32 buf in
        Decode.pad buf 12;
        (let data = (Decode.list ~item:Decode.i32) ~len:n buf in
         { datum; data })) : Get_tex_enviv.Reply.t)
    let decode_get_tex_gendv_reply ~length:(length : int)  buf =
      (Decode.pad buf 12;
       (let n = Decode.u32 buf in
        let datum = Decode.double buf in
        Decode.pad buf 8;
        (let data = (Decode.list ~item:Decode.double) ~len:n buf in
         { datum; data })) : Get_tex_gendv.Reply.t)
    let decode_get_tex_genfv_reply ~length:(length : int)  buf =
      (Decode.pad buf 12;
       (let n = Decode.u32 buf in
        let datum = Decode.float buf in
        Decode.pad buf 12;
        (let data = (Decode.list ~item:Decode.float) ~len:n buf in
         { datum; data })) : Get_tex_genfv.Reply.t)
    let decode_get_tex_geniv_reply ~length:(length : int)  buf =
      (Decode.pad buf 12;
       (let n = Decode.u32 buf in
        let datum = Decode.i32 buf in
        Decode.pad buf 12;
        (let data = (Decode.list ~item:Decode.i32) ~len:n buf in
         { datum; data })) : Get_tex_geniv.Reply.t)
    let decode_get_tex_image_reply ~length:(length : int)  buf =
      (Decode.pad buf 16;
       (let width = Decode.i32 buf in
        let height = Decode.i32 buf in
        let depth = Decode.i32 buf in
        Decode.pad buf 4;
        (let data = (Decode.list ~item:Decode.byte) ~len:(length * 4) buf in
         { width; height; depth; data })) : Get_tex_image.Reply.t)
    let decode_get_tex_parameterfv_reply ~length:(length : int)  buf =
      (Decode.pad buf 12;
       (let n = Decode.u32 buf in
        let datum = Decode.float buf in
        Decode.pad buf 12;
        (let data = (Decode.list ~item:Decode.float) ~len:n buf in
         { datum; data })) : Get_tex_parameterfv.Reply.t)
    let decode_get_tex_parameteriv_reply ~length:(length : int)  buf =
      (Decode.pad buf 12;
       (let n = Decode.u32 buf in
        let datum = Decode.i32 buf in
        Decode.pad buf 12;
        (let data = (Decode.list ~item:Decode.i32) ~len:n buf in
         { datum; data })) : Get_tex_parameteriv.Reply.t)
    let decode_get_tex_level_parameterfv_reply ~length:(length : int)  buf =
      (Decode.pad buf 12;
       (let n = Decode.u32 buf in
        let datum = Decode.float buf in
        Decode.pad buf 12;
        (let data = (Decode.list ~item:Decode.float) ~len:n buf in
         { datum; data })) : Get_tex_level_parameterfv.Reply.t)
    let decode_get_tex_level_parameteriv_reply ~length:(length : int)  buf =
      (Decode.pad buf 12;
       (let n = Decode.u32 buf in
        let datum = Decode.i32 buf in
        Decode.pad buf 12;
        (let data = (Decode.list ~item:Decode.i32) ~len:n buf in
         { datum; data })) : Get_tex_level_parameteriv.Reply.t)
    let decode_is_enabled_reply ~length:(length : int)  buf =
      (Decode.pad buf 8; (let ret_val = Decode.u32 buf in ret_val) : 
      Is_enabled.Reply.t)
    let decode_is_list_reply ~length:(length : int)  buf =
      (Decode.pad buf 8; (let ret_val = Decode.u32 buf in ret_val) : 
      Is_list.Reply.t)
    let decode_are_textures_resident_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let ret_val = Decode.u32 buf in
        Decode.pad buf 20;
        (let data = (Decode.list ~item:Decode.bool) ~len:(length * 4) buf in
         { ret_val; data })) : Are_textures_resident.Reply.t)
    let decode_gen_textures_reply ~length:(length : int)  buf =
      (Decode.pad buf 32;
       (let data = (Decode.list ~item:Decode.u32) ~len:length buf in data) : 
      Gen_textures.Reply.t)
    let decode_is_texture_reply ~length:(length : int)  buf =
      (Decode.pad buf 8; (let ret_val = Decode.u32 buf in ret_val) : 
      Is_texture.Reply.t)
    let decode_get_color_table_reply ~length:(length : int)  buf =
      (Decode.pad buf 16;
       (let width = Decode.i32 buf in
        Decode.pad buf 12;
        (let data = (Decode.list ~item:Decode.byte) ~len:(length * 4) buf in
         { width; data })) : Get_color_table.Reply.t)
    let decode_get_color_table_parameterfv_reply ~length:(length : int)  buf
      =
      (Decode.pad buf 12;
       (let n = Decode.u32 buf in
        let datum = Decode.float buf in
        Decode.pad buf 12;
        (let data = (Decode.list ~item:Decode.float) ~len:n buf in
         { datum; data })) : Get_color_table_parameterfv.Reply.t)
    let decode_get_color_table_parameteriv_reply ~length:(length : int)  buf
      =
      (Decode.pad buf 12;
       (let n = Decode.u32 buf in
        let datum = Decode.i32 buf in
        Decode.pad buf 12;
        (let data = (Decode.list ~item:Decode.i32) ~len:n buf in
         { datum; data })) : Get_color_table_parameteriv.Reply.t)
    let decode_get_convolution_filter_reply ~length:(length : int)  buf =
      (Decode.pad buf 16;
       (let width = Decode.i32 buf in
        let height = Decode.i32 buf in
        Decode.pad buf 8;
        (let data = (Decode.list ~item:Decode.byte) ~len:(length * 4) buf in
         { width; height; data })) : Get_convolution_filter.Reply.t)
    let decode_get_convolution_parameterfv_reply ~length:(length : int)  buf
      =
      (Decode.pad buf 12;
       (let n = Decode.u32 buf in
        let datum = Decode.float buf in
        Decode.pad buf 12;
        (let data = (Decode.list ~item:Decode.float) ~len:n buf in
         { datum; data })) : Get_convolution_parameterfv.Reply.t)
    let decode_get_convolution_parameteriv_reply ~length:(length : int)  buf
      =
      (Decode.pad buf 12;
       (let n = Decode.u32 buf in
        let datum = Decode.i32 buf in
        Decode.pad buf 12;
        (let data = (Decode.list ~item:Decode.i32) ~len:n buf in
         { datum; data })) : Get_convolution_parameteriv.Reply.t)
    let decode_get_separable_filter_reply ~length:(length : int)  buf =
      (Decode.pad buf 16;
       (let row_w = Decode.i32 buf in
        let col_h = Decode.i32 buf in
        Decode.pad buf 8;
        (let rows_and_cols =
           (Decode.list ~item:Decode.byte) ~len:(length * 4) buf in
         { row_w; col_h; rows_and_cols })) : Get_separable_filter.Reply.t)
    let decode_get_histogram_reply ~length:(length : int)  buf =
      (Decode.pad buf 16;
       (let width = Decode.i32 buf in
        Decode.pad buf 12;
        (let data = (Decode.list ~item:Decode.byte) ~len:(length * 4) buf in
         { width; data })) : Get_histogram.Reply.t)
    let decode_get_histogram_parameterfv_reply ~length:(length : int)  buf =
      (Decode.pad buf 12;
       (let n = Decode.u32 buf in
        let datum = Decode.float buf in
        Decode.pad buf 12;
        (let data = (Decode.list ~item:Decode.float) ~len:n buf in
         { datum; data })) : Get_histogram_parameterfv.Reply.t)
    let decode_get_histogram_parameteriv_reply ~length:(length : int)  buf =
      (Decode.pad buf 12;
       (let n = Decode.u32 buf in
        let datum = Decode.i32 buf in
        Decode.pad buf 12;
        (let data = (Decode.list ~item:Decode.i32) ~len:n buf in
         { datum; data })) : Get_histogram_parameteriv.Reply.t)
    let decode_get_minmax_reply ~length:(length : int)  buf =
      (Decode.pad buf 32;
       (let data = (Decode.list ~item:Decode.byte) ~len:(length * 4) buf in
        data) : Get_minmax.Reply.t)
    let decode_get_minmax_parameterfv_reply ~length:(length : int)  buf =
      (Decode.pad buf 12;
       (let n = Decode.u32 buf in
        let datum = Decode.float buf in
        Decode.pad buf 12;
        (let data = (Decode.list ~item:Decode.float) ~len:n buf in
         { datum; data })) : Get_minmax_parameterfv.Reply.t)
    let decode_get_minmax_parameteriv_reply ~length:(length : int)  buf =
      (Decode.pad buf 12;
       (let n = Decode.u32 buf in
        let datum = Decode.i32 buf in
        Decode.pad buf 12;
        (let data = (Decode.list ~item:Decode.i32) ~len:n buf in
         { datum; data })) : Get_minmax_parameteriv.Reply.t)
    let decode_get_compressed_tex_image_arb_reply ~length:(length : int)  buf
      =
      (Decode.pad buf 16;
       (let size = Decode.i32 buf in
        Decode.pad buf 12;
        (let data = (Decode.list ~item:Decode.byte) ~len:(length * 4) buf in
         { size; data })) : Get_compressed_tex_image_arb.Reply.t)
    let decode_gen_queries_arb_reply ~length:(length : int)  buf =
      (Decode.pad buf 32;
       (let data = (Decode.list ~item:Decode.u32) ~len:length buf in data) : 
      Gen_queries_arb.Reply.t)
    let decode_is_query_arb_reply ~length:(length : int)  buf =
      (Decode.pad buf 8; (let ret_val = Decode.u32 buf in ret_val) : 
      Is_query_arb.Reply.t)
    let decode_get_queryiv_arb_reply ~length:(length : int)  buf =
      (Decode.pad buf 12;
       (let n = Decode.u32 buf in
        let datum = Decode.i32 buf in
        Decode.pad buf 12;
        (let data = (Decode.list ~item:Decode.i32) ~len:n buf in
         { datum; data })) : Get_queryiv_arb.Reply.t)
    let decode_get_query_objectiv_arb_reply ~length:(length : int)  buf =
      (Decode.pad buf 12;
       (let n = Decode.u32 buf in
        let datum = Decode.i32 buf in
        Decode.pad buf 12;
        (let data = (Decode.list ~item:Decode.i32) ~len:n buf in
         { datum; data })) : Get_query_objectiv_arb.Reply.t)
    let decode_get_query_objectuiv_arb_reply ~length:(length : int)  buf =
      (Decode.pad buf 12;
       (let n = Decode.u32 buf in
        let datum = Decode.u32 buf in
        Decode.pad buf 12;
        (let data = (Decode.list ~item:Decode.u32) ~len:n buf in
         { datum; data })) : Get_query_objectuiv_arb.Reply.t)
    let decode_error ~number  buf =
      (Decode.pad buf 4;
       (match number with
        | (-1) -> let error = decode_generic_error buf in `Generic error
        | 0 -> let error = decode_bad_context_error buf in `Bad_context error
        | 1 ->
            let error = decode_bad_context_state_error buf in
            `Bad_context_state error
        | 2 ->
            let error = decode_bad_drawable_error buf in `Bad_drawable error
        | 3 -> let error = decode_bad_pixmap_error buf in `Bad_pixmap error
        | 4 ->
            let error = decode_bad_context_tag_error buf in
            `Bad_context_tag error
        | 5 ->
            let error = decode_bad_current_window_error buf in
            `Bad_current_window error
        | 6 ->
            let error = decode_bad_render_request_error buf in
            `Bad_render_request error
        | 7 ->
            let error = decode_bad_large_request_error buf in
            `Bad_large_request error
        | 8 ->
            let error = decode_unsupported_private_request_error buf in
            `Unsupported_private_request error
        | 9 ->
            let error = decode_bad_fb_config_error buf in
            `Bad_fb_config error
        | 10 ->
            let error = decode_bad_pbuffer_error buf in `Bad_pbuffer error
        | 11 ->
            let error = decode_bad_current_drawable_error buf in
            `Bad_current_drawable error
        | 12 -> let error = decode_bad_window_error buf in `Bad_window error
        | 13 ->
            let error = decode_glx_bad_profile_arb_error buf in
            `Glx_bad_profile_arb error
        | n -> invalid_arg ("Invalid error number: " ^ (string_of_int n))) : 
      Error.t)
  end
module Randr_codec =
  struct
    open Protocol.Randr
    let decode_bad_output_error buf =
      (Decode.align buf 32; () : Error.Bad_output.t)
    let decode_bad_crtc_error buf =
      (Decode.align buf 32; () : Error.Bad_crtc.t)
    let decode_bad_mode_error buf =
      (Decode.align buf 32; () : Error.Bad_mode.t)
    let decode_bad_provider_error buf =
      (Decode.align buf 32; () : Error.Bad_provider.t)
    let decode_screen_size buf =
      (let width = Decode.u16 buf in
       let height = Decode.u16 buf in
       let mwidth = Decode.u16 buf in
       let mheight = Decode.u16 buf in { width; height; mwidth; mheight } : 
      screen_size)
    let decode_refresh_rates buf =
      (let n_rates = Decode.u16 buf in
       let rates = (Decode.list ~item:Decode.u16) ~len:n_rates buf in rates : 
      refresh_rates)
    let decode_query_version_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let major_version = Decode.u32 buf in
        let minor_version = Decode.u32 buf in
        Decode.pad buf 16; { major_version; minor_version }) : Query_version.Reply.t)
    let decode_set_screen_config_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let status =
          ((Decode.u8 %> Conv.To_int.u8) %> Set_config_enum.of_int) buf in
        Decode.pad buf 6;
        (let new_timestamp = Decode.u32 buf in
         let config_timestamp = Decode.u32 buf in
         let root = Decode.xid buf in
         let subpixel_order =
           ((Decode.u16 %> Conv.To_int.u16) %> Render.Sub_pixel_enum.of_int)
             buf in
         Decode.pad buf 10;
         { status; new_timestamp; config_timestamp; root; subpixel_order })) : 
      Set_screen_config.Reply.t)
    let decode_get_screen_info_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let rotations =
          ((Decode.u8 %> Conv.To_i32.u8) %> Rotation_mask.of_int32) buf in
        Decode.pad buf 6;
        (let root = Decode.xid buf in
         let timestamp = Decode.u32 buf in
         let config_timestamp = Decode.u32 buf in
         let n_sizes = Decode.u16 buf in
         let size_id = Decode.u16 buf in
         let rotation =
           ((Decode.u16 %> Conv.To_i32.u16) %> Rotation_mask.of_int32) buf in
         let rate = Decode.u16 buf in
         let n_info = Decode.u16 buf in
         Decode.pad buf 2;
         (let sizes = (Decode.list ~item:decode_screen_size) ~len:n_sizes buf in
          let rates =
            (Decode.list ~item:decode_refresh_rates) ~len:(n_info - n_sizes)
              buf in
          {
            rotations;
            root;
            timestamp;
            config_timestamp;
            size_id;
            rotation;
            rate;
            n_info;
            sizes;
            rates
          }))) : Get_screen_info.Reply.t)
    let decode_get_screen_size_range_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let min_width = Decode.u16 buf in
        let min_height = Decode.u16 buf in
        let max_width = Decode.u16 buf in
        let max_height = Decode.u16 buf in
        Decode.pad buf 16; { min_width; min_height; max_width; max_height }) : 
      Get_screen_size_range.Reply.t)
    let decode_mode_info buf =
      (let id = Decode.u32 buf in
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
       {
         id;
         width;
         height;
         dot_clock;
         hsync_start;
         hsync_end;
         htotal;
         hskew;
         vsync_start;
         vsync_end;
         vtotal;
         name_len;
         mode_flags
       } : mode_info)
    let decode_get_screen_resources_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let timestamp = Decode.u32 buf in
        let config_timestamp = Decode.u32 buf in
        let num_crtcs = Decode.u16 buf in
        let num_outputs = Decode.u16 buf in
        let num_modes = Decode.u16 buf in
        let names_len = Decode.u16 buf in
        Decode.pad buf 8;
        (let crtcs = (Decode.list ~item:Decode.xid) ~len:num_crtcs buf in
         let outputs = (Decode.list ~item:Decode.xid) ~len:num_outputs buf in
         let modes = (Decode.list ~item:decode_mode_info) ~len:num_modes buf in
         let names = (Decode.list ~item:Decode.byte) ~len:names_len buf in
         { timestamp; config_timestamp; crtcs; outputs; modes; names })) : 
      Get_screen_resources.Reply.t)
    let decode_get_output_info_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let status =
          ((Decode.u8 %> Conv.To_int.u8) %> Set_config_enum.of_int) buf in
        Decode.pad buf 6;
        (let timestamp = Decode.u32 buf in
         let crtc = Decode.xid buf in
         let mm_width = Decode.u32 buf in
         let mm_height = Decode.u32 buf in
         let connection =
           ((Decode.u8 %> Conv.To_int.u8) %> Connection_enum.of_int) buf in
         let subpixel_order =
           ((Decode.u8 %> Conv.To_int.u8) %> Render.Sub_pixel_enum.of_int)
             buf in
         let num_crtcs = Decode.u16 buf in
         let num_modes = Decode.u16 buf in
         let num_preferred = Decode.u16 buf in
         let num_clones = Decode.u16 buf in
         let name_len = Decode.u16 buf in
         let crtcs = (Decode.list ~item:Decode.xid) ~len:num_crtcs buf in
         let modes = (Decode.list ~item:Decode.xid) ~len:num_modes buf in
         let clones = (Decode.list ~item:Decode.xid) ~len:num_clones buf in
         let name = (Decode.list ~item:Decode.byte) ~len:name_len buf in
         {
           status;
           timestamp;
           crtc;
           mm_width;
           mm_height;
           connection;
           subpixel_order;
           num_preferred;
           crtcs;
           modes;
           clones;
           name
         })) : Get_output_info.Reply.t)
    let decode_list_output_properties_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let num_atoms = Decode.u16 buf in
        Decode.pad buf 22;
        (let atoms = (Decode.list ~item:Decode.xid) ~len:num_atoms buf in
         atoms)) : List_output_properties.Reply.t)
    let decode_query_output_property_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let pending = Decode.bool buf in
        let range = Decode.bool buf in
        let immutable = Decode.bool buf in
        Decode.pad buf 21;
        (let valid_values = (Decode.list ~item:Decode.i32) ~len:length buf in
         { pending; range; immutable; valid_values })) : Query_output_property.Reply.t)
    let decode_get_output_property_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let format = Decode.u8 buf in
        Decode.pad buf 6;
        (let type_ =
           (Decode.xid %>
              (Conv.alt_enum ~enum_of_int:Core.Atom_enum.of_int
                 ~int_of_t:Conv.To_int.xid)) buf in
         let bytes_after = Decode.u32 buf in
         let num_items = Decode.u32 buf in
         Decode.pad buf 12;
         (let data =
            (Decode.list ~item:Decode.byte) ~len:(num_items * (format / 8))
              buf in
          { format; type_; bytes_after; num_items; data }))) : Get_output_property.Reply.t)
    let decode_create_mode_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let mode = Decode.xid buf in Decode.pad buf 20; mode) : Create_mode.Reply.t)
    let decode_get_crtc_info_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let status =
          ((Decode.u8 %> Conv.To_int.u8) %> Set_config_enum.of_int) buf in
        Decode.pad buf 6;
        (let timestamp = Decode.u32 buf in
         let x = Decode.i16 buf in
         let y = Decode.i16 buf in
         let width = Decode.u16 buf in
         let height = Decode.u16 buf in
         let mode = Decode.xid buf in
         let rotation =
           ((Decode.u16 %> Conv.To_i32.u16) %> Rotation_mask.of_int32) buf in
         let rotations =
           ((Decode.u16 %> Conv.To_i32.u16) %> Rotation_mask.of_int32) buf in
         let num_outputs = Decode.u16 buf in
         let num_possible_outputs = Decode.u16 buf in
         let outputs = (Decode.list ~item:Decode.xid) ~len:num_outputs buf in
         let possible =
           (Decode.list ~item:Decode.xid) ~len:num_possible_outputs buf in
         {
           status;
           timestamp;
           x;
           y;
           width;
           height;
           mode;
           rotation;
           rotations;
           outputs;
           possible
         })) : Get_crtc_info.Reply.t)
    let decode_set_crtc_config_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let status =
          ((Decode.u8 %> Conv.To_int.u8) %> Set_config_enum.of_int) buf in
        Decode.pad buf 6;
        (let timestamp = Decode.u32 buf in
         Decode.pad buf 20; { status; timestamp })) : Set_crtc_config.Reply.t)
    let decode_get_crtc_gamma_size_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let size = Decode.u16 buf in Decode.pad buf 22; size) : Get_crtc_gamma_size.Reply.t)
    let decode_get_crtc_gamma_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let size = Decode.u16 buf in
        Decode.pad buf 22;
        (let red = (Decode.list ~item:Decode.u16) ~len:size buf in
         let green = (Decode.list ~item:Decode.u16) ~len:size buf in
         let blue = (Decode.list ~item:Decode.u16) ~len:size buf in
         { red; green; blue })) : Get_crtc_gamma.Reply.t)
    let decode_get_screen_resources_current_reply ~length:(length : int)  buf
      =
      (Decode.pad buf 8;
       (let timestamp = Decode.u32 buf in
        let config_timestamp = Decode.u32 buf in
        let num_crtcs = Decode.u16 buf in
        let num_outputs = Decode.u16 buf in
        let num_modes = Decode.u16 buf in
        let names_len = Decode.u16 buf in
        Decode.pad buf 8;
        (let crtcs = (Decode.list ~item:Decode.xid) ~len:num_crtcs buf in
         let outputs = (Decode.list ~item:Decode.xid) ~len:num_outputs buf in
         let modes = (Decode.list ~item:decode_mode_info) ~len:num_modes buf in
         let names = (Decode.list ~item:Decode.byte) ~len:names_len buf in
         { timestamp; config_timestamp; crtcs; outputs; modes; names })) : 
      Get_screen_resources_current.Reply.t)
    let decode_get_crtc_transform_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let pending_transform = Render_codec.decode_transform buf in
        let has_transforms = Decode.bool buf in
        Decode.pad buf 3;
        (let current_transform = Render_codec.decode_transform buf in
         Decode.pad buf 4;
         (let pending_len = Decode.u16 buf in
          let pending_nparams = Decode.u16 buf in
          let current_len = Decode.u16 buf in
          let current_nparams = Decode.u16 buf in
          let pending_filter_name = Decode.string ~len:pending_len buf in
          Decode.align buf 4;
          (let pending_params =
             (Decode.list ~item:Decode.i32) ~len:pending_nparams buf in
           let current_filter_name = Decode.string ~len:current_len buf in
           Decode.align buf 4;
           (let current_params =
              (Decode.list ~item:Decode.i32) ~len:current_nparams buf in
            {
              pending_transform;
              has_transforms;
              current_transform;
              pending_filter_name;
              pending_params;
              current_filter_name;
              current_params
            }))))) : Get_crtc_transform.Reply.t)
    let decode_get_panning_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let status =
          ((Decode.u8 %> Conv.To_int.u8) %> Set_config_enum.of_int) buf in
        Decode.pad buf 6;
        (let timestamp = Decode.u32 buf in
         let left = Decode.u16 buf in
         let top = Decode.u16 buf in
         let width = Decode.u16 buf in
         let height = Decode.u16 buf in
         let track_left = Decode.u16 buf in
         let track_top = Decode.u16 buf in
         let track_width = Decode.u16 buf in
         let track_height = Decode.u16 buf in
         let border_left = Decode.i16 buf in
         let border_top = Decode.i16 buf in
         let border_right = Decode.i16 buf in
         let border_bottom = Decode.i16 buf in
         {
           status;
           timestamp;
           left;
           top;
           width;
           height;
           track_left;
           track_top;
           track_width;
           track_height;
           border_left;
           border_top;
           border_right;
           border_bottom
         })) : Get_panning.Reply.t)
    let decode_set_panning_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let status =
          ((Decode.u8 %> Conv.To_int.u8) %> Set_config_enum.of_int) buf in
        Decode.pad buf 6;
        (let timestamp = Decode.u32 buf in { status; timestamp })) : 
      Set_panning.Reply.t)
    let decode_get_output_primary_reply ~length:(length : int)  buf =
      (Decode.pad buf 8; (let output = Decode.xid buf in output) : Get_output_primary.Reply.t)
    let decode_get_providers_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let timestamp = Decode.u32 buf in
        let num_providers = Decode.u16 buf in
        Decode.pad buf 18;
        (let providers =
           (Decode.list ~item:Decode.xid) ~len:num_providers buf in
         { timestamp; providers })) : Get_providers.Reply.t)
    let decode_get_provider_info_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let status = Decode.u8 buf in
        Decode.pad buf 6;
        (let timestamp = Decode.u32 buf in
         let capabilities =
           ((Decode.u32 %> Conv.To_i32.u32) %>
              Provider_capability_mask.of_int32) buf in
         let num_crtcs = Decode.u16 buf in
         let num_outputs = Decode.u16 buf in
         let num_associated_providers = Decode.u16 buf in
         let name_len = Decode.u16 buf in
         Decode.pad buf 8;
         (let crtcs = (Decode.list ~item:Decode.xid) ~len:num_crtcs buf in
          let outputs = (Decode.list ~item:Decode.xid) ~len:num_outputs buf in
          let associated_providers =
            (Decode.list ~item:Decode.xid) ~len:num_associated_providers buf in
          let associated_capability =
            (Decode.list ~item:Decode.u32) ~len:num_associated_providers buf in
          let name = Decode.string ~len:name_len buf in
          {
            status;
            timestamp;
            capabilities;
            crtcs;
            outputs;
            associated_providers;
            associated_capability;
            name
          }))) : Get_provider_info.Reply.t)
    let decode_list_provider_properties_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let num_atoms = Decode.u16 buf in
        Decode.pad buf 22;
        (let atoms = (Decode.list ~item:Decode.xid) ~len:num_atoms buf in
         atoms)) : List_provider_properties.Reply.t)
    let decode_query_provider_property_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let pending = Decode.bool buf in
        let range = Decode.bool buf in
        let immutable = Decode.bool buf in
        Decode.pad buf 21;
        (let valid_values = (Decode.list ~item:Decode.i32) ~len:length buf in
         { pending; range; immutable; valid_values })) : Query_provider_property.Reply.t)
    let decode_get_provider_property_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let format = Decode.u8 buf in
        Decode.pad buf 6;
        (let type_ = Decode.xid buf in
         let bytes_after = Decode.u32 buf in
         let num_items = Decode.u32 buf in
         Decode.pad buf 12;
         (let data = Decode.string ~len:(num_items * (format / 8)) buf in
          { format; type_; bytes_after; num_items; data }))) : Get_provider_property.Reply.t)
    let decode_screen_change_notify_event buf =
      (Decode.pad buf 1;
       (let rotation =
          ((Decode.u8 %> Conv.To_i32.u8) %> Rotation_mask.of_int32) buf in
        Decode.pad buf 2;
        (let timestamp = Decode.u32 buf in
         let config_timestamp = Decode.u32 buf in
         let root = Decode.xid buf in
         let request_window = Decode.xid buf in
         let size_id = Decode.u16 buf in
         let subpixel_order =
           ((Decode.u16 %> Conv.To_int.u16) %> Render.Sub_pixel_enum.of_int)
             buf in
         let width = Decode.u16 buf in
         let height = Decode.u16 buf in
         let mwidth = Decode.u16 buf in
         let mheight = Decode.u16 buf in
         {
           rotation;
           timestamp;
           config_timestamp;
           root;
           request_window;
           size_id;
           subpixel_order;
           width;
           height;
           mwidth;
           mheight
         })) : Event.Screen_change_notify.t)
    let decode_crtc_change buf =
      (let timestamp = Decode.u32 buf in
       let window = Decode.xid buf in
       let crtc = Decode.xid buf in
       let mode = Decode.xid buf in
       let rotation =
         ((Decode.u16 %> Conv.To_i32.u16) %> Rotation_mask.of_int32) buf in
       Decode.pad buf 2;
       (let x = Decode.i16 buf in
        let y = Decode.i16 buf in
        let width = Decode.u16 buf in
        let height = Decode.u16 buf in
        { timestamp; window; crtc; mode; rotation; x; y; width; height }) : 
      crtc_change)
    let decode_output_change buf =
      (let timestamp = Decode.u32 buf in
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
       {
         timestamp;
         config_timestamp;
         window;
         output;
         crtc;
         mode;
         rotation;
         connection;
         subpixel_order
       } : output_change)
    let decode_output_property buf =
      (let window = Decode.xid buf in
       let output = Decode.xid buf in
       let atom = Decode.xid buf in
       let timestamp = Decode.u32 buf in
       let status =
         ((Decode.u8 %> Conv.To_int.u8) %> Core.Property_enum.of_int) buf in
       Decode.pad buf 11; { window; output; atom; timestamp; status } : 
      output_property)
    let decode_provider_change buf =
      (let timestamp = Decode.u32 buf in
       let window = Decode.xid buf in
       let provider = Decode.xid buf in
       Decode.pad buf 16; { timestamp; window; provider } : provider_change)
    let decode_provider_property buf =
      (let window = Decode.xid buf in
       let provider = Decode.xid buf in
       let atom = Decode.xid buf in
       let timestamp = Decode.u32 buf in
       let state = Decode.u8 buf in
       Decode.pad buf 11; { window; provider; atom; timestamp; state } : 
      provider_property)
    let decode_resource_change buf =
      (let timestamp = Decode.u32 buf in
       let window = Decode.xid buf in
       Decode.pad buf 20; { timestamp; window } : resource_change)
    let decode_monitor_info buf =
      (let name = Decode.xid buf in
       let primary = Decode.bool buf in
       let automatic = Decode.bool buf in
       let n_output = Decode.u16 buf in
       let x = Decode.i16 buf in
       let y = Decode.i16 buf in
       let width = Decode.u16 buf in
       let height = Decode.u16 buf in
       let width_in_millimeters = Decode.u32 buf in
       let height_in_millimeters = Decode.u32 buf in
       let outputs = (Decode.list ~item:Decode.xid) ~len:n_output buf in
       {
         name;
         primary;
         automatic;
         x;
         y;
         width;
         height;
         width_in_millimeters;
         height_in_millimeters;
         outputs
       } : monitor_info)
    let decode_get_monitors_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let timestamp = Decode.u32 buf in
        let n_monitors = Decode.u32 buf in
        let n_outputs = Decode.u32 buf in
        Decode.pad buf 12;
        (let monitors =
           (Decode.list ~item:decode_monitor_info) ~len:n_monitors buf in
         { timestamp; n_outputs; monitors })) : Get_monitors.Reply.t)
    let decode_create_lease_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let nfd = Decode.u8 buf in
        Decode.pad buf 6;
        (let master_fd = Decode.file_descr buf in
         Decode.pad buf 24; { nfd; master_fd })) : Create_lease.Reply.t)
    let decode_lease_notify buf =
      (let timestamp = Decode.u32 buf in
       let window = Decode.xid buf in
       let lease = Decode.xid buf in
       let created = Decode.u8 buf in
       Decode.pad buf 15; { timestamp; window; lease; created } : lease_notify)
    let decode_notify_variant buf ~tag  =
      (match tag with
       | 0 -> let cc = decode_crtc_change buf in `Crtc_change cc
       | 1 -> let oc = decode_output_change buf in `Output_change oc
       | 2 -> let op = decode_output_property buf in `Output_property op
       | 3 -> let pc = decode_provider_change buf in `Provider_change pc
       | 4 -> let pp = decode_provider_property buf in `Provider_property pp
       | 5 -> let rc = decode_resource_change buf in `Resource_change rc
       | 6 -> let lc = decode_lease_notify buf in `Lease lc
       | n -> invalid_arg ("Invalid enum value: " ^ (string_of_int n)) : 
      Notify.t)
    let decode_notify_event buf =
      (Decode.pad buf 1;
       (let u_tag = Decode.u8 buf in
        Decode.pad buf 2; (let u = decode_notify_variant ~tag:u_tag buf in u)) : 
      Event.Notify.t)
    let decode_error ~number  buf =
      (Decode.pad buf 4;
       (match number with
        | 0 -> let error = decode_bad_output_error buf in `Bad_output error
        | 1 -> let error = decode_bad_crtc_error buf in `Bad_crtc error
        | 2 -> let error = decode_bad_mode_error buf in `Bad_mode error
        | 3 ->
            let error = decode_bad_provider_error buf in `Bad_provider error
        | n -> invalid_arg ("Invalid error number: " ^ (string_of_int n))) : 
      Error.t)
  end
module Sync_codec =
  struct
    open Protocol.Sync
    let decode_int64 buf =
      (let hi = Decode.i32 buf in let lo = Decode.u32 buf in { hi; lo } : 
      int64)
    let decode_systemcounter buf =
      (let counter = Decode.xid buf in
       let resolution = decode_int64 buf in
       let name_len = Decode.u16 buf in
       let name = Decode.string ~len:name_len buf in
       Decode.align buf 4; { counter; resolution; name } : systemcounter)
    let decode_trigger buf =
      (let counter = Decode.xid buf in
       let wait_type =
         ((Decode.u32 %> Conv.To_int.u32) %> Valuetype_enum.of_int) buf in
       let wait_value = decode_int64 buf in
       let test_type =
         ((Decode.u32 %> Conv.To_int.u32) %> Testtype_enum.of_int) buf in
       { counter; wait_type; wait_value; test_type } : trigger)
    let decode_waitcondition buf =
      (let trigger = decode_trigger buf in
       let event_threshold = decode_int64 buf in { trigger; event_threshold } : 
      waitcondition)
    let decode_counter_error buf =
      (let bad_counter = Decode.u32 buf in
       Decode.pad buf 3; Decode.align buf 32; bad_counter : Error.Counter.t)
    let decode_alarm_error buf =
      (let bad_alarm = Decode.u32 buf in
       Decode.pad buf 3; Decode.align buf 32; bad_alarm : Error.Alarm.t)
    let decode_initialize_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let major_version = Decode.u8 buf in
        let minor_version = Decode.u8 buf in
        Decode.pad buf 22; { major_version; minor_version }) : Initialize.Reply.t)
    let decode_list_system_counters_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let counters_len = Decode.u32 buf in
        Decode.pad buf 20;
        (let counters =
           (Decode.list ~item:decode_systemcounter) ~len:counters_len buf in
         counters)) : List_system_counters.Reply.t)
    let decode_query_counter_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let counter_value = decode_int64 buf in counter_value) : Query_counter.Reply.t)
    let decode_query_alarm_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let trigger = decode_trigger buf in
        let delta = decode_int64 buf in
        let events = Decode.bool buf in
        let state =
          ((Decode.u8 %> Conv.To_int.u8) %> Alarmstate_enum.of_int) buf in
        Decode.pad buf 2; { trigger; delta; events; state }) : Query_alarm.Reply.t)
    let decode_get_priority_reply ~length:(length : int)  buf =
      (Decode.pad buf 8; (let priority = Decode.i32 buf in priority) : 
      Get_priority.Reply.t)
    let decode_query_fence_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let triggered = Decode.bool buf in Decode.pad buf 23; triggered) : 
      Query_fence.Reply.t)
    let decode_counter_notify_event buf =
      (Decode.pad buf 1;
       (let kind = Decode.u8 buf in
        Decode.pad buf 2;
        (let counter = Decode.xid buf in
         let wait_value = decode_int64 buf in
         let counter_value = decode_int64 buf in
         let timestamp = Decode.u32 buf in
         let count = Decode.u16 buf in
         let destroyed = Decode.bool buf in
         Decode.pad buf 1;
         {
           kind;
           counter;
           wait_value;
           counter_value;
           timestamp;
           count;
           destroyed
         })) : Event.Counter_notify.t)
    let decode_alarm_notify_event buf =
      (Decode.pad buf 1;
       (let kind = Decode.u8 buf in
        Decode.pad buf 2;
        (let alarm = Decode.xid buf in
         let counter_value = decode_int64 buf in
         let alarm_value = decode_int64 buf in
         let timestamp = Decode.u32 buf in
         let state =
           ((Decode.u8 %> Conv.To_int.u8) %> Alarmstate_enum.of_int) buf in
         Decode.pad buf 3;
         { kind; alarm; counter_value; alarm_value; timestamp; state })) : 
      Event.Alarm_notify.t)
    let decode_error ~number  buf =
      (Decode.pad buf 4;
       (match number with
        | 0 -> let error = decode_counter_error buf in `Counter error
        | 1 -> let error = decode_alarm_error buf in `Alarm error
        | n -> invalid_arg ("Invalid error number: " ^ (string_of_int n))) : 
      Error.t)
  end
module Present_codec =
  struct
    open Protocol.Present
    let decode_notify buf =
      (let window = Decode.xid buf in
       let serial = Decode.u32 buf in { window; serial } : notify)
    let decode_query_version_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let major_version = Decode.u32 buf in
        let minor_version = Decode.u32 buf in
        { major_version; minor_version }) : Query_version.Reply.t)
    let decode_query_capabilities_reply ~length:(length : int)  buf =
      (Decode.pad buf 8; (let capabilities = Decode.u32 buf in capabilities) : 
      Query_capabilities.Reply.t)
    let decode_generic_event buf =
      (Decode.pad buf 1;
       (let extension = Decode.u8 buf in
        Decode.pad buf 2;
        (let length = Decode.u32 buf in
         let evtype = Decode.u16 buf in
         Decode.pad buf 2;
         (let event = Decode.xid buf in { extension; length; evtype; event }))) : 
      Event.Generic.t)
    let decode_configure_notify_event buf =
      (Decode.pad buf 5;
       (let event = Decode.xid buf in
        let window = Decode.xid buf in
        let x = Decode.i16 buf in
        let y = Decode.i16 buf in
        let width = Decode.u16 buf in
        let height = Decode.u16 buf in
        let off_x = Decode.i16 buf in
        let off_y = Decode.i16 buf in
        let pixmap_width = Decode.u16 buf in
        let pixmap_height = Decode.u16 buf in
        let pixmap_flags = Decode.u32 buf in
        {
          event;
          window;
          x;
          y;
          width;
          height;
          off_x;
          off_y;
          pixmap_width;
          pixmap_height;
          pixmap_flags
        }) : Event.Configure_notify.t)
    let decode_complete_notify_event buf =
      (Decode.pad buf 1;
       (let kind =
          ((Decode.u8 %> Conv.To_int.u8) %> Complete_kind_enum.of_int) buf in
        Decode.pad buf 2;
        (let mode =
           ((Decode.u8 %> Conv.To_int.u8) %> Complete_mode_enum.of_int) buf in
         let event = Decode.xid buf in
         let window = Decode.xid buf in
         let serial = Decode.u32 buf in
         let ust = Decode.u64 buf in
         let msc = Decode.u64 buf in
         { kind; mode; event; window; serial; ust; msc })) : Event.Complete_notify.t)
    let decode_idle_notify_event buf =
      (Decode.pad buf 5;
       (let event = Decode.xid buf in
        let window = Decode.xid buf in
        let serial = Decode.u32 buf in
        let pixmap = Decode.xid buf in
        let idle_fence = Decode.xid buf in
        { event; window; serial; pixmap; idle_fence }) : Event.Idle_notify.t)
    let decode_redirect_notify_event buf =
      (Decode.pad buf 1;
       (let update_window = Decode.bool buf in
        Decode.pad buf 3;
        (let event = Decode.xid buf in
         let event_window = Decode.xid buf in
         let window = Decode.xid buf in
         let pixmap = Decode.xid buf in
         let serial = Decode.u32 buf in
         let valid_region = Decode.xid buf in
         let update_region = Decode.xid buf in
         let valid_rect = Core_codec.decode_rectangle buf in
         let update_rect = Core_codec.decode_rectangle buf in
         let x_off = Decode.i16 buf in
         let y_off = Decode.i16 buf in
         let target_crtc = Decode.xid buf in
         let wait_fence = Decode.xid buf in
         let idle_fence = Decode.xid buf in
         let options = Decode.u32 buf in
         Decode.pad buf 4;
         (let target_msc = Decode.u64 buf in
          let divisor = Decode.u64 buf in
          let remainder = Decode.u64 buf in
          let notifies = Decode.list_no_length ~item:decode_notify buf in
          {
            update_window;
            event;
            event_window;
            window;
            pixmap;
            serial;
            valid_region;
            update_region;
            valid_rect;
            update_rect;
            x_off;
            y_off;
            target_crtc;
            wait_fence;
            idle_fence;
            options;
            target_msc;
            divisor;
            remainder;
            notifies
          }))) : Event.Redirect_notify.t)
    let decode_error ~number  buf =
      (Decode.pad buf 4;
       (match number with
        | n -> invalid_arg ("Invalid error number: " ^ (string_of_int n))) : 
      Error.t)
  end
module Record_codec =
  struct
    open Protocol.Record
    let decode_range8 buf =
      (let first = Decode.u8 buf in
       let last = Decode.u8 buf in { first; last } : range8)
    let decode_range16 buf =
      (let first = Decode.u16 buf in
       let last = Decode.u16 buf in { first; last } : range16)
    let decode_ext_range buf =
      (let major = decode_range8 buf in
       let minor = decode_range16 buf in { major; minor } : ext_range)
    let decode_range buf =
      (let core_requests = decode_range8 buf in
       let core_replies = decode_range8 buf in
       let ext_requests = decode_ext_range buf in
       let ext_replies = decode_ext_range buf in
       let delivered_events = decode_range8 buf in
       let device_events = decode_range8 buf in
       let errors = decode_range8 buf in
       let client_started = Decode.bool buf in
       let client_died = Decode.bool buf in
       {
         core_requests;
         core_replies;
         ext_requests;
         ext_replies;
         delivered_events;
         device_events;
         errors;
         client_started;
         client_died
       } : range)
    let decode_client_info buf =
      (let client_resource = Decode.u32 buf in
       let num_ranges = Decode.u32 buf in
       let ranges = (Decode.list ~item:decode_range) ~len:num_ranges buf in
       { client_resource; ranges } : client_info)
    let decode_bad_context_error buf =
      (let invalid_record = Decode.u32 buf in
       Decode.align buf 32; invalid_record : Error.Bad_context.t)
    let decode_query_version_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let major_version = Decode.u16 buf in
        let minor_version = Decode.u16 buf in
        { major_version; minor_version }) : Query_version.Reply.t)
    let decode_get_context_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let enabled = Decode.bool buf in
        Decode.pad buf 6;
        (let element_header = Decode.u8 buf in
         Decode.pad buf 3;
         (let num_intercepted_clients = Decode.u32 buf in
          Decode.pad buf 16;
          (let intercepted_clients =
             (Decode.list ~item:decode_client_info)
               ~len:num_intercepted_clients buf in
           { enabled; element_header; intercepted_clients })))) : Get_context.Reply.t)
    let decode_enable_context_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let category = Decode.u8 buf in
        Decode.pad buf 6;
        (let element_header = Decode.u8 buf in
         let client_swapped = Decode.bool buf in
         Decode.pad buf 2;
         (let xid_base = Decode.u32 buf in
          let server_time = Decode.u32 buf in
          let rec_sequence_num = Decode.u32 buf in
          Decode.pad buf 8;
          (let data = (Decode.list ~item:Decode.byte) ~len:(length * 4) buf in
           {
             category;
             element_header;
             client_swapped;
             xid_base;
             server_time;
             rec_sequence_num;
             data
           })))) : Enable_context.Reply.t)
    let decode_error ~number  buf =
      (Decode.pad buf 4;
       (match number with
        | 0 -> let error = decode_bad_context_error buf in `Bad_context error
        | n -> invalid_arg ("Invalid error number: " ^ (string_of_int n))) : 
      Error.t)
  end
module Res_codec =
  struct
    open Protocol.Res
    let decode_client buf =
      (let resource_base = Decode.u32 buf in
       let resource_mask = Decode.u32 buf in { resource_base; resource_mask } : 
      client)
    let decode_type buf =
      (let resource_type = Decode.xid buf in
       let count = Decode.u32 buf in { resource_type; count } : type_)
    let decode_client_id_spec buf =
      (let client = Decode.u32 buf in
       let mask =
         ((Decode.u32 %> Conv.To_i32.u32) %> Client_id_mask.of_int32) buf in
       { client; mask } : client_id_spec)
    let decode_client_id_value buf =
      (let spec = decode_client_id_spec buf in
       let length = Decode.u32 buf in
       let length = length * 4 in
       let value = (Decode.list ~item:Decode.u32) ~len:length buf in
       { spec; value } : client_id_value)
    let decode_resource_id_spec buf =
      (let resource = Decode.u32 buf in
       let type_ = Decode.u32 buf in { resource; type_ } : resource_id_spec)
    let decode_resource_size_spec buf =
      (let spec = decode_resource_id_spec buf in
       let bytes = Decode.u32 buf in
       let ref_count = Decode.u32 buf in
       let use_count = Decode.u32 buf in
       { spec; bytes; ref_count; use_count } : resource_size_spec)
    let decode_resource_size_value buf =
      (let size = decode_resource_size_spec buf in
       let num_cross_references = Decode.u32 buf in
       let cross_references =
         (Decode.list ~item:decode_resource_size_spec)
           ~len:num_cross_references buf in
       { size; cross_references } : resource_size_value)
    let decode_query_version_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let server_major = Decode.u16 buf in
        let server_minor = Decode.u16 buf in { server_major; server_minor }) : 
      Query_version.Reply.t)
    let decode_query_clients_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let num_clients = Decode.u32 buf in
        Decode.pad buf 20;
        (let clients = (Decode.list ~item:decode_client) ~len:num_clients buf in
         clients)) : Query_clients.Reply.t)
    let decode_query_client_resources_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let num_types = Decode.u32 buf in
        Decode.pad buf 20;
        (let types = (Decode.list ~item:decode_type) ~len:num_types buf in
         types)) : Query_client_resources.Reply.t)
    let decode_query_client_pixmap_bytes_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let bytes = Decode.u32 buf in
        let bytes_overflow = Decode.u32 buf in { bytes; bytes_overflow }) : 
      Query_client_pixmap_bytes.Reply.t)
    let decode_query_client_ids_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let num_ids = Decode.u32 buf in
        Decode.pad buf 20;
        (let ids =
           (Decode.list ~item:decode_client_id_value) ~len:num_ids buf in
         ids)) : Query_client_ids.Reply.t)
    let decode_query_resource_bytes_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let num_sizes = Decode.u32 buf in
        Decode.pad buf 20;
        (let sizes =
           (Decode.list ~item:decode_resource_size_value) ~len:num_sizes buf in
         sizes)) : Query_resource_bytes.Reply.t)
    let decode_error ~number  buf =
      (Decode.pad buf 4;
       (match number with
        | n -> invalid_arg ("Invalid error number: " ^ (string_of_int n))) : 
      Error.t)
  end
module Screensaver_codec =
  struct
    open Protocol.Screensaver
    let decode_query_version_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let server_major_version = Decode.u16 buf in
        let server_minor_version = Decode.u16 buf in
        Decode.pad buf 20; { server_major_version; server_minor_version }) : 
      Query_version.Reply.t)
    let decode_query_info_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let state = Decode.u8 buf in
        Decode.pad buf 6;
        (let saver_window = Decode.xid buf in
         let ms_until_server = Decode.u32 buf in
         let ms_since_user_input = Decode.u32 buf in
         let event_mask = Decode.u32 buf in
         let kind =
           ((Decode.byte %> Conv.To_int.byte) %> Kind_enum.of_int) buf in
         Decode.pad buf 7;
         {
           state;
           saver_window;
           ms_until_server;
           ms_since_user_input;
           event_mask;
           kind
         })) : Query_info.Reply.t)
    let decode_notify_event buf =
      (Decode.pad buf 1;
       (let state =
          ((Decode.byte %> Conv.To_int.byte) %> State_enum.of_int) buf in
        Decode.pad buf 2;
        (let time = Decode.u32 buf in
         let root = Decode.xid buf in
         let window = Decode.xid buf in
         let kind =
           ((Decode.byte %> Conv.To_int.byte) %> Kind_enum.of_int) buf in
         let forced = Decode.bool buf in
         Decode.pad buf 14; { state; time; root; window; kind; forced })) : 
      Event.Notify.t)
    let decode_error ~number  buf =
      (Decode.pad buf 4;
       (match number with
        | n -> invalid_arg ("Invalid error number: " ^ (string_of_int n))) : 
      Error.t)
  end
module Shm_codec =
  struct
    open Protocol.Shm
    let decode_completion_event buf =
      (Decode.pad buf 4;
       (let drawable = Decode.xid buf in
        let minor_event = Decode.u16 buf in
        let major_event = Decode.byte buf in
        Decode.pad buf 1;
        (let shmseg = Decode.xid buf in
         let offset = Decode.u32 buf in
         { drawable; minor_event; major_event; shmseg; offset })) : Event.Completion.t)
    let decode_bad_seg_error = Core_codec.decode_value_error
    let decode_query_version_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let shared_pixmaps = Decode.bool buf in
        Decode.pad buf 6;
        (let major_version = Decode.u16 buf in
         let minor_version = Decode.u16 buf in
         let uid = Decode.u16 buf in
         let gid = Decode.u16 buf in
         let pixmap_format = Decode.u8 buf in
         Decode.pad buf 15;
         {
           shared_pixmaps;
           major_version;
           minor_version;
           uid;
           gid;
           pixmap_format
         })) : Query_version.Reply.t)
    let decode_get_image_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let depth = Decode.u8 buf in
        Decode.pad buf 6;
        (let visual = Decode.u32 buf in
         let size = Decode.u32 buf in { depth; visual; size })) : Get_image.Reply.t)
    let decode_create_segment_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let nfd = Decode.u8 buf in
        Decode.pad buf 6;
        (let shm_fd = Decode.file_descr buf in
         Decode.pad buf 24; { nfd; shm_fd })) : Create_segment.Reply.t)
    let decode_error ~number  buf =
      (Decode.pad buf 4;
       (match number with
        | 0 -> let error = decode_bad_seg_error buf in `Bad_seg error
        | n -> invalid_arg ("Invalid error number: " ^ (string_of_int n))) : 
      Error.t)
  end
module Xc_misc_codec =
  struct
    open Protocol.Xc_misc
    let decode_get_version_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let server_major_version = Decode.u16 buf in
        let server_minor_version = Decode.u16 buf in
        { server_major_version; server_minor_version }) : Get_version.Reply.t)
    let decode_get_xid_range_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let start_id = Decode.u32 buf in
        let count = Decode.u32 buf in { start_id; count }) : Get_xid_range.Reply.t)
    let decode_get_xid_list_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let ids_len = Decode.u32 buf in
        Decode.pad buf 20;
        (let ids = (Decode.list ~item:Decode.u32) ~len:ids_len buf in ids)) : 
      Get_xid_list.Reply.t)
    let decode_error ~number  buf =
      (Decode.pad buf 4;
       (match number with
        | n -> invalid_arg ("Invalid error number: " ^ (string_of_int n))) : 
      Error.t)
  end
module Xevie_codec =
  struct
    open Protocol.Xevie
    let decode_query_version_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let server_major_version = Decode.u16 buf in
        let server_minor_version = Decode.u16 buf in
        Decode.pad buf 20; { server_major_version; server_minor_version }) : 
      Query_version.Reply.t)
    let decode_start_reply ~length:(length : int)  buf =
      (Decode.pad buf 32; () : Start.Reply.t)
    let decode_end_reply ~length:(length : int)  buf =
      (Decode.pad buf 32; () : End_.Reply.t)
    let decode_event buf = (Decode.pad buf 32; () : event)
    let decode_send_reply ~length:(length : int)  buf =
      (Decode.pad buf 32; () : Send.Reply.t)
    let decode_select_input_reply ~length:(length : int)  buf =
      (Decode.pad buf 32; () : Select_input.Reply.t)
    let decode_error ~number  buf =
      (Decode.pad buf 4;
       (match number with
        | n -> invalid_arg ("Invalid error number: " ^ (string_of_int n))) : 
      Error.t)
  end
module Xf86dri_codec =
  struct
    open Protocol.Xf86dri
    let decode_drm_clip_rect buf =
      (let x1 = Decode.i16 buf in
       let y1 = Decode.i16 buf in
       let x2 = Decode.i16 buf in
       let x3 = Decode.i16 buf in { x1; y1; x2; x3 } : drm_clip_rect)
    let decode_query_version_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let dri_major_version = Decode.u16 buf in
        let dri_minor_version = Decode.u16 buf in
        let dri_minor_patch = Decode.u32 buf in
        { dri_major_version; dri_minor_version; dri_minor_patch }) : 
      Query_version.Reply.t)
    let decode_query_direct_rendering_capable_reply ~length:(length : int) 
      buf =
      (Decode.pad buf 8; (let is_capable = Decode.bool buf in is_capable) : 
      Query_direct_rendering_capable.Reply.t)
    let decode_open_connection_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let sarea_handle_low = Decode.u32 buf in
        let sarea_handle_high = Decode.u32 buf in
        let bus_id_len = Decode.u32 buf in
        Decode.pad buf 12;
        (let bus_id = Decode.string ~len:bus_id_len buf in
         { sarea_handle_low; sarea_handle_high; bus_id })) : Open_connection.Reply.t)
    let decode_get_client_driver_name_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let client_driver_major_version = Decode.u32 buf in
        let client_driver_minor_version = Decode.u32 buf in
        let client_driver_patch_version = Decode.u32 buf in
        let client_driver_name_len = Decode.u32 buf in
        Decode.pad buf 8;
        (let client_driver_name =
           Decode.string ~len:client_driver_name_len buf in
         {
           client_driver_major_version;
           client_driver_minor_version;
           client_driver_patch_version;
           client_driver_name
         })) : Get_client_driver_name.Reply.t)
    let decode_create_context_reply ~length:(length : int)  buf =
      (Decode.pad buf 8; (let hw_context = Decode.u32 buf in hw_context) : 
      Create_context.Reply.t)
    let decode_create_drawable_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let hw_drawable_handle = Decode.u32 buf in hw_drawable_handle) : 
      Create_drawable.Reply.t)
    let decode_get_drawable_info_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let drawable_table_index = Decode.u32 buf in
        let drawable_table_stamp = Decode.u32 buf in
        let drawable_origin_x = Decode.i16 buf in
        let drawable_origin_y = Decode.i16 buf in
        let drawable_size_w = Decode.i16 buf in
        let drawable_size_h = Decode.i16 buf in
        let num_clip_rects = Decode.u32 buf in
        let back_x = Decode.i16 buf in
        let back_y = Decode.i16 buf in
        let num_back_clip_rects = Decode.u32 buf in
        let clip_rects =
          (Decode.list ~item:decode_drm_clip_rect) ~len:num_clip_rects buf in
        let back_clip_rects =
          (Decode.list ~item:decode_drm_clip_rect) ~len:num_back_clip_rects
            buf in
        {
          drawable_table_index;
          drawable_table_stamp;
          drawable_origin_x;
          drawable_origin_y;
          drawable_size_w;
          drawable_size_h;
          back_x;
          back_y;
          clip_rects;
          back_clip_rects
        }) : Get_drawable_info.Reply.t)
    let decode_get_device_info_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let framebuffer_handle_low = Decode.u32 buf in
        let framebuffer_handle_high = Decode.u32 buf in
        let framebuffer_origin_offset = Decode.u32 buf in
        let framebuffer_size = Decode.u32 buf in
        let framebuffer_stride = Decode.u32 buf in
        let device_private_size = Decode.u32 buf in
        let device_private =
          (Decode.list ~item:Decode.u32) ~len:device_private_size buf in
        {
          framebuffer_handle_low;
          framebuffer_handle_high;
          framebuffer_origin_offset;
          framebuffer_size;
          framebuffer_stride;
          device_private
        }) : Get_device_info.Reply.t)
    let decode_auth_connection_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let authenticated = Decode.u32 buf in authenticated) : Auth_connection.Reply.t)
    let decode_error ~number  buf =
      (Decode.pad buf 4;
       (match number with
        | n -> invalid_arg ("Invalid error number: " ^ (string_of_int n))) : 
      Error.t)
  end
module Xf86vidmode_codec =
  struct
    open Protocol.Xf86vidmode
    let decode_mode_info buf =
      (let dotclock = Decode.u32 buf in
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
        Decode.pad buf 12;
        (let privsize = Decode.u32 buf in
         {
           dotclock;
           hdisplay;
           hsyncstart;
           hsyncend;
           htotal;
           hskew;
           vdisplay;
           vsyncstart;
           vsyncend;
           vtotal;
           flags;
           privsize
         })) : mode_info)
    let decode_query_version_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let major_version = Decode.u16 buf in
        let minor_version = Decode.u16 buf in
        { major_version; minor_version }) : Query_version.Reply.t)
    let decode_get_mode_line_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let dotclock = Decode.u32 buf in
        let hdisplay = Decode.u16 buf in
        let hsyncstart = Decode.u16 buf in
        let hsyncend = Decode.u16 buf in
        let htotal = Decode.u16 buf in
        let hskew = Decode.u16 buf in
        let vdisplay = Decode.u16 buf in
        let vsyncstart = Decode.u16 buf in
        let vsyncend = Decode.u16 buf in
        let vtotal = Decode.u16 buf in
        Decode.pad buf 2;
        (let flags =
           ((Decode.u32 %> Conv.To_i32.u32) %> Mode_flag_mask.of_int32) buf in
         Decode.pad buf 12;
         (let privsize = Decode.u32 buf in
          let private_ = (Decode.list ~item:Decode.u8) ~len:privsize buf in
          {
            dotclock;
            hdisplay;
            hsyncstart;
            hsyncend;
            htotal;
            hskew;
            vdisplay;
            vsyncstart;
            vsyncend;
            vtotal;
            flags;
            private_
          }))) : Get_mode_line.Reply.t)
    let decode_get_monitor_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let vendor_length = Decode.u8 buf in
        let model_length = Decode.u8 buf in
        let num_hsync = Decode.u8 buf in
        let num_vsync = Decode.u8 buf in
        Decode.pad buf 20;
        (let hsync = (Decode.list ~item:Decode.u32) ~len:num_hsync buf in
         let vsync = (Decode.list ~item:Decode.u32) ~len:num_vsync buf in
         let vendor = Decode.string ~len:vendor_length buf in
         let alignment_pad =
           Decode.string
             ~len:(((vendor_length + 3) land (lnot 3)) - vendor_length) buf in
         let model = Decode.string ~len:model_length buf in
         { hsync; vsync; vendor; alignment_pad; model })) : Get_monitor.Reply.t)
    let decode_get_all_mode_lines_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let modecount = Decode.u32 buf in
        Decode.pad buf 20;
        (let modeinfo =
           (Decode.list ~item:decode_mode_info) ~len:modecount buf in
         modeinfo)) : Get_all_mode_lines.Reply.t)
    let decode_validate_mode_line_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let status = Decode.u32 buf in Decode.pad buf 20; status) : Validate_mode_line.Reply.t)
    let decode_get_view_port_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let x = Decode.u32 buf in
        let y = Decode.u32 buf in Decode.pad buf 16; { x; y }) : Get_view_port.Reply.t)
    let decode_get_dot_clocks_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let flags =
          ((Decode.u32 %> Conv.To_i32.u32) %> Clock_flag_mask.of_int32) buf in
        let clocks = Decode.u32 buf in
        let maxclocks = Decode.u32 buf in
        Decode.pad buf 12;
        (let clock =
           (Decode.list ~item:Decode.u32)
             ~len:((1 - ((Clock_flag_mask.to_int32 flags) land 1)) * clocks)
             buf in
         { flags; clocks; maxclocks; clock })) : Get_dot_clocks.Reply.t)
    let decode_get_gamma_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let red = Decode.u32 buf in
        let green = Decode.u32 buf in
        let blue = Decode.u32 buf in Decode.pad buf 12; { red; green; blue }) : 
      Get_gamma.Reply.t)
    let decode_get_gamma_ramp_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let size = Decode.u16 buf in
        Decode.pad buf 22;
        (let red =
           (Decode.list ~item:Decode.u16) ~len:((size + 1) land (lnot 1)) buf in
         let green =
           (Decode.list ~item:Decode.u16) ~len:((size + 1) land (lnot 1)) buf in
         let blue =
           (Decode.list ~item:Decode.u16) ~len:((size + 1) land (lnot 1)) buf in
         { size; red; green; blue })) : Get_gamma_ramp.Reply.t)
    let decode_get_gamma_ramp_size_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let size = Decode.u16 buf in Decode.pad buf 22; size) : Get_gamma_ramp_size.Reply.t)
    let decode_get_permissions_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let permissions =
          ((Decode.u32 %> Conv.To_i32.u32) %> Permission_mask.of_int32) buf in
        Decode.pad buf 20; permissions) : Get_permissions.Reply.t)
    let decode_bad_clock_error buf =
      (Decode.align buf 32; () : Error.Bad_clock.t)
    let decode_bad_h_timings_error buf =
      (Decode.align buf 32; () : Error.Bad_h_timings.t)
    let decode_bad_v_timings_error buf =
      (Decode.align buf 32; () : Error.Bad_v_timings.t)
    let decode_mode_unsuitable_error buf =
      (Decode.align buf 32; () : Error.Mode_unsuitable.t)
    let decode_extension_disabled_error buf =
      (Decode.align buf 32; () : Error.Extension_disabled.t)
    let decode_client_not_local_error buf =
      (Decode.align buf 32; () : Error.Client_not_local.t)
    let decode_zoom_locked_error buf =
      (Decode.align buf 32; () : Error.Zoom_locked.t)
    let decode_error ~number  buf =
      (Decode.pad buf 4;
       (match number with
        | 0 -> let error = decode_bad_clock_error buf in `Bad_clock error
        | 1 ->
            let error = decode_bad_h_timings_error buf in
            `Bad_h_timings error
        | 2 ->
            let error = decode_bad_v_timings_error buf in
            `Bad_v_timings error
        | 3 ->
            let error = decode_mode_unsuitable_error buf in
            `Mode_unsuitable error
        | 4 ->
            let error = decode_extension_disabled_error buf in
            `Extension_disabled error
        | 5 ->
            let error = decode_client_not_local_error buf in
            `Client_not_local error
        | 6 -> let error = decode_zoom_locked_error buf in `Zoom_locked error
        | n -> invalid_arg ("Invalid error number: " ^ (string_of_int n))) : 
      Error.t)
  end
module Xinerama_codec =
  struct
    open Protocol.Xinerama
    let decode_screen_info buf =
      (let x_org = Decode.i16 buf in
       let y_org = Decode.i16 buf in
       let width = Decode.u16 buf in
       let height = Decode.u16 buf in { x_org; y_org; width; height } : 
      screen_info)
    let decode_query_version_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let major = Decode.u16 buf in
        let minor = Decode.u16 buf in { major; minor }) : Query_version.Reply.t)
    let decode_get_state_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let state = Decode.byte buf in
        Decode.pad buf 6; (let window = Decode.xid buf in { state; window })) : 
      Get_state.Reply.t)
    let decode_get_screen_count_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let screen_count = Decode.byte buf in
        Decode.pad buf 6;
        (let window = Decode.xid buf in { screen_count; window })) : 
      Get_screen_count.Reply.t)
    let decode_get_screen_size_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let width = Decode.u32 buf in
        let height = Decode.u32 buf in
        let window = Decode.xid buf in
        let screen = Decode.u32 buf in { width; height; window; screen }) : 
      Get_screen_size.Reply.t)
    let decode_is_active_reply ~length:(length : int)  buf =
      (Decode.pad buf 8; (let state = Decode.u32 buf in state) : Is_active.Reply.t)
    let decode_query_screens_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let number = Decode.u32 buf in
        Decode.pad buf 20;
        (let screen_info =
           (Decode.list ~item:decode_screen_info) ~len:number buf in
         screen_info)) : Query_screens.Reply.t)
    let decode_error ~number  buf =
      (Decode.pad buf 4;
       (match number with
        | n -> invalid_arg ("Invalid error number: " ^ (string_of_int n))) : 
      Error.t)
  end
module Xinput_codec =
  struct
    open Protocol.Xinput
    let decode_fp3232 buf =
      (let integral = Decode.i32 buf in
       let frac = Decode.u32 buf in { integral; frac } : fp3232)
    let decode_get_extension_version_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let xi_reply_type = Decode.u8 buf in
        Decode.pad buf 6;
        (let server_major = Decode.u16 buf in
         let server_minor = Decode.u16 buf in
         let present = Decode.bool buf in
         Decode.pad buf 19;
         { xi_reply_type; server_major; server_minor; present })) : Get_extension_version.Reply.t)
    let decode_device_info buf =
      (let device_type = Decode.xid buf in
       let device_id = Decode.u8 buf in
       let num_class_info = Decode.u8 buf in
       let device_use =
         ((Decode.u8 %> Conv.To_int.u8) %> Device_use_enum.of_int) buf in
       Decode.pad buf 1;
       { device_type; device_id; num_class_info; device_use } : device_info)
    let decode_key_info buf =
      (let class_id =
         ((Decode.u8 %> Conv.To_int.u8) %> Input_class_enum.of_int) buf in
       let len = Decode.u8 buf in
       let min_keycode = Decode.u8 buf in
       let max_keycode = Decode.u8 buf in
       let num_keys = Decode.u16 buf in
       Decode.pad buf 2;
       { class_id; len; min_keycode; max_keycode; num_keys } : key_info)
    let decode_button_info buf =
      (let class_id =
         ((Decode.u8 %> Conv.To_int.u8) %> Input_class_enum.of_int) buf in
       let len = Decode.u8 buf in
       let num_buttons = Decode.u16 buf in { class_id; len; num_buttons } : 
      button_info)
    let decode_axis_info buf =
      (let resolution = Decode.u32 buf in
       let minimum = Decode.i32 buf in
       let maximum = Decode.i32 buf in { resolution; minimum; maximum } : 
      axis_info)
    let decode_valuator_info buf =
      (let class_id =
         ((Decode.u8 %> Conv.To_int.u8) %> Input_class_enum.of_int) buf in
       let len = Decode.u8 buf in
       let axes_len = Decode.u8 buf in
       let mode =
         ((Decode.u8 %> Conv.To_int.u8) %> Valuator_mode_enum.of_int) buf in
       let motion_size = Decode.u32 buf in
       let axes = (Decode.list ~item:decode_axis_info) ~len:axes_len buf in
       { class_id; len; mode; motion_size; axes } : valuator_info)
    let decode_input_class_variant buf ~tag  =
      (match tag with
       | 0 ->
           let min_keycode = Decode.u8 buf in
           let max_keycode = Decode.u8 buf in
           let num_keys = Decode.u16 buf in
           (Decode.pad buf 2; `Key { min_keycode; max_keycode; num_keys })
       | 1 -> let num_buttons = Decode.u16 buf in `Button num_buttons
       | 2 ->
           let axes_len = Decode.u8 buf in
           let mode =
             ((Decode.u8 %> Conv.To_int.u8) %> Valuator_mode_enum.of_int) buf in
           let motion_size = Decode.u32 buf in
           let axes = (Decode.list ~item:decode_axis_info) ~len:axes_len buf in
           `Valuator { mode; motion_size; axes }
       | n -> invalid_arg ("Invalid enum value: " ^ (string_of_int n)) : 
      Input_class.t)
    let decode_input_info buf =
      (let info_tag = Decode.u8 buf in
       let len = Decode.u8 buf in
       let info = decode_input_class_variant ~tag:info_tag buf in
       { len; info } : input_info)
    let decode_device_name buf =
      (let len = Decode.u8 buf in
       let string = Decode.string ~len buf in string : device_name)
    let decode_list_input_devices_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let xi_reply_type = Decode.u8 buf in
        Decode.pad buf 6;
        (let devices_len = Decode.u8 buf in
         Decode.pad buf 23;
         (let devices =
            (Decode.list ~item:decode_device_info) ~len:devices_len buf in
          let infos =
            (Decode.list ~item:decode_input_info)
              ~len:(Conv.sum_map
                      ~f:(fun list_element_ref ->
                            list_element_ref.num_class_info) devices) buf in
          let names =
            (Decode.list ~item:Core_codec.decode_str) ~len:devices_len buf in
          Decode.align buf 4; { xi_reply_type; devices; infos; names }))) : 
      List_input_devices.Reply.t)
    let decode_input_class_info buf =
      (let class_id =
         ((Decode.u8 %> Conv.To_int.u8) %> Input_class_enum.of_int) buf in
       let event_type_base = Decode.u8 buf in { class_id; event_type_base } : 
      input_class_info)
    let decode_open_device_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let xi_reply_type = Decode.u8 buf in
        Decode.pad buf 6;
        (let num_classes = Decode.u8 buf in
         Decode.pad buf 23;
         (let class_info =
            (Decode.list ~item:decode_input_class_info) ~len:num_classes buf in
          Decode.align buf 4; { xi_reply_type; class_info }))) : Open_device.Reply.t)
    let decode_set_device_mode_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let xi_reply_type = Decode.u8 buf in
        Decode.pad buf 6;
        (let status =
           ((Decode.u8 %> Conv.To_int.u8) %> Core.Grab_status_enum.of_int)
             buf in
         Decode.pad buf 23; { xi_reply_type; status })) : Set_device_mode.Reply.t)
    let decode_get_selected_extension_events_reply ~length:(length : int) 
      buf =
      (Decode.pad buf 1;
       (let xi_reply_type = Decode.u8 buf in
        Decode.pad buf 6;
        (let num_this_classes = Decode.u16 buf in
         let num_all_classes = Decode.u16 buf in
         Decode.pad buf 20;
         (let this_classes =
            (Decode.list ~item:Decode.u32) ~len:num_this_classes buf in
          let all_classes =
            (Decode.list ~item:Decode.u32) ~len:num_all_classes buf in
          { xi_reply_type; this_classes; all_classes }))) : Get_selected_extension_events.Reply.t)
    let decode_get_device_dont_propagate_list_reply ~length:(length : int) 
      buf =
      (Decode.pad buf 1;
       (let xi_reply_type = Decode.u8 buf in
        Decode.pad buf 6;
        (let num_classes = Decode.u16 buf in
         Decode.pad buf 22;
         (let classes = (Decode.list ~item:Decode.u32) ~len:num_classes buf in
          { xi_reply_type; classes }))) : Get_device_dont_propagate_list.Reply.t)
    let decode_device_time_coord ~num_axes:ext_num_axes  buf =
      (let time = Decode.u32 buf in
       let axisvalues =
         (Decode.list ~item:Decode.i32) ~len:(Conv.To_int.u8 ext_num_axes)
           buf in
       { time; axisvalues } : device_time_coord)
    let decode_get_device_motion_events_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let xi_reply_type = Decode.u8 buf in
        Decode.pad buf 6;
        (let num_events = Decode.u32 buf in
         let num_axes = Decode.u8 buf in
         let device_mode =
           ((Decode.u8 %> Conv.To_int.u8) %> Valuator_mode_enum.of_int) buf in
         Decode.pad buf 18;
         (let events =
            (Decode.list ~item:(decode_device_time_coord ~num_axes))
              ~len:num_events buf in
          { xi_reply_type; num_axes; device_mode; events }))) : Get_device_motion_events.Reply.t)
    let decode_change_keyboard_device_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let xi_reply_type = Decode.u8 buf in
        Decode.pad buf 6;
        (let status =
           ((Decode.u8 %> Conv.To_int.u8) %> Core.Grab_status_enum.of_int)
             buf in
         Decode.pad buf 23; { xi_reply_type; status })) : Change_keyboard_device.Reply.t)
    let decode_change_pointer_device_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let xi_reply_type = Decode.u8 buf in
        Decode.pad buf 6;
        (let status =
           ((Decode.u8 %> Conv.To_int.u8) %> Core.Grab_status_enum.of_int)
             buf in
         Decode.pad buf 23; { xi_reply_type; status })) : Change_pointer_device.Reply.t)
    let decode_grab_device_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let xi_reply_type = Decode.u8 buf in
        Decode.pad buf 6;
        (let status =
           ((Decode.u8 %> Conv.To_int.u8) %> Core.Grab_status_enum.of_int)
             buf in
         Decode.pad buf 23; { xi_reply_type; status })) : Grab_device.Reply.t)
    let decode_get_device_focus_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let xi_reply_type = Decode.u8 buf in
        Decode.pad buf 6;
        (let focus =
           (Decode.xid %>
              (Conv.alt_enum ~enum_of_int:Core.Input_focus_enum.of_int
                 ~int_of_t:Conv.To_int.xid)) buf in
         let time = Decode.u32 buf in
         let revert_to =
           ((Decode.u8 %> Conv.To_int.u8) %> Core.Input_focus_enum.of_int)
             buf in
         Decode.pad buf 15; { xi_reply_type; focus; time; revert_to })) : 
      Get_device_focus.Reply.t)
    let decode_kbd_feedback_state buf =
      (let class_id =
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
       (let auto_repeats = (Decode.list ~item:Decode.u8) ~len:32 buf in
        {
          class_id;
          feedback_id;
          len;
          pitch;
          duration;
          led_mask;
          led_values;
          global_auto_repeat;
          click;
          percent;
          auto_repeats
        }) : kbd_feedback_state)
    let decode_ptr_feedback_state buf =
      (let class_id =
         ((Decode.u8 %> Conv.To_int.u8) %> Feedback_class_enum.of_int) buf in
       let feedback_id = Decode.u8 buf in
       let len = Decode.u16 buf in
       Decode.pad buf 2;
       (let accel_num = Decode.u16 buf in
        let accel_denom = Decode.u16 buf in
        let threshold = Decode.u16 buf in
        { class_id; feedback_id; len; accel_num; accel_denom; threshold }) : 
      ptr_feedback_state)
    let decode_integer_feedback_state buf =
      (let class_id =
         ((Decode.u8 %> Conv.To_int.u8) %> Feedback_class_enum.of_int) buf in
       let feedback_id = Decode.u8 buf in
       let len = Decode.u16 buf in
       let resolution = Decode.u32 buf in
       let min_value = Decode.i32 buf in
       let max_value = Decode.i32 buf in
       { class_id; feedback_id; len; resolution; min_value; max_value } : 
      integer_feedback_state)
    let decode_string_feedback_state buf =
      (let class_id =
         ((Decode.u8 %> Conv.To_int.u8) %> Feedback_class_enum.of_int) buf in
       let feedback_id = Decode.u8 buf in
       let len = Decode.u16 buf in
       let max_symbols = Decode.u16 buf in
       let num_keysyms = Decode.u16 buf in
       let keysyms = (Decode.list ~item:Decode.u32) ~len:num_keysyms buf in
       { class_id; feedback_id; len; max_symbols; keysyms } : string_feedback_state)
    let decode_bell_feedback_state buf =
      (let class_id =
         ((Decode.u8 %> Conv.To_int.u8) %> Feedback_class_enum.of_int) buf in
       let feedback_id = Decode.u8 buf in
       let len = Decode.u16 buf in
       let percent = Decode.u8 buf in
       Decode.pad buf 3;
       (let pitch = Decode.u16 buf in
        let duration = Decode.u16 buf in
        { class_id; feedback_id; len; percent; pitch; duration }) : bell_feedback_state)
    let decode_led_feedback_state buf =
      (let class_id =
         ((Decode.u8 %> Conv.To_int.u8) %> Feedback_class_enum.of_int) buf in
       let feedback_id = Decode.u8 buf in
       let len = Decode.u16 buf in
       let led_mask = Decode.u32 buf in
       let led_values = Decode.u32 buf in
       { class_id; feedback_id; len; led_mask; led_values } : led_feedback_state)
    let decode_feedback_class_variant buf ~tag  =
      (match tag with
       | 0 ->
           let pitch = Decode.u16 buf in
           let duration = Decode.u16 buf in
           let led_mask = Decode.u32 buf in
           let led_values = Decode.u32 buf in
           let global_auto_repeat = Decode.bool buf in
           let click = Decode.u8 buf in
           let percent = Decode.u8 buf in
           (Decode.pad buf 1;
            (let auto_repeats = (Decode.list ~item:Decode.u8) ~len:32 buf in
             `Keyboard
               {
                 pitch;
                 duration;
                 led_mask;
                 led_values;
                 global_auto_repeat;
                 click;
                 percent;
                 auto_repeats
               }))
       | 1 ->
           (Decode.pad buf 2;
            (let accel_num = Decode.u16 buf in
             let accel_denom = Decode.u16 buf in
             let threshold = Decode.u16 buf in
             `Pointer { accel_num; accel_denom; threshold }))
       | 2 ->
           let max_symbols = Decode.u16 buf in
           let num_keysyms = Decode.u16 buf in
           let keysyms = (Decode.list ~item:Decode.u32) ~len:num_keysyms buf in
           `String { max_symbols; keysyms }
       | 3 ->
           let resolution = Decode.u32 buf in
           let min_value = Decode.i32 buf in
           let max_value = Decode.i32 buf in
           `Integer { resolution; min_value; max_value }
       | 4 ->
           let led_mask = Decode.u32 buf in
           let led_values = Decode.u32 buf in `Led { led_mask; led_values }
       | 5 ->
           let percent = Decode.u8 buf in
           (Decode.pad buf 3;
            (let pitch = Decode.u16 buf in
             let duration = Decode.u16 buf in
             `Bell { percent; pitch; duration }))
       | n -> invalid_arg ("Invalid enum value: " ^ (string_of_int n)) : 
      Feedback_class.t)
    let decode_feedback_state buf =
      (let data_tag = Decode.u8 buf in
       let feedback_id = Decode.u8 buf in
       let len = Decode.u16 buf in
       let data = decode_feedback_class_variant ~tag:data_tag buf in
       { feedback_id; len; data } : feedback_state)
    let decode_get_feedback_control_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let xi_reply_type = Decode.u8 buf in
        Decode.pad buf 6;
        (let num_feedbacks = Decode.u16 buf in
         Decode.pad buf 22;
         (let feedbacks =
            (Decode.list ~item:decode_feedback_state) ~len:num_feedbacks buf in
          { xi_reply_type; feedbacks }))) : Get_feedback_control.Reply.t)
    let decode_kbd_feedback_ctl buf =
      (let class_id =
         ((Decode.u8 %> Conv.To_int.u8) %> Feedback_class_enum.of_int) buf in
       let feedback_id = Decode.u8 buf in
       let len = Decode.u16 buf in
       let key = Decode.u8 buf in
       let auto_repeat_mode = Decode.u8 buf in
       let key_click_percent = Decode.i8 buf in
       let bell_percent = Decode.i8 buf in
       let bell_pitch = Decode.i16 buf in
       let bell_duration = Decode.i16 buf in
       let led_mask = Decode.u32 buf in
       let led_values = Decode.u32 buf in
       {
         class_id;
         feedback_id;
         len;
         key;
         auto_repeat_mode;
         key_click_percent;
         bell_percent;
         bell_pitch;
         bell_duration;
         led_mask;
         led_values
       } : kbd_feedback_ctl)
    let decode_ptr_feedback_ctl buf =
      (let class_id =
         ((Decode.u8 %> Conv.To_int.u8) %> Feedback_class_enum.of_int) buf in
       let feedback_id = Decode.u8 buf in
       let len = Decode.u16 buf in
       Decode.pad buf 2;
       (let num = Decode.i16 buf in
        let denom = Decode.i16 buf in
        let threshold = Decode.i16 buf in
        { class_id; feedback_id; len; num; denom; threshold }) : ptr_feedback_ctl)
    let decode_integer_feedback_ctl buf =
      (let class_id =
         ((Decode.u8 %> Conv.To_int.u8) %> Feedback_class_enum.of_int) buf in
       let feedback_id = Decode.u8 buf in
       let len = Decode.u16 buf in
       let int_to_display = Decode.i32 buf in
       { class_id; feedback_id; len; int_to_display } : integer_feedback_ctl)
    let decode_string_feedback_ctl buf =
      (let class_id =
         ((Decode.u8 %> Conv.To_int.u8) %> Feedback_class_enum.of_int) buf in
       let feedback_id = Decode.u8 buf in
       let len = Decode.u16 buf in
       Decode.pad buf 2;
       (let num_keysyms = Decode.u16 buf in
        let keysyms = (Decode.list ~item:Decode.u32) ~len:num_keysyms buf in
        { class_id; feedback_id; len; keysyms }) : string_feedback_ctl)
    let decode_bell_feedback_ctl buf =
      (let class_id =
         ((Decode.u8 %> Conv.To_int.u8) %> Feedback_class_enum.of_int) buf in
       let feedback_id = Decode.u8 buf in
       let len = Decode.u16 buf in
       let percent = Decode.i8 buf in
       Decode.pad buf 3;
       (let pitch = Decode.i16 buf in
        let duration = Decode.i16 buf in
        { class_id; feedback_id; len; percent; pitch; duration }) : bell_feedback_ctl)
    let decode_led_feedback_ctl buf =
      (let class_id =
         ((Decode.u8 %> Conv.To_int.u8) %> Feedback_class_enum.of_int) buf in
       let feedback_id = Decode.u8 buf in
       let len = Decode.u16 buf in
       let led_mask = Decode.u32 buf in
       let led_values = Decode.u32 buf in
       { class_id; feedback_id; len; led_mask; led_values } : led_feedback_ctl)
    let decode_feedback_ctl buf =
      (let data_tag = Decode.u8 buf in
       let feedback_id = Decode.u8 buf in
       let len = Decode.u16 buf in
       let data = decode_feedback_class_variant ~tag:data_tag buf in
       { feedback_id; len; data } : feedback_ctl)
    let decode_get_device_key_mapping_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let xi_reply_type = Decode.u8 buf in
        Decode.pad buf 6;
        (let keysyms_per_keycode = Decode.u8 buf in
         Decode.pad buf 23;
         (let keysyms = (Decode.list ~item:Decode.u32) ~len:length buf in
          { xi_reply_type; keysyms_per_keycode; keysyms }))) : Get_device_key_mapping.Reply.t)
    let decode_get_device_modifier_mapping_reply ~length:(length : int)  buf
      =
      (Decode.pad buf 1;
       (let xi_reply_type = Decode.u8 buf in
        Decode.pad buf 6;
        (let keycodes_per_modifier = Decode.u8 buf in
         let keycodes_per_modifier = keycodes_per_modifier / 8 in
         Decode.pad buf 23;
         (let keymaps =
            (Decode.list ~item:Decode.u8) ~len:keycodes_per_modifier buf in
          { xi_reply_type; keymaps }))) : Get_device_modifier_mapping.Reply.t)
    let decode_set_device_modifier_mapping_reply ~length:(length : int)  buf
      =
      (Decode.pad buf 1;
       (let xi_reply_type = Decode.u8 buf in
        Decode.pad buf 6;
        (let status =
           ((Decode.u8 %> Conv.To_int.u8) %> Core.Mapping_status_enum.of_int)
             buf in
         Decode.pad buf 23; { xi_reply_type; status })) : Set_device_modifier_mapping.Reply.t)
    let decode_get_device_button_mapping_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let xi_reply_type = Decode.u8 buf in
        Decode.pad buf 6;
        (let map_size = Decode.u8 buf in
         Decode.pad buf 23;
         (let map = (Decode.list ~item:Decode.u8) ~len:map_size buf in
          Decode.align buf 4; { xi_reply_type; map }))) : Get_device_button_mapping.Reply.t)
    let decode_set_device_button_mapping_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let xi_reply_type = Decode.u8 buf in
        Decode.pad buf 6;
        (let status =
           ((Decode.u8 %> Conv.To_int.u8) %> Core.Mapping_status_enum.of_int)
             buf in
         Decode.pad buf 23; { xi_reply_type; status })) : Set_device_button_mapping.Reply.t)
    let decode_key_state buf =
      (let class_id =
         ((Decode.u8 %> Conv.To_int.u8) %> Input_class_enum.of_int) buf in
       let len = Decode.u8 buf in
       let num_keys = Decode.u8 buf in
       Decode.pad buf 1;
       (let keys = (Decode.list ~item:Decode.u8) ~len:32 buf in
        { class_id; len; num_keys; keys }) : key_state)
    let decode_button_state buf =
      (let class_id =
         ((Decode.u8 %> Conv.To_int.u8) %> Input_class_enum.of_int) buf in
       let len = Decode.u8 buf in
       let num_buttons = Decode.u8 buf in
       Decode.pad buf 1;
       (let buttons = (Decode.list ~item:Decode.u8) ~len:32 buf in
        { class_id; len; num_buttons; buttons }) : button_state)
    let decode_valuator_state buf =
      (let class_id =
         ((Decode.u8 %> Conv.To_int.u8) %> Input_class_enum.of_int) buf in
       let len = Decode.u8 buf in
       let num_valuators = Decode.u8 buf in
       let mode =
         ((Decode.u8 %> Conv.To_i32.u8) %> Valuator_state_mode_mask.of_int32)
           buf in
       let valuators = (Decode.list ~item:Decode.i32) ~len:num_valuators buf in
       { class_id; len; mode; valuators } : valuator_state)
    let decode_input_state buf =
      (let data_tag = Decode.u8 buf in
       let len = Decode.u8 buf in
       let data = decode_input_class_variant ~tag:data_tag buf in
       { len; data } : input_state)
    let decode_query_device_state_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let xi_reply_type = Decode.u8 buf in
        Decode.pad buf 6;
        (let num_classes = Decode.u8 buf in
         Decode.pad buf 23;
         (let classes =
            (Decode.list ~item:decode_input_state) ~len:num_classes buf in
          { xi_reply_type; classes }))) : Query_device_state.Reply.t)
    let decode_set_device_valuators_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let xi_reply_type = Decode.u8 buf in
        Decode.pad buf 6;
        (let status =
           ((Decode.u8 %> Conv.To_int.u8) %> Core.Grab_status_enum.of_int)
             buf in
         Decode.pad buf 23; { xi_reply_type; status })) : Set_device_valuators.Reply.t)
    let decode_device_resolution_state buf =
      (let control_id =
         ((Decode.u16 %> Conv.To_int.u16) %> Device_control_enum.of_int) buf in
       let len = Decode.u16 buf in
       let num_valuators = Decode.u32 buf in
       let resolution_values =
         (Decode.list ~item:Decode.u32) ~len:num_valuators buf in
       let resolution_min =
         (Decode.list ~item:Decode.u32) ~len:num_valuators buf in
       let resolution_max =
         (Decode.list ~item:Decode.u32) ~len:num_valuators buf in
       { control_id; len; resolution_values; resolution_min; resolution_max } : 
      device_resolution_state)
    let decode_device_abs_calib_state buf =
      (let control_id =
         ((Decode.u16 %> Conv.To_int.u16) %> Device_control_enum.of_int) buf in
       let len = Decode.u16 buf in
       let min_x = Decode.i32 buf in
       let max_x = Decode.i32 buf in
       let min_y = Decode.i32 buf in
       let max_y = Decode.i32 buf in
       let flip_x = Decode.u32 buf in
       let flip_y = Decode.u32 buf in
       let rotation = Decode.u32 buf in
       let button_threshold = Decode.u32 buf in
       {
         control_id;
         len;
         min_x;
         max_x;
         min_y;
         max_y;
         flip_x;
         flip_y;
         rotation;
         button_threshold
       } : device_abs_calib_state)
    let decode_device_abs_area_state buf =
      (let control_id =
         ((Decode.u16 %> Conv.To_int.u16) %> Device_control_enum.of_int) buf in
       let len = Decode.u16 buf in
       let offset_x = Decode.u32 buf in
       let offset_y = Decode.u32 buf in
       let width = Decode.u32 buf in
       let height = Decode.u32 buf in
       let screen = Decode.u32 buf in
       let following = Decode.u32 buf in
       {
         control_id;
         len;
         offset_x;
         offset_y;
         width;
         height;
         screen;
         following
       } : device_abs_area_state)
    let decode_device_core_state buf =
      (let control_id =
         ((Decode.u16 %> Conv.To_int.u16) %> Device_control_enum.of_int) buf in
       let len = Decode.u16 buf in
       let status = Decode.u8 buf in
       let iscore = Decode.u8 buf in
       Decode.pad buf 2; { control_id; len; status; iscore } : device_core_state)
    let decode_device_enable_state buf =
      (let control_id =
         ((Decode.u16 %> Conv.To_int.u16) %> Device_control_enum.of_int) buf in
       let len = Decode.u16 buf in
       let enable = Decode.u8 buf in
       Decode.pad buf 3; { control_id; len; enable } : device_enable_state)
    let decode_device_control_variant buf ~tag  =
      (match tag with
       | 1 ->
           let num_valuators = Decode.u32 buf in
           let resolution_values =
             (Decode.list ~item:Decode.u32) ~len:num_valuators buf in
           let resolution_min =
             (Decode.list ~item:Decode.u32) ~len:num_valuators buf in
           let resolution_max =
             (Decode.list ~item:Decode.u32) ~len:num_valuators buf in
           `Resolution { resolution_values; resolution_min; resolution_max }
       | 2 ->
           let min_x = Decode.i32 buf in
           let max_x = Decode.i32 buf in
           let min_y = Decode.i32 buf in
           let max_y = Decode.i32 buf in
           let flip_x = Decode.u32 buf in
           let flip_y = Decode.u32 buf in
           let rotation = Decode.u32 buf in
           let button_threshold = Decode.u32 buf in
           `Abs_calib
             {
               min_x;
               max_x;
               min_y;
               max_y;
               flip_x;
               flip_y;
               rotation;
               button_threshold
             }
       | 3 ->
           let status = Decode.u8 buf in
           let iscore = Decode.u8 buf in
           (Decode.pad buf 2; `Core { status; iscore })
       | 4 ->
           let enable = Decode.u8 buf in (Decode.pad buf 3; `Enable enable)
       | 5 ->
           let offset_x = Decode.u32 buf in
           let offset_y = Decode.u32 buf in
           let width = Decode.u32 buf in
           let height = Decode.u32 buf in
           let screen = Decode.u32 buf in
           let following = Decode.u32 buf in
           `Abs_area { offset_x; offset_y; width; height; screen; following }
       | n -> invalid_arg ("Invalid enum value: " ^ (string_of_int n)) : 
      Device_control.t)
    let decode_device_state buf =
      (let data_tag = Decode.u16 buf in
       let len = Decode.u16 buf in
       let data = decode_device_control_variant ~tag:data_tag buf in
       { len; data } : device_state)
    let decode_get_device_control_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let xi_reply_type = Decode.u8 buf in
        Decode.pad buf 6;
        (let status =
           (Decode.u8 %>
              (Conv.alt_enum ~enum_of_int:Core.Grab_status_enum.of_int
                 ~int_of_t:Conv.To_int.u8)) buf in
         Decode.pad buf 23;
         (let control = decode_device_state buf in
          { xi_reply_type; status; control }))) : Get_device_control.Reply.t)
    let decode_device_resolution_ctl buf =
      (let control_id =
         ((Decode.u16 %> Conv.To_int.u16) %> Device_control_enum.of_int) buf in
       let len = Decode.u16 buf in
       let first_valuator = Decode.u8 buf in
       let num_valuators = Decode.u8 buf in
       Decode.pad buf 2;
       (let resolution_values =
          (Decode.list ~item:Decode.u32) ~len:num_valuators buf in
        { control_id; len; first_valuator; resolution_values }) : device_resolution_ctl)
    let decode_device_abs_calib_ctl buf =
      (let control_id =
         ((Decode.u16 %> Conv.To_int.u16) %> Device_control_enum.of_int) buf in
       let len = Decode.u16 buf in
       let min_x = Decode.i32 buf in
       let max_x = Decode.i32 buf in
       let min_y = Decode.i32 buf in
       let max_y = Decode.i32 buf in
       let flip_x = Decode.u32 buf in
       let flip_y = Decode.u32 buf in
       let rotation = Decode.u32 buf in
       let button_threshold = Decode.u32 buf in
       {
         control_id;
         len;
         min_x;
         max_x;
         min_y;
         max_y;
         flip_x;
         flip_y;
         rotation;
         button_threshold
       } : device_abs_calib_ctl)
    let decode_device_abs_area_ctrl buf =
      (let control_id =
         ((Decode.u16 %> Conv.To_int.u16) %> Device_control_enum.of_int) buf in
       let len = Decode.u16 buf in
       let offset_x = Decode.u32 buf in
       let offset_y = Decode.u32 buf in
       let width = Decode.i32 buf in
       let height = Decode.i32 buf in
       let screen = Decode.i32 buf in
       let following = Decode.u32 buf in
       {
         control_id;
         len;
         offset_x;
         offset_y;
         width;
         height;
         screen;
         following
       } : device_abs_area_ctrl)
    let decode_device_core_ctrl buf =
      (let control_id =
         ((Decode.u16 %> Conv.To_int.u16) %> Device_control_enum.of_int) buf in
       let len = Decode.u16 buf in
       let status = Decode.u8 buf in
       Decode.pad buf 3; { control_id; len; status } : device_core_ctrl)
    let decode_device_enable_ctrl buf =
      (let control_id =
         ((Decode.u16 %> Conv.To_int.u16) %> Device_control_enum.of_int) buf in
       let len = Decode.u16 buf in
       let enable = Decode.u8 buf in
       Decode.pad buf 3; { control_id; len; enable } : device_enable_ctrl)
    let decode_device_ctl buf =
      (let data_tag = Decode.u16 buf in
       let len = Decode.u16 buf in
       let data = decode_device_control_variant ~tag:data_tag buf in
       { len; data } : device_ctl)
    let decode_change_device_control_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let xi_reply_type = Decode.u8 buf in
        Decode.pad buf 6;
        (let status =
           (Decode.u8 %>
              (Conv.alt_enum ~enum_of_int:Core.Grab_status_enum.of_int
                 ~int_of_t:Conv.To_int.u8)) buf in
         Decode.pad buf 23; { xi_reply_type; status })) : Change_device_control.Reply.t)
    let decode_list_device_properties_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let xi_reply_type = Decode.u8 buf in
        Decode.pad buf 6;
        (let num_atoms = Decode.u16 buf in
         Decode.pad buf 22;
         (let atoms = (Decode.list ~item:Decode.xid) ~len:num_atoms buf in
          { xi_reply_type; atoms }))) : List_device_properties.Reply.t)
    let decode_property_format_variant buf ~tag  ext_num_items =
      (match tag with
       | 8 ->
           let data8 =
             (Decode.list ~item:Decode.u8)
               ~len:(Conv.To_int.u32 ext_num_items) buf in
           (Decode.align buf 4; `Property_8_bits data8)
       | 16 ->
           let data16 =
             (Decode.list ~item:Decode.u16)
               ~len:(Conv.To_int.u32 ext_num_items) buf in
           (Decode.align buf 4; `Property_16_bits data16)
       | 32 ->
           let data32 =
             (Decode.list ~item:Decode.u32)
               ~len:(Conv.To_int.u32 ext_num_items) buf in
           `Property_32_bits data32
       | n -> invalid_arg ("Invalid enum value: " ^ (string_of_int n)) : 
      Property_format.t)
    let decode_get_device_property_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let xi_reply_type = Decode.u8 buf in
        Decode.pad buf 6;
        (let type_ = Decode.xid buf in
         let bytes_after = Decode.u32 buf in
         let num_items = Decode.u32 buf in
         let items_tag = Decode.u8 buf in
         let device_id = Decode.u8 buf in
         Decode.pad buf 10;
         (let items =
            decode_property_format_variant ~tag:items_tag buf num_items in
          { xi_reply_type; type_; bytes_after; num_items; device_id; items }))) : 
      Get_device_property.Reply.t)
    let decode_group_info buf =
      (let base = Decode.u8 buf in
       let latched = Decode.u8 buf in
       let locked = Decode.u8 buf in
       let effective = Decode.u8 buf in { base; latched; locked; effective } : 
      group_info)
    let decode_modifier_info buf =
      (let base = Decode.u32 buf in
       let latched = Decode.u32 buf in
       let locked = Decode.u32 buf in
       let effective = Decode.u32 buf in { base; latched; locked; effective } : 
      modifier_info)
    let decode_query_pointer_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let root = Decode.xid buf in
        let child = Decode.xid buf in
        let root_x = Decode.i32 buf in
        let root_y = Decode.i32 buf in
        let win_x = Decode.i32 buf in
        let win_y = Decode.i32 buf in
        let same_screen = Decode.bool buf in
        Decode.pad buf 1;
        (let buttons_len = Decode.u16 buf in
         let mods = decode_modifier_info buf in
         let group = decode_group_info buf in
         let buttons = (Decode.list ~item:Decode.u32) ~len:buttons_len buf in
         {
           root;
           child;
           root_x;
           root_y;
           win_x;
           win_y;
           same_screen;
           mods;
           group;
           buttons
         })) : Query_pointer.Reply.t)
    let decode_add_master buf =
      (let type_ =
         ((Decode.u16 %> Conv.To_int.u16) %>
            Hierarchy_change_type_enum.of_int) buf in
       let len = Decode.u16 buf in
       let name_len = Decode.u16 buf in
       let send_core = Decode.bool buf in
       let enable = Decode.bool buf in
       let name = Decode.string ~len:name_len buf in
       Decode.align buf 4; { type_; len; send_core; enable; name } : 
      add_master)
    let decode_remove_master buf =
      (let type_ =
         ((Decode.u16 %> Conv.To_int.u16) %>
            Hierarchy_change_type_enum.of_int) buf in
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
        { type_; len; deviceid; return_mode; return_pointer; return_keyboard
        }) : remove_master)
    let decode_attach_slave buf =
      (let type_ =
         ((Decode.u16 %> Conv.To_int.u16) %>
            Hierarchy_change_type_enum.of_int) buf in
       let len = Decode.u16 buf in
       let deviceid =
         (Decode.u16 %>
            (Conv.alt_enum ~enum_of_int:Device_enum.of_int
               ~int_of_t:Conv.To_int.u16)) buf in
       let master =
         (Decode.u16 %>
            (Conv.alt_enum ~enum_of_int:Device_enum.of_int
               ~int_of_t:Conv.To_int.u16)) buf in
       { type_; len; deviceid; master } : attach_slave)
    let decode_detach_slave buf =
      (let type_ =
         ((Decode.u16 %> Conv.To_int.u16) %>
            Hierarchy_change_type_enum.of_int) buf in
       let len = Decode.u16 buf in
       let deviceid =
         (Decode.u16 %>
            (Conv.alt_enum ~enum_of_int:Device_enum.of_int
               ~int_of_t:Conv.To_int.u16)) buf in
       Decode.pad buf 2; { type_; len; deviceid } : detach_slave)
    let decode_hierarchy_change_type_variant buf ~tag  =
      (match tag with
       | 1 ->
           let name_len = Decode.u16 buf in
           let send_core = Decode.bool buf in
           let enable = Decode.bool buf in
           let name = Decode.string ~len:name_len buf in
           (Decode.align buf 4; `Add_master { send_core; enable; name })
       | 2 ->
           let deviceid =
             (Decode.u16 %>
                (Conv.alt_enum ~enum_of_int:Device_enum.of_int
                   ~int_of_t:Conv.To_int.u16)) buf in
           let return_mode =
             ((Decode.u8 %> Conv.To_int.u8) %> Change_mode_enum.of_int) buf in
           (Decode.pad buf 1;
            (let return_pointer =
               (Decode.u16 %>
                  (Conv.alt_enum ~enum_of_int:Device_enum.of_int
                     ~int_of_t:Conv.To_int.u16)) buf in
             let return_keyboard =
               (Decode.u16 %>
                  (Conv.alt_enum ~enum_of_int:Device_enum.of_int
                     ~int_of_t:Conv.To_int.u16)) buf in
             `Remove_master
               { deviceid; return_mode; return_pointer; return_keyboard }))
       | 3 ->
           let deviceid =
             (Decode.u16 %>
                (Conv.alt_enum ~enum_of_int:Device_enum.of_int
                   ~int_of_t:Conv.To_int.u16)) buf in
           let master =
             (Decode.u16 %>
                (Conv.alt_enum ~enum_of_int:Device_enum.of_int
                   ~int_of_t:Conv.To_int.u16)) buf in
           `Attach_slave { deviceid; master }
       | 4 ->
           let deviceid =
             (Decode.u16 %>
                (Conv.alt_enum ~enum_of_int:Device_enum.of_int
                   ~int_of_t:Conv.To_int.u16)) buf in
           (Decode.pad buf 2; `Detach_slave deviceid)
       | n -> invalid_arg ("Invalid enum value: " ^ (string_of_int n)) : 
      Hierarchy_change_type.t)
    let decode_hierarchy_change buf =
      (let data_tag = Decode.u16 buf in
       let len = Decode.u16 buf in
       let data = decode_hierarchy_change_type_variant ~tag:data_tag buf in
       { len; data } : hierarchy_change)
    let decode_get_client_pointer_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let set = Decode.bool buf in
        Decode.pad buf 1;
        (let deviceid =
           (Decode.u16 %>
              (Conv.alt_enum ~enum_of_int:Device_enum.of_int
                 ~int_of_t:Conv.To_int.u16)) buf in
         Decode.pad buf 20; { set; deviceid })) : Get_client_pointer.Reply.t)
    let decode_event_mask buf =
      (let deviceid =
         (Decode.u16 %>
            (Conv.alt_enum ~enum_of_int:Device_enum.of_int
               ~int_of_t:Conv.To_int.u16)) buf in
       let mask_len = Decode.u16 buf in
       let mask =
         (Decode.list
            ~item:((Decode.u32 %> Conv.To_i32.u32) %> Xi_event_mask.of_int32))
           ~len:mask_len buf in
       { deviceid; mask } : event_mask)
    let decode_query_version_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let major_version = Decode.u16 buf in
        let minor_version = Decode.u16 buf in
        Decode.pad buf 20; { major_version; minor_version }) : Query_version.Reply.t)
    let decode_button_class buf =
      (let type_ =
         ((Decode.u16 %> Conv.To_int.u16) %> Device_class_type_enum.of_int)
           buf in
       let len = Decode.u16 buf in
       let sourceid = Decode.u16 buf in
       let num_buttons = Decode.u16 buf in
       let state =
         (Decode.list ~item:Decode.u32) ~len:((num_buttons + 31) / 32) buf in
       let labels = (Decode.list ~item:Decode.xid) ~len:num_buttons buf in
       { type_; len; sourceid; state; labels } : button_class)
    let decode_key_class buf =
      (let type_ =
         ((Decode.u16 %> Conv.To_int.u16) %> Device_class_type_enum.of_int)
           buf in
       let len = Decode.u16 buf in
       let sourceid = Decode.u16 buf in
       let num_keys = Decode.u16 buf in
       let keys = (Decode.list ~item:Decode.u32) ~len:num_keys buf in
       { type_; len; sourceid; keys } : key_class)
    let decode_scroll_class buf =
      (let type_ =
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
        let increment = decode_fp3232 buf in
        { type_; len; sourceid; number; scroll_type; flags; increment }) : 
      scroll_class)
    let decode_touch_class buf =
      (let type_ =
         ((Decode.u16 %> Conv.To_int.u16) %> Device_class_type_enum.of_int)
           buf in
       let len = Decode.u16 buf in
       let sourceid = Decode.u16 buf in
       let mode =
         ((Decode.u8 %> Conv.To_int.u8) %> Touch_mode_enum.of_int) buf in
       let num_touches = Decode.u8 buf in
       { type_; len; sourceid; mode; num_touches } : touch_class)
    let decode_valuator_class buf =
      (let type_ =
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
       Decode.pad buf 3;
       {
         type_;
         len;
         sourceid;
         number;
         label;
         min;
         max;
         value;
         resolution;
         mode
       } : valuator_class)
    let decode_device_class_type_variant buf ~tag  =
      (match tag with
       | 0 ->
           let num_keys = Decode.u16 buf in
           let keys = (Decode.list ~item:Decode.u32) ~len:num_keys buf in
           `Key keys
       | 1 ->
           let num_buttons = Decode.u16 buf in
           let state =
             (Decode.list ~item:Decode.u32) ~len:((num_buttons + 31) / 32)
               buf in
           let labels = (Decode.list ~item:Decode.xid) ~len:num_buttons buf in
           `Button { state; labels }
       | 2 ->
           let number = Decode.u16 buf in
           let label = Decode.xid buf in
           let min = decode_fp3232 buf in
           let max = decode_fp3232 buf in
           let value = decode_fp3232 buf in
           let resolution = Decode.u32 buf in
           let mode =
             ((Decode.u8 %> Conv.To_int.u8) %> Valuator_mode_enum.of_int) buf in
           (Decode.pad buf 3;
            `Valuator { number; label; min; max; value; resolution; mode })
       | 3 ->
           let number = Decode.u16 buf in
           let scroll_type =
             ((Decode.u16 %> Conv.To_int.u16) %> Scroll_type_enum.of_int) buf in
           (Decode.pad buf 2;
            (let flags =
               ((Decode.u32 %> Conv.To_i32.u32) %> Scroll_flags_mask.of_int32)
                 buf in
             let increment = decode_fp3232 buf in
             `Scroll { number; scroll_type; flags; increment }))
       | 8 ->
           let mode =
             ((Decode.u8 %> Conv.To_int.u8) %> Touch_mode_enum.of_int) buf in
           let num_touches = Decode.u8 buf in `Touch { mode; num_touches }
       | n -> invalid_arg ("Invalid enum value: " ^ (string_of_int n)) : 
      Device_class_type.t)
    let decode_device_class buf =
      (let data_tag = Decode.u16 buf in
       let len = Decode.u16 buf in
       let sourceid = Decode.u16 buf in
       let data = decode_device_class_type_variant ~tag:data_tag buf in
       { len; sourceid; data } : device_class)
    let decode_xi_device_info buf =
      (let deviceid =
         (Decode.u16 %>
            (Conv.alt_enum ~enum_of_int:Device_enum.of_int
               ~int_of_t:Conv.To_int.u16)) buf in
       let type_ =
         ((Decode.u16 %> Conv.To_int.u16) %> Device_type_enum.of_int) buf in
       let attachment =
         (Decode.u16 %>
            (Conv.alt_enum ~enum_of_int:Device_enum.of_int
               ~int_of_t:Conv.To_int.u16)) buf in
       let num_classes = Decode.u16 buf in
       let name_len = Decode.u16 buf in
       let enabled = Decode.bool buf in
       Decode.pad buf 1;
       (let name = Decode.string ~len:name_len buf in
        Decode.align buf 4;
        (let classes =
           (Decode.list ~item:decode_device_class) ~len:num_classes buf in
         { deviceid; type_; attachment; enabled; name; classes })) : 
      xi_device_info)
    let decode_query_device_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let num_infos = Decode.u16 buf in
        Decode.pad buf 22;
        (let infos =
           (Decode.list ~item:decode_xi_device_info) ~len:num_infos buf in
         infos)) : Query_device.Reply.t)
    let decode_get_focus_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let focus = Decode.xid buf in Decode.pad buf 20; focus) : Get_focus.Reply.t)
    let decode_xi_grab_device_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let status =
          ((Decode.u8 %> Conv.To_int.u8) %> Core.Grab_status_enum.of_int) buf in
        Decode.pad buf 23; status) : Xi_grab_device.Reply.t)
    let decode_grab_modifier_info buf =
      (let modifiers =
         ((Decode.u32 %> Conv.To_i32.u32) %> Modifier_mask.of_int32) buf in
       let status =
         ((Decode.u8 %> Conv.To_int.u8) %> Core.Grab_status_enum.of_int) buf in
       Decode.pad buf 3; { modifiers; status } : grab_modifier_info)
    let decode_passive_grab_device_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let num_modifiers = Decode.u16 buf in
        Decode.pad buf 22;
        (let modifiers =
           (Decode.list ~item:decode_grab_modifier_info) ~len:num_modifiers
             buf in
         modifiers)) : Passive_grab_device.Reply.t)
    let decode_list_properties_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let num_properties = Decode.u16 buf in
        Decode.pad buf 22;
        (let properties =
           (Decode.list ~item:Decode.xid) ~len:num_properties buf in
         properties)) : List_properties.Reply.t)
    let decode_get_property_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let type_ = Decode.xid buf in
        let bytes_after = Decode.u32 buf in
        let num_items = Decode.u32 buf in
        let items_tag = Decode.u8 buf in
        Decode.pad buf 11;
        (let items =
           decode_property_format_variant ~tag:items_tag buf num_items in
         { type_; bytes_after; num_items; items })) : Get_property.Reply.t)
    let decode_get_selected_events_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let num_masks = Decode.u16 buf in
        Decode.pad buf 22;
        (let masks = (Decode.list ~item:decode_event_mask) ~len:num_masks buf in
         masks)) : Get_selected_events.Reply.t)
    let decode_barrier_release_pointer_info buf =
      (let deviceid = Decode.u16 buf in
       Decode.pad buf 2;
       (let barrier = Decode.xid buf in
        let eventid = Decode.u32 buf in { deviceid; barrier; eventid }) : 
      barrier_release_pointer_info)
    let decode_device_valuator_event buf =
      (Decode.pad buf 1;
       (let device_id = Decode.u8 buf in
        Decode.pad buf 2;
        (let device_state = Decode.u16 buf in
         let num_valuators = Decode.u8 buf in
         let first_valuator = Decode.u8 buf in
         let valuators = (Decode.list ~item:Decode.i32) ~len:6 buf in
         { device_id; device_state; num_valuators; first_valuator; valuators
         })) : Event.Device_valuator.t)
    let decode_device_key_press_event buf =
      (Decode.pad buf 1;
       (let detail = Decode.byte buf in
        Decode.pad buf 2;
        (let time = Decode.u32 buf in
         let root = Decode.xid buf in
         let event = Decode.xid buf in
         let child =
           (Decode.xid %>
              (Conv.alt_enum ~enum_of_int:Core.Window_enum.of_int
                 ~int_of_t:Conv.To_int.xid)) buf in
         let root_x = Decode.i16 buf in
         let root_y = Decode.i16 buf in
         let event_x = Decode.i16 buf in
         let event_y = Decode.i16 buf in
         let state =
           ((Decode.u16 %> Conv.To_i32.u16) %> Core.Key_but_mask.of_int32)
             buf in
         let same_screen = Decode.bool buf in
         let device_id =
           ((Decode.u8 %> Conv.To_i32.u8) %> More_events_mask.of_int32) buf in
         {
           detail;
           time;
           root;
           event;
           child;
           root_x;
           root_y;
           event_x;
           event_y;
           state;
           same_screen;
           device_id
         })) : Event.Device_key_press.t)
    let decode_device_key_release_event = decode_device_key_press_event
    let decode_device_button_press_event = decode_device_key_press_event
    let decode_device_button_release_event = decode_device_key_press_event
    let decode_device_motion_notify_event = decode_device_key_press_event
    let decode_device_focus_in_event buf =
      (Decode.pad buf 1;
       (let detail =
          ((Decode.byte %> Conv.To_int.byte) %>
             Core.Notify_detail_enum.of_int) buf in
        Decode.pad buf 2;
        (let time = Decode.u32 buf in
         let window = Decode.xid buf in
         let mode =
           ((Decode.byte %> Conv.To_int.byte) %> Core.Notify_mode_enum.of_int)
             buf in
         let device_id = Decode.u8 buf in
         Decode.pad buf 18; { detail; time; window; mode; device_id })) : 
      Event.Device_focus_in.t)
    let decode_device_focus_out_event = decode_device_focus_in_event
    let decode_proximity_in_event = decode_device_key_press_event
    let decode_proximity_out_event = decode_device_key_press_event
    let decode_device_state_notify_event buf =
      (Decode.pad buf 1;
       (let device_id =
          ((Decode.byte %> Conv.To_i32.byte) %> More_events_mask.of_int32)
            buf in
        Decode.pad buf 2;
        (let time = Decode.u32 buf in
         let num_keys = Decode.u8 buf in
         let num_buttons = Decode.u8 buf in
         let num_valuators = Decode.u8 buf in
         let classes_reported =
           ((Decode.u8 %> Conv.To_i32.u8) %> Classes_reported_mask.of_int32)
             buf in
         let buttons = (Decode.list ~item:Decode.u8) ~len:4 buf in
         let keys = (Decode.list ~item:Decode.u8) ~len:4 buf in
         let valuators = (Decode.list ~item:Decode.u32) ~len:3 buf in
         {
           device_id;
           time;
           num_keys;
           num_buttons;
           num_valuators;
           classes_reported;
           buttons;
           keys;
           valuators
         })) : Event.Device_state_notify.t)
    let decode_device_mapping_notify_event buf =
      (Decode.pad buf 1;
       (let device_id = Decode.byte buf in
        Decode.pad buf 2;
        (let request =
           ((Decode.u8 %> Conv.To_int.u8) %> Core.Mapping_enum.of_int) buf in
         let first_keycode = Decode.u8 buf in
         let count = Decode.u8 buf in
         Decode.pad buf 1;
         (let time = Decode.u32 buf in
          Decode.pad buf 20;
          { device_id; request; first_keycode; count; time }))) : Event.Device_mapping_notify.t)
    let decode_change_device_notify_event buf =
      (Decode.pad buf 1;
       (let device_id = Decode.byte buf in
        Decode.pad buf 2;
        (let time = Decode.u32 buf in
         let request =
           ((Decode.u8 %> Conv.To_int.u8) %> Change_device_enum.of_int) buf in
         Decode.pad buf 23; { device_id; time; request })) : Event.Change_device_notify.t)
    let decode_device_key_state_notify_event buf =
      (Decode.pad buf 1;
       (let device_id =
          ((Decode.byte %> Conv.To_i32.byte) %> More_events_mask.of_int32)
            buf in
        Decode.pad buf 2;
        (let keys = (Decode.list ~item:Decode.u8) ~len:28 buf in
         { device_id; keys })) : Event.Device_key_state_notify.t)
    let decode_device_button_state_notify_event buf =
      (Decode.pad buf 1;
       (let device_id =
          ((Decode.byte %> Conv.To_i32.byte) %> More_events_mask.of_int32)
            buf in
        Decode.pad buf 2;
        (let buttons = (Decode.list ~item:Decode.u8) ~len:28 buf in
         { device_id; buttons })) : Event.Device_button_state_notify.t)
    let decode_device_presence_notify_event buf =
      (Decode.pad buf 4;
       (let time = Decode.u32 buf in
        let devchange =
          ((Decode.byte %> Conv.To_int.byte) %> Device_change_enum.of_int)
            buf in
        let device_id = Decode.byte buf in
        let control = Decode.u16 buf in
        Decode.pad buf 20; { time; devchange; device_id; control }) : 
      Event.Device_presence_notify.t)
    let decode_device_property_notify_event buf =
      (Decode.pad buf 1;
       (let state =
          ((Decode.byte %> Conv.To_int.byte) %> Core.Property_enum.of_int)
            buf in
        Decode.pad buf 2;
        (let time = Decode.u32 buf in
         let property = Decode.xid buf in
         Decode.pad buf 19;
         (let device_id = Decode.u8 buf in
          { state; time; property; device_id }))) : Event.Device_property_notify.t)
    let decode_device_changed_event buf =
      (Decode.pad buf 1;
       (let deviceid =
          (Decode.u16 %>
             (Conv.alt_enum ~enum_of_int:Device_enum.of_int
                ~int_of_t:Conv.To_int.u16)) buf in
        Decode.pad buf 2;
        (let time =
           (Decode.u32 %>
              (Conv.alt_enum ~enum_of_int:Core.Time_enum.of_int
                 ~int_of_t:Conv.To_int.u32)) buf in
         let num_classes = Decode.u16 buf in
         let sourceid =
           (Decode.u16 %>
              (Conv.alt_enum ~enum_of_int:Device_enum.of_int
                 ~int_of_t:Conv.To_int.u16)) buf in
         let reason =
           ((Decode.u8 %> Conv.To_int.u8) %> Change_reason_enum.of_int) buf in
         Decode.pad buf 11;
         (let classes =
            (Decode.list ~item:decode_device_class) ~len:num_classes buf in
          { deviceid; time; sourceid; reason; classes }))) : Event.Device_changed.t)
    let decode_key_press_event buf =
      (Decode.pad buf 1;
       (let deviceid =
          (Decode.u16 %>
             (Conv.alt_enum ~enum_of_int:Device_enum.of_int
                ~int_of_t:Conv.To_int.u16)) buf in
        Decode.pad buf 2;
        (let time =
           (Decode.u32 %>
              (Conv.alt_enum ~enum_of_int:Core.Time_enum.of_int
                 ~int_of_t:Conv.To_int.u32)) buf in
         let detail = Decode.u32 buf in
         let root = Decode.xid buf in
         let event = Decode.xid buf in
         let child = Decode.xid buf in
         let root_x = Decode.i32 buf in
         let root_y = Decode.i32 buf in
         let event_x = Decode.i32 buf in
         let event_y = Decode.i32 buf in
         let buttons_len = Decode.u16 buf in
         let valuators_len = Decode.u16 buf in
         let sourceid =
           (Decode.u16 %>
              (Conv.alt_enum ~enum_of_int:Device_enum.of_int
                 ~int_of_t:Conv.To_int.u16)) buf in
         Decode.pad buf 2;
         (let flags =
            ((Decode.u32 %> Conv.To_i32.u32) %> Key_event_flags_mask.of_int32)
              buf in
          let mods = decode_modifier_info buf in
          let group = decode_group_info buf in
          let button_mask =
            (Decode.list ~item:Decode.u32) ~len:buttons_len buf in
          let valuator_mask =
            (Decode.list ~item:Decode.u32) ~len:valuators_len buf in
          let axisvalues =
            (Decode.list ~item:decode_fp3232)
              ~len:(Conv.sum_map
                      ~f:(fun list_element_ref ->
                            Conv.pop_count list_element_ref) valuator_mask)
              buf in
          {
            deviceid;
            time;
            detail;
            root;
            event;
            child;
            root_x;
            root_y;
            event_x;
            event_y;
            sourceid;
            flags;
            mods;
            group;
            button_mask;
            valuator_mask;
            axisvalues
          }))) : Event.Key_press.t)
    let decode_key_release_event = decode_key_press_event
    let decode_button_press_event buf =
      (Decode.pad buf 1;
       (let deviceid =
          (Decode.u16 %>
             (Conv.alt_enum ~enum_of_int:Device_enum.of_int
                ~int_of_t:Conv.To_int.u16)) buf in
        Decode.pad buf 2;
        (let time =
           (Decode.u32 %>
              (Conv.alt_enum ~enum_of_int:Core.Time_enum.of_int
                 ~int_of_t:Conv.To_int.u32)) buf in
         let detail = Decode.u32 buf in
         let root = Decode.xid buf in
         let event = Decode.xid buf in
         let child = Decode.xid buf in
         let root_x = Decode.i32 buf in
         let root_y = Decode.i32 buf in
         let event_x = Decode.i32 buf in
         let event_y = Decode.i32 buf in
         let buttons_len = Decode.u16 buf in
         let valuators_len = Decode.u16 buf in
         let sourceid =
           (Decode.u16 %>
              (Conv.alt_enum ~enum_of_int:Device_enum.of_int
                 ~int_of_t:Conv.To_int.u16)) buf in
         Decode.pad buf 2;
         (let flags =
            ((Decode.u32 %> Conv.To_i32.u32) %>
               Pointer_event_flags_mask.of_int32) buf in
          let mods = decode_modifier_info buf in
          let group = decode_group_info buf in
          let button_mask =
            (Decode.list ~item:Decode.u32) ~len:buttons_len buf in
          let valuator_mask =
            (Decode.list ~item:Decode.u32) ~len:valuators_len buf in
          let axisvalues =
            (Decode.list ~item:decode_fp3232)
              ~len:(Conv.sum_map
                      ~f:(fun list_element_ref ->
                            Conv.pop_count list_element_ref) valuator_mask)
              buf in
          {
            deviceid;
            time;
            detail;
            root;
            event;
            child;
            root_x;
            root_y;
            event_x;
            event_y;
            sourceid;
            flags;
            mods;
            group;
            button_mask;
            valuator_mask;
            axisvalues
          }))) : Event.Button_press.t)
    let decode_button_release_event = decode_button_press_event
    let decode_motion_event = decode_button_press_event
    let decode_enter_event buf =
      (Decode.pad buf 1;
       (let deviceid =
          (Decode.u16 %>
             (Conv.alt_enum ~enum_of_int:Device_enum.of_int
                ~int_of_t:Conv.To_int.u16)) buf in
        Decode.pad buf 2;
        (let time =
           (Decode.u32 %>
              (Conv.alt_enum ~enum_of_int:Core.Time_enum.of_int
                 ~int_of_t:Conv.To_int.u32)) buf in
         let sourceid =
           (Decode.u16 %>
              (Conv.alt_enum ~enum_of_int:Device_enum.of_int
                 ~int_of_t:Conv.To_int.u16)) buf in
         let mode =
           ((Decode.u8 %> Conv.To_int.u8) %> Notify_mode_enum.of_int) buf in
         let detail =
           ((Decode.u8 %> Conv.To_int.u8) %> Notify_detail_enum.of_int) buf in
         let root = Decode.xid buf in
         let event = Decode.xid buf in
         let child = Decode.xid buf in
         let root_x = Decode.i32 buf in
         let root_y = Decode.i32 buf in
         let event_x = Decode.i32 buf in
         let event_y = Decode.i32 buf in
         let same_screen = Decode.bool buf in
         let focus = Decode.bool buf in
         let buttons_len = Decode.u16 buf in
         let mods = decode_modifier_info buf in
         let group = decode_group_info buf in
         let buttons = (Decode.list ~item:Decode.u32) ~len:buttons_len buf in
         {
           deviceid;
           time;
           sourceid;
           mode;
           detail;
           root;
           event;
           child;
           root_x;
           root_y;
           event_x;
           event_y;
           same_screen;
           focus;
           mods;
           group;
           buttons
         })) : Event.Enter.t)
    let decode_leave_event = decode_enter_event
    let decode_focus_in_event = decode_enter_event
    let decode_focus_out_event = decode_enter_event
    let decode_hierarchy_info buf =
      (let deviceid =
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
        { deviceid; attachment; type_; enabled; flags }) : hierarchy_info)
    let decode_hierarchy_event buf =
      (Decode.pad buf 1;
       (let deviceid =
          (Decode.u16 %>
             (Conv.alt_enum ~enum_of_int:Device_enum.of_int
                ~int_of_t:Conv.To_int.u16)) buf in
        Decode.pad buf 2;
        (let time =
           (Decode.u32 %>
              (Conv.alt_enum ~enum_of_int:Core.Time_enum.of_int
                 ~int_of_t:Conv.To_int.u32)) buf in
         let flags =
           ((Decode.u32 %> Conv.To_i32.u32) %> Hierarchy_mask.of_int32) buf in
         let num_infos = Decode.u16 buf in
         Decode.pad buf 10;
         (let infos =
            (Decode.list ~item:decode_hierarchy_info) ~len:num_infos buf in
          { deviceid; time; flags; infos }))) : Event.Hierarchy.t)
    let decode_property_event buf =
      (Decode.pad buf 1;
       (let deviceid =
          (Decode.u16 %>
             (Conv.alt_enum ~enum_of_int:Device_enum.of_int
                ~int_of_t:Conv.To_int.u16)) buf in
        Decode.pad buf 2;
        (let time =
           (Decode.u32 %>
              (Conv.alt_enum ~enum_of_int:Core.Time_enum.of_int
                 ~int_of_t:Conv.To_int.u32)) buf in
         let property = Decode.xid buf in
         let what =
           ((Decode.u8 %> Conv.To_int.u8) %> Property_flag_enum.of_int) buf in
         Decode.pad buf 11; { deviceid; time; property; what })) : Event.Property.t)
    let decode_raw_key_press_event buf =
      (Decode.pad buf 1;
       (let deviceid =
          (Decode.u16 %>
             (Conv.alt_enum ~enum_of_int:Device_enum.of_int
                ~int_of_t:Conv.To_int.u16)) buf in
        Decode.pad buf 2;
        (let time =
           (Decode.u32 %>
              (Conv.alt_enum ~enum_of_int:Core.Time_enum.of_int
                 ~int_of_t:Conv.To_int.u32)) buf in
         let detail = Decode.u32 buf in
         let sourceid = Decode.u16 buf in
         let valuators_len = Decode.u16 buf in
         let flags =
           ((Decode.u32 %> Conv.To_i32.u32) %> Key_event_flags_mask.of_int32)
             buf in
         Decode.pad buf 4;
         (let valuator_mask =
            (Decode.list ~item:Decode.u32) ~len:valuators_len buf in
          let axisvalues =
            (Decode.list ~item:decode_fp3232)
              ~len:(Conv.sum_map
                      ~f:(fun list_element_ref ->
                            Conv.pop_count list_element_ref) valuator_mask)
              buf in
          let axisvalues_raw =
            (Decode.list ~item:decode_fp3232)
              ~len:(Conv.sum_map
                      ~f:(fun list_element_ref ->
                            Conv.pop_count list_element_ref) valuator_mask)
              buf in
          {
            deviceid;
            time;
            detail;
            sourceid;
            flags;
            valuator_mask;
            axisvalues;
            axisvalues_raw
          }))) : Event.Raw_key_press.t)
    let decode_raw_key_release_event = decode_raw_key_press_event
    let decode_raw_button_press_event buf =
      (Decode.pad buf 1;
       (let deviceid =
          (Decode.u16 %>
             (Conv.alt_enum ~enum_of_int:Device_enum.of_int
                ~int_of_t:Conv.To_int.u16)) buf in
        Decode.pad buf 2;
        (let time =
           (Decode.u32 %>
              (Conv.alt_enum ~enum_of_int:Core.Time_enum.of_int
                 ~int_of_t:Conv.To_int.u32)) buf in
         let detail = Decode.u32 buf in
         let sourceid = Decode.u16 buf in
         let valuators_len = Decode.u16 buf in
         let flags =
           ((Decode.u32 %> Conv.To_i32.u32) %>
              Pointer_event_flags_mask.of_int32) buf in
         Decode.pad buf 4;
         (let valuator_mask =
            (Decode.list ~item:Decode.u32) ~len:valuators_len buf in
          let axisvalues =
            (Decode.list ~item:decode_fp3232)
              ~len:(Conv.sum_map
                      ~f:(fun list_element_ref ->
                            Conv.pop_count list_element_ref) valuator_mask)
              buf in
          let axisvalues_raw =
            (Decode.list ~item:decode_fp3232)
              ~len:(Conv.sum_map
                      ~f:(fun list_element_ref ->
                            Conv.pop_count list_element_ref) valuator_mask)
              buf in
          {
            deviceid;
            time;
            detail;
            sourceid;
            flags;
            valuator_mask;
            axisvalues;
            axisvalues_raw
          }))) : Event.Raw_button_press.t)
    let decode_raw_button_release_event = decode_raw_button_press_event
    let decode_raw_motion_event = decode_raw_button_press_event
    let decode_touch_begin_event buf =
      (Decode.pad buf 1;
       (let deviceid =
          (Decode.u16 %>
             (Conv.alt_enum ~enum_of_int:Device_enum.of_int
                ~int_of_t:Conv.To_int.u16)) buf in
        Decode.pad buf 2;
        (let time =
           (Decode.u32 %>
              (Conv.alt_enum ~enum_of_int:Core.Time_enum.of_int
                 ~int_of_t:Conv.To_int.u32)) buf in
         let detail = Decode.u32 buf in
         let root = Decode.xid buf in
         let event = Decode.xid buf in
         let child = Decode.xid buf in
         let root_x = Decode.i32 buf in
         let root_y = Decode.i32 buf in
         let event_x = Decode.i32 buf in
         let event_y = Decode.i32 buf in
         let buttons_len = Decode.u16 buf in
         let valuators_len = Decode.u16 buf in
         let sourceid =
           (Decode.u16 %>
              (Conv.alt_enum ~enum_of_int:Device_enum.of_int
                 ~int_of_t:Conv.To_int.u16)) buf in
         Decode.pad buf 2;
         (let flags =
            ((Decode.u32 %> Conv.To_i32.u32) %>
               Touch_event_flags_mask.of_int32) buf in
          let mods = decode_modifier_info buf in
          let group = decode_group_info buf in
          let button_mask =
            (Decode.list ~item:Decode.u32) ~len:buttons_len buf in
          let valuator_mask =
            (Decode.list ~item:Decode.u32) ~len:valuators_len buf in
          let axisvalues =
            (Decode.list ~item:decode_fp3232)
              ~len:(Conv.sum_map
                      ~f:(fun list_element_ref ->
                            Conv.pop_count list_element_ref) valuator_mask)
              buf in
          {
            deviceid;
            time;
            detail;
            root;
            event;
            child;
            root_x;
            root_y;
            event_x;
            event_y;
            sourceid;
            flags;
            mods;
            group;
            button_mask;
            valuator_mask;
            axisvalues
          }))) : Event.Touch_begin.t)
    let decode_touch_update_event = decode_touch_begin_event
    let decode_touch_end_event = decode_touch_begin_event
    let decode_touch_ownership_event buf =
      (Decode.pad buf 1;
       (let deviceid =
          (Decode.u16 %>
             (Conv.alt_enum ~enum_of_int:Device_enum.of_int
                ~int_of_t:Conv.To_int.u16)) buf in
        Decode.pad buf 2;
        (let time =
           (Decode.u32 %>
              (Conv.alt_enum ~enum_of_int:Core.Time_enum.of_int
                 ~int_of_t:Conv.To_int.u32)) buf in
         let touchid = Decode.u32 buf in
         let root = Decode.xid buf in
         let event = Decode.xid buf in
         let child = Decode.xid buf in
         let sourceid =
           (Decode.u16 %>
              (Conv.alt_enum ~enum_of_int:Device_enum.of_int
                 ~int_of_t:Conv.To_int.u16)) buf in
         Decode.pad buf 2;
         (let flags =
            ((Decode.u32 %> Conv.To_int.u32) %>
               Touch_ownership_flags_enum.of_int) buf in
          Decode.pad buf 8;
          { deviceid; time; touchid; root; event; child; sourceid; flags }))) : 
      Event.Touch_ownership.t)
    let decode_raw_touch_begin_event buf =
      (Decode.pad buf 1;
       (let deviceid =
          (Decode.u16 %>
             (Conv.alt_enum ~enum_of_int:Device_enum.of_int
                ~int_of_t:Conv.To_int.u16)) buf in
        Decode.pad buf 2;
        (let time =
           (Decode.u32 %>
              (Conv.alt_enum ~enum_of_int:Core.Time_enum.of_int
                 ~int_of_t:Conv.To_int.u32)) buf in
         let detail = Decode.u32 buf in
         let sourceid = Decode.u16 buf in
         let valuators_len = Decode.u16 buf in
         let flags =
           ((Decode.u32 %> Conv.To_i32.u32) %>
              Touch_event_flags_mask.of_int32) buf in
         Decode.pad buf 4;
         (let valuator_mask =
            (Decode.list ~item:Decode.u32) ~len:valuators_len buf in
          let axisvalues =
            (Decode.list ~item:decode_fp3232)
              ~len:(Conv.sum_map
                      ~f:(fun list_element_ref ->
                            Conv.pop_count list_element_ref) valuator_mask)
              buf in
          let axisvalues_raw =
            (Decode.list ~item:decode_fp3232)
              ~len:(Conv.sum_map
                      ~f:(fun list_element_ref ->
                            Conv.pop_count list_element_ref) valuator_mask)
              buf in
          {
            deviceid;
            time;
            detail;
            sourceid;
            flags;
            valuator_mask;
            axisvalues;
            axisvalues_raw
          }))) : Event.Raw_touch_begin.t)
    let decode_raw_touch_update_event = decode_raw_touch_begin_event
    let decode_raw_touch_end_event = decode_raw_touch_begin_event
    let decode_barrier_hit_event buf =
      (Decode.pad buf 1;
       (let deviceid =
          (Decode.u16 %>
             (Conv.alt_enum ~enum_of_int:Device_enum.of_int
                ~int_of_t:Conv.To_int.u16)) buf in
        Decode.pad buf 2;
        (let time =
           (Decode.u32 %>
              (Conv.alt_enum ~enum_of_int:Core.Time_enum.of_int
                 ~int_of_t:Conv.To_int.u32)) buf in
         let eventid = Decode.u32 buf in
         let root = Decode.xid buf in
         let event = Decode.xid buf in
         let barrier = Decode.xid buf in
         let dtime = Decode.u32 buf in
         let flags =
           ((Decode.u32 %> Conv.To_i32.u32) %> Barrier_flags_mask.of_int32)
             buf in
         let sourceid =
           (Decode.u16 %>
              (Conv.alt_enum ~enum_of_int:Device_enum.of_int
                 ~int_of_t:Conv.To_int.u16)) buf in
         Decode.pad buf 2;
         (let root_x = Decode.i32 buf in
          let root_y = Decode.i32 buf in
          let dx = decode_fp3232 buf in
          let dy = decode_fp3232 buf in
          {
            deviceid;
            time;
            eventid;
            root;
            event;
            barrier;
            dtime;
            flags;
            sourceid;
            root_x;
            root_y;
            dx;
            dy
          }))) : Event.Barrier_hit.t)
    let decode_barrier_leave_event = decode_barrier_hit_event
    let decode_device_error buf = (Decode.align buf 32; () : Error.Device.t)
    let decode_event_error buf = (Decode.align buf 32; () : Error.Event.t)
    let decode_mode_error buf = (Decode.align buf 32; () : Error.Mode.t)
    let decode_device_busy_error buf =
      (Decode.align buf 32; () : Error.Device_busy.t)
    let decode_class_error buf = (Decode.align buf 32; () : Error.Class_.t)
    let decode_error ~number  buf =
      (Decode.pad buf 4;
       (match number with
        | 0 -> let error = decode_device_error buf in `Device error
        | 1 -> let error = decode_event_error buf in `Event error
        | 2 -> let error = decode_mode_error buf in `Mode error
        | 3 -> let error = decode_device_busy_error buf in `Device_busy error
        | 4 -> let error = decode_class_error buf in `Class_ error
        | n -> invalid_arg ("Invalid error number: " ^ (string_of_int n))) : 
      Error.t)
  end
module Xprint_codec =
  struct
    open Protocol.Xprint
    let decode_printer buf =
      (let name_len = Decode.u32 buf in
       let name = Decode.string ~len:name_len buf in
       Decode.align buf 4;
       (let desc_len = Decode.u32 buf in
        let description = Decode.string ~len:desc_len buf in
        Decode.align buf 4; { name; description }) : printer)
    let decode_print_query_version_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let major_version = Decode.u16 buf in
        let minor_version = Decode.u16 buf in
        { major_version; minor_version }) : Print_query_version.Reply.t)
    let decode_print_get_printer_list_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let list_count = Decode.u32 buf in
        Decode.pad buf 20;
        (let printers =
           (Decode.list ~item:decode_printer) ~len:list_count buf in
         printers)) : Print_get_printer_list.Reply.t)
    let decode_print_get_context_reply ~length:(length : int)  buf =
      (Decode.pad buf 8; (let context = Decode.u32 buf in context) : 
      Print_get_context.Reply.t)
    let decode_print_get_screen_of_context_reply ~length:(length : int)  buf
      =
      (Decode.pad buf 8; (let root = Decode.xid buf in root) : Print_get_screen_of_context.Reply.t)
    let decode_print_get_document_data_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let status_code = Decode.u32 buf in
        let finished_flag = Decode.u32 buf in
        let data_len = Decode.u32 buf in
        Decode.pad buf 12;
        (let data = (Decode.list ~item:Decode.byte) ~len:data_len buf in
         { status_code; finished_flag; data })) : Print_get_document_data.Reply.t)
    let decode_print_input_selected_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let event_mask = Decode.u32 buf in
        let all_events_mask = Decode.u32 buf in
        { event_mask; all_events_mask }) : Print_input_selected.Reply.t)
    let decode_print_get_attributes_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let string_len = Decode.u32 buf in
        Decode.pad buf 20;
        (let attributes = Decode.string ~len:string_len buf in attributes)) : 
      Print_get_attributes.Reply.t)
    let decode_print_get_one_attributes_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let value_len = Decode.u32 buf in
        Decode.pad buf 20;
        (let value = Decode.string ~len:value_len buf in value)) : Print_get_one_attributes.Reply.t)
    let decode_print_get_page_dimensions_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let width = Decode.u16 buf in
        let height = Decode.u16 buf in
        let offset_x = Decode.u16 buf in
        let offset_y = Decode.u16 buf in
        let reproducible_width = Decode.u16 buf in
        let reproducible_height = Decode.u16 buf in
        {
          width;
          height;
          offset_x;
          offset_y;
          reproducible_width;
          reproducible_height
        }) : Print_get_page_dimensions.Reply.t)
    let decode_print_query_screens_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let list_count = Decode.u32 buf in
        Decode.pad buf 20;
        (let roots = (Decode.list ~item:Decode.xid) ~len:list_count buf in
         roots)) : Print_query_screens.Reply.t)
    let decode_print_set_image_resolution_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let status = Decode.bool buf in
        Decode.pad buf 6;
        (let previous_resolutions = Decode.u16 buf in
         { status; previous_resolutions })) : Print_set_image_resolution.Reply.t)
    let decode_print_get_image_resolution_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let image_resolution = Decode.u16 buf in image_resolution) : 
      Print_get_image_resolution.Reply.t)
    let decode_notify_event buf =
      (Decode.pad buf 1;
       (let detail = Decode.u8 buf in
        Decode.pad buf 2;
        (let context = Decode.xid buf in
         let cancel = Decode.bool buf in { detail; context; cancel })) : 
      Event.Notify.t)
    let decode_attribut_notify_event buf =
      (Decode.pad buf 1;
       (let detail = Decode.u8 buf in
        Decode.pad buf 2;
        (let context = Decode.xid buf in { detail; context })) : Event.Attribut_notify.t)
    let decode_bad_context_error buf =
      (Decode.align buf 32; () : Error.Bad_context.t)
    let decode_bad_sequence_error buf =
      (Decode.align buf 32; () : Error.Bad_sequence.t)
    let decode_error ~number  buf =
      (Decode.pad buf 4;
       (match number with
        | 0 -> let error = decode_bad_context_error buf in `Bad_context error
        | 1 ->
            let error = decode_bad_sequence_error buf in `Bad_sequence error
        | n -> invalid_arg ("Invalid error number: " ^ (string_of_int n))) : 
      Error.t)
  end
module Xselinux_codec =
  struct
    open Protocol.Xselinux
    let decode_query_version_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let server_major = Decode.u16 buf in
        let server_minor = Decode.u16 buf in { server_major; server_minor }) : 
      Query_version.Reply.t)
    let decode_get_device_create_context_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let context_len = Decode.u32 buf in
        Decode.pad buf 20;
        (let context = Decode.string ~len:context_len buf in context)) : 
      Get_device_create_context.Reply.t)
    let decode_get_device_context_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let context_len = Decode.u32 buf in
        Decode.pad buf 20;
        (let context = Decode.string ~len:context_len buf in context)) : 
      Get_device_context.Reply.t)
    let decode_get_window_create_context_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let context_len = Decode.u32 buf in
        Decode.pad buf 20;
        (let context = Decode.string ~len:context_len buf in context)) : 
      Get_window_create_context.Reply.t)
    let decode_get_window_context_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let context_len = Decode.u32 buf in
        Decode.pad buf 20;
        (let context = Decode.string ~len:context_len buf in context)) : 
      Get_window_context.Reply.t)
    let decode_list_item buf =
      (let name = Decode.xid buf in
       let object_context_len = Decode.u32 buf in
       let data_context_len = Decode.u32 buf in
       let object_context = Decode.string ~len:object_context_len buf in
       Decode.align buf 4;
       (let data_context = Decode.string ~len:data_context_len buf in
        Decode.align buf 4; { name; object_context; data_context }) : 
      list_item)
    let decode_get_property_create_context_reply ~length:(length : int)  buf
      =
      (Decode.pad buf 8;
       (let context_len = Decode.u32 buf in
        Decode.pad buf 20;
        (let context = Decode.string ~len:context_len buf in context)) : 
      Get_property_create_context.Reply.t)
    let decode_get_property_use_context_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let context_len = Decode.u32 buf in
        Decode.pad buf 20;
        (let context = Decode.string ~len:context_len buf in context)) : 
      Get_property_use_context.Reply.t)
    let decode_get_property_context_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let context_len = Decode.u32 buf in
        Decode.pad buf 20;
        (let context = Decode.string ~len:context_len buf in context)) : 
      Get_property_context.Reply.t)
    let decode_get_property_data_context_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let context_len = Decode.u32 buf in
        Decode.pad buf 20;
        (let context = Decode.string ~len:context_len buf in context)) : 
      Get_property_data_context.Reply.t)
    let decode_list_properties_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let properties_len = Decode.u32 buf in
        Decode.pad buf 20;
        (let properties =
           (Decode.list ~item:decode_list_item) ~len:properties_len buf in
         properties)) : List_properties.Reply.t)
    let decode_get_selection_create_context_reply ~length:(length : int)  buf
      =
      (Decode.pad buf 8;
       (let context_len = Decode.u32 buf in
        Decode.pad buf 20;
        (let context = Decode.string ~len:context_len buf in context)) : 
      Get_selection_create_context.Reply.t)
    let decode_get_selection_use_context_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let context_len = Decode.u32 buf in
        Decode.pad buf 20;
        (let context = Decode.string ~len:context_len buf in context)) : 
      Get_selection_use_context.Reply.t)
    let decode_get_selection_context_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let context_len = Decode.u32 buf in
        Decode.pad buf 20;
        (let context = Decode.string ~len:context_len buf in context)) : 
      Get_selection_context.Reply.t)
    let decode_get_selection_data_context_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let context_len = Decode.u32 buf in
        Decode.pad buf 20;
        (let context = Decode.string ~len:context_len buf in context)) : 
      Get_selection_data_context.Reply.t)
    let decode_list_selections_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let selections_len = Decode.u32 buf in
        Decode.pad buf 20;
        (let selections =
           (Decode.list ~item:decode_list_item) ~len:selections_len buf in
         selections)) : List_selections.Reply.t)
    let decode_get_client_context_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let context_len = Decode.u32 buf in
        Decode.pad buf 20;
        (let context = Decode.string ~len:context_len buf in context)) : 
      Get_client_context.Reply.t)
    let decode_error ~number  buf =
      (Decode.pad buf 4;
       (match number with
        | n -> invalid_arg ("Invalid error number: " ^ (string_of_int n))) : 
      Error.t)
  end
module Xtest_codec =
  struct
    open Protocol.Xtest
    let decode_get_version_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let major_version = Decode.u8 buf in
        Decode.pad buf 6;
        (let minor_version = Decode.u16 buf in
         { major_version; minor_version })) : Get_version.Reply.t)
    let decode_compare_cursor_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let same = Decode.bool buf in Decode.pad buf 6; same) : Compare_cursor.Reply.t)
    let decode_error ~number  buf =
      (Decode.pad buf 4;
       (match number with
        | n -> invalid_arg ("Invalid error number: " ^ (string_of_int n))) : 
      Error.t)
  end
module Xv_codec =
  struct
    open Protocol.Xv
    let decode_rational buf =
      (let numerator = Decode.i32 buf in
       let denominator = Decode.i32 buf in { numerator; denominator } : 
      rational)
    let decode_format buf =
      (let visual = Decode.u32 buf in
       let depth = Decode.u8 buf in Decode.pad buf 3; { visual; depth } : 
      format)
    let decode_adaptor_info buf =
      (let base_id = Decode.xid buf in
       let name_size = Decode.u16 buf in
       let num_ports = Decode.u16 buf in
       let num_formats = Decode.u16 buf in
       let type_ = ((Decode.u8 %> Conv.To_i32.u8) %> Type_mask.of_int32) buf in
       Decode.pad buf 1;
       (let name = Decode.string ~len:name_size buf in
        Decode.align buf 4;
        (let formats = (Decode.list ~item:decode_format) ~len:num_formats buf in
         { base_id; num_ports; type_; name; formats })) : adaptor_info)
    let decode_encoding_info buf =
      (let encoding = Decode.xid buf in
       let name_size = Decode.u16 buf in
       let width = Decode.u16 buf in
       let height = Decode.u16 buf in
       Decode.pad buf 2;
       (let rate = decode_rational buf in
        let name = Decode.string ~len:name_size buf in
        Decode.align buf 4; { encoding; width; height; rate; name }) : 
      encoding_info)
    let decode_image buf =
      (let id = Decode.u32 buf in
       let width = Decode.u16 buf in
       let height = Decode.u16 buf in
       let data_size = Decode.u32 buf in
       let num_planes = Decode.u32 buf in
       let pitches = (Decode.list ~item:Decode.u32) ~len:num_planes buf in
       let offsets = (Decode.list ~item:Decode.u32) ~len:num_planes buf in
       let data = (Decode.list ~item:Decode.u8) ~len:data_size buf in
       { id; width; height; pitches; offsets; data } : image)
    let decode_attribute_info buf =
      (let flags =
         ((Decode.u32 %> Conv.To_i32.u32) %> Attribute_flag_mask.of_int32)
           buf in
       let min = Decode.i32 buf in
       let max = Decode.i32 buf in
       let size = Decode.u32 buf in
       let name = Decode.string ~len:size buf in
       Decode.align buf 4; { flags; min; max; name } : attribute_info)
    let decode_image_format_info buf =
      (let id = Decode.u32 buf in
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
             ((Decode.u8 %> Conv.To_int.u8) %> Scanline_order_enum.of_int)
               buf in
           Decode.pad buf 11;
           {
             id;
             type_;
             byte_order;
             guid;
             bpp;
             num_planes;
             depth;
             red_mask;
             green_mask;
             blue_mask;
             format;
             y_sample_bits;
             u_sample_bits;
             v_sample_bits;
             vhorz_y_period;
             vhorz_u_period;
             vhorz_v_period;
             vvert_y_period;
             vvert_u_period;
             vvert_v_period;
             vcomp_order;
             vscanline_order
           })))) : image_format_info)
    let decode_bad_port_error buf =
      (Decode.align buf 32; () : Error.Bad_port.t)
    let decode_bad_encoding_error buf =
      (Decode.align buf 32; () : Error.Bad_encoding.t)
    let decode_bad_control_error buf =
      (Decode.align buf 32; () : Error.Bad_control.t)
    let decode_video_notify_event buf =
      (Decode.pad buf 1;
       (let reason =
          ((Decode.byte %> Conv.To_int.byte) %>
             Video_notify_reason_enum.of_int) buf in
        Decode.pad buf 2;
        (let time = Decode.u32 buf in
         let drawable = Decode.xid buf in
         let port = Decode.xid buf in { reason; time; drawable; port })) : 
      Event.Video_notify.t)
    let decode_port_notify_event buf =
      (Decode.pad buf 4;
       (let time = Decode.u32 buf in
        let port = Decode.xid buf in
        let attribute = Decode.xid buf in
        let value = Decode.i32 buf in { time; port; attribute; value }) : 
      Event.Port_notify.t)
    let decode_query_extension_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let major = Decode.u16 buf in
        let minor = Decode.u16 buf in { major; minor }) : Query_extension.Reply.t)
    let decode_query_adaptors_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let num_adaptors = Decode.u16 buf in
        Decode.pad buf 22;
        (let info =
           (Decode.list ~item:decode_adaptor_info) ~len:num_adaptors buf in
         info)) : Query_adaptors.Reply.t)
    let decode_query_encodings_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let num_encodings = Decode.u16 buf in
        Decode.pad buf 22;
        (let info =
           (Decode.list ~item:decode_encoding_info) ~len:num_encodings buf in
         info)) : Query_encodings.Reply.t)
    let decode_grab_port_reply ~length:(length : int)  buf =
      (Decode.pad buf 1;
       (let result =
          ((Decode.byte %> Conv.To_int.byte) %> Grab_port_status_enum.of_int)
            buf in
        Decode.pad buf 6; result) : Grab_port.Reply.t)
    let decode_query_best_size_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let actual_width = Decode.u16 buf in
        let actual_height = Decode.u16 buf in { actual_width; actual_height }) : 
      Query_best_size.Reply.t)
    let decode_get_port_attribute_reply ~length:(length : int)  buf =
      (Decode.pad buf 8; (let value = Decode.i32 buf in value) : Get_port_attribute.Reply.t)
    let decode_query_port_attributes_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let num_attributes = Decode.u32 buf in
        let text_size = Decode.u32 buf in
        Decode.pad buf 16;
        (let attributes =
           (Decode.list ~item:decode_attribute_info) ~len:num_attributes buf in
         { text_size; attributes })) : Query_port_attributes.Reply.t)
    let decode_list_image_formats_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let num_formats = Decode.u32 buf in
        Decode.pad buf 20;
        (let format =
           (Decode.list ~item:decode_image_format_info) ~len:num_formats buf in
         format)) : List_image_formats.Reply.t)
    let decode_query_image_attributes_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let num_planes = Decode.u32 buf in
        let data_size = Decode.u32 buf in
        let width = Decode.u16 buf in
        let height = Decode.u16 buf in
        Decode.pad buf 12;
        (let pitches = (Decode.list ~item:Decode.u32) ~len:num_planes buf in
         let offsets = (Decode.list ~item:Decode.u32) ~len:num_planes buf in
         { data_size; width; height; pitches; offsets })) : Query_image_attributes.Reply.t)
    let decode_error ~number  buf =
      (Decode.pad buf 4;
       (match number with
        | 0 -> let error = decode_bad_port_error buf in `Bad_port error
        | 1 ->
            let error = decode_bad_encoding_error buf in `Bad_encoding error
        | 2 -> let error = decode_bad_control_error buf in `Bad_control error
        | n -> invalid_arg ("Invalid error number: " ^ (string_of_int n))) : 
      Error.t)
  end
module Xvmc_codec =
  struct
    open Protocol.Xvmc
    let decode_surface_info buf =
      (let id = Decode.xid buf in
       let chroma_format = Decode.u16 buf in
       let pad0 = Decode.u16 buf in
       let max_width = Decode.u16 buf in
       let max_height = Decode.u16 buf in
       let subpicture_max_width = Decode.u16 buf in
       let subpicture_max_height = Decode.u16 buf in
       let mc_type = Decode.u32 buf in
       let flags = Decode.u32 buf in
       {
         id;
         chroma_format;
         pad0;
         max_width;
         max_height;
         subpicture_max_width;
         subpicture_max_height;
         mc_type;
         flags
       } : surface_info)
    let decode_query_version_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let major = Decode.u32 buf in
        let minor = Decode.u32 buf in { major; minor }) : Query_version.Reply.t)
    let decode_list_surface_types_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let num = Decode.u32 buf in
        Decode.pad buf 20;
        (let surfaces = (Decode.list ~item:decode_surface_info) ~len:num buf in
         surfaces)) : List_surface_types.Reply.t)
    let decode_create_context_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let width_actual = Decode.u16 buf in
        let height_actual = Decode.u16 buf in
        let flags_return = Decode.u32 buf in
        Decode.pad buf 20;
        (let priv_data = (Decode.list ~item:Decode.u32) ~len:length buf in
         { width_actual; height_actual; flags_return; priv_data })) : 
      Create_context.Reply.t)
    let decode_create_surface_reply ~length:(length : int)  buf =
      (Decode.pad buf 32;
       (let priv_data = (Decode.list ~item:Decode.u32) ~len:length buf in
        priv_data) : Create_surface.Reply.t)
    let decode_create_subpicture_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let width_actual = Decode.u16 buf in
        let height_actual = Decode.u16 buf in
        let num_palette_entries = Decode.u16 buf in
        let entry_bytes = Decode.u16 buf in
        let component_order = (Decode.list ~item:Decode.u8) ~len:4 buf in
        Decode.pad buf 12;
        (let priv_data = (Decode.list ~item:Decode.u32) ~len:length buf in
         {
           width_actual;
           height_actual;
           num_palette_entries;
           entry_bytes;
           component_order;
           priv_data
         })) : Create_subpicture.Reply.t)
    let decode_list_subpicture_types_reply ~length:(length : int)  buf =
      (Decode.pad buf 8;
       (let num = Decode.u32 buf in
        Decode.pad buf 20;
        (let types =
           (Decode.list ~item:Xv_codec.decode_image_format_info) ~len:num buf in
         types)) : List_subpicture_types.Reply.t)
    let decode_error ~number  buf =
      (Decode.pad buf 4;
       (match number with
        | n -> invalid_arg ("Invalid error number: " ^ (string_of_int n))) : 
      Error.t)
  end
let decode_error
  ~extensions:(extensions :
                (int * (string * Protocol.Core.Query_extension.Reply.t)) list)
   buf =
  Decode.pad buf 1;
  (let number = Decode.u8 buf in
   Decode.pad buf 6;
   (let major_opcode = Decode.u16 buf in
    Decode.reset buf;
    if major_opcode > 128
    then (let error = Core_codec.decode_error buf ~number in `Core error)
    else
      (let (name, extension) = List.assoc major_opcode extensions in
       let number = extension.first_error in
       match name with
       | "BIG-REQUESTS" ->
           let error = Bigreq_codec.decode_error ~number buf in `Bigreq error
       | "RENDER" ->
           let error = Render_codec.decode_error ~number buf in `Render error
       | "SHAPE" ->
           let error = Shape_codec.decode_error ~number buf in `Shape error
       | "XFIXES" ->
           let error = Xfixes_codec.decode_error ~number buf in `Xfixes error
       | "Composite" ->
           let error = Composite_codec.decode_error ~number buf in
           `Composite error
       | "DAMAGE" ->
           let error = Damage_codec.decode_error ~number buf in `Damage error
       | "DPMS" ->
           let error = Dpms_codec.decode_error ~number buf in `Dpms error
       | "DRI2" ->
           let error = Dri2_codec.decode_error ~number buf in `Dri2 error
       | "DRI3" ->
           let error = Dri3_codec.decode_error ~number buf in `Dri3 error
       | "Generic Event Extension" ->
           let error = Ge_codec.decode_error ~number buf in `Ge error
       | "GLX" ->
           let error = Glx_codec.decode_error ~number buf in `Glx error
       | "RANDR" ->
           let error = Randr_codec.decode_error ~number buf in `Randr error
       | "SYNC" ->
           let error = Sync_codec.decode_error ~number buf in `Sync error
       | "Present" ->
           let error = Present_codec.decode_error ~number buf in
           `Present error
       | "RECORD" ->
           let error = Record_codec.decode_error ~number buf in `Record error
       | "X-Resource" ->
           let error = Res_codec.decode_error ~number buf in `Res error
       | "MIT-SCREEN-SAVER" ->
           let error = Screensaver_codec.decode_error ~number buf in
           `Screensaver error
       | "MIT-SHM" ->
           let error = Shm_codec.decode_error ~number buf in `Shm error
       | "XC-MISC" ->
           let error = Xc_misc_codec.decode_error ~number buf in
           `Xc_misc error
       | "XEVIE" ->
           let error = Xevie_codec.decode_error ~number buf in `Xevie error
       | "XFree86-DRI" ->
           let error = Xf86dri_codec.decode_error ~number buf in
           `Xf86dri error
       | "XFree86-VidModeExtension" ->
           let error = Xf86vidmode_codec.decode_error ~number buf in
           `Xf86vidmode error
       | "XINERAMA" ->
           let error = Xinerama_codec.decode_error ~number buf in
           `Xinerama error
       | "XInputExtension" ->
           let error = Xinput_codec.decode_error ~number buf in `Xinput error
       | "XpExtension" ->
           let error = Xprint_codec.decode_error ~number buf in `Xprint error
       | "SELinux" ->
           let error = Xselinux_codec.decode_error ~number buf in
           `Xselinux error
       | "XTEST" ->
           let error = Xtest_codec.decode_error ~number buf in `Xtest error
       | "XVideo" ->
           let error = Xv_codec.decode_error ~number buf in `Xv error
       | "XVideo-MotionCompensation" ->
           let error = Xvmc_codec.decode_error ~number buf in `Xvmc error
       | str -> invalid_arg ("Unknown extension name: " ^ str))))
