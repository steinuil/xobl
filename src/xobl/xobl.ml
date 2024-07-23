module X11 = struct
  include Types
  module Protocol = Protocol
  module Display_name = Display_name
  module Xauthority = Xauth
end
