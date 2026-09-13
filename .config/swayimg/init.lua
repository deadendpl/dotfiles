function go_by_1_10th(direction)
  local wnd = swayimg.get_window_size()
  local pos = swayimg.viewer.get_position()
  local direction_to_go

  if direction == "left" then
    direction_to_go = math.floor(pos.x + wnd.width / 10)
  elseif direction == "right" then
    direction_to_go = math.floor(pos.x - wnd.width / 10)
  elseif direction == "up" then
    direction_to_go = math.floor(pos.y + wnd.height / 10)
  elseif direction == "down" then
    direction_to_go = math.floor(pos.y - wnd.height / 10)
  end

  if direction == "left" or direction == "right" then
    swayimg.viewer.set_abs_position(direction_to_go, pos.y)
  elseif direction == "up" or direction == "down" then
    swayimg.viewer.set_abs_position(pos.x, direction_to_go)
  end
end

function text_toggle()
  swayimg.text.visible = not swayimg.text.visible
end

-- General config
swayimg.mode = "viewer"                -- mode at startup
swayimg.antialiasing = false        -- anti-aliasing
swayimg.decoration = true           -- window title/buttons/borders
swayimg.overlay = false             -- window overlay mode
swayimg.exif_orientation = true     -- image orientation by EXIF
swayimg.dnd_button = "MouseRight"      -- drag-and-drop mouse button

-- Image list configuration
swayimg.imagelist.order = "numeric"    -- list order
swayimg.imagelist.reverse = false   -- reverse order
swayimg.imagelist.recursive = false -- recursive directory reading
swayimg.imagelist.adjacent = false  -- add adjacent files from same dir
swayimg.imagelist.fsmon = true      -- enable file system monitoring

-- Text overlay configuration
swayimg.text.font = "monospace"        -- font name
swayimg.text.size = 15                 -- font size in pixels
swayimg.text.spacing = 0               -- line spacing
swayimg.text.padding = 10              -- padding from window edge
swayimg.text.color = 0xffcccccc   -- foreground text color
swayimg.text.background = 0x00000000   -- text background color
swayimg.text.shadow = 0x0d000000       -- text shadow color
swayimg.text.timeout = 5               -- layer hide timeout
swayimg.text.status_timeout = 3        -- status message hide timeout

-- Image viewer mode
swayimg.viewer.default_scale = "optimal"      -- default image scale
swayimg.viewer.default_position = "center"    -- default image position
swayimg.viewer.drag_button = "MouseLeft"      -- mouse button to drag image
swayimg.viewer.set_window_background(0xff000000) -- window background color
swayimg.viewer.set_image_chessboard(20, 0xff333333, 0xff4c4c4c) -- chessboard
swayimg.viewer.autocenter = true            -- enable automatic centering
swayimg.viewer.loop = true                 -- enable image list loop mode
swayimg.viewer.preload = 1                  -- number of images to preload
swayimg.viewer.history = 1                  -- number of the history cache
swayimg.viewer.mark_color = 0xff808080        -- mark icon color
swayimg.viewer.text = {
  topleft = {             -- top left text block scheme
    "File: {name}",
    "Format: {format}",
    "File size: {sizehr}",
    "File time: {time}",
    "EXIF date: {meta.Exif.Photo.DateTimeOriginal}",
    "EXIF camera: {meta.Exif.Image.Model}"
  },
  topright = {            -- top right text block scheme
    "Image: {list.index} of {list.total}",
    "Frame: {frame.index} of {frame.total}",
    "Dimensions: {frame.width}x{frame.height}"
  },
  bottomleft = {          -- bottom left text block scheme
    "Scale: {scale}"
  }
}

-- Key and mouse bindings in viewer mode (example only, not all):

-- bind a key for exit
swayimg.viewer.on_key("q", function()
  swayimg.exit()
end)

-- bind the hjkl to move the image to a side by 1/10 of the application
-- window size
swayimg.viewer.on_key("h", function()
  go_by_1_10th("left")
end)

swayimg.viewer.on_key("l", function()
  go_by_1_10th("right")
end)

swayimg.viewer.on_key("j", function()
  go_by_1_10th("down")
end)

swayimg.viewer.on_key("k", function()
  go_by_1_10th("up")
end)

swayimg.viewer.on_key("0", function()
  swayimg.viewer.set_fix_scale("real")
end)

-- FIXME doesn't work
swayimg.viewer.on_key("Shift-j", function()
  swayimg.viewer.open("next")
end)

-- FIXME doesn't work
swayimg.viewer.on_key("Shift-k", function()
  swayimg.viewer.open("prev")
end)

swayimg.viewer.on_key("i", text_toggle)

-- bind mouse vertical scroll button with pressed Ctrl to zoom in the
-- image at mouse pointer coordinates
swayimg.viewer.on_mouse("Ctrl-ScrollUp", function()
  local pos = swayimg.get_mouse_pos()
  local scale = swayimg.viewer.get_scale()
  scale = scale + scale / 10
  swayimg.viewer.set_abs_scale(scale, pos.x, pos.y);
end)

swayimg.viewer.on_mouse("Ctrl-ScrollDown", function()
  local pos = swayimg.get_mouse_pos()
  local scale = swayimg.viewer.get_scale()
  scale = scale - scale / 10
  swayimg.viewer.set_abs_scale(scale, pos.x, pos.y);
end)

-- Slide show mode, same config as for viewer mode with the following defaults:
swayimg.slideshow.timeout = 5                    -- timeout to switch image
swayimg.slideshow.default_scale = "fit"          -- default image scale
swayimg.slideshow.set_window_background("auto")     -- window background mode
swayimg.slideshow.history = 0                  -- number of the history cache
swayimg.slideshow.text = {topleft = { "{name}" }} -- top left text block scheme


-- Gallery mode
swayimg.gallery.aspect = "fill"                  -- thumbnail aspect ratio
swayimg.gallery.thumb_size = 200                 -- thumbnail size in pixels
swayimg.gallery.padding_size = 5                 -- padding between thumbnails
swayimg.gallery.border_size = 5                  -- border size for selected thumbnail
swayimg.gallery.border_color = 0xffaaaaaa        -- border color for selected thumbnail
swayimg.gallery.selected_scale = 1.15            -- scale for selected thumbnail
swayimg.gallery.selected_color = 0xff404040      -- background color for selected thumbnail
swayimg.gallery.unselected_color = 0xff202020    -- background color for unselected thumbnail
swayimg.gallery.window_color = 0xff000000        -- window background color
swayimg.gallery.cache = 100                    -- number of thumbnails stored in memory
swayimg.gallery.preload = false               -- preloading invisible thumbnails
swayimg.gallery.pstore = false                -- enable persistent storage for thumbnails
swayimg.gallery.text = {
  topleft = {               -- top left text block scheme
    "File: {name}"
  },
  topright = {              -- top right text block scheme
    "{list.index} of {list.total}"
  }
}

-- Key and mouse bindings in gallery mode (example only, not all):

-- bind Enter key to open image in viewer
swayimg.gallery.on_key("Return", function()
  swayimg.set_mode("viewer")
end)
-- bind the left arrow key to select thumbnail on the left side
swayimg.gallery.on_key("Left", function()
  swayimg.gallery.switch_image("left")
end)

--
-- Other configuration examples
--

-- force set scale mode on window resize (useful for tiling compositors)
swayimg.on_window_resize(function()
  swayimg.viewer.set_fix_scale("optimal")
end)
