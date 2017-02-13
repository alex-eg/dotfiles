local wibox = require("wibox")
local awful = require("awful")

volume_widget_text = wibox.widget.textbox()
volume_widget_text:set_align("right")

function update_volume(widget)
   local fd = io.popen("amixer sget Master")
   local status = fd:read("*all")
   fd:close()

   local volume = string.match(status, "(%d?%d?%d)%%")
   volume = string.format("%d", volume)

   status = string.match(status, "%[(o[^%]]*)%]")

   if string.find(status, "on", 1, true) then
       volume = " <span font_weight='bold'>" .. volume .. "</span> "
   else
       volume = " <span font_weight='bold' strikethrough='true'>" .. volume .. "</span> "
   end
   widget:set_markup(volume)
end

update_volume(volume_widget_text)

volume_widget = wibox.widget.background()
volume_widget:set_widget(volume_widget_text)
--volume_widget:set_bg('#000066')
