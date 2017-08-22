--[[
battery.lua - acpi Battery Status Widget
Shows the power of the laptop battery.
Requires the acpi package. Install via your preferred package
manager.
This widget will be updated in My GitHub Repository:
    Lawful-Lazy/ultimate-awesome-config/rcmods/widgets/battery.lua
How to use this widget:
1. Copy this file to your awesome config.
2. Obtain the widget via this command:
    local battw = require("path.to.battery")
    -- Where the file is in ~/.config/awesome/path/to/battery.lua
3. Add the widget to your panel.
UX:
* Green when above 95%
* Yellow when down to 40%
* Orange when down to 20%
  * Notifies that battery is low
* Red when down to 10%
  * Notifies that battery is critical
* Bold when charging

Copyright (c) 2016 Samuel Hunter <mathematicalchromosomes@gmail.com>
Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or
sell copies of the Software, and to permit persons to whom
the Software is furnished to do so, subject to the following
conditions:
The above copyright notice and this permission notice shall
be included in all copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO
THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS
OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
]]--

local awful = require("awful")
local naughty = require("naughty")
local wibox = require("wibox")


local battw = wibox.widget.textbox()
battw.status = 100

local warning_note = {
  title = "Your battery is getting %s",
  text = "Battery level at %i%%.",
  timeout = 10
}

-- Uses command `acpi` to see if charging
local function ischarging()
  -- Runs acpi, then only gets the on-line/off-line text
  local fh = assert(io.popen("acpi -a | cut -d: -f 2 | sed 's/ //g'"), "r")
  local status = fh:read("*l")
  fh:close()

  -- If offline, then the status would be "off-line".
  return status == "on-line"
end


-- Runs every so often to update everything
local function update()
  -- Gets battery status by percent
  local fh = assert(io.popen("acpi | cut -d, -f 2 | grep -Po [0-9]+", "r"))
  local status = fh:read("*l")
  fh:close()
  status = tonumber(status)

  local note = awful.util.table.clone(warning_note, false)
  local shouldINotify = false

  -- local markup = "<span color='%s'>"..status.."%% &#128267;</span>"
  local markup = "<span color='%s'> "..status.."%%</span>"
  -- local markup = " "..status.."%%"
  local color = nil

  if status <= 10 then
    color = "#FF0000"
    note.title = note.title:format("critical!")
    if battw.status > 10 then
      shouldINotify = true
    end
  elseif status <= 25 then
    color = "#FF8800"
    note.title = note.title:format("low.")
    if battw.status > 25 then
      shouldINotify = true
    end
  elseif status <= 40 then
    color = "#FFFF00"
  elseif status > 95 then
    color = "#00FF00"
  else
    color = "#FFFFFF"
    markup = " "..status.."%%"
  end

  if shouldINotify then
    note.text = note.text:format(status)
    note.bg = color
    if battw.msgid then
      note.replaces_id = battw.msgid
    end
    battw.msgid = naughty.notify(note)
  end

  if ischarging() and status < 90 then
    markup = "<b>".. markup .."</b>"
  end

  battw:set_markup(markup:format(color))
  battw.status = status
end
update()


local bwtimer = timer({ timeout = 5 })
bwtimer:connect_signal("timeout", update)
bwtimer:start()

return battw
