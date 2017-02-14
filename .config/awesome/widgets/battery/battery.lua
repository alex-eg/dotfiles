local wibox = require("wibox")
local beautiful = require("beautiful")
local awful = require("awful")
local naughty = require("naughty")

local io = io
local math = math
local tonumber = tonumber
local tostring = tostring
local print = print
local pairs = pairs

local limits = {{12, 3},
		{ 5, 1},
		{0}}

function get_bat_state (adapter)
    -- power_supply/charge_now changes to energy_now when resuming from hibernation
    local fcur = io.open("/sys/class/power_supply/"..adapter.."/charge_now")
    local fcap = io.open("/sys/class/power_supply/"..adapter.."/charge_full")
    local fsta = io.open("/sys/class/power_supply/"..adapter.."/status")
    if fcur == nil then
        fcur = io.open("/sys/class/power_supply/"..adapter.."/energy_now")
        if fcur == nil then
            -- most probably, we just returned from sleep or
            -- nibernation and file is not here yet
            return nil, nil
        end
    end

    if fcap == nil then
        fcap = io.open("/sys/class/power_supply/"..adapter.."/energy_full")
    end

    local cur = fcur:read()
    local cap = fcap:read()
    local sta = fsta:read()
    fcur:close()
    fcap:close()
    fsta:close()
    local battery = math.floor(cur * 100 / cap)
    if sta:match("Charging") then
        dir = 1
    elseif sta:match("Discharging") then
        dir = -1
    else
        dir = 0
        battery = ""
    end
    return battery, dir
end

function getnextlim (num)
    for ind, pair in pairs(limits) do
        lim = pair[1]; step = pair[2]; nextlim = limits[ind+1][1] or 0
        if num > nextlim then
            repeat
                lim = lim - step
            until num > lim
            if lim < nextlim then
                lim = nextlim
            end
            return lim
        end
    end
end


function batclosure (adapter)
    local nextlim = limits[1][1]
    return function ()
        local prefix = "⚡C⚡"
        local battery, dir = get_bat_state(adapter)
        if battery == nil then return end -- just thawed or woke up
        if dir == -1 then
            dirsign = "↓"
            prefix = "Bat:"
            if battery <= nextlim then
                naughty.notify({title = "⚡ Warning ⚡",
                                text = "Battery charge is low ( ⚡ "..battery.."%)!",
                                timeout = 7,
                                position = "bottom_right",
                                fg = beautiful.fg_focus,
                                bg = beautiful.bg_focus
                               })
                nextlim = getnextlim(battery)
            end
            return "<span font_weight='bold'> "..prefix.." "..dirsign..battery..dirsign.." </span>"
        elseif dir == 1 then
            dirsign = "↑"
            nextlim = limits[1][1]
            return "<span font_weight='bold'> "..prefix.." "..dirsign..battery..dirsign.." </span>"
        else
            dirsign = ""
            return "<span font_weight='bold'> "..prefix.." </span>"
        end
        if dir ~= 0 then battery = battery.."%" end

    end
end

battery_widget_text = wibox.widget.textbox()
battery_widget_text:set_align("right")

bat_clo = batclosure("BAT0")

battery_widget_text:set_markup(bat_clo())
battimer = timer({ timeout = 30 })
battimer:connect_signal("timeout",
			function()
                            battery_widget_text:set_markup(bat_clo())
			end)
battimer:start()

battery_widget = wibox.widget.background()
battery_widget:set_widget(battery_widget_text)
--battery_widget:set_bg('#660066')
