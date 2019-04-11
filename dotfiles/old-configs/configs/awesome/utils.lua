require("lfs")
local awful = require("awful")

local utils = {}

local function processwalker()
    local function yieldprocess()
        for dir in lfs.dir("/proc") do
        -- All directories in /proc containing a number, represent a process
        if tonumber(dir) ~= nil then
          local f, err = io.open("/proc/"..dir.."/cmdline")
          if f then
            local cmdline = f:read("*all")
            f:close()
            if cmdline ~= "" then
              coroutine.yield(cmdline, dir)
            end
          end
        end
      end
    end
    return coroutine.wrap(yieldprocess)
end

local function run_once(process, cmd, rerun)
   assert(type(process) == "string")
   local regex_killer = {
      ["+"]  = "%+", ["-"] = "%-",
      ["*"]  = "%*", ["?"]  = "%?" }

   for p, pid in processwalker() do
      if p:find(process:gsub("[-+?*]", regex_killer)) then
          if rerun then
              awful.util.spawn_with_shell("kill "..pid)
          else
              return
          end
      end
   end
   return awful.util.spawn_with_shell(cmd or process)
end

local function rerun(process, cmd)
    run_once(process, cmd, true)
end
-- }}}

utils.run_once = run_once
utils.run = awful.util.spawn_with_shell
utils.rerun = rerun

return utils
