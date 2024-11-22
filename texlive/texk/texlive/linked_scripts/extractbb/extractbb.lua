#!/usr/bin/env texlua
-- extractbb-lua
-- https://github.com/gucci-on-fleek/extractbb
-- SPDX-License-Identifier: MPL-2.0+
-- SPDX-FileCopyrightText: 2024 Max Chernoff
--
-- A wrapper script to allow you to choose which implementation of extractbb to
-- use. Should hopefully be replaced with the ``scratch'' file in TeX Live 2025.
--
-- v1.0.6 (2024-11-21) %%version %%dashdate

---------------------
--- Configuration ---
---------------------
-- Choose which implementation of extractbb to use.
local DEFAULT = "wrapper"


-----------------
--- Execution ---
-----------------

-- Send the error messages to stderr.
local function error(...)
    -- Header
    io.stderr:write("! extractbb ERROR: ")

    -- Message
    for i = 1, select("#", ...) do
        io.stderr:write(tostring(select(i, ...)), " ")
    end

    -- Flush and exit
    io.stderr:write("\n")
    io.stderr:flush()
    os.exit(1)
end

-- Get the value of the environment variable that decides which version to run.
local env_choice = os.env["TEXLIVE_EXTRACTBB"]

-- If the environment variable is set to a file path, run that directly.
local env_mode = lfs.attributes(env_choice or "", "mode")
if (env_mode == "file") or (env_mode == "link") then
    arg[0] = env_choice
    table.insert(arg, 1, env_choice)
    arg[-1] = nil
    return os.exec(arg)
end

-- Find the subscripts
kpse.set_program_name("texlua", "extractbb")

local function find_script(name)
    -- Find the script, searching **only** in the scripts directories.
    local path = kpse.lookup(
        name,
        { path = kpse.var_value("TEXMFSCRIPTS"), format = "lua" }
    )

    -- Make sure that the script is not writable.
    if kpse.out_name_ok_silent_extended(path) then
        if os.env["TEXLIVE_EXTRACTBB_UNSAFE"] == "unsafe" then
            -- If we're running in development mode, then we can allow this.
        else
            error("Refusing to run a writable script.")
        end
    end

    return path
end

-- Map the choice names to file names.
local choice_mapping = {
    wrapper = find_script("extractbb-wrapper.lua"),
    scratch = find_script("extractbb-scratch.lua"),
}

-- Choose the implementation to run.
local choice = choice_mapping[env_choice] or choice_mapping[DEFAULT]

if not choice then
    error("No implementation of extractbb found.")
end

-- And run it.
dofile(choice)
