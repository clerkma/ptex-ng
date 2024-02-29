#!/usr/bin/env texlua

--[[

File l3build.lua Copyright (C) 2014-2024 The LaTeX Project

It may be distributed and/or modified under the conditions of the
LaTeX Project Public License (LPPL), either version 1.3c of this
license or (at your option) any later version.  The latest version
of this license is in the file

   https://www.latex-project.org/lppl.txt

This file is part of the "l3build bundle" (The Work in LPPL)
and all files in that bundle must be distributed together.

-----------------------------------------------------------------------

The development version of the bundle can be found at

   https://github.com/latex3/l3build

for those people who are interested.

--]]

-- Version information
release_date = "2024-02-08"

-- File operations are aided by the LuaFileSystem module
local lfs = require("lfs")

-- Local access to functions

local ipairs           = ipairs
local insert           = table.insert
local lookup           = kpse.lookup
local match            = string.match
local gsub             = string.gsub
local next             = next
local print            = print
local exit             = os.exit
local open             = io.open

-- l3build setup and functions
kpse.set_program_name("kpsewhich")
build_kpse_path = match(lookup("l3build.lua"),"(.*[/])")
local function build_require(s)
  require(lookup("l3build-"..s..".lua", { path = build_kpse_path } ) )
end

-- Minimal code to do basic checks
build_require("arguments")
build_require("help")

build_require("file-functions")
build_require("typesetting")
build_require("aux")
build_require("clean")
build_require("check")
build_require("ctan")
build_require("install")
build_require("unpack")
build_require("manifest")
build_require("manifest-setup")
build_require("tagging")
build_require("upload")
build_require("stdmain")

-- This has to come after stdmain(),
-- and that has to come after the functions are defined
if options["target"] == "help" then
  help()
  exit(0)
elseif options["target"] == "version" then
  version()
  exit(0)
end

-- Look for some configuration details
if fileexists("build.lua") then
  dofile("build.lua")
else
  print("Error: Cannot find configuration build.lua")
  exit(1)
end

-- Load standard settings for variables:
-- comes after any user versions
build_require("variables")

-- Ensure that directories are 'space safe'
maindir       = escapepath(maindir)
docfiledir    = escapepath(docfiledir)
sourcefiledir = escapepath(sourcefiledir)
supportdir    = escapepath(supportdir)
testfiledir   = escapepath(testfiledir)
testsuppdir   = escapepath(testsuppdir)
builddir      = escapepath(builddir)
distribdir    = escapepath(distribdir)
localdir      = escapepath(localdir)
resultdir     = escapepath(resultdir)
testdir       = escapepath(testdir)
typesetdir    = escapepath(typesetdir)
unpackdir     = escapepath(unpackdir)

-- Tidy up the epoch setting
-- Force an epoch if set at the command line
-- Must be done after loading variables, etc.
if options["epoch"] then
  epoch           = options["epoch"]
  forcecheckepoch = true
  forcedocepoch   = true
end
epoch = normalise_epoch(epoch)
-- LuaTeX needs the `-utc` option
if forcecheckepoch then
  if next(specialformats) and next(specialformats.latex)
    and next (specialformats.latex.luatex) then
    local options = specialformats.latex.luatex.options
    specialformats.latex.luatex.options = (options and (options .. " ") or "") .. "-utc"
  end
  if next(specialformats) and next(specialformats["latex-dev"])
    and next (specialformats["latex-dev"].luatex) then
    local options = specialformats["latex-dev"].luatex.options
    specialformats["latex-dev"].luatex.options = (options and (options .. " ") or "") .. "-utc"
  end
end
if forcedocepoch then
  if match(typesetexe,"luatex") or match(typesetexe,"lualatex") then
    typesetopts = typsetopts .. " -utc"
  end
end

--
-- Deal with multiple configs for tests
--

-- When we have specific files to deal with, only use explicit configs
-- (or just the default one)
if options["names"] then
  checkconfigs = options["config"] or {"build"}
else
  checkconfigs = options["config"] or checkconfigs
end

if #checkconfigs > 1 then
  if options["target"] == "check" or options["target"] == "bundlecheck" then
    local errorlevel = 0
    local failed = { }
    for i = 1, #checkconfigs do
      options["config"] = {checkconfigs[i]}
      errorlevel = call({"."}, "check", options)
      if errorlevel ~= 0 then
        if options["halt-on-error"] then
          exit(1)
        else
          insert(failed,checkconfigs[i])
        end
      end
    end
    if next(failed) then
      for _,config in ipairs(failed) do
        checkdiff(config)
      end
      if options["show-saves"] then
        local savecmds, recheckcmds = "", ""
        for _,config in ipairs(failed) do
          local testdir = testdir
          if config ~= "build" then
            testdir = testdir .. "-" .. config
          end
          local f = open(testdir .. "/.savecommands")
          if not f then
            print("Error: Cannot find save commands for configuration \"" ..
              config .. "\"")
            exit(2)
          end
          for line in f:lines() do
            if line == "" then break end
            savecmds = savecmds .. "  " .. line .. "\n"
          end
          for line in f:lines() do
            recheckcmds = recheckcmds .. "  " .. line .. "\n"
          end
          f:close()
        end
        print"To regenerate the test files, run\n"
        print(savecmds)
        if recheckcmds ~= "" and #checkengines ~= 1 then
          print"To detect engine-specific differences, run after that\n"
          print(recheckcmds)
        end
      end
      exit(1)
    else
      -- Avoid running the 'main' set of tests twice
      exit(0)
    end
  elseif options["target"] == "clean" then
    local failure
    for i = 1, #checkconfigs do
      options["config"] = {checkconfigs[i]}
      failure = 0 ~= call({"."}, "clean", options) or failure
    end
    exit(failure and 1 or 0)
  end
end
if #checkconfigs == 1 and
  (options["target"] == "check" or options["target"] == "save" or options["target"] == "clean") then
  if checkconfigs[1] == "build" then
    -- Sanity check for default config
    check_engines("build.lua")
  else
    local configname  = gsub(checkconfigs[1], "%.lua$", "")
    local config = "./" .. configname .. ".lua"
    if fileexists(config) then
      local savedtestfiledir = testfiledir
      dofile(config)
      -- Sanity check for non-default config
      check_engines(configname .. ".lua")
      testdir = testdir .. "-" .. configname
      -- Reset testsuppdir if required
      if savedtestfiledir ~= testfiledir and
        testsuppdir == savedtestfiledir .. "/support" then
        testsuppdir = testfiledir .. "/support"
      end
    else
      print("Error: Cannot find configuration \"" ..  configname .. ".lua\"")
      exit(1)
    end
  end
end

-- Call the main function
main(options["target"], options["names"])
