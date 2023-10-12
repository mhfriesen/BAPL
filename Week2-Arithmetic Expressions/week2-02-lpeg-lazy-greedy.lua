local lpeg = require"lpeg"
local lfs = require"lfs" --luafilesystem
dofile(lfs.currentdir().."\\pt.lua") --table to string
dofile(lfs.currentdir().."\\test.lua") -- run use cases
loc = lpeg.locale()

--print("\n".."LPEG possessiveness end of WEEK1 inclass exercise")
--print("regex",string.match("AAABAAA:B", ".*:") )  --> AAABAAA:
--print("lpeg", lpeg.match(p,"AAABAAA:B"))

function concat(...) return table.concat({...}) end
subject = "hello!hello!hello!"
lazy = lpeg.C(lpeg.P(1-lpeg.P("!"))^0 * "!" ) / concat
greedy = lazy^1 / concat
print(lazy:match(subject))
print(greedy:match(subject))

