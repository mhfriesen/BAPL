local lpeg = require"lpeg"
local pt = require"pt"

-- take members of a table
--    first n1 members (n2 nil)
--    n1 to n2 members
function take(t,n1, n2)
  local r = {}
  if not n2 then -- take first n1
    table.move(t, 1, n1, 1, r)
  else -- take n1 to n2
    table.move(t, n1, n2, 1, r)
  end
  return r
end

-- iterate a table by sorted keys
function kpairs (t, f)
  local a = {}
  f = f or function(a, b) return a < b end
  for n in pairs(t) do table.insert(a, n) end
  table.sort(a, f)
  local i = 0      -- iterator variable
  local iter = function ()   -- iterator function
    i = i + 1
    if a[i] == nil then 
      return nil
    else 
      return a[i], t[a[i]]
    end
  end
  return iter
end


function split(s, sep)
  sep = lpeg.P(sep)
  local elem = lpeg.C((1 - sep)^0)
  local p = lpeg.Ct(elem * (sep * elem)^0)   -- make a table capture
  return lpeg.match(p, s)
end

function factorial (n)
  n = tonumber(n)
  if n <= 0 then
    return 1
  else
    return n * factorial(n-1)
  end
end

-- overload arithmetic and relational operators on zone type
-- hydro generator zones; similar to intervals but with subtleties to come
function tozone(t)
  local convert = function(a) return type(a)=="number" and {a,a} or a end -- promote number to a zone
  t["size"] = 2 -- this size attribute makes zone type inherit the functionality of array type 
  return setmetatable(t, {
    __concat = function(z1,z2) z1, z2 = convert(z1), convert(z2)
      return tozone({math.min(z1[1],z2[1]), math.max(z1[2],z2[2])}) end,
    __add = function(z1,z2) z1, z2 = convert(z1), convert(z2)
      return tozone({z1[1]+z2[1], z1[2]+z2[2]}) end,
    __sub = function(z1,z2) z1, z2 = convert(z1), convert(z2)
      return tozone({z1[1]-z2[1], z1[2]-z2[2]}) end,
    __unm = function(z) return tozone({-z[2], -z[1]}) end,
    __eq = function(z1,z2) z1, z2 = convert(z1), convert(z2)
      return z1[1]==z2[1] and z1[2]==z2[2] end,
    __lt = function(z1,z2) z1, z2 = convert(z1), convert(z2)
      return z1[1]<z2[1] end,
    __tostring = function(z) return table.concat(z,":") end,
  })
end

-- this way exports recursive functions properly
local M = {}
M.take = take
M.split = split
M.kpairs = kpairs
M.tozone = tozone
M.factorial = factorial

return M