-- test LPEG pattern on a table of strings e.g. test("arithmetic", p, {"1+1", "   1 +2"})
function test(name, pattern, tests)
  print("\n"..name)
  local str, chk
  for i,test in ipairs(tests) do
    if type(test)=="table" then -- includes a check
      str, chk = test[1], test[2]
    else
      str = test
      chk = nil
    end
    io.write(str,"--->")
    local results = {pattern:match(str)}
    if #results == 0 then
        io.write("nil")
        if chk then
          local isgood = tostring(results[#results]) == tostring(chk)
          io.write("===>",tostring(chk)," ", isgood and "GREAT!" or "OH NO!")
        end
    else
      for _,r in ipairs(results) do
        io.write(tostring(r)," ")
      end
      if chk then 
        local isgood = tostring(results[#results]) == tostring(chk)
        io.write("===>",tostring(chk)," ", isgood and "GREAT!" or "OH NO!")
      end
    end
    io.write("\n")
  end
end
