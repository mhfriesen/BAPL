local lfs = require"lfs" --luafilesystem
dofile(lfs.currentdir().."\\pt.lua") --table to string

function test(stk)
  print("stk=", stk)

  print"-----push one"
  stk:push("one")
  print("stk=",stk)
  print("top="..tostring(stk:top()))

  --[[ TRY CORRUPTING THE STACK OBJECT to see which implemenations are more robust
  print("stk.stk=", pt(stk.stk))
  stk.stk = "corrupt"
  print("stk=", pt(stk)) --stack prototype, aka class, stk (stk of instances are private)

  print("stk.pop=", pop)
  stk.pop = "corrupt"
  print("stk=", pt(stk)) --stack prototype, aka class, stk (stk of instances are private)
  --]]

  print"-----push two"
  stk:push("two")
  print("stk=",stk)
  print("top="..tostring(stk:top()))

  print"-----pop"
  x = stk:pop()
  print("x="..x, "stk="..tostring(stk))
  print("top="..tostring(stk:top()))
  
  print"-----pop"
  x = stk:pop()
  print("x="..x, "stk="..tostring(stk))
  print("top="..tostring(stk:top()))
end

---[=[
print"Ex 21.1 1object oriented stack ------------------------------------------------"
stack = {}

function stack:pop()
  return table.remove(self.stk)
end

function stack:top()
  return self.stk[#self.stk]
end

function stack:push(data) 
  table.insert(self.stk, data)
end

function stack:new(o) 
  o = o or {}
  o.stk = {} -- stk of stack instance is private
  local mt = {
    __index = self, 
    __tostring = function(stk) return pt(stk.stk) end
  }
  return setmetatable(o, mt) 
end

stk1 = stack:new()
test(stk1)

--]=]


---[=[
print"\n\nEx 21.2 queue (inherit from stack) ----------------------------------------"
queue = stack:new({})

function queue:insertbottom(data) 
  table.insert(self.stk,1,data) 
end

q = queue:new()
test(q)
print"-----insertbottom"
q:insertbottom("bot")
print("x="..x, "q="..tostring(q))
print("top="..tostring(q:top()))
--]=]


---[=[
print"\n\nEx 21.3 stack (using a dual representation) ----------------------------------"
stack = {}

local stks = {} 

function stack:new(o) 
  o = o or {}
  stks[o] = {} -- stk of stack instance is private
  local mt = {
    __index = self, 
    __tostring = function(stk) return pt(stks[stk]) end
  }
  return setmetatable(o, mt) 
end

function stack:pop()
  return table.remove(stks[self])
end

function stack:top()
  return stks[self][#stks[self]]
end

function stack:push(data) 
  table.insert(stks[self], data)
end

stk2 = stack:new()
test(stk2)

--]=]

print"\n\nEx 21.4 Account (variation of dual representation approach) ---------------"

Account = {} 

local proxies = {} -- proxies table that keeps state for each account instance

function Account:new(acct)
  acct = acct and acct or {}
  local proxy = {balance = 0}
  proxies[acct] = proxy
  return setmetatable(acct, {__index = self})
end

function Account:withdrawal(v)
  proxies[self].balance = proxies[self].balance - v
end

function Account:deposit(v)
  proxies[self].balance = proxies[self].balance + v
end
  
function Account:balance(v)
  return proxies[self].balance 
end
  
acct = Account:new()
acct2 = Account:new()
print("acct bal", acct:balance())  --> 0
acct:deposit(100)
acct2:deposit(1)
acct:withdrawal(10)
print("acct bal",acct:balance())  --> 90
print("acct2 bal",acct2:balance()) --? 1

-- compared to dual representation implementation
-- pros: just one proxies table is required to support multiple state variables, inheritence the same
-- cons: getting state requires extra level of indirection, need weak table/s?
