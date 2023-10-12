nl = "\n"

function tostringt(name,t)
  local str =""
  str = str .. name..": "
  local indexes = {}
  for k,v in pairs(t) do
    table.insert(indexes,k)
  end
  table.sort(indexes,function(a,b) return tostring(a) < tostring(b) end)
  --print("indexes",table.concat(indexes,","))
  for _,k in ipairs(indexes) do
    local isnumber =  type(k) == "number"
    str = str .. tostring(k) .. "=" .. tostring(isnumber and t[k] or t[tostring(k)]) .. "  "
  end
  return str
end

function show(t)
  io.write("#t=",#t," t=", table.concat(t,","), nl)
  io.write("pairs=")
  for k,v in pairs(t) do
    io.write("(",k,",",v,") ")
  end
  io.write(nl)
  io.write("ipairs=")
  for k,v in ipairs(t) do
    io.write("(",k,",",v,") ")
  end
  io.write(nl,nl)
end

-- differences between sequence and non sequence tables
t = {[1]=1, [2]=2}
print("sequence")
show(t)

t = {[3]=1, [4]=2}
print("non sequence")
show(t)

t = {1,2,3,4}
table.move(t,1,2,5)
print(table.concat(t,","))

function clone(t)
  return table.move(t,1,#t,1,{})
end

t2=clone(t)
t2[1] = "a"
print(table.concat(t,","))
print(table.concat(t2,","))
print()

a = {}; a.a = a
print(a.a.a)
print(a.a.a.a)
a.a.a.a = 3
--print(a.a.a.a) -> error
print()

function poly(t,x)
  local y = 0
  for i,v in ipairs(t) do
    print(x^i, v*x^i)
    y = y + v*x^i
  end
  return y
end

function poly2(t,x)
  local y = 0
  xns = {x}
  for i=2,#t do
    table.insert(xns,xns[i-1]*x)
  end
  for i,v in ipairs(xns) do
    print(i, v, t[i]*v)
    y = y + t[i]*v
  end
  return y
end

print(poly({1,.1,.01},200))
print(poly2({1,.1,.01},200))



function isavalidsequence(t)
  local indexes = {}
  for k,v in pairs(t) do
    if type(k) == "number" then
     local i,f = math.modf(k)
     if f == 0 then
       table.insert(indexes,k)
      end
    end
  end
  --print(table.concat(indexes,","))
  table.sort(indexes)
  --print(table.concat(indexes,","))
  for i=1,#indexes do
    if indexes[i] ~= i then 
      return false
    end
  end
  return true -- if indexes is empty its a valid empty sequence
end

print()
print("test sequence validity")
tests = {{1,2}, {a=1,2}, {a=1,b=2}, {[1]=1,[2]=nil,[3]=3}, {1,2,[3.1]=3}}
for _,t in ipairs(tests) do
  io.write(tostringt("t",t), " ",tostring(isavalidsequence(t)),nl)
end

function insertall(l1,l2,p)
  local t1 = {}
  table.move(l1,1,#l1,1,t1) -- all l1
  --print("t1",table.concat(t1,","))
  local t2 = {}
  table.move(l2,1,p-1,1,t2) -- before p of l2
  --print("t2",table.concat(t2,","))
  local t3 = {}
  table.move(l2,p,#l2,1,t3) -- p and after of l2
  --print("t3",table.concat(t3,","))
  table.move(t1,1,#t1,#t2+1,t2) -- t2 .. t1
  --print("t2",table.concat(t2,","))
  table.move(t3,1,#t3,#t2+1,t2) -- t1 .. t3
  return t2
end

print("\ninsertall")
t1,t2 = {1,2,3},{"a","b"}
print(table.concat( insertall(t1,t2,1),","))
print(table.concat( insertall(t1,t2,2),","))
print(table.concat( insertall(t1,t2,3),","))

function myconcat(t,sep)
  local s = ""
  for _,v in ipairs(t) do
    s = s .. tostring(v)..sep
  end
  return string.sub(s,1,#s-1)
end

print()

l = {}
for i=1,1000000 do
  table.insert(l, i)
end
print(#l)

t1 = os.time()
s = table.concat(l,",")
print(#s)
print(string.sub(s,1,20))
print("time", os.time()-t1)

print()

t1 = os.time()
s = myconcat(l,",")
print(#s)
print(string.sub(s,1,20))
print("time", os.time()-t1)

