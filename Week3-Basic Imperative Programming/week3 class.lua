local lpeg = require"lpeg"
local lfs = require"lfs" --luafilesystem
dofile(lfs.currentdir().."\\pt.lua") --table to string

ws = lpeg.S(' \n\t')^0

digits = lpeg.R('09')^1
sign = lpeg.S('+-')^-1
num = (sign * digits / tonumber) * ws
opA = lpeg.C(lpeg.S('+-')) * ws
opM = lpeg.C(lpeg.S('*/%')) * ws
opE = lpeg.S('^')
Lparen = lpeg.P('(') * ws
Rparen = lpeg.P(')') * ws
stop = lpeg.P(-1)

function add (lst)
    local e = lst[1]
    for i = 2, #lst, 2  do
        if lst[i] == "+" then
            e = e + lst[i+1]
        elseif lst[i] == "-" then
            e = e - lst[i+1]
        end
    end
    return e
end

function mult (lst)
    local e = lst[1]
    for i = 2, #lst, 2  do
        if lst[i] == "*" then
            e = e * lst[i+1]
        elseif lst[i] == "/" then
            e = e / lst[i+1]
        elseif lst[i] == "%" then
            e = e % lst[i+1]
        end
    end
    return e
end

function raise(lst)
    if #lst == 1 then
        return lst[1]
    end
    return lst[1]^lst[2]
end

g = lpeg.P({"expression",
    primary = num + Lparen * lpeg.V("expression") * Rparen,
    power = lpeg.Ct( lpeg.V("primary") * (opE * lpeg.V("primary"))^-1 ) / raise,
    term = lpeg.Ct( lpeg.V("power") * (opM * lpeg.V("power"))^0 ) / mult,
    expression = lpeg.Ct( lpeg.V("term") * (opA * lpeg.V("term"))^0 )  / add   
})
g = g * -1