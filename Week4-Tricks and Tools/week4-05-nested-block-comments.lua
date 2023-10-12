local lpeg = require"lpeg"

-- matchtime nested block comments
local level = 0
local function nested_block_comments(s,p) 
  local tag = string.sub(s,p-2,p-1)
  if  tag == "#{" then
    p = p + 1
    level = level + 1
  elseif tag == "}#" then
    level = level -1
    if level == 0 then
      p = p - 2
      return p 
    else
      p = p + 1
    end
  else
    p = p + 1
    if string.len(s) < p then
      return nil 
    end
  end
  return nested_block_comments(s,p)
end

local block_comment = lpeg.P"#{" * nested_block_comments * lpeg.P"}#"
print(block_comment:match(
    [[#{my block comment #{ with a nested block comment # just a comment too
    here }# and rest of comment }# p=106 yadda yadda
    ]])
)

P = lpeg.P
block_comment = P("#{")*((1-P("}#"))^0)*P("}#")
print(block_comment:match(
    [[#{my block comment #{ with a nested block comment # just a comment too
    here }# and rest of comment }# p=106 yadda yadda
    ]])
  )
