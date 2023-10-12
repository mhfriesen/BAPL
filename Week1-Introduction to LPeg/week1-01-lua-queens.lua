N = 4
isa_soln_printed = false
n_isplaceok_called = 0
n_solutions = 0

function reset_count()
  n_solutions = 0
end

function isplaceok (rows_col, cur_row, col)
  n_isplaceok_called = n_isplaceok_called + 1
  for row = 1, cur_row - 1 do -- for each queen already placed
    local previous_queen_col = rows_col[row]
    if(previous_queen_col == col or                    -- same column
      (previous_queen_col == col - cur_row + row) or   -- same diagonal to left
      (previous_queen_col == col + cur_row - row) ) then -- same diagonal to right
        return false
    end
  end
  return true
end

 function printsolution (rows_col)
  n_solutions = n_solutions + 1
  for row = 1, #rows_col do
   for col = 1, #rows_col do
    io.write(rows_col[row] == col and "X" or "_")
    io.write (" ")
   end
   io.write ("\n")
  end
  io.write( "\n" )
end

function write_rows_col(rows_col)
  for i=1,#rows_col do
    io.write(tostring(rows_col[i]), " ")
  end
end

function addqueen(rows_col, cur_row)
  --io.write(tostring(cur_row), ": ")
  --write_rows_col(rows_col)
  --io.write("\n")
  if cur_row > N then
    printsolution(rows_col)
    isa_soln_printed = true
  else
    for col = 1, N do
      local isok = isplaceok(rows_col, cur_row, col)
      io.write("isok=",tostring(isok)," col=",tostring(col), " cur_row=", tostring(cur_row),"\n")
      if isok then
        rows_col[cur_row] = col
        --if true or not isa_soln_printed then --prevent backtracking on other columns after first soln is found
          addqueen(rows_col, cur_row + 1)
        --end
      end
    end
  end
end

n_isplaceok_called = 0
reset_count()
addqueen({},1)
print("BACKTRACKING")
print("n_solutions", n_solutions)
print("n_isplaceok_called", n_isplaceok_called)
print( )

--[[

 function eq_tabs(t1, t2)
   local equal = true
   for i=1, #t1 do
     if t1[i] ~= t2[i] then
       equal = false
       break
    end
  end
  return equal
end

 function isplaceok2 (rows_col)
   for row1 = 1, #rows_col do
    for row2 = row1 + 1, #rows_col do -- for each queen already placed
      local row1_col = rows_col[row1]
      local row2_col = rows_col[row2]
      --io.write("row1=",row1," row1_col=", row1_col, "row2=", row2, " row2_col=", row2_col, "\n")
      if(
        (row2_col == row1_col - row2 + row1) or   -- same diagonal to left
        (row2_col == row1_col + row2 - row1) ) then -- same diagonal to right
          --io.write("row1=",row1," row1_col=", row1_col, "row2=", row2, " row2_col=", row2_col, "\n")
          return false
      end
    end
  end
  return true
end

function permutation(a, n)
	if n == 0 then
    n_perms = n_perms + 1
    if true or eq_tabs(a,{4,2,1,3,5}) then -- use false for testing a single solution
    -- test if queens placed ok
    --print(table.concat(a,","))
    local ok = isplaceok2(a)
    --print(ok)
    if ok then
      printsolution(a)
    end
    end
	else
		for i = 1, n do
			a[i], a[n] = a[n], a[i]
			permutation(a, n - 1)
			a[i], a[n] = a[n], a[i]
		end
	end
end


n_perms = 0
reset_count()
permutation({1,2,3,4}, 4)
print("PERMUTATIONS")
print("n_solutions=", n_solutions)
print("n_perms", n_perms)
--]]


