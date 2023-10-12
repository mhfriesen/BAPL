local function permutation(a, n)
	if n == 0 then
    print('{'..table.concat(a, ', ')..'}')
	else
		for i = 1, n do
			a[i], a[n] = a[n], a[i]
			permutation(a, n - 1)
			a[i], a[n] = a[n], a[i]
		end
	end
end

permutation({1,2,3}, 3)