local BAPL = require"language"
local tests = require"tests"
local n1, n2, options = 0, 0, {}
io.write("BAPL (Basically Another Programming Language) Martin Friesen 2023-10-20\n")

io.write("\nTests after functions were introduced.\n")
options.supress_prnt_wrap = false
options.trace_ast = false
options.trace_code = false
options.trace_interp = false
n1, n2 = 2 , 99
BAPL.pl(take(tests.main_cases, n1, n2), options)

io.write("\nMandelbrot using for loops and supressing print wrap.\n")
options.supress_prnt_wrap = true
options.trace_ast = false
options.trace_code = false
options.trace_interp = false
n1, n2 = 1 , 1
BAPL.pl(take(tests.main_cases, n1, n2), options)

io.write("\nTests after statements were introduced.\n")
options.supress_prnt_wrap = false
options.trace_ast = false
options.trace_code = false
options.trace_interp = false
n1, n2 = 1 , 99
BAPL.pl(take(tests.prefunction_cases, n1, n2), options)

io.write("\nTests when just expressions were available.\n")
options.trace_ast = false
options.trace_code = false
options.trace_interp = false
n1, n2 = 1 , 99
BAPL.pl(take(tests.primordial_cases, n1, n2), options)

