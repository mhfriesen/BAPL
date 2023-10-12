```local grammer = lpeg.P{"stmts",
  stmts = stmt * (SC * stmts)^-1 / nodeSeq,
  block = OB * (stmts) * SC^-1 * CB,
  stmt = OB * CB -- empty block
      + block 
      + (ID + keywords) * Assgn * notty / nodeAssgn 
      + ret * notty / nodeReturn 
      + prnt * notty / nodePrint 
      + space, 
  factor = number + OP * notty * CP + var, 
  prefix = lpeg.Ct((opP + opPs)^1  * factor)  / foldPre  
           + factor,        
  zone = lpeg.Ct(prefix * (opZ  * prefix)^0 ) / foldBin  * space,
  suffix = lpeg.Ct(zone * opS^0) / foldSuf * space,
  expo = lpeg.Ct(suffix * (opE * suffix)^0) / foldRight  * space,
  term = lpeg.Ct(expo * (opM * expo)^0 ) / foldBin  * space,
  expr = lpeg.Ct(term * (opA  * term)^0 ) / foldBin  * space,
  relation = lpeg.Ct(expr * (opR  * expr)^0) / foldBin  * space,
  notty =  lpeg.Ct((lpeg.C"not" * spaces)^1 *  relation) / foldPre 
          + relation
}
grammer = space * grammer * -1```

