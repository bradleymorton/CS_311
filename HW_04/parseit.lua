-- parseit.lua by Bradley Morton
-- adapted from code provided by Dr. Chappell
-- for CS331 spring 2020

local parseit = {}
lexit = require "lexit"

-- Symbolic Constants for AST
local STMT_LIST   = 1
local PRINT_STMT  = 2
local FUNCT_DEF   = 3
local FUNC_CALL   = 4
local IF_STMT     = 5
local WHILE_STMT  = 6
local RETURN_STMT = 7
local ASSN_STMT   = 8
local STRLIT_OUT  = 9
local CHAR_CALL   = 10
local BIN_OP      = 11
local UN_OP       = 12
local NUMLIT_VAL  = 13
local BOOLLIT_VAL = 14
local INPUT_CALL  = 15
local SIMPLE_VAR  = 16
local ARRAY_VAR   = 17

-- Variables

-- For iterating through the lexer
local iter
local state
local lexer_out_s
local lexer_out_c

-- For current lexeme
local lexstr = ""
local lexcat = 0

-- Local statements for parsing functions
local parse_stmt_list
local parse_statement
local parse_print_arg
local parse_expr
local parse_comp_expr
local parse_arith_exp
local parse_term
local parse_factor

-- Utility Functions provided by Dr. Chappell

-- advance
-- Go to the next lexeme and load it into lexstr, lexcat
-- Should be called once before any parsing is done.
-- Function init must be called before this function is called.
local function advance()
  lexer_out_s, lexer_out_c  = iter(state, lexer_out_s)
  -- If we're not past the end, copy current lexeme into variables
  if lexer_out_s ~= nil then
    lexstr, lexcat = lexer_out_s, lexer_out_c 
  else
    lexstr, lexcat = "", 0
  end
end

-- init
-- Initial call. Sets the input for parsing functions.
local function init(prog)
  iter = lexit.lex(prog)
  advance()
end

-- atEnd
-- Return true if position has reached end of input.
-- Function init must be called before this function is called.
local function atEnd()
  return lexcat == 0
end

-- matchString
-- Given string, see if current lexeme string form is equal to it. If
-- so, then advance to next lexeme & return true. If not, then do not
-- advance, return false.
-- Function init must be called before this function is called.
local function matchString(string)
  if lexstr == string then
    advance()
    return true
  else
    return false
  end
end

-- matchCat
-- Given lexeme category (integer), see if current lexeme category is
-- equal to it. If so, then advance to next lexeme & return true. If
-- not, then do not advance, return false.
-- Function init must be called before this function is called.
local function matchCat(c)
  if lexcat == c then
    advance()
    return true
  else
    return false
  end
end

-- parse
-- takes program, returns two booleans and the AST
-- AST is valid if both booleans are true
function parseit.parse(prog)
  init(prog)
  local good, ast = parse_program()
  local done = atEnd()
  return good, done, ast
end

-- parse_program
-- Parsing function for nonterminal "program"
-- Function init must be called before this function is called.
function parse_program()
  local good, ast 
  good, ast = parse_stmt_list()
  return good, ast
end

-- parse_stmt_list
-- Parsing function for nonterminal "stmt_list"
-- Function init must be called before this function is called.
function parse_stmt_list()
  local good, ast, newast
  ast = { STMT_LIST }
  
  while true do
    if lexstr ~= "print"
      and lexstr ~= "func"
      and lexstr ~= "if"
      and lexstr ~= "while"
      and lexstr ~= "return"
      and lexcat ~= lexit.ID then
        return true, ast
    end
    good, newast = parse_statement()
    if not good then
      return false, nil
    end
    table.insert(ast, newast)
  end
end

-- parse_statement
-- Parsing function for nonterminal "statement"
-- Function init must be called before this function is called.
function parse_statement()
  local good, ast1, ast2, ast3, savelex
  if matchString("print") then
    if not matchString("(") then
        return false, nil
    end
    if matchString(")") then
        return true, { PRINT_STMT }
    end
    good, ast1 = parse_print_arg()
    if not good then
        return false, nil
    end
    ast2 = { PRINT_STMT, ast1 }
    while matchString(",") do
        good, ast1 = parse_print_arg()
        if not good then
            return false, nil
        end
        table.insert(ast2, ast1)
    end
    if not matchString(")") then
        return false, nil
    end
    return true, ast2
  elseif matchString("func") then
    savelex = lexstr
    if matchCat(lexit.ID) then
      if matchString("(") then
        if matchString(")") then
            good, ast1 = parse_stmt_list()
        end
        if not good then
            return false, nil
        end
      ast2 = { FUNCT_DEF, savelex, ast1 }
    else
        return false, nil
    end
      if matchString("end") then
        return true, ast2
      end
      return false, nil
    end
  elseif matchString("if") then
    good, ast1 = parse_expr()
    if not good then
      return false, nil
    end
    good, ast2 = parse_stmt_list()
    if not good then
      return false, nil
    end
    ast3 = {IF_STMT, ast1, ast2}
    while matchString("elif") do
      good, ast1 = parse_expr()
      if not good then
        return false, nil
      end
      good, ast2 = parse_stmt_list()
      if not good then
        return false, nil
      end
      table.insert(ast3, ast1)
      table.insert(ast3, ast2)
    end
    if matchString("else") then
      good, ast1 = parse_stmt_list()
      if not good then
        return false, nil
      end
      table.insert(ast3, ast1)
    end
    if matchString("end") then
      return true, ast3
    end 
  elseif matchString("while") then
    good, ast1 = parse_expr()
    if not good then
      return false, nil
    end
    good, ast2 = parse_stmt_list()
    if not good then
      return false, nil
    end
    ast3 = {WHILE_STMT, ast1, ast2}
    if matchString("end") then
      return true, ast3
    end   
  elseif matchString("return") then
    good, ast1 = parse_expr()
    if not good then
      return false, nil
    end
    return true, {RETURN_STMT, ast1}
  else
    savelex = lexstr
    if matchCat(lexit.ID) then
        if matchString("(") then
            if matchString(")") then
                return true, {FUNC_CALL, savelex}
            end
        elseif matchString("=") then
            good, ast2 = parse_expr()
            if not good then
                return false, nil
            end
        return true, {ASSN_STMT, {SIMPLE_VAR, savelex}, ast2}
        elseif matchString("[") then
            good, ast2 = parse_expr()
            if not good then
                return false, nil
            end
            if matchString("]") then
                if matchString ("=") then
                    good, ast3 = parse_expr()
                    if not good then
                        return false, nil
                    end
                end
            return true, {ASSN_STMT, {ARRAY_VAR, savelex, ast2}, ast3}
            end
        end
    end
    return false, nil
  end
  
  return false, nil
end

-- parse_print_arg
-- Parsing function for nonterminal "print_arg"
-- Function init must be called before this function is called.
function parse_print_arg()
  local good, ast, savelex
  savelex = lexstr
  if matchCat(lexit.STRLIT) then
    ast = {STRLIT_OUT, savelex }
  elseif matchString("char") then
    if matchString("(") then
        good, ast = parse_expr()
        if not good then
          return false, nil
        end
        if not matchString(")") then
          return false, nil
        end
        return true, {CHAR_CALL, ast}
    end
  else
    good, ast = parse_expr()
    if not good then
      return false, nil
    end
  end
  return true, ast
end

-- parse_expr
-- Parsing function for nonterminal "expr"
-- Function init must be called before this function is called.
function parse_expr()
  local good, ast1, ast2, savelex
  good, ast1 = parse_comp_expr()
  if not good then
    return false, nil
  end
  while true do
    savelex = lexstr
    if not matchString("and") and not matchString("or") then
         break
    end
    good, ast2 = parse_comp_expr()
    if not good then
      return false, nil
    end   
    ast1 = { {BIN_OP, savelex}, ast1, ast2}
  end 
  return true, ast1
end

-- parse_comp_expr
-- Parsing function for nonterminal "comp_expr"
-- Function init must be called before this function is called.
function parse_comp_expr()
  local good, ast1, ast2, savelex
  good, ast1 = parse_arith_expr()
  if not good then
    return false, nil
  end
  while true do
    savelex = lexstr
    if not matchString("==") and not matchString("!=") and not matchString("<") and not matchString("<=") and not matchString(">") and not matchString(">=") then
         break
    end
    good, ast2 = parse_arith_expr()
    if not good then
      return false, nil
    end
    ast1 = { {BIN_OP, savelex}, ast1, ast2}
  end
  return true, ast1
end

-- parse_arith_expr
-- Parsing function for nonterminal "arith_expr"
-- Function init must be called before this function is called.
function parse_arith_expr()
  local good, ast1, ast2, savelex
  good, ast1 = parse_term()
  if not good then
    return false, nil
  end
  while true do
    savelex = lexstr
    if not matchString("+") and not matchString("-") then
         break
    end
    good, ast2 = parse_term()
    if not good then
      return false, nil
    end
    ast1 = { {BIN_OP, savelex}, ast1, ast2}
  end
  return true, ast1
end

-- parse_term
-- Parsing function for nonterminal "term"
-- Function init must be called before this function is called.
function parse_term()
  local good, ast1, ast2, savelex
  good, ast1 = parse_factor()
  if not good then
    return false, nil
  end
  while true do
    savelex = lexstr
    if not matchString("*") and not matchString("/") and not matchString("%") then
         break
    end
    good, ast2 = parse_factor()
    if not good then
      return false, nil
    end
    ast1 = { {BIN_OP, savelex}, ast1, ast2}
  end
  return true, ast1  
end

-- parse_factor
-- Parsing function for nonterminal "factor"
-- Function init must be called before this function is called.
function parse_factor()
  local good, ast1, ast2, savelex
  savelex = lexstr
  if matchString("(") then
    good, ast1 = parse_expr()
    if not good  or not matchString(")") then
      return false, nil
    end
    return true, ast1
  elseif matchString("+") or matchString("-") or matchString("not") then
           good, ast1 = parse_factor()
           if not good then
             return false, nil
           end
           ast2 = { {UN_OP, savelex}, ast1}
           return true, ast2
  elseif matchCat(lexit.NUMLIT) then
    ast1 = { NUMLIT_VAL, savelex }
    return true, ast1
  elseif matchString("true") or matchString("false") then
    ast1 = { BOOLLIT_VAL, savelex }
    return true, ast1
  elseif matchString("input") then
    if matchString("(") then
        if matchString(")")then
            return true, {INPUT_CALL}
        end
    end
  elseif matchCat(lexit.ID) then
    if matchString("(") then
        if matchString(")") then
            return true, {FUNC_CALL, savelex}
        end
    elseif matchString("[") then
        good, ast2 = parse_expr()
        if not good then
            return false, nil
        end
        if matchString("]") then
           return true, {ARRAY_VAR, savelex, ast2}
        end
    else
        return true, {SIMPLE_VAR, savelex}
    end
end
  return false, nil
end

return parseit