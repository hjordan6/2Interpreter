#
# Class Interpreter 3
# With numbers, plus, minus, if0, with, variable references,
# user-defined functions (lambda), and function calls.
#

module ExtInt

push!(LOAD_PATH, pwd())

using Error
using Lexer
export parse, calc, interp

#
# ==================================================
#

abstract type AE
end

abstract type Bind
end

function divide(l::Real, r::Real)
    if r == 0
        throw(LispError("You can't divide by zero..."))
    end
    return l / r
end

function collatz( n::Real )
  return collatz_helper( n, 0 )
end

function collatz_helper( n::Real, num_iters::Int )
  if n == 1
    return num_iters
  end
  if mod(n,2)==0
    return collatz_helper( n/2, num_iters+1 )
  else
    return collatz_helper( 3*n+1, num_iters+1 )
  end
end

# <AE> ::= <number>
struct NumNode <: AE
    n::Real
end

struct BinopNode <: AE
    op::Function
    lhs::AE
    rhs::AE
end

struct UnaryNode <: AE
    op::Function
    elm::AE
end

# <AE> ::= (if0 <AE> <AE> <AE>)
struct If0Node <: AE
    cond::AE
    zerobranch::AE
    nzerobranch::AE
end

struct Binding <: Bind
    sym::Symbol
    binding_expr::AE
end

# <AE> ::= (with <Bind> <AE>)
struct WithNode <: AE
    binding::Array{Binding}
    body::AE
end

# <AE> ::= <id>
struct VarRefNode <: AE
    sym::Symbol
end

# <AE> ::= (lambda <id> <AE>)
struct FuncDefNode <: AE
    formal::Symbol
    body::AE
end

# <AE> ::= (<AE> <AE>)
struct FuncAppNode <: AE
    fun_expr::AE
    arg_expr::AE
end

#
# ==================================================
#

abstract type RetVal
end

abstract type Environment
end

struct NumVal <: RetVal
    n::Real
end

struct ClosureVal <: RetVal
    formal::Symbol
    body::AE
    env::Environment
end

#
# ==================================================
#

struct EmptyEnv <: Environment
end

struct ExtendedEnv <: Environment
    # vars::Dict{Symbol, RetVal}
    sym::Symbol
    val::RetVal
    parent::Environment
end

#
# ==================================================
#

funcs = Dict([(:+, +),(:-, -),(:*, *),(:/, divide),(:mod, mod),(:collatz, collatz)])

#
# ==================================================
#

function parse( expr::Number )
    return NumNode( expr )
end

function parse( expr::Symbol )
    return VarRefNode( expr )
end

function parse( expr::Array{Any} )

    if haskey(funcs, expr[1])

        if length(expr) == 2 && (expr[1] == :- || expr[1] == :collatz)
            return UnaryNode( funcs[expr[1]], parse(expr[2]) )

        elseif length(expr) == 3 && expr[1] != :collatz
            return BinopNode( funcs[expr[1]], parse( expr[2] ), parse( expr[3] ) )

        else
            throw(LispError("Improper number of operandsxs"))
        end

    elseif expr[1] == :if0
        return If0Node( parse(expr[2]), parse(expr[3]) , parse(expr[4]) )

    elseif expr[1] == :with
        return WithNode( parseBinding(expr[2]), parse(expr[3]) )

    elseif expr[1] == :lambda
        return FuncDefNode( expr[2], parse(expr[3]) )

    else
        return FuncAppNode( parse(expr[1]), parse(expr[2]) )

    end

    throw(LispError("Unknown operator!"))
end

function parse( expr::Any )
  throw( LispError("Invalid type $expr") )
end

function parseBinding( expr::Array{Any} )
    if typeof(expr[1]) == Symbol
        return Binding( expr[1], parse( expr[2] ) )

    else
        toReturn = []
        for i = 1:length(expr)
            append!(toReturn, [parseBinding(expr[i])])
        end
        return toReturn
    end
end

#
# ==================================================
#

function calc( ast::NumNode, env::Environment )
    return NumVal( ast.n )
end

function calc( ast::BinopNode, env::Environment )
    lhs = calc( ast.lhs, env )
    rhs = calc( ast.rhs, env )
    return NumVal( ast.op( lhs.n, rhs.n ) )
end

function calc( ast::UnaryNode, env::Environment )
    elm = calc( ast.elm, env )
    return NumVal( ast.op( elm.n ) )
end

function calc( ast::If0Node, env::Environment )
    cond = calc( ast.cond, env )
    if cond.n == 0
        return calc( ast.zerobranch, env )
    else
        return calc( ast.nzerobranch, env )
    end
end

function calc( ast::WithNode, env::Environment )
    binding_val = calc( ast.binding[1].binding_expr, env )
    ext_env = ExtendedEnv( ast.binding[1].sym, binding_val, env )
    return calc( ast.body, ext_env )
end

function calc( ast::VarRefNode, env::EmptyEnv )
    throw( Error.LispError("Undefined variable " * string( ast.sym )) )
end

function calc( ast::VarRefNode, env::ExtendedEnv )
    if ast.sym == env.sym
        return env.val
    else
        return calc( ast, env.parent )
    end
end

function calc( ast::FuncDefNode, env::Environment )
    return ClosureVal( ast.formal, ast.body , env )
end

function calc( ast::FuncAppNode, env::Environment )
    closure_val = calc( ast.fun_expr, env )
    actual_parameter = calc( ast.arg_expr, env )
    ext_env = ExtendedEnv( closure_val.formal,
                           actual_parameter,
                           closure_val.env )
    return calc( closure_val.body, ext_env )
end

function calc( ast::AE )
    return calc( ast, EmptyEnv() )
end

#
# ==================================================
#

function interp( cs::AbstractString )
    lxd = Lexer.lex( cs )
    ast = parse( lxd )
    return calc( ast, EmptyEnv() )
end

# evaluate a series of tests in a file
function interpf( fn::AbstractString )
  f = open( fn )

  cur_prog = ""
  for ln in eachline(f)
      ln = chomp( ln )
      if length(ln) == 0 && length(cur_prog) > 0
          println( "" )
          println( "--------- Evaluating ----------" )
          println( cur_prog )
          println( "---------- Returned -----------" )
          try
              println( interp( cur_prog ) )
          catch errobj
              println( ">> ERROR: lxd" )
              lxd = Lexer.lex( cur_prog )
              println( lxd )
              println( ">> ERROR: ast" )
              ast = parse( lxd )
              println( ast )
              println( ">> ERROR: rethrowing error" )
              throw( errobj )
          end
          println( "------------ done -------------" )
          println( "" )
          cur_prog = ""
      else
          cur_prog *= ln
      end
  end

  close( f )
end

end #module
