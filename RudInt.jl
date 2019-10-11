#
# Class Interpreter 0
# Base interpreter with numbers, plus, and minus
#

module RudInt

push!(LOAD_PATH, pwd())

using Error
using Lexer
export parse, calc, interp

#
# ==================================================
#

abstract type AE
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

function divide(l::Real, r::Real)
    return l / r
end

function collatz( n::Real )
  return collatz_helper( n, 0 )
end

x = Dict([(:+, +),(:-, -),(:*, *),(:/, divide),(:mod, mod),(:collatz, collatz)])

#
# ==================================================
#

function parse( expr::Number )
    return NumNode( expr )
end

function parse( expr::Array{Any} )
    if haskey(x, expr[1])
        if length(expr) == 3 && expr[1] != :collatz
            return BinopNode(x[expr[1]], parse( expr[2] ), parse( expr[3] ) )
        elseif length(expr) == 2 && (expr[1] == :collatz || expr[1] == :-)
            return UnaryNode( x[expr[1]], parse(expr[2]) )
        end
        throw(LispError("Improper number of args!"))
    end
    throw(LispError("Unknown operator!"))
end

function parse( expr::Any )
  throw( LispError("Invalid type $expr") )
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

#
# ==================================================
#

function calc( ast::NumNode )
    return ast.n
end

function calc( ast::BinopNode )
    l = calc( ast.lhs )
    r = calc( ast.rhs )
    op = ast.op
    if r == 0 && (op == divide || op == mod)
        throw(LispError("You can't divide by zero..."))
    end
    return op(l, r)

end

function calc( ast::UnaryNode )
    if calc(ast.elm) < 1 && ast.op == collatz
        throw(LispError("You can't collatz numbers less than 1"))
    end
    return ast.op( calc( ast.elm ) )
end

#
# ==================================================
#

function interp( cs::AbstractString )
    lxd = Lexer.lex( cs )
    ast = parse( lxd )
    return calc( ast )
end

end #module
