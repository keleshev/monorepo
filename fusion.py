from typing import NamedTuple
from dataclasses import dataclass

# type t =
#   | Unit
#   | Boolean of bool
#   | Number of int
#   | Id of string
#   | Divide of t * t
#   | Sequence of t * t
#   | Let of {id: string; value: t; body: t}
#   | If of {conditional: t; consequence: t; alternative: t}

class AST:
    pass


@dataclass
class Boolean(AST):
    value: bool

    def visit(self, visitor):
        return visitor.visitBoolean(self)


@dataclass
class Number(AST):
    value: int

    def visit(self, visitor):
        return visitor.visitNumber(self)

@dataclass
class Sequence(AST):
    left: AST
    right: AST

    def visit(self, visitor):
        return visitor.visitSequence(self)
'''

class Sequence implements AST {
  constructor(public left: AST, public right: AST) {}

  visit(visitor: Visitor) {
    return visitor.visitSequence(self);
  }

  equals(other: AST) {…}
}

'''


class Visitor:

    def visitBoolean(self, node):
        return Boolean(node.value)

    def visitNumber(self, node):
        return Number(node.value)

    def visitSequence(self, node):
        return Sequence(node.left.visit(self), node.right.visit(self))


class SwapVisitor(Visitor):

    def visitSequence(self, node):
        return Sequence(node.right.visit(self), node.left.visit(self))


class IncrementVisitor(Visitor):

    def visitNumber(self, node):
        return Number(node.value + 1)


class SwapIncrementVisitor(SwapVisitor, IncrementVisitor):
    pass


assert Boolean(True).value == True
assert Number(42).value == 42

node = Sequence(Number(42), Boolean(True))
assert node.left == Number(42)

assert node.visit(SwapVisitor()) == Sequence(Boolean(True), Number(42))
assert node.visit(SwapVisitor()) == Sequence(Boolean(True), Number(42))
print(node.visit(IncrementVisitor()))
assert node.visit(IncrementVisitor()) == Sequence(Number(43), Boolean(True))


assert node.visit(SwapIncrementVisitor()) == Sequence(Boolean(True), Number(43))
