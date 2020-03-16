
namespace Crowbar.Lang

module AbstractSyntaxTree =
    type BiExpKind = 
        | Add           // operator: +
        | Subtract      // operator: -
        | Multiply      // operator: *
        | Divide        // operator: /
        | Mod           // operator: %
        | Greater       // operator: >
        | GreaterEqual  // operator: >=
        | Less          // operator: <
        | LessEqual     // operator: <=
        | Equal         // operator: ==
        | NotEqual      // operator: !=
        | And           // operator: &&
        | Or            // operator: ||

    type Expression =
        | IntLiteral of int
        | DoubleLiteral of double
        | BoolLiteral of bool
        | StringLiteral of string
        | VarRefExp of VarRefExp
        | FuncInvokeExp of FuncInvokeExp
        | BinaryExp of BinaryExp
    and VarRefExp = {name: string}
    and FuncInvokeExp = {name: string; args: Expression list}
    and BinaryExp = {kind: BiExpKind; left: Expression; right: Expression}

    type Statement =
        | RawExpStmt of RawExpStmt
        | GlobalRefStmt of GlobalRefStmt
        | AssignStmt of AssignStmt
        | ReturnStmt of ReturnStmt
        | IfElseStmt of IfElseStmt
        | WhileStmt of WhileStmt
        | ForStmt of ForStmt
        | BreakStmt
        | ContinueStmt
        | FuncDefStmt of FuncDefStmt
    and RawExpStmt = {expression: Expression}
    and GlobalRefStmt = {names: string list}
    and AssignStmt = {name: string; value: Expression}
    and ReturnStmt = {exp: Expression option}
    and IfElseStmt = {condition: Expression; trueBody: Statement list; falseBody: Statement list}
    and WhileStmt = {condition: Expression; body: Statement list}
    and ForStmt = {init: Statement; inc: Statement; condition: Expression; body: Statement list}
    and FuncDefStmt = {name: string; args: string list; body: Statement list}

    type ProgramAST = Statement list