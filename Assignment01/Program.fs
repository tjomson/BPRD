(* Programming language concepts for software developers, 2010-08-28 *)

(* Evaluating simple expressions with variables *)
module Intro2
open System

(* Association lists map object language variables to their values *)

let env = [("a", 3); ("c", 78); ("baf", 666); ("b", 111)];;

let emptyenv = []; (* the empty environment *)

let rec lookup env x =
    match env with 
    | []        -> failwith (x + " not found")
    | (y, v)::r -> if x=y then v else lookup r x;;

let cvalue = lookup env "c";;


(* Object language expressions with variables *)

type expr = 
  | CstI of int
  | Var of string
  | Prim of string * expr * expr
  | If of expr * expr * expr

let e1 = CstI 17;;

let e2 = Prim("+", CstI 3, Var "a");;

let e3 = Prim("+", Prim("*", Var "b", CstI 9), Var "a");;


(* Evaluation within an environment *)
// Exercise 1.1
let rec eval e (env : (string * int) list) : int =
    match e with
    | CstI i            -> i
    | Var x             -> lookup env x 
    | If (a, b, c) -> 
        let x = eval a env
        match x with
        | 0 -> eval c env
        | 1 -> eval b env
        | _ -> failwith "not a boolean expression"
    | Prim("+", e1, e2) -> eval e1 env + eval e2 env
    | Prim("*", e1, e2) -> eval e1 env * eval e2 env
    | Prim("-", e1, e2) -> eval e1 env - eval e2 env
    | Prim("max", e1, e2) -> max (eval e1 env) (eval e2 env)
    | Prim("min", e1, e2) -> min (eval e1 env) (eval e2 env)
    | Prim("==", e1, e2) -> if eval e1 env = eval e2 env
                                then 1
                                else 0
    | Prim _            -> failwith "unknown primitive";;

let rec eval2 e (env : (string * int) list) : int =
    match e with 
    | CstI i -> i
    | Var x -> lookup env x 
    | If (e1, e2, e3) -> 
        match eval e1 env with
        | 0 -> eval e3 env
        | _ -> eval e2 env
    | Prim (ope, e1, e2) ->
        let i1 = eval2 e1 env
        let i2 = eval2 e2 env
        match ope with
        | "+" -> i1 + i2
        | "*" -> i1 * i2
        | "-" -> i1 - i2
        | "max" -> max i1 i2
        | "min" -> min i1 i2
        | "==" -> if i1 = i2 then 1 else 0
        | _ -> failwith "unknown primitive"

let e4 = Prim("max", CstI 100, CstI 50)
let e5 = Prim("min", CstI 100, CstI 50)
let e6 = Prim("==", CstI 100, CstI 100)
let e7 = If (CstI 1, CstI 4, CstI 10)



let e1v  = eval e1 env;;
let e2v1 = eval e2 env;;
let e2v2 = eval e2 [("a", 314)];;
let e3v  = eval e3 env;;
let e4v  = eval e4 env;;
let e5v  = eval e5 env;;
let e6v  = eval e6 env;;
let e7v  = eval e7 env;;

// Exercise 1.2
type aexpr = 
    | CstI of int
    | Var of string
    | Add of aexpr * aexpr
    | Mul of aexpr * aexpr
    | Sub of aexpr * aexpr

let e8 = Sub(Var "v", Add(Var "w", Var "z"))
let e9 = Mul (CstI 2, Sub (Var "v", Add (Var "w", Var "z")))
let e10 = Add (Var "x", Add (Var "y", Add (Var "z", Var "v")))

let rec fmt aex : string =
    match aex with
    | CstI a -> string a
    | Var a -> a
    | Add (a, b) -> "(" + (fmt a) + " + " + (fmt b) + ")"
    | Mul (a, b) -> "(" + (fmt a) + " * " + (fmt b) + ")"
    | Sub (a, b) -> "(" + (fmt a) + " - " + (fmt b) + ")"

let isZero aexp =
    match aexp with 
    | CstI a -> a = 0
    | _ -> false

let isOne aexp =
    match aexp with 
    | CstI a -> a = 1
    | _ -> false

let areEqual a b =
    match a with
    | CstI x -> match b with
                    | CstI y -> x = y
                    | _ -> false
    | _ -> false

let rec simplify aexp =
    match aexp with 
    | CstI a -> CstI a
    | Var a -> Var a
    | Add (CstI 0, b) -> simplify b
    | Add (a, CstI 0) -> simplify a
    | Add (a, b) -> Add (simplify a, simplify b)
    | Sub (a, CstI 0) -> simplify a
    | Sub (CstI a, CstI b) when a = b -> CstI 0
    | Sub (a, b) -> Sub (simplify a, simplify b)
    | Mul (CstI 0, _) -> CstI 0
    | Mul (_, CstI 0) -> CstI 0
    | Mul (CstI 1, b) -> simplify b
    | Mul (a, CstI 1) -> a
    | Mul (a, b) -> Mul (simplify a, simplify b)

let test = Add (CstI 0, Sub (CstI 4, Sub(CstI 10, CstI 10)))

let rec diff aexp var =
    match aexp with
    | CstI _ -> CstI 0
    | Var s when s = var -> CstI 1
    | Var _ -> CstI 0
    | Add (a, b) -> Add (diff a var, diff b var)
    | Sub (a, b) -> Sub (diff a var, diff b var)
    | Mul (a, b) -> Mul (diff a var, diff b var)

let diffTest = Add(CstI 4, Mul (CstI 3, Var ("x")))


