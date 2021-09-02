using System;
using System.Collections.Generic;

namespace part1._4
{
    public abstract class Expr
    {
        public abstract int eval (Dictionary<string, int> env);
        public abstract Expr simplify ();
    }

    public class CstI : Expr
    {
        public int value;
        public CstI (int value)
        {
            this.value = value;
        }

        public override int eval(Dictionary<string, int> env)
        {
            return value;
        }

        public override Expr simplify()
        {
            return this;
        }

        public override string ToString()
        {
            return value + "";
        }
    }

    public class Var : Expr
    {
        string value;
        public Var (string value)
        {
            this.value = value;
        }

        public override int eval(Dictionary<string, int> env)
        {
            return env[value];
        }

        public override Expr simplify()
        {
            return this;
        }

        public override string ToString()
        {
            return value;
        }
    }

    public abstract class Binop : Expr
    {
        public Expr firstArg;
        public Expr secondArg;
        public char symbol;
        public Func<int, int, int> func;
        public Binop(Expr firstArg, Expr secondArg)
        {
            this.firstArg = firstArg;
            this.secondArg = secondArg;
            
        }

        public override int eval (Dictionary<string, int> env)
        {
            return func (firstArg.eval(env), secondArg.eval(env));
        }

        public override string ToString()
        {
            return $"{firstArg} {symbol} {secondArg}";
        }
    }

    public class Add : Binop
    {
        public Add(Expr firstArg, Expr secondArg) : base(firstArg, secondArg)
        {
            this.symbol = '+';
            this.func = (a, b) => (a + b);
        }

        public override Expr simplify()
        {
            var s1 = firstArg.simplify();
            var s2 = secondArg.simplify();
            var c1 = s1 as CstI;
            var c2 = s2 as CstI;
            var v1 = s1 as Var;
            var v2 = s2 as Var;

            if (c1 is not null && c1.value == 0)
            {
                return secondArg;
            }
            if (c2 is not null && c2.value == 0)
            {
                return firstArg;
            }

        }
    }

    public class Sub : Binop
    {
        public Sub(Expr firstArg, Expr secondArg) : base(firstArg, secondArg)
        {
            this.symbol = '-';
            this.func = (a, b) => (a - b);
        }
    }

    public class Mult : Binop
    {
        public Mult(Expr firstArg, Expr secondArg) : base(firstArg, secondArg)
        {
            this.symbol = '*';
            this.func = (a, b) => (a * b);
        }
    }

    public class Mod : Binop
    {
        public Mod(Expr firstArg, Expr secondArg) : base(firstArg, secondArg)
        {
            this.symbol = '%';
            this.func = (a, b) => (a % b);
        }
    }

    public class BinOr : Binop
    {
        public BinOr(Expr firstArg, Expr secondArg) : base(firstArg, secondArg)
        {
            this.symbol = '|';
            this.func = (a, b) => (a | b);
        }
    }

    public class BinAnd : Binop
    {
        public BinAnd(Expr firstArg, Expr secondArg) : base(firstArg, secondArg)
        {
            this.symbol = '&';
            this.func = (a, b) => (a & b);
        }
    }

    public class Program
    {
        public static void Main(String[] args)
        {
            var env = new Dictionary<string, int>{
                {"x", 10}
            };
            var test = new Add(new CstI(3), new Var("x"));
            Console.WriteLine(test.eval(env));
        }
    }

}
