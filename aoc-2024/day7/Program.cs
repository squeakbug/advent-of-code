using System.Collections.Generic;

sealed class Program
{
    struct Statement {
        public long result;
        public List<long> operands;
    };

    private static long Sum(long a, long b) => a + b;
    private static long Multiply(long a, long b) => a * b;
    private static long Concat(long b, long a) {
        long trailing_zeros = 0;
        long reversed_a = 0;
        while (a % 10 == 0 && a > 0) { 
            trailing_zeros += 1;
            a /= 10;
        }
        while (a > 0) {
            reversed_a = reversed_a * 10 + a % 10;
            a /= 10;
        }

        long result = b;
        while (reversed_a > 0) {
            result = result * 10 + reversed_a % 10;
            reversed_a /= 10;
        }
        for (long i = 0; i < trailing_zeros; ++i) {
            result *= 10;
        }
        return result;
    }

    public delegate long Op(long left, long right);
    private static readonly List<Op> _part1Ops = [ Sum, Multiply ];
    private static readonly List<Op> _part2Ops = [ Sum, Multiply, Concat ];

    private static Statement ParseStatement(string content) {
        var parts = content
            .Split(':').Select(p => p.Trim()).ToArray();
        
        return new Statement
        {
            result = long.Parse(parts[0]),
            operands = parts.Length > 1 
                ? parts[1].Split(' ').Select(long.Parse).ToList() 
                : []
        };
    }

    private static long PermutateOps(
        List<Op> opers,
        long acc, 
        long req, 
        List<long> ops
    ) {
        if (ops.Count == 0) {
            if (acc == req) return acc; else return 0;
        } else {
            return opers.Max(op =>
            {
                return PermutateOps(opers, op(acc, ops[0]), req, [.. ops.Skip(1)]);
            });
        }
    }

    private static long Part1(List<Statement> statements) {
        return statements
            .Select(s => PermutateOps(_part1Ops, 0, s.result, s.operands))
            .Sum();
    }

    private static long Part2(List<Statement> statements) {
        return statements
            .Select(s => PermutateOps(_part2Ops, 0, s.result, s.operands))
            .Sum();
    }

    public static int Main(string[] args)
    {
        if (args.Length < 1)
        {
            Console.WriteLine("Usage: app <path_to_file>");
            return -1;
        }

        try
        {
            string pathToInput = args[0];
            var statements = File
                .ReadLines(pathToInput)
                .Select(p => ParseStatement(p))
                .ToList();

            long part1_sln = Part1(statements);
            Console.WriteLine($"part1_sln = {part1_sln}");

            long part2_sln = Part2(statements);
            Console.WriteLine($"part2_sln = {part2_sln}");
        }
        catch (System.Exception ex)
        {
            Console.WriteLine(ex.Message);
            return -1;
        }

        return 0;
    }
}