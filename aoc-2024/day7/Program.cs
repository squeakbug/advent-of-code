using System.Collections.Generic;

sealed class Program
{
    struct Statement {
        public long result;
        public List<long> operands;
    };

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

    private static long PermutateOps(long acc, long req, List<long> ops) {
        if (ops.Count == 0) {
            if (acc == req) return acc; else return 0;
        } else {
            long resAdd = PermutateOps(acc + ops[0], req, [.. ops.Skip(1)]);
            long resMul = PermutateOps(acc * ops[0], req, [.. ops.Skip(1)]);
            return Math.Max(resAdd, resMul);
        }
    }

    private static long Part1(List<Statement> statements) {
        return statements
            .Select(s => PermutateOps(0, s.result, s.operands))
            .Sum();
    }

    private static int Part2(List<Statement> statements) {
        return 0;
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