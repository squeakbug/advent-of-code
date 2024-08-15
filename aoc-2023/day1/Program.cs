sealed class Program
{
    private static int Part1(string pathToInput)
    {
        var totalSum = 0;
        foreach (string line in File.ReadLines(pathToInput))
        {
            var lst = line.ToList();
            var indx1 = lst.FindIndex(0, (char c) => { return Char.IsDigit(c); });
            var indx2 = lst.FindLastIndex(line.Count() - 1, (char c) => { return Char.IsDigit(c); });
            if (indx2 == -1)
                indx2 = indx1;
            totalSum += (int)Char.GetNumericValue(lst[indx1]) * 10
                + (int)Char.GetNumericValue(lst[indx2]);
        }
        return totalSum;
    }

    private static int Part2(string pathToInput)
    {
        var replacements = new (string, int)[] {
            ("one",   1),
            ("two",   2),
            ("three", 3),
            ("four",  4),
            ("five",  5),
            ("six",   6),
            ("seven", 7),
            ("eight", 8),
            ("nine",  9),
        };

        var totalSum = 0;
        foreach (string line in File.ReadLines(pathToInput))
        {
            var lst = line.ToList();

            var indx1 = lst.FindIndex(0, (char c) => { return Char.IsDigit(c); });
            int val1 = 0;
            if (indx1 != -1)
                val1 = (int)Char.GetNumericValue(line[indx1]);

            int minIndx1 = -1;
            int repIndx1 = -1, i = 0;
            foreach (var rep in replacements)
            {
                int indx = line.IndexOf(rep.Item1);
                if ((indx != -1 && indx < minIndx1) || minIndx1 == -1)
                {
                    minIndx1 = indx;
                    repIndx1 = i;
                }
                i++;
            }
            if (minIndx1 != -1 && (minIndx1 < indx1 || indx1 == -1)) {
                val1 = replacements[repIndx1].Item2;
            }

            var indx2 = lst.FindLastIndex(line.Count() - 1, (char c) => { return Char.IsDigit(c); });
            int val2 = 0;
            if (indx2 != -1)
                val2 = (int)Char.GetNumericValue(line[indx2]);

            int maxIndx2 = -1;
            int repIndx2 = -1;
            i = 0;
            foreach (var rep in replacements)
            {
                int indx = line.LastIndexOf(rep.Item1);
                if ((indx != -1 && indx > maxIndx2) || maxIndx2 == -1)
                {
                    maxIndx2 = indx;
                    repIndx2 = i;
                }
                i++;
            }
            if (maxIndx2 != -1 && (maxIndx2 > indx2 || indx2 == -1)) {
                indx2 = maxIndx2;
                val2 = replacements[repIndx2].Item2;
            }

            if (indx2 == -1)
                val2 = val1;

            totalSum += val1 * 10 + val2;
        }

        return totalSum;
    }

    public static int Main(string[] args)
    {

        if (args.Count() < 1)
        {
            Console.WriteLine("Usage: app <path_to_file>");
            return -1;
        }

        string pathToInput = args[0];

        try
        {
            int part1_sln = Part1(pathToInput);
            Console.WriteLine($"part1_sln = {part1_sln}");

            int part2_sln = Part2(pathToInput);
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
