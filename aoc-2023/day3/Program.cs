sealed class Program
{
    private static void ParseFile(string pathToInput, out List<(int, int, int, int)> numbers, out List<(char, int, int)> symbols)
    {
        numbers = new List<(int, int, int, int)>();
        symbols = new List<(char, int, int)>();
        int lineCnt = 0;
        foreach (var line in File.ReadLines(pathToInput))
        {
            int numberLen = 0;
            int curNumber = 0;
            bool firstDot = true;
            for (int i = 0; i < line.Length; i++)
            {
                bool isDigit = Char.IsDigit(line[i]);
                if (isDigit)
                {
                    curNumber = curNumber * 10 + (int)Char.GetNumericValue(line[i]);
                    numberLen++;
                    firstDot = true;
                }
                else
                {
                    if (line[i] != '.')
                    {
                        symbols.Add((line[i], lineCnt, i));
                        if (curNumber != 0)
                        {
                            numbers.Add((curNumber, lineCnt, i - numberLen, numberLen));
                            curNumber = 0;
                            numberLen = 0;
                        }
                        firstDot = false;
                    }
                    else if (firstDot)
                    {
                        if (i != 0)
                        {
                            numbers.Add((curNumber, lineCnt, i - numberLen, numberLen));
                            curNumber = 0;
                            numberLen = 0;
                        }
                        firstDot = false;
                    }
                }

                if (i == line.Length - 1 && isDigit) {
                    numbers.Add((curNumber, lineCnt, i - numberLen + 1, numberLen));
                    curNumber = 0;
                    numberLen = 0;
                }
            }
            lineCnt++;
        }
    }

    private static int Part1(string pathToInput)
    {
        int totalSum = 0;
        
        ParseFile(pathToInput, out List<(int, int, int, int)> numbers, out List<(char, int, int)> symbols);

        foreach (var (nval, ny, nx, nlen) in numbers)
        {
            foreach (var (sval, sy, sx) in symbols)
            {
                if (sx <= nx + nlen && sx >= nx - 1 && sy <= ny + 1 && sy >= ny - 1)
                {
                    totalSum += nval;
                    break;
                };
            }
        }

        return totalSum;
    }

    private static int Part2(string pathToInput)
    {
        int totalSum = 0;
        
        ParseFile(pathToInput, out List<(int, int, int, int)> numbers, out List<(char, int, int)> symbols);

        foreach (var (sval, sy, sx) in symbols)
        {
            if (sval == '*')
            {
                int presCnt = 0;
                int prod = 1;
                foreach (var (nval, ny, nx, nlen) in numbers)
                {
                    if (sx <= nx + nlen && sx >= nx - 1 && sy <= ny + 1 && sy >= ny - 1)
                    {
                        prod *= nval;
                        presCnt++;
                    };
                }

                if (presCnt == 2)
                {
                    totalSum += prod;
                }
            }
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
