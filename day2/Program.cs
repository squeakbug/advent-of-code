sealed class Program
{

    private static (int, int, int) ColorsFromSet(string str)
    {
        int red_cnt = 0, blue_cnt = 0, green_cnt = 0;
        string[] words = str.Split(", ");
        foreach (var word in words)
        {
            string[] subWords = word.Split(" ");
            if (subWords[1] == "red")
                red_cnt = int.Parse(subWords[0]);
            else if (subWords[1] == "green")
                green_cnt = int.Parse(subWords[0]);
            else if (subWords[1] == "blue")
                blue_cnt = int.Parse(subWords[0]);
        }

        return (red_cnt, green_cnt, blue_cnt);
    }

    private static int Part1(string pathToInput)
    {
        int totalSum = 0, gameId = 1;
        const int max_red = 12, max_green = 13, max_blue = 14;
        foreach (string line in File.ReadLines(pathToInput))
        {
            string spart = line.Split(": ")[1];
            string[] sets = spart.Split("; ");
            bool isExceeded = false;
            foreach (var set in sets)
            {
                var (red_cnt, green_cnt, blue_cnt) = ColorsFromSet(set);
                if (red_cnt > max_red || green_cnt > max_green || blue_cnt > max_blue)
                    isExceeded = true;
            }
            if (!isExceeded)
            {
                totalSum += gameId;
            }
            gameId++;
        }
        return totalSum;
    }

    private static int Part2(string pathToInput)
    {
        return 0;
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
