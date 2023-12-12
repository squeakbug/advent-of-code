import itertools
import math
import sys

def part1(data: [str]) -> int:
    template = data[0]

    new_form = dict()
    for line in data[2:]:
        parts = line.split("=")
        v = parts[1].strip().replace("(", "").replace(")", "").split(", ")
        k = parts[0].strip()
        new_form[k] = v

    curr_v = "AAA"
    path = itertools.cycle(template)
    iter_cnt = 0
    while curr_v != "ZZZ":
        d = next(path)
        if d == "L":
            curr_v = new_form[curr_v][0]
        else:
            curr_v = new_form[curr_v][1]
        iter_cnt += 1
    return iter_cnt

def part2(data: [str]) -> int:
    template = data[0]

    new_form = dict()
    for line in data[2:]:
        parts = line.split("=")
        v = parts[1].strip().replace("(", "").replace(")", "").split(", ")
        k = parts[0].strip()
        new_form[k] = v

    curr_vs = [x for x in new_form if x.endswith("A")]
    cycles = []
    path = itertools.cycle(template)
    for curr_v in curr_vs:
        iter_cnt = 0
        while curr_v[2] != 'Z':
            d = int(next(path) != "L")
            curr_v = new_form[curr_v][d]
            iter_cnt += 1
        cycles.append(iter_cnt)
    return math.lcm(*cycles)


if __name__ == "__main__":
    data = []
    with open(sys.argv[1], "r") as file:
        data = file.read().splitlines()

    part1_sln = part1(data)
    part2_sln = part2(data)

    print(f"part1_sln = {part1_sln}")
    print(f"part2_sln = {part2_sln}")
