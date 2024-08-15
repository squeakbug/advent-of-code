import itertools
import math
import sys

def diff_seq(arr: list) -> list:
    return [x[1] - x[0] for x in zip(arr, arr[1:])]


def extrapolate_seq(arr: list) -> int:
    if all([x == 0 for x in arr]):
        return 0
    return arr[-1] + extrapolate_seq(diff_seq(arr))


def part1(data: [str]) -> int:
    total_sum = 0
    for line in data:
        init_seq = [int(x) for x in line.split()]
        total_sum += extrapolate_seq(init_seq)
    return total_sum


def part2(data: [str]) -> int:
    total_sum = 0
    for line in data:
        init_seq = [int(x) for x in line.split()]
        total_sum += extrapolate_seq(list(reversed(init_seq)))
    return total_sum


if __name__ == "__main__":
    data = []
    with open(sys.argv[1], "r") as file:
        data = file.read().splitlines()

    part1_sln = part1(data)
    part2_sln = part2(data)

    print(f"part1_sln = {part1_sln}")
    print(f"part2_sln = {part2_sln}")
