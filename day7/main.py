import time
import sys

ordered_types = [
    [5],
    [4, 1],
    [3, 2],
    [3, 1, 1],
    [2, 2, 1],
    [2, 1, 1, 1],
    [1, 1, 1, 1, 1] 
]

def hand_type_part1(hand: str) -> int:
    label_counts = [hand.count(card) for card in set(hand)]
    label_counts.sort(reverse=True)
    return ordered_types.index(label_counts)


def hand_type_part2(hand: str) -> int:
    label_counts = [hand.count(card) for card in set(hand) if card != 'J'] \
        or [0]
    label_counts.sort(reverse=True)
    label_counts[0] += hand.count('J')
    return ordered_types.index(label_counts)


def cards_as_int_part1(hand: str) -> tuple:
    ordered_labels = 'AKQJT98765432'
    return (ordered_labels.index(card) for card in hand)


def cards_as_int_part2(hand: str) -> tuple:
    ordered_labels = 'AKQT98765432J'
    return (ordered_labels.index(card) for card in hand)


def get_winnings(data, f_hand_type, f_cards_as_int):
    hands = []
    for line in data:
        hand, bid = line.split(' ')
        encoded_hand = (
            f_hand_type(hand),
            *f_cards_as_int(hand),
            int(bid)
        )
        hands.append(encoded_hand)
    
    hands.sort(reverse=True)
    winnings = sum(rank * hand[-1]
                for rank, hand in enumerate(hands, start=1))

    return winnings


def part1(data: [str]) -> int:
    return get_winnings(data, hand_type_part1, cards_as_int_part1)


def part2(data: [str]) -> int:
    return get_winnings(data, hand_type_part2, cards_as_int_part2)


if __name__ == "__main__":
    data = []
    with open(sys.argv[1], "r") as file:
        data = file.read().splitlines()

    part1_sln = part1(data)
    part2_sln = part2(data)

    print(f"part1_sln = {part1_sln}")
    print(f"part2_sln = {part2_sln}")
