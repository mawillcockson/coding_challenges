"""
https://en.wikipedia.org/wiki/Boy_or_Girl_paradox

Two people visit a store, and buy two items, one at a time.
Both are equally likely to buy a bag of flour, or a bag of sugar, each time.

You need to borrow some flour.

Person A says one of the items they bought is flour, and that they used it.
Person B says the first item they bought was flour, and that they used it.

Which person is more likely to have an extra bag of flour?
"""
from random import randint
from typing import Optional, Tuple, Union

NUMBER_OF_ITERATIONS = 1_000
FLOUR = True
SUGAR = False
Item = Union[type(FLOUR), type(SUGAR)]
Items = Tuple[Item, Item]


def person_A_requirement(first_item: Item, second_item: Item) -> Optional[Items]:
    "only returns pairs that person A could have"
    if first_item or second_item:
        return (first_item, second_item)


def person_B_requirement(first_item: Item, second_item: Item) -> Optional[Items]:
    "only returns pairs that person B could have"
    if first_item:
        return (first_item, second_item)


def main(iterations=NUMBER_OF_ITERATIONS) -> None:
    """
    print the percentage of the iterations where the selected person had the
    required item
    """
    times_person_A_has_extra_flour = 0
    times_person_A_does_not_have_extra_flour = 0
    times_person_B_has_extra_flour = 0
    times_person_B_does_not_have_extra_flour = 0

    # local redefinition to make things slightly faster
    FLOUR = True
    SUGAR = False

    for _ in range(iterations):
        first_item = FLOUR if randint(0, 1) else SUGAR
        second_item = FLOUR if randint(0, 1) else SUGAR

        if person_A_requirement(first_item, second_item):
            if first_item == second_item == FLOUR:
                times_person_A_has_extra_flour += 1
            else:
                times_person_A_does_not_have_extra_flour += 1

        if person_B_requirement(first_item, second_item):
            if first_item == second_item == FLOUR:
                times_person_B_has_extra_flour += 1
            else:
                times_person_B_does_not_have_extra_flour += 1

    print(f"out of {iterations} iterations:")

    total_valid_person_A_scenarios = (
        times_person_A_has_extra_flour + times_person_A_does_not_have_extra_flour
    )
    percent_of_times_person_A_has_flour = (
        times_person_A_has_extra_flour / total_valid_person_A_scenarios
    )
    print(f"person A had flour {percent_of_times_person_A_has_flour:.1%} of the time")

    total_valid_person_B_scenarios = (
        times_person_B_has_extra_flour + times_person_B_does_not_have_extra_flour
    )
    percent_of_times_person_B_has_flour = (
        times_person_B_has_extra_flour / total_valid_person_B_scenarios
    )
    print(f"person B had flour {percent_of_times_person_B_has_flour:.1%} of the time")


if __name__ == "__main__":
    main()
