"""
https://en.wikipedia.org/wiki/Boy_or_Girl_paradox

Two people visit a store, and each buy two items, one at a time.
Both are equally likely to buy a bag of flour, or a bag of sugar, each time.

You need to borrow some flour.
You ask both of them if they have a bag of flour you can use.

Person A says one of the items they bought was flour, and that they used it.
Person B says the first item they bought was flour, and that they used it.

Which person is more likely to have an extra bag of flour?
"""
from fractions import Fraction
from random import randint

NUMBER_OF_ITERATIONS = 10_000
FLOUR = True
SUGAR = False


def main(iterations: int = NUMBER_OF_ITERATIONS) -> None:
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

        # only choose pairs that person A could have, based on their statement
        # that one of the two items they have is flour
        if first_item or second_item:
            # person A has extra flour if both items are flour
            if first_item == second_item == FLOUR:
                times_person_A_has_extra_flour += 1
            else:
                times_person_A_does_not_have_extra_flour += 1

        # only choose pairs that person B could have, based on their statement
        # that the first item they bought is flour
        if first_item:
            # person B has extra flour if both items are flour
            if first_item == second_item == FLOUR:
                times_person_B_has_extra_flour += 1
            else:
                times_person_B_does_not_have_extra_flour += 1

    print(__doc__)
    print(
        f"""If they visit the store {iterations:,} times, and you pick someone at random
each time they respond as in the scenario:
"""
    )

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

    person_A_fraction = Fraction(
        times_person_A_has_extra_flour, iterations
    ).limit_denominator(iterations)
    person_B_fraction = Fraction(
        times_person_B_has_extra_flour, iterations
    ).limit_denominator(iterations)
    print(
        f"""
If you don't wait for a time when their answers are correct,
{person_A_fraction.numerator:,} times out of every {person_A_fraction.denominator:,} visits to the store ({float(person_A_fraction):.1%}) person A will have flour,
and person B will have it {person_B_fraction.numerator:,} times out of {person_B_fraction.denominator:,} visits ({float(person_B_fraction):.1%})

This is because out of {iterations:,} scenarios, person A's statement was true in {total_valid_person_A_scenarios / iterations:.1%} of the scenarios,
while person B's statement was true in only {total_valid_person_B_scenarios / iterations:.1%} of the scenarios.
"""
    )


if __name__ == "__main__":
    main()
