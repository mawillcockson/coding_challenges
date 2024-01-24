"""Functions used in preparing Guido's gorgeous lasagna.

Learn about Guido, the creator of the Python language:
https://en.wikipedia.org/wiki/Guido_van_Rossum

This is a module docstring, used to describe the functionality
of a module and its functions and/or classes.
"""


#DONE: define the 'EXPECTED_BAKE_TIME' constant.
EXPECTED_BAKE_TIME = 40


#DONE: Remove 'pass' and complete the 'bake_time_remaining()' function below.
def bake_time_remaining(elapsed_bake_time: int) -> int:
    """Calculate the bake time remaining.

    :param elapsed_bake_time: int - baking time already elapsed.
    :return: int - remaining bake time (in minutes) derived from 'EXPECTED_BAKE_TIME'.

    Function that takes the actual minutes the lasagna has been in the oven as
    an argument and returns how many minutes the lasagna still needs to bake
    based on the `EXPECTED_BAKE_TIME`.
    """
    if (remaining := EXPECTED_BAKE_TIME - elapsed_bake_time) < 0:
        raise ValueError(f"lasagna is overcooked by {abs(remaining):.3g} minutes")
    return remaining


#DONE: Define the 'preparation_time_in_minutes()' function below.
# You might also consider using 'PREPARATION_TIME' here, if you have it defined.
PREPARATION_TIME = 2  # minutes per layer


def preparation_time_in_minutes(number_of_layers: int) -> int:
    """Calculate the time required to prepare lasagna for baking

    :param number_of_layers: int - desired number of layers to prepare in lasagna
    :return: int - the number of minutes it will take to prepare the whole lasagna

    Function that takes the number of layers that should be in the final
    lasagna and returns how much time it will take the prepare all of the
    layers in total. This is based on the `PREPARATION_TIME`.
    """
    if (prep := int(number_of_layers * PREPARATION_TIME)) < 0:
        raise ValueError(f"prep time cannot be less than zero: {prep}")
    return prep



#DONE: define the 'elapsed_time_in_minutes()' function below.
# Remember to add a docstring (you can copy and then alter the one from bake_time_remaining.)
def elapsed_time_in_minutes(number_of_layers: int, elapsed_bake_time: int) -> int:
    """Calculate the total time spent on the lasagna

    :param number_of_layers: int - number of layers in prepared lasagna
    :param elapsed_bake_time: int - the amount of time the lasagna has been cooking
    :return: int - the number of minutes spend on the lasagna

    Function that takes the number of layers that were in the lasagna that was
    put into the oven, as well as the amount of time in minutes that it's spent
    baking so far, and returns the total amount of time that has gone into the
    lasagna so far
    """
    if elapsed_bake_time < 0:
        raise ValueError(f"elapsed_bake_time must be positive: {elapsed_bake_time}")
    return preparation_time_in_minutes(number_of_layers) + elapsed_bake_time
