# /// script
# dependencies = [
#     "marimo>=0.25.2",
#     "ruff==0.17.6",
# ]
# [tool.marimo]
# diagnostics.enabled = true
# ///

import marimo

__generated_with = "0.25.2"
app = marimo.App(css_file="../custom_marimo.css", auto_download=["html"])

with app.setup(hide_code=True):
    import marimo as mo
    from typing import Final, Literal, NamedTuple
    from collections.abc import Callable
    import traceback
    from unittest.mock import patch


@app.cell(hide_code=True)
def _():
    class RevisionVisibility(NamedTuple):
        "which version of the document do you want to see"

        mine: Literal["show only my version"] = "show only my version"
        both: Literal["show edits"] = "show edits"

    a: list[str] = [1]

    RevisionVisibilityType = Literal["show only my version", "show edits"]
    RV: Final = RevisionVisibility()
    REVISION_VISIBILITY: RevisionVisibilityType = mo.ui.dropdown(
        RV,
        value=RV.both,
        allow_select_none=False,
    )
    mo.md(
        f"Would you like the edits I've made to this notebook to be visible: {REVISION_VISIBILITY}"
    )
    return REVISION_VISIBILITY, RV, RevisionVisibilityType


@app.cell(hide_code=True)
def _():
    mo.md(f"""
    {mo.md(f"Callouts like this and the following are always my own:\n{mo.md('A neutral').callout(kind='neutral', title='Neutral')}\n{mo.md('A danger').callout(kind='danger', title='Danger')}\n{mo.md('A warning').callout(kind='warn', title='Warning')}\n{mo.md('A success').callout(kind='success', title='Success')}\n{mo.md('An info').callout(kind='info', title='Info')}").callout(title="Note")}
    """)
    return


@app.cell(hide_code=True)
def _(
    REVISION_VISIBILITY: "RevisionVisibilityType",
    RV: Final,
    RevisionVisibilityType,
):
    CV: RevisionVisibilityType = REVISION_VISIBILITY.value

    def ins_(text: str) -> mo.Html:
        "render inserted text for markdown/html"
        if CV == RV.both:
            return mo.Html(f"<ins>{mo.md(text)}</ins>")
        return text

    def del_(text: str) -> mo.Html:
        "render deleted text for markdown/html"
        if CV == RV.both:
            return mo.Html(f"<del>{mo.md(text)}</del>")
        return ""

    def rep_(old: str, new: str) -> mo.Html:
        "shortcut for deleting the old and inserting the new"
        return mo.Html(f"{del_(old)}{ins_(new)}")

    def note(content: str, kind: Literal["neutral", "danger", "warning", "success", "info"] = "info", title: str | None = "Remark") -> mo.Html:
        "renders an comment about surrounding text"
        return mo.md(content).callout(kind="info", title=title)

    return del_, ins_, note, rep_


@app.cell(hide_code=True)
def _():
    mo.md(r"""
    # Practice Notebook - Errors and Exceptions
    """)
    return


@app.cell(hide_code=True)
def _(note):
    mo.md(f"""
    Below we have a function that removes an item from an input list.  Run it to see what it does.

    ```python
    my_list = [27, 5, 9, 6, 8]

    def RemoveValue(myVal):
        my_list.remove(myVal)
        return my_list

    print(RemoveValue(27))
    ```

    {note("The function naming convention used for the above snippet makes it look like a class. This is corrected in the below snippet:")}
    """)
    return


@app.cell
def _():
    my_list: list[int] = [27, 5, 9, 6, 8]

    def remove_value(val: int) -> list[int]:
        my_list.remove(val)
        return my_list

    print(remove_value(27))
    return my_list, remove_value


@app.cell(hide_code=True)
def _(del_, note, rep_):
    mo.md(f"""
    We used the {rep_("`RemoveValue()`", "`remove_value()`")} function to remove the number{del_(",")} {rep_("27", "`27`")} from the given list.  Great! The function seems to be working fine. However, there is a problem when we try to call the function on the number {rep_("27", "`27`")} again. Run the following cell to see what happens.

    {note('''The cell used to look like this:

    ```python
    print(RemoveValue(27))
    ```

    It has been updated to use the new function name.
    ''')}
    """)
    return


@app.cell
def _(remove_value):
    try:
        print(remove_value(27))
    except ValueError as err:
        traceback.print_exception(err)
    return


@app.cell(hide_code=True)
def _(note, rep_):
    mo.md(f"""
    From the above output we see that our function now raises a {rep_("**<span class=red>ValueError</span>**", "`ValueError`")}.  This is because we are trying to remove a number from a list that is not in the list.  When we removed {rep_("27", "`27`")} from the list the first time, it was no longer available in the list to be removed a second time. Python is letting us know that the number {rep_("27", "`27`")} no longer makes sense for our {rep_("`RemoveValue()`", "`remove_value()`")} function.

    We'd like to take control of the error messaging here and pre-empt this error. Fill in the blanks below to raise a {rep_("ValueError", "`ValueError`")} in the {rep_("`RemoveValue()`", "`remove_value()`")} function if a value is not in the list. You can have the error message say something obvious like "Value must be in the given list".

    {note('''The code has been adjusted, as previously. This is the original code:

    ```python
    def RemoveValue_1(myVal):
        if myVal not in my_list:
            ___
        else:
            my_list.remove(myVal)
        return my_list
    print(RemoveValue_1(27))
    ```

    And here is the solution:

    ```python
    def RemoveValue_1(myVal):
        if myVal not in my_list:
            raise ValueError("Value must be in the given list")
        else:
            my_list.remove(myVal)
        return my_list
    print(RemoveValue_1(27))
    ```
    ''', title="Solution")}

    {note('''I don't think this is an example of a good use of manually throwing an error, because the error message isn't particularly useful, compared to the original one produced by the builtin. A more useful example might be in the following:''')}
    """)
    return


@app.function
def find_occurrence(text: str, letter: str, nth: int) -> int:
    """
    finds index of the nth occurrence of a letter in a given text

    >>> find_occurrence("abc abc", "b", 2)
    5
    """
    occurrences: dict[str, list[int]] = {}
    for index, _letter in enumerate(text):
        if _letter not in occurrences:
            occurrences[_letter] = []
        occurrences[_letter].append(index)

    return occurrences.get(letter)[nth - 1]


@app.cell(hide_code=True)
def _(ins_):
    mo.md(f"""
    {mo.md('''
    The above function is intentionally incredibly brittle: it intentionally does not perform checks to detect if the inputs are self-consistent and sensical.
    ''').callout(kind="warn", title="Note")}

    {ins_("It works as expected when the inputs are self-consistent:")}
    """)
    return


@app.cell
def _():
    #                                                ⮦27th character
    my_sentence: Final = "This is a sentence with a few words."
    #                                ⮤1 ⮤2 ⮤3        ⮤4th occurrence
    assert find_occurrence(my_sentence, "e", 4) == 27
    return (my_sentence,)


@app.cell(hide_code=True)
def _(ins_):
    mo.md(f"""
    {ins_("But when the letter does not occur in the given text, an error is thrown that requires knowledge of the function's implementation to understand.")}
    """)
    return


@app.cell
def _(my_sentence: Final):
    try:
        find_occurrence(my_sentence, "x", 1)
    except TypeError as err:
        traceback.print_exception(err)
    return


@app.cell(hide_code=True)
def _(ins_):
    mo.md(f"""
    {ins_("In this case, it's useful to insert code that detects this specific error situation, and throws a more understandable error.")}

    {mo.md("This does not correct any other errors present in the function.").callout(kind="warn", title="Note")}
    """)
    return


@app.cell
def _(my_sentence: Final):
    def find_occurrence_fixed(text: str, letter: str, nth: int) -> int:
        """
        finds index of the nth occurrence of a letter in a given text

        >>> find_occurrence_fixed("abc abc", "b", 2)
        5
        >>> find_occurrence_fixed("abc abc", "X", 1)
        Traceback (most recent call last):
            ...
        ValueError: the letter 'X' does not occur in the text
        """
        # detect as early as possible, to prevent doing unneeded work
        if letter not in text:
            raise ValueError(f"the letter {repr(letter)} does not occur in the text")

        occurrences: dict[str, list[int]] = {}
        for index, _letter in enumerate(text):
            if _letter not in occurrences:
                occurrences[_letter] = []
            occurrences[_letter].append(index)

        return occurrences.get(letter)[nth - 1]

    try:
        find_occurrence_fixed(my_sentence, "X", 1)
    except ValueError as err:
        assert err.args[0] == "the letter 'X' does not occur in the text"
    else:
        assert False, "expected an error, but none were raised"
    return


@app.cell(hide_code=True)
def _(note):
    mo.md(f"""
    {note("Regardless of my detractions, here's a solution updated to match the new function names:", title="Solution")}
    """)
    return


@app.cell
def _(my_list: list[int]):
    def remove_value_2(val: int) -> list[int]:
        if val not in my_list:
            raise ValueError("Value must be in `my_list` at the time of this function's execution")
        my_list.remove(val)
        return my_list


    try:
        remove_value_2(27)
    except ValueError as err:
        print("".join(traceback.format_exception_only(err)), end="")
    return


@app.cell(hide_code=True)
def _(note, rep_):
    mo.md(f"""
    Did your error message print correctly? Was the output something like: {rep_("**<span class=red>ValueError:</span> Value must be in the given list**", "<code>ValueError: Value must be in &grave;my_list&grave; at the time of this function's execution</code>")}? If not, go back to the previous cell and make sure you filled in the blanks correctly.  If your error message did print correctly, great! You are on your way to mastering the basics of handling errors and exceptions.

    Now, let's look at a different function.  Below we have a function that sorts an input list alphabetically.  Run it to see what it does.

    {note('''The cell originally contained the following:

    ```python
    my_word_list = ['east', 'after', 'up', 'over', 'inside']

    def OrganizeList(myList):
        myList.sort()
        return myList

    print(OrganizeList(my_word_list))
    ```
    ''')}
    """)
    return


@app.cell
def _():
    my_word_list: list[str] = ['east', 'after', 'up', 'over', 'inside']

    def organize_list[T](to_organize: list[T]) -> list[T]:
        to_organize.sort()
        return to_organize

    organize_list(my_word_list)
    return (organize_list,)


@app.cell(hide_code=True)
def _(note, rep_):
    mo.md(f"""
    We used the {rep_("`OrganizeList()`", "`organize_list()`")} function to sort a given list alphabetically. The function seems to be working fine. However, there is a problem when we try to call the function on a list containing number values. Run the following cell to see what happens:

    {note('''
    The cell originally contained the following:

    ```python
    my_new_list = [6, 3, 8, "12", 42]
    print(OrganizeList(my_new_list))
    ```
    ''')}
    """)
    return


@app.cell
def _(organize_list):
    my_new_list: list[int | str] = [6, 3, 8, "12", 42]
    organize_list(my_new_list)
    return (my_new_list,)


@app.cell(hide_code=True)
def _(note, rep_):
    mo.md(f"""
    {note("This is a much better example of a useful, manually-raised error. Perhaps the first one was showing how to recreate an error message similar to a built-in one, to make it easier to understand the modification.")}

    From the above output we see that our function now raises a {rep_("<span class=red>**TypeError**</span>", "`TypeError`")}. This is because the {rep_("`OrganizeList()`", "`organize_list()`")} function makes sense for lists that are filled with only strings.  Take control of the error messaging here and pre-empt this error by filling in the blanks below to add an assert {rep_("type argument", "statement")} that verifies whether the input list is filled with only strings. You can have the error message say something like "Word list must be a list of strings".

    {note('''
    The code originally contained the following:

    ```python
    def OrganizeList_1(myList):
        for item in myList:
            ___
        myList.sort()
        return myList
    print(OrganizeList_1(my_new_list))
    ```

    And this is the solution:

    ```python
    def OrganizeList_1(myList):
        for item in myList:
            assert str(item) == item, "Word list must be a list of strings"
        myList.sort()
        return myList
    print(OrganizeList_1(my_new_list))
    ```
    ''', title="Solution")}

    {note("I'm unhappy that I can't think of a nice-looking solution to this that looks nice _and_ doesn't use anything that hasn't been taught already. I think a short introduction to the [`type()`](https://docs.python.org/3/library/functions.html#type) and [`isinstance()`](https://docs.python.org/3/library/functions.html#isinstance) built-ins would have been really useful.")}
    """)
    return


@app.cell
def _(my_new_list: list[int | str]):
    def organize_list_2(to_organize: list[str]) -> list[str]:
        assert all(isinstance(item, str) for item in to_organize), (
            "Word list must be a list of strings"
        )
        to_organize.sort()
        return to_organize

    organize_list_2(my_new_list)
    return


@app.cell
def _(note, rep_):
    mo.md(f"""
    Did your error message print correctly? Was the output something like: {rep_("**<span class=red>AssertionError:</span> Word list must be a list of strings**", "`AssertionError: Word list must be a list of strings`")}? If not, go back to the previous cell and make sure you filled in the blanks correctly. If your error message did print correctly, excellent! You are another step closer to mastering the basics of handling errors and exceptions.

    Let's look at one last code block.  The {rep_("`Guess()`", "`guess()`")} function below takes a list of participants, assigns each a random number from 1 to 9, and stores this information in a dictionary with the participant name as the key.  It then returns {rep_("_True_", "`True`")} if Larry was assigned the number 9 and {rep_("_False_", "`False`")} if this was not the case. Run it to see what it does.

    {note('''
    The cell originally contained the following:

    ```python
    import random
    _participants = ['Jack', 'Jill', 'Larry', 'Tom']

    def Guess(participants):
        my_participant_dict = {}
        for participant in participants:
            my_participant_dict[participant] = random.randint(1, 9)
        if my_participant_dict['Larry'] == 9:
            return True
        else:
            return False
    print(Guess(_participants))
    ```
    ''')}

    {mo.md("I object vehemently to the nature of this")}
    """)
    return


@app.cell
def _(Guess):
    import random

    PARTICIPANTS: Final = ['Jack', 'Jill', 'Larry', 'Tom']

    def guess(participants: list[str]) -> str:
        my_participant_dict = {}
        for participant in participants:
            my_participant_dict[participant] = random.randint(1, 9)
        if my_participant_dict['Larry'] == 9:
            return True
        else:
            return False
    print(Guess(_participants))
    return (random,)


@app.cell(hide_code=True)
def _():
    mo.md(r"""
    The code seems to be working fine.  However, there are some things that could go wrong, so find the part that might throw an exception and wrap it in a try-except block to ensure that you get sensible behavior.  Do this in the cell below. Code your function to return *None* if an exception occurs.
    """)
    return


@app.cell
def _(random):
    # Revised Guess() function
    def Guess_1(participants):
        my_participant_dict = {}
        for participant in participants:
            my_participant_dict[participant] = random.randint(1, 9)
        if my_participant_dict['Larry'] == 9:
            return True
        else:
            return False

    return (Guess_1,)


@app.cell(hide_code=True)
def _():
    mo.md(r"""
    Call your revised `Guess()` function with the following participant list.
    """)
    return


@app.cell
def _(Guess_1):
    _participants = ['Cathy', 'Fred', 'Jack', 'Tom']
    print(Guess_1(_participants))
    return


@app.cell(hide_code=True)
def _():
    mo.md(r"""
    Was the above output *None*? If not, go back to the code block containing your revised `Guess()` function and make edits so that the output is *None* for the previous code block.  If the above output was indeed *None*, congratulations! You've mastered the basics of handling errors and exceptions in Python and you are all done with this notebook!
    """)
    return


if __name__ == "__main__":
    app.run()
