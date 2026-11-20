# /// script
# dependencies = ["marimo"]
# requires-python = ">=3.14"
# ///

import marimo

__generated_with = "0.25.2"
app = marimo.App(
    width="medium",
    css_file="../custom_marimo.css",
    auto_download=["html"],
)

with app.setup:
    import marimo as mo
    from typing import Final, Literal, NamedTuple
    from collections.abc import Callable
    import traceback
    from unittest.mock import patch


@app.cell(hide_code=True)
def _():
    class RevisionVisibility(NamedTuple):
        "which version of the document do you want to see"

        mine: Literal["mine"] = "mine"
        both: Literal["both"] = "both"

    RevisionVisibilityType = Literal["mine", "both"]
    RV: Final = RevisionVisibility()
    REVISION_VISIBILITY: RevisionVisibilityType = mo.ui.dropdown(
        RV,
        value=RV.both,
        allow_select_none=False,
    )
    mo.md(
        f"Which version of this notebook would you like to see: {REVISION_VISIBILITY}"
    )
    return REVISION_VISIBILITY, RV, RevisionVisibilityType


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

    def note(content: str, title: str | None = "Remark") -> mo.Html:
        "renders an comment about surrounding text"
        return mo.md(content).callout(kind="info", title=title)
        # title_attr = f" title={title}" if title is not None else ""
        # return mo.Html(f'<aside role="note"{title_attr}>{mo.md(content)}</aside>')

    # mo.md(f"you chose {rep_(CV, CV)}")
    return del_, ins_, note, rep_


@app.cell(hide_code=True)
def _(del_, note):
    mo.md(f"""
    # Practice Notebook - Unit Tests and Edge Cases

    Below we have some code that makes a list of specific letters found in any string.{
        del_(" If you run it, you can see what it does.")
    }

    {
        note('''In the below code snippet:
    - the `LetterCompiler` function uses a weird naming convention, making it look like a class
    - the `result` variable is entirely pointless, as it's returned unchanged
    ''')
    }

    ```python
    import re

    my_txt = "An investment in knowledge pays the best interest."

    def LetterCompiler(txt):
        result = re.findall(r'([a-c]).', txt)
        return result

    print(LetterCompiler(my_txt))
    ```

    Instead, I'll use the following:
    """)
    return


@app.cell
def _():
    import re

    SENTENCE: Final = "An investment in knowledge pays the best interest."
    PATTERN: Final = re.compile(r"([a-c]).")

    def compile_letters(text: str) -> list[str]:
        return PATTERN.findall(text)

    mo.show_code(compile_letters(SENTENCE), position="above")
    return (compile_letters,)


@app.cell(hide_code=True)
def _(del_, ins_, note, rep_):
    mo.md(f"""
    From the output, you can see that the {
        rep_("`LetterCompiler()`", "`compile_letter()`")
    } function finds all matches for the letters `a` through `c` in an input string<ins>,</ins> if followed by another character{
        rep_(" and", ". It")
    } returns them as a list of strings, with each string representing one match.{
        del_(" Nice.")
    }

    But can we be sure that this function will always do what we expect it to do? We need to write code to help us catch mistakes, errors{
        ins_(",")
    } and bugs. This code should automate the process of checking if the returned value of our code matches the expectations by dynamically feeding into it test cases. Since we're dynamically feeding in different strings, it would be prudent to create unit tests for our code. We can use the module [`unittest`][unittest docs] for this.

    Fill in the blanks below to create an {
        del_("automatic ")
    }unit test that verifies whether input strings have the correct list of string matches.

    ```python
    import unittest

    class TestCompiler(unittest.TestCase):

        def test_basic(self):
            testcase = "The best preparation for tomorrow is doing your best today."
            expected = ['b', 'a', 'a', 'b', 'a']
            self.assertEqual(LetterCompiler(___), ___)
    ```

    {
        note(
            '''```python
    import unittest

    class TestCompiler(unittest.TestCase):

        def test_basic(self):
            testcase = "The best preparation for tomorrow is doing your best today."
            expected = ['b', 'a', 'a', 'b', 'a']
            self.assertEqual(LetterCompiler(testcase), expected)
    ```''',
            title="Solution",
        )
    }

    {ins_("With the updated code, this would look like:")}

    [unittest docs]: <https://docs.python.org/3/library/unittest.html>
    """)
    return


@app.cell
def _(compile_letters):
    import unittest

    class TestCompiler(unittest.TestCase):
        "test compile_letters()"

        def test_happy(self) -> None:
            result = compile_letters(
                "The best preparation for tomorrow is doing your best today."
            )
            self.assertEqual(result, ["b", "a", "a", "b", "a"])

    mo.show_code()
    return (unittest,)


@app.cell
def _(ins_, rep_):
    mo.md(f"""
    Now that your {rep_("automatic", "unit")} test is coded, you need to call the [`unittest.main( )`][unittest-main] function to run the test. It is important to note that the configuration for running unit tests in Jupyter is different than running unit tests from the command line. Running [`unittest.main( )`][unittest-main] {ins_("without any arguments")} in Jupyter **will result in an error**. You can see this by running the following cell to execute your {rep_("automatic", "unit")} test.{ins_(" In order to produce the expected output, the cell recreates the conditions that Coursera configures Jupyter to run under. Also, in order to display the error, it's necessary to explicitly capture it.")}

    [unittest-main]: <https://docs.python.org/3/library/unittest.html#unittest.main>
    """)
    return


@app.cell
def systemexit_error(unittest):
    output: str = ""

    # As of 2026-11, Coursera configures Jupyter to be started with this command. This context manager temporarily recreates that just for this execuation of unittest.main()
    with patch(
        "sys.argv",
        new=[
            "/opt/conda/lib/python3.6/site-packages/ipykernel_launcher.py",
            "-f",
            "/home/jovyan/.local/share/jupyter/runtime/kernel-3a18e197-8c17-4c3e-a32e-489966e574a5.json",
        ],
    ), mo.capture_stdout() as stdout, mo.capture_stderr() as stderr:
        exception: str = ""
        try:
            unittest.main()
        except SystemExit as err:
            exception += "<br>".join(traceback.format_exception(err))

        output = str.join("<br>", [stdout.getvalue(), stderr.getvalue(), exception])

    mo.show_code(mo.Html(f"<pre>{output}</pre>"), position="above")
    return


@app.cell
def _(ins_, rep_):
    mo.md(f"""
    Yikes! **<code><span class="red">SystemExit:</span> {rep_("True", "1")}</code>** means an error occurred, as expected. The reason is that [`unittest.main( )`][unittest-main] looks at {rep_("_sys.argv_", "[`sys.argv`](https://docs.python.org/3/library/sys.html#sys.argv)")}. In Jupyter, by default, the first argument of {rep_("_sys.argv_", "`sys.argv`")} is what started the Jupyter kernel{ins_(",")} which is not the case when executing it from the command line. This default parameter is passed into [`unittest.main( )`][unittest-main] as an attribute when you don't explicitly pass it attributes{ins_(",")} and is therefore what causes the error about the kernel connection file not being a valid attribute. Passing an explicit list to [`unittest.main( )`][unittest-main] prevents it from looking at {rep_("_sys.argv_", "`sys.argv`")}.

    Let's pass it the list {rep_(r"\['first-arg-is-ignored']", '`["first-arg-is-ignored"]`')} for example. In addition, we will pass it the parameter {rep_("_exit = False_", "`exit=False`")} to prevent [`unittest.main( )`][unittest-main] from shutting down the kernel process. Run the following cell with the {rep_("_argv_", "`argv`")} and {rep_("_exit_", "`exit`")} parameters passed into [`unittest.main( )`][unittest-main] to rerun your {rep_("automatic", "unit")} test.

    ```python
    unittest.main(argv = ['first-arg-is-ignored'], exit = False)
    ```

    [unittest-main]: <https://docs.python.org/3/library/unittest.html#unittest.main>
    """)
    return


@app.cell
def _(unittest):
    with mo.redirect_stdout():
        unittest.main(argv=["first-arg-is-ignored"], exit=False)
    return


@app.cell(hide_code=True)
def _(ins_, rep_):
    mo.md(f"""
    Did your {rep_("automatic", "unit")} test pass? Was {rep_("**OK**", "`OK`")} the result? If not, go back to your {rep_("automatic", "unit")} test code and make sure you filled in the blanks correctly. If your {rep_("automatic", "unit")} test passed, great! You have successfully filled in the gaps to create {rep_("an automatic", "a unit")} test that verifies {rep_("whether input strings have the correct list of string matches", "the function under test can produce the expected matches for certain strings")}.

    This is great work so far, but your {rep_("automatic", "unit")} test includes only one test case. You need to make it grow. You can feed in more strings as test cases to {rep_("test whether your code works in the general case", "gain more confidence your function works in the general case")}. But you should also see what happens when you give it some input that you might not expect it to run into under normal operations.

    Edge cases are inputs to code that produce unexpected results, and are found at the extreme ends of the ranges of input we imagine problems will typically work with. Can you use the cell below to write some edge cases? We've already filled in another test case for you! As it is, this test will run fine. Can you come up with at least one test case that you think could result in a wrong return value? {rep_("No wrong answers! Feel free to play around", "Give it your best shot")}!

    ```python
    class TestCompiler2(unittest.TestCase):

        def test_two(self):
            testcase = "A b c d e f g h i j k l m n o p q r s t u v w x y z"
            expected = ['b', 'c']
            self.assertEqual(LetterCompiler(testcase), expected)

    # EDGE CASES HERE

    unittest.main(argv = ['first-arg-is-ignored'], exit = False)
    ```

    {ins_("If we update the code to reflect the new function name, and separate the edge cases out into their own class, I get the following:")}
    """)
    return


app._unparsable_cell(
    r"""
    class MoreHappyPath(unittest.TestCase):
        "more tests for copmile_letters()"

        def test_capital_single(self) -> None:
            result = compile_letters("A b c d e f g h i j k l m n o p q r s t u v w x y z")
            self.assertEqual(result, ["b", "c"])

    class EdgeCases(unittest.TestCase):
        "these should fail"

        def test_
    """,
    name="_"
)


@app.cell
def _(compile_letters):
    # arguably, a noncharacter shouldn't count as a character
    (a := [compile_letters("a\ufdd0")]).append(
        compile_letters("abc")  # could expect ["a", "b"]
    )
    a
    return


if __name__ == "__main__":
    app.run()
