#!/usr/bin/env -S uv run --no-project --script
# /// script
# dependencies = [
#     "marimo",
#     "ruff>=0.17.2",
# ]
# requires-python = ">=3.14"
# ///

import marimo

__generated_with = "0.25.2"
app = marimo.App(width="medium")

with app.setup:
    import marimo as mo
    import tempfile
    import atexit
    import pathlib


@app.cell(hide_code=True)
def _():
    mo.md("""
    # Practice Notebook: Reading and Writing Files
    """)
    return


@app.cell(hide_code=True)
def _():
    mo.md(r"""
    In this exercise, we will test your knowledge of readig and writing files by playing around with some text files.
    """)
    return


@app.cell(hide_code=True)
def _():
    mo.md(r"""
    Let's say we have a text file containing current visitors at a hotel. We'll call it, _guests.txt_. Run the followig code to create the file. The file will automatically populate with each initial guest's first name on its own line.
    """)
    return


@app.cell
def _():
    tmpdir = tempfile.TemporaryDirectory()
    atexit.register(tmpdir.cleanup)
    maybeRelative = (
        pathlib.Path("~")
        / pathlib.Path(tmpdir.name).relative_to(pathlib.Path.home())
        if pathlib.Path(tmpdir.name).is_relative_to(pathlib.Path.home())
        else pathlib.Path(tmpdir.name)
    )
    mo.md(
        f"first, I'll create a temporary directory (`{maybeRelative}`), so we don't accidentally overwrite anything"
    )
    return (tmpdir,)


@app.cell(hide_code=True)
def _(tmpdir):
    guests = pathlib.Path(tmpdir.name) / "guests.txt"
    guests.touch()
    mo.md(f"Now, I can create the file: `{guests}`")
    return (guests,)


@app.cell
def _(guests):

    initial_guests = ["Bob", "Andrea", "Manuel", "Polly", "Khalid"]

    def write_initial_guests() -> None:
        with guests.open(mode="wt") as file:
            for name in initial_guests:
                file.write(f"{name}\n")

    write_initial_guests()

    def read_guest_list() -> str:
        contents = ""
        with guests.open(mode="rt") as file:
            for line in file:
                contents += line

        return contents

    mo.md(f"""
    The names of the initial guests are written to the file.

    No output is generated. The contents that were written can be read by using the `open()` built-in, or `pathlib.Path.open()` method:

    ```
    {read_guest_list()}
    ```

    It can also be read with `guests.read_text()`, producing the same thing:

    ```
    {guests.read_text()}
    ```
    """)
    return read_guest_list, write_initial_guests


@app.cell(hide_code=True)
def _():
    mo.md(r"""
    The output shows that out `guests.txt` file is correctly populated with each initial guest's first name on its own line. Cool!
    """)
    return


@app.cell(hide_code=True)
def _():
    mo.md(r"""
    Now suppose we want to update our file as guests check in and out. Fill in the missing code in the following cell to add guests to the `guests.txt` file as the check in.
    """)
    return


@app.cell
def _(guests):
    def add_guests() -> None:
        new_guests = ["Sam", "Danielle", "Jacob"]

        with guests.open(mode="at") as file:
            for name in new_guests:
                file.write(f"{name}\n")

        # We don't need to close the file descriptor, because we used a context manager above
        # guests.close()

    add_guests()
    return (add_guests,)


@app.cell(hide_code=True)
def _():
    mo.md(r"""
    To check whether your code correctly added the new guests to the `guests.txt` file, run the following cell.
    """)
    return


@app.cell
def _(guests):
    def print_guest_list() -> None:
        with guests.open() as file:
            for line in file:
                print(line)

    print_guest_list()
    return (print_guest_list,)


@app.cell(hide_code=True)
def _():
    mo.md(r"""
    The current names in the `guests.txt` file should be: Bob, Andrea, Manuel, Polly, Khalid, Sam, Danielle, and Jacob.

    Was the `guests.txt` file correctly appended with the new guests? If not, go back and edit your code, making sure to fill in the gaps appropriately so that the new guests are correctly added to the guests.txt file. Once the new guests are successfully added, you have filled in the missing code correctly. Great!

    Now let's remove the guests that have checked out already. There are several ways to do this. However, the method we will choose for this exercise is outlined as follows:

    1. Open the file in "read" mode.
    2. Iterate over each line in the file and put each guest's name into a Python list.
    3. Open the file once again in "write" mode.
    4. Add each guest's name in the Python list to the file one by one.

    Ready? Fill in the missing code in the following cell to remove the guests that have checked out already.
    """)
    return


@app.cell
def _(add_guests, guests, read_guest_list, write_initial_guests):
    checked_out = ["Andrea", "Manuel", "Khalid"]

    def remove_checked_out() -> None:
        temp_list = []

        with guests.open(mode="rt") as file:
            for guest in file:
                temp_list.append(guest.strip())

        with guests.open(mode="wt") as file:
            for name in temp_list:
                if name not in checked_out:
                    file.write(f"{name}\n")

    def remove_checked_out_better() -> None:
        current_guests = guests.read_text().splitlines()
        not_checked_out = filter(lambda name: name not in checked_out, current_guests)
        guests.write_text(str.join("\n", not_checked_out) + "\n")

    def compare_techniques() -> None:
        remove_checked_out()
        theirs = read_guest_list()
    
        write_initial_guests()
        add_guests()
        remove_checked_out_better()
        mine = read_guest_list()

        #print(repr(theirs))
        #print(f"{mine!r}")

        assert theirs == mine, "we did something different"

    compare_techniques()
    return


@app.cell(hide_code=True)
def _():
    mo.md(r"""
    To check whether your code correctly removed the checked out guests from the `guests.txt` file, run the following cell.
    """)
    return


@app.cell
def _(print_guest_list):
    print_guest_list()
    return


@app.cell(hide_code=True)
def _():
    mo.md(r"""
    The current names in the `guests.txt` file should be: Bob, Polly, Sam, Danielle, and Jacob.

    Were the names of the checked out guests correctly removed from the `guests.txt` file? If not, go back and edit your code, making sure to fill in the gaps appropriately so that the checked out guests are successfully removed from the `guests.txt` file. Once the checked out guests are successfully removed, you have filled in the missing code correctly. Awesome!

    Now let's check whether Bob and Andrea are still checked in. How could we do this? We'll just read through each line in the file to see if their name is in there. Run the following code to check whether Bob and Andrea are still checked in.
    """)
    return


@app.cell
def _(guests):
    def is_checked_in(to_check: list[str]) -> bool:
        not_checked_in = set(to_check)
        with guests.open("rt") as file:
            for name in file:
                if name.strip() in not_checked_in:
                    print(f"{name.strip()} is checked in")
                    not_checked_in.remove(name.strip())

        if len(not_checked_in) > 0:
            print(str.join("\n", (f"{name} is not checked in" for name in not_checked_in)))

        return len(not_checked_in) == 0

    is_checked_in(["Bob", "Andrea"])
    return


@app.cell(hide_code=True)
def _():
    mo.md(r"""
    We can see that Bob is checked in while Andrea is not. Nice work! You've learned the basics of reading and writing files in Python!
    """)
    return


if __name__ == "__main__":
    app.run()
