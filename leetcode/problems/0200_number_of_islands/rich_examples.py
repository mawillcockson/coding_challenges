import sys
import random
from rich import print
from rich.table import Table
from rich import inspect
from itertools import cycle
from rich.console import Console
from time import sleep
from rich.color import ANSI_COLOR_NAMES

console = Console()
# inspect(console)
# sys.exit(0)
grid = Table.grid()
# colors = ["red", "green", "blue"]
colors = list(ANSI_COLOR_NAMES)
grid.add_column()
grid.add_column()
grid.add_column()
grid.add_row("[red]r[/]", "[blue]b[/]", "[green]g[/]")
grid.add_row("[green]g[/]", "[red]r[/]", "[blue]b[/]")
inspect(grid, methods=True)
sys.exit(0)
with console.screen():
    for _ in range(console.width):
        grid.add_column()
    for i in range(console.height - 1):
        random.seed(i)
        random.shuffle(colors)
        color = cycle(colors)
        cells = []
        for j in range(len(grid.columns)):
            # cells.append(f"[white on {next(color)}] [/]")
            cells.append(f"[white on {random.choice(colors)}] [/]")
        grid.add_row(*cells)

    print(grid, end="")
    sleep(5)
