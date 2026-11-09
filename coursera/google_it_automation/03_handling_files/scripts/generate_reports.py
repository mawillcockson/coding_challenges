#!/usr/bin/env python3
import csv
import sys
from collections import Counter
from pathlib import Path
from typing import Final, Iterable

RUN_COUNT_FILEPATH: Final = Path("/tmp/generate_report_runs.txt")


def check_run_count() -> int:
    "check and update run count"
    if not RUN_COUNT_FILEPATH.exists():
        RUN_COUNT_FILEPATH.touch()
        RUN_COUNT_FILEPATH.write_text("1")
        return 1

    runs = int(RUN_COUNT_FILEPATH.read_text().strip(), base=10) + 1
    RUN_COUNT_FILEPATH.write_text(str(runs))
    return runs


RUN_COUNT: Final = check_run_count()
DIALECT_NAME: Final = "empDialect"
csv.register_dialect(DIALECT_NAME, skipinitialspace=True, strict=True)
FILEPATH: Final = f"{Path(__file__).parent}/../data/employees.csv"
FILE_CONTENTS: Final = [
    {"Full Name": "Audrey Miller", "Username": "audrey", "Department": "Development"},
    {"Full Name": "Arden Garcia", "Username": "ardeng", "Department": "Sales"},
    {
        "Full Name": "Bailey Thomas",
        "Username": "baileyt",
        "Department": "Human Resources",
    },
    {
        "Full Name": "Blake Sousa",
        "Username": "sousa",
        "Department": "IT infrastructure",
    },
    {"Full Name": "Cameron Nguyen", "Username": "nguyen", "Department": "Marketing"},
    {"Full Name": "Charlie Grey", "Username": "greyc", "Department": "Development"},
    {
        "Full Name": "Chris Black",
        "Username": "chrisb",
        "Department": "User Experience Research",
    },
    {
        "Full Name": "Courtney Silva",
        "Username": "silva",
        "Department": "IT infrastructure",
    },
    {
        "Full Name": "Darcy Johnsonn",
        "Username": "darcy",
        "Department": "IT infrastructure",
    },
    {"Full Name": "Elliot Lamb", "Username": "elliotl", "Department": "Development"},
    {"Full Name": "Emery Halls", "Username": "halls", "Department": "Sales"},
    {"Full Name": "Flynn McMillan", "Username": "flynn", "Department": "Marketing"},
    {
        "Full Name": "Harley Klose",
        "Username": "harley",
        "Department": "Human Resources",
    },
    {
        "Full Name": "Jean May Coy",
        "Username": "jeanm",
        "Department": "Vendor operations",
    },
    {"Full Name": "Kay Stevens", "Username": "kstev", "Department": "Sales"},
    {
        "Full Name": "Lio Nelson",
        "Username": "lion",
        "Department": "User Experience Research",
    },
    {
        "Full Name": "Logan Tillas",
        "Username": "tillas",
        "Department": "Vendor operations",
    },
    {"Full Name": "Micah Lopes", "Username": "micah", "Department": "Development"},
    {"Full Name": "Sol Mansi", "Username": "solm", "Department": "IT infrastructure"},
]
REPORT_OUTPUT_FILEPATH: Final = f"{Path(__file__).parent}/../data/report.txt"


def write_csv(path: str) -> None:
    "creates the csv file with the example contents"
    with Path(path).open(mode="wt+", newline="") as f:
        writer = csv.DictWriter(
            f, dialect=DIALECT_NAME, fieldnames=FILE_CONTENTS[0].keys()
        )
        writer.writeheader()
        writer.writerows(FILE_CONTENTS)


def read_file(path: str) -> Iterable[dict[str, str]]:
    """
    reads the contents of the file, returning its contents as a list of
    dictionaries
    """
    with Path(path).open(mode="rt", newline="") as f:
        reader = csv.DictReader(f, dialect=DIALECT_NAME)
        for row in reader:
            yield dict(row)


def read_employees(csv_file_location: str) -> list[dict[str, str]]:
    return list(read_file(csv_file_location))


def process_data(employee_list: list[dict[str, str]]) -> dict[str, int]:
    "list how many members each department has"
    return dict(Counter(row["Department"] for row in employee_list))


def write_report(department_headcounts: dict[str, int], output_path: str) -> None:
    "write out the department head counts to a file, formatted"
    path = Path(output_path)
    path.touch()
    formatted = "\n".join(f"{k}:{v}" for k, v in department_headcounts.items())
    path.write_text(formatted)


if RUN_COUNT == 1:
    write_csv(FILEPATH)
employee_list = read_employees(FILEPATH)
if RUN_COUNT == 1:
    print(employee_list)
    sys.exit(0)
department_headcounts = process_data(employee_list)
if RUN_COUNT == 2:
    print(department_headcounts)
    sys.exit(0)
write_report(department_headcounts, REPORT_OUTPUT_FILEPATH)
