#!/usr/bin/env python3

# Import libraries
import csv
import re
from pathlib import Path
from typing import Final

DATA_FILEPATH: Final = Path(__file__).parent.parent / "data" / "user_emails.csv"
UPDATED_FILEPATH: Final = DATA_FILEPATH.parent / "updated_user_emails.csv"
DIALECT_NAME: Final = "empDialect"
csv.register_dialect(DIALECT_NAME, skipinitialspace=True, strict=True)
OLD_DOMAIN: Final = "abc.edu"
NEW_DOMAIN: Final = "xyz.edu"


def contains_domain(address: str, domain: str) -> bool:
    """Returns True if the email address contains the given domain,
    in the domain position, false if not."""
    return re.search(rf"[\w\.-]@{re.escape(domain)}", address) is not None


def replace_domain(address: str, old_domain: str, new_domain: str) -> str:
    """Replaces the old domain with the new domain in
    the received address."""
    return re.sub(rf"{re.escape(old_domain)}$", new_domain, address)


def main() -> None:
    """Processes the list of emails, replacing any instances of the
    old domain with the new domain."""
    with (
        DATA_FILEPATH.open(mode="rt", newline="") as old_f,
        UPDATED_FILEPATH.open(mode="w+", newline="") as new_f,
    ):
        reader = csv.DictReader(old_f, dialect=DIALECT_NAME)
        writer = csv.DictWriter(
            new_f, dialect=DIALECT_NAME, fieldnames=["Full Name", " Email Address"]
        )
        writer.writeheader()

        for row in reader:
            email_address = row["Email Address"]
            if contains_domain(email_address, OLD_DOMAIN):
                row["Email Address"] = replace_domain(
                    email_address, OLD_DOMAIN, NEW_DOMAIN
                )
            row[" Email Address"] = " " + row["Email Address"]
            del row["Email Address"]
            writer.writerow(row)


if __name__ == "__main__":
    main()
