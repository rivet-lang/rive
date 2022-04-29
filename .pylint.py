import sys, pylint

pylint.run_pylint(["src", "-f", "colorized", "-d", "convention"])
