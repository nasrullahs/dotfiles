#!/usr/bin/env python3
import subprocess
import re
import sys

def get_package_temp():
    try:
        # Run `sensors` command
        output = subprocess.check_output(["sensors"], text=True)
    except Exception as e:
        return None

    # Look for "Package id 0" line
    for line in output.splitlines():
        if "Package id 0" in line:
            match = re.search(r"\+([0-9]+\.[0-9])°C", line)
            if match:
                return float(match.group(1))
    return None

temp = get_package_temp()

if temp is None:
    print("N/A")
    print("N/A")
    print("#FF0000")
    sys.exit(0)

# Pick color thresholds
if temp < 50:
    color = "#AAAAAA"  # green
elif temp < 70:
    color = "#FFFF00"  # yellow
else:
    color = "#FF0000"  # red

# i3blocks output format: line1, line2, line3(color)
print(f"{temp:.1f}°C")
print(f"{temp:.1f}°C")
print(color)
