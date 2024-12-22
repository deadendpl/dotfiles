#!/usr/bin/env python

import subprocess
import argparse

# Set up the argument parser
parser = argparse.ArgumentParser(
  prog = "playerctl-volume",
  description = "Increase or decrease volume using playerctl"
)

# Argument for increasing (1) or decreasing (0) volume
parser.add_argument(
  "change_direction",
  nargs = "?",
  type = int,
  choices = [0, 1],
  default = 1,
  help = "Set to 1 to increase volume (default) or 0 to decrease volume"
)

# Optional float argument for volume step
parser.add_argument(
  "number",
  nargs = "?",
  type = float,
  default = 0.1,
  help = "Amount to change the volume (default: 0.1)"
)

# Parse the arguments
args = parser.parse_args()

# Get current volume
current_volume = float(subprocess.run(["playerctl", "volume"], capture_output = True).stdout[:-1:])

if args.change_direction == 1 and current_volume == 1:
  subprocess.run(["notify-send", "Max volume"])
  exit()
elif args.change_direction == 0 and current_volume == 0:
  subprocess.run(["notify-send", "Min volume"])
  exit()

if args.change_direction == 0:
  symbol = "-"
else:
  symbol = "+"

# Update the volume
subprocess.run(["playerctl", "volume", str(args.number) + symbol])
