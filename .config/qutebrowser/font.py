#!/usr/bin/env python3

# this script checks for fonts

import pygame

# Initialize pygame
pygame.init()

# Get the available system fonts
fonts = pygame.font.get_fonts()

# Print the available fonts
for font in fonts:
    print(font)
