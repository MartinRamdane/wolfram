# Wolfram
The goal of this project is to implement Wolfram’s elementary cellular automaton in the terminal.

## Usage
```
USAGE:
    ./wolfram [OPTIONS]
```

## Options
```
OPTIONS:
    –rule : the ruleset to use (no default value, mandatory)
    –start : the generation number at which to start the display. The default value is 0.
    –lines : the number of lines to display. When omitted, the program never stops.
    –window : the number of cells to display on each line (line width). If even,
    the central cell is displayed in the next cell on the right. The default value is 80.
    –move : a translation to apply on the window. If negative, the window is translated to the left.
    If positive, it’s translated to the right.
```

## Examples
```
./wolfram --rule 30 --lines 20
./wolfram --rule 30 --lines 20 --window 40
./wolfram --rule 30 --lines 20 --window 40 --move -10
```
