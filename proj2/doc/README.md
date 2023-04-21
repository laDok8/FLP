# Logic Programming project - FLP
___

year: 2023

Author : Ladislav Dokoupil, xdokou14

## Description
Non deterministic finite automata simulator

## Usage
```
$ ./flp22-fun < <input_file>
```
or using stdin

## Example input format
```
State TapeSymbol NewState {Left|Right|NewTapeSymbol}
State TapeSymbol NewState {Left|Right|NewTapeSymbol}
.
.
.
InputWord
```
note that the input word is assumed to be last line

## Testing
there are few test files in the tests directory that can be run via
```
make && make run
```
