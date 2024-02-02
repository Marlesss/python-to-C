# pytoc

Program to convert a python-like code to C

## Usage

1) Compile Python Parser via [happy](https://hackage.haskell.org/package/happy):
```shell
happy -i ./src/Python/Parser.y -o ./src/Python/Parser.hs
```
2) Run pytoc
```shell
stack run pytoc
```