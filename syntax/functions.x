# Create a function declaration (just the signature)

function add(x: int, y: int) -> int;

# Label parameters as keyword-specified with ''

function split(str: string, 'by' delimiter: char = "\n") -> List<string>;

# ... at the call site ...

split("1, 2, 3", by: ",")
  .for_each() |x| { int(x)! }
  .collect<List<int>>();

# Label parameters as mutable (take ownership)

function increment(mutable x: int);

# Create a function definition by adding a block at the end

function add(x: int, y: int) -> int {
  x + y
}

function split(str: string, 'by' delimiter: char = "\n") -> List<string> {
  let str_copy = str;
  
  function s(str: string, d: char) -> ('item' string, 'rem' Maybe<string>) {
    if let Some i = str.find(d) {
      (item: str[0..i], result: Some str[i..]) 
    } else {
      (item: str, result: None)
    }
  }

  let mutable res: List<string> = [];
  while let item, Some rem = s(str_copy, delimiter) {
    res.append(item);
    str_copy = rem
  }

  res
}

function increment(mutable x: int) {
  x++;
}