//
// F# program to input a string and print out information
// about the # of vowels and digraphs in that string.
//
// Name: Travis Lee
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

//
// explode:
//
// Given a string s, explodes the string into a list of characters.
// Example: explode "apple" => ['a';'p';'p';'l';'e']
//
let explode (S:string) = 
  List.ofArray (S.ToCharArray())

//
// implode
//
// The opposite of explode --- given a list of characters, returns
// the list as a string. Example: implode ['t';'h';'e'] => "the"
//
let implode (L:char list) = 
  new string(List.toArray L)


let rec length L = 
  match L with
  | [] -> 0
  | hd::tl -> 1 + length tl


let rec numVowels L = 
  match L with
  | [] -> 0
  | hd::tl when hd = 'a' -> 1 + numVowels tl
  | hd::tl when hd = 'e' -> 1 + numVowels tl
  | hd::tl when hd = 'i' -> 1 + numVowels tl
  | hd::tl when hd = 'o' -> 1 + numVowels tl
  | hd::tl when hd = 'u' -> 1 + numVowels tl
  | hd::tl -> 0 + numVowels tl


[<EntryPoint>]
let main argv =
  printfn "Starting"
  printfn ""

  //
  // input string, output length and # of vowels:
  //
  printf("input> ")
  let input = System.Console.ReadLine()

  let L = explode input
  printfn "exploded: %A" L

  let len = length L
  printfn "length: %A" len

  let num = numVowels L
  printfn "vowels: %A" num

  //
  // TODO: print count of each vowel:
  //
  let rec count vowel L =
    match L with
    | [] -> 0
    | hd::tl when hd = vowel -> 1 + count vowel tl
    | hd::tl -> 0 + count vowel tl

  printfn "'a': %A" (count 'a' L)
  printfn "'e': %A" (count 'e' L)
  printfn "'i': %A" (count 'i' L)
  printfn "'o': %A" (count 'o' L)
  printfn "'u': %A" (count 'u' L)
  //
  // TODO: print number of digraphs, count of each:
  //


  let rec countDigraph digraph L =
    let c1 = List.head digraph
    let c2 = digraph.Tail.Head
    match L with
    | [] -> 0
    | hd::[] -> 0
    | hd::tl when (hd = c1) && (tl.Head = c2) -> 1 + countDigraph digraph tl
    | _::tl -> 0 + countDigraph digraph tl

  let aidigraph = ['a';'i']
  let aiResult = countDigraph aidigraph L
  let chdigraph = ['c';'h']
  let chResult = countDigraph chdigraph L
  let eadigraph = ['e';'a']
  let eaResult = countDigraph eadigraph L
  let iedigraph = ['i';'e']
  let ieResult = countDigraph iedigraph L
  let oudigraph = ['o';'u']
  let ouResult = countDigraph oudigraph L
  let phdigraph = ['p';'h']
  let phResult = countDigraph phdigraph L
  let shdigraph = ['s';'h']
  let shResult = countDigraph shdigraph L
  let thdigraph = ['t';'h']
  let thResult = countDigraph thdigraph L
  let whdigraph = ['w';'h']
  let whResult = countDigraph whdigraph L
  printfn "digraphs: %A" (aiResult + chResult + eaResult + ieResult + ouResult + phResult + shResult + thResult + whResult)

  printfn "'a','i': %A" (countDigraph aidigraph L)
  printfn "'c','h': %A" (countDigraph chdigraph L)
  printfn "'e','a': %A" (countDigraph eadigraph L)
  printfn "'i','e': %A" (countDigraph iedigraph L)
  printfn "'o','u': %A" (countDigraph oudigraph L)
  printfn "'p','h': %A" (countDigraph phdigraph L)
  printfn "'s','h': %A" (countDigraph shdigraph L)
  printfn "'t','h': %A" (countDigraph thdigraph L)
  printfn "'w','h': %A" (countDigraph whdigraph L) 

  //
  // done: implode list, print, and return
  //
  let S = implode L
  printfn "imploded: %A" S

  printfn ""
  printfn "Done"
  0  // return 0 => success, much like C++