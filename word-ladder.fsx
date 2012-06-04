let words = System.IO.File.ReadAllLines("four-char-dictionary.txt")

let isWordInDict word =
  words |> Seq.exists (fun w -> w = word)

let filterToDict words =
  words |> List.filter isWordInDict

let filterNotSeen (seen : Set<string>) words =
  words |> List.filter (fun w -> not(seen.Contains(w)))

let findChildren (word : string) = [
  let generateCandidates prefix char postfix =
    ['a'..'z'] |> List.filter (fun i -> i <> char)
      |> List.map (fun l -> prefix + l.ToString() + postfix)

  yield! generateCandidates "" (word.[0]) (word.[1..])
  for i in 1..3 do
    yield! generateCandidates (word.[0..(i-1)]) (word.[i]) (word.[(i+1)..])
]

let findValidUniqueChildren seen = findChildren >> filterNotSeen seen >> filterToDict

type node = string List * string // parent List * word

let rec buildNodes (words : string List) (parents : string List) (nodes : node List) = 
  match words with
    | [] -> nodes
    | w :: tail -> buildNodes tail parents ((parents, w) :: nodes)

let rec findLadderWorker (searchNodes : node List) (endword : string) (seen : Set<string>) =
  let queuechildren word parents searchNodes =
    let children = findValidUniqueChildren seen word
    let newparents = word :: parents
    let childnodes = buildNodes children newparents []
    let newSearchNodes = searchNodes @ childnodes
    findLadderWorker newSearchNodes endword (seen + (Set.ofList children))
    
  let testnode (node : node) (searchNodes : node List) =
    match node with
      | parents, word when word = endword -> word :: parents |> List.rev
      | parents, word -> queuechildren word parents searchNodes

  match searchNodes with
    | [] -> []
    | node :: others -> testnode node others
  
let findLadder startword endword =
  if startword = endword then
    [endword]
  else
    let children = findValidUniqueChildren Set.empty startword
    let seen = Set.ofList (startword :: children)
    let startNodes = buildNodes children [startword] []
    findLadderWorker startNodes endword seen


let ladder = findLadder "blue" "suck"
printfn "%A" ladder
