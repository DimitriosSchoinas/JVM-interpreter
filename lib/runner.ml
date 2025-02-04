(* Tipos diferentes de instruções *)
type instruction =
  | PUSH of int  (* Empurra um valor inteiro para o topo da pilha *)
  | POP          (* Remove o valor do topo da pilha *)
  | DUP          (* Duplica o valor do topo da pilha *)
  | SWP          (* Troca os dois valores do topo da pilha *)
  | OVER         (* Duplica o segundo valor da pilha e coloca no topo *)
  | ADD          (* Soma os dois valores do topo da pilha *)
  | SUB          (* Subtrai o topo da pilha do segundo valor *)
  | MUL          (* Multiplica os dois valores do topo da pilha *)
  | DIV          (* Divide o segundo valor pelo topo da pilha *)
  | CMP          (* Compara os dois valores do topo da pilha e empurra 1 se iguais, 0 caso contrário *)
  | JMP of string (* Salta para a label especificada *)
  | JZ of string  (* Salta para a label se o valor no topo da pilha for zero *)
  | JNZ of string (* Salta para a label se o valor no topo da pilha não for zero *)
  | RETURN       (* Finaliza a execução e retorna o valor do topo da pilha *)

(* Módulo para mapear strings para valores *)
module StringMap = Map.Make(String)

(* Função responsável por modificar a pilha de acordo com a instrução recebida *)
let execute_instruction stack instruction =
  let result = match instruction with
    | PUSH n -> n :: stack  
    | POP -> (match stack with [] -> failwith "Unexpected empty stack" | _ :: t -> t) 
    | DUP -> (match stack with [] -> failwith "Unexpected empty stack" | n::_ -> n::stack) 
    | SWP -> (match stack with [] | [_] -> failwith "Unexpected empty stack" | x :: y :: t -> y :: x :: t) 
    | OVER -> (match stack with [] | [_] -> failwith "Unexpected empty stack" | x :: y :: t -> y :: x :: y :: t) 
    | ADD -> (match stack with [] | [_] -> failwith "Unexpected empty stack" | x :: y :: t -> (x + y) :: t) 
    | SUB -> (match stack with [] | [_] -> failwith "Unexpected empty stack" | x :: y :: t -> (y - x) :: t) 
    | MUL -> (match stack with [] | [_] -> failwith "Unexpected empty stack" | x :: y :: t -> (x * y) :: t) 
    | DIV -> (match stack with [] | [_] -> failwith "Unexpected empty stack" | x :: y :: t -> if x = 0 then failwith "Division by zero" else (y / x) :: t) 
    | CMP -> (match stack with [] | [_] -> failwith "Unexpected empty stack" | x :: y :: t -> if x = y then 1 :: t else 0 :: t) 
    | JMP _ | JZ _ | JNZ _ | RETURN -> stack  
  in
  result

(* Interpretador do programa *)
let rec interpret_program first_map current_map current_label stack =
  match StringMap.find_opt current_label current_map with
  | None -> failwith ("Label not found:"^current_label^"")  (* Falha se a label não for encontrada *)
  | Some instructions ->
      match instructions with
      | [] -> failwith "No return instruction"  (* Falha se não houver instruções restantes *)
      | instruction :: rest ->
          match instruction with
          | RETURN -> (match stack with [] -> failwith "Unexpected empty stack" | [x] -> x | _ -> failwith "Stack not empty") 
          | JMP lbl -> interpret_program first_map first_map lbl stack  
          | JZ lbl -> (match stack with [] -> failwith "Unexpected empty stack" | hd :: tl -> if hd = 0 then interpret_program first_map first_map lbl tl else interpret_program first_map(StringMap.add current_label rest current_map) current_label tl)  
          | JNZ lbl -> (match stack with [] -> failwith "Unexpected empty stack" | hd :: tl -> if hd <> 0 then interpret_program first_map first_map lbl tl else interpret_program first_map (StringMap.add current_label rest current_map) current_label tl)  
          | _ -> interpret_program first_map (StringMap.add current_label rest current_map) current_label (execute_instruction stack instruction)  (* Executa a instrução e continua *)

(* Função para converter uma string em uma instrução *)
let parse_instruction str =
  let trimmed_str = String.trim str in
  match String.split_on_char ' ' trimmed_str with
  | ["PUSH"; n] -> PUSH (int_of_string n)
  | ["POP"] -> POP
  | ["DUP"] -> DUP
  | ["SWP"] -> SWP
  | ["OVER"] -> OVER
  | ["ADD"] -> ADD
  | ["SUB"] -> SUB
  | ["MUL"] -> MUL
  | ["DIV"] -> DIV
  | ["CMP"] -> CMP
  | ["JMP"; lbl] -> JMP lbl
  | ["JZ"; lbl] -> JZ lbl
  | ["JNZ"; lbl] -> JNZ lbl
  | ["RETURN"] -> RETURN
  | _ -> failwith "Invalid instruction"

(* Função para verificar se a instrução é um JMP *)
let check_if_is_jmp instr =
  match instr with
  | PUSH _ | POP | DUP | SWP | OVER | ADD | SUB | MUL | DIV | CMP | JZ _ | JNZ _ | RETURN -> false
  | JMP _ -> true

(* Função para verificar se o programa começa com uma label *)
let check_start_label program =
  match program with
  | [] -> false
  | head::_ -> if String.contains head ':' = false then failwith "Expecting label" else true

(* Função principal para executar o programa *)
let run (program : string list) (label : string) =
  if check_start_label program = false then failwith "Label not found:Start" else
    (* Função para construir o mapa que age como blocos*)
  let construir_blocos program =
    let initialMap = StringMap.empty in
    let rec construir_blocos_aux program current_map current_label current_instructions =
      match program with
      | [] -> (
          if current_label <> "" then
            StringMap.add current_label (List.rev current_instructions) current_map
          else
            current_map
        )
      | head :: tail ->
        let test = String.split_on_char ':' head in
        if List.length test > 2 then failwith "Invalid line" else
          if String.contains head ':' then
            let lbl = String.sub head 0 (String.index head ':') in
            let instruction = String.sub head ((String.index head ':') + 1) ((String.length head) - (String.index head ':') - 1) |> String.trim in
            if lbl = "Start" then
              construir_blocos_aux tail (StringMap.add lbl (List.rev current_instructions) current_map) lbl [parse_instruction instruction]
            else if List.exists (fun instr -> match instr with JMP _ -> true | RETURN -> true  | _ -> false) current_instructions then
              construir_blocos_aux tail (StringMap.add current_label (List.rev current_instructions) current_map) lbl [parse_instruction instruction]
            else
              construir_blocos_aux tail current_map current_label (parse_instruction instruction :: current_instructions)
          else
            let instruction = parse_instruction head in
            if check_if_is_jmp instruction = true then match tail with
              | head :: _ ->
                if String.contains head ':' = false then failwith "Expecting label" 
                else construir_blocos_aux tail current_map current_label (instruction :: current_instructions)
              | [] -> failwith "Expecting label"
              else construir_blocos_aux tail current_map current_label (instruction :: current_instructions)
    in
    construir_blocos_aux program initialMap "" []
  in 
  let instruction_map = construir_blocos program in
  interpret_program instruction_map instruction_map label []
